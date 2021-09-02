module Restyled.Agent.AWS.LifecycleHooks
    ( LifecycleHookActionResult(..)
    , withPendingLifecycleHook
    , withTerminatingLifecycleHook
    ) where

import RIO

import Control.Lens ((?~))
import Data.Aeson
import qualified Network.AWS.AutoScaling.CompleteLifecycleAction as AWS
import RIO.Text (unpack)
import Restyled.Agent.AWS
import Restyled.Agent.AWS.SQS.DecodedMessage
import Restyled.Agent.Options

data LifecycleTransition
    = InstanceLaunching
    | InstanceTerminating
    deriving stock (Eq, Show)

instance Display LifecycleTransition where
    display = \case
        InstanceLaunching -> "LAUNCHING"
        InstanceTerminating -> "TERMINATING"

instance FromJSON LifecycleTransition where
    parseJSON = withText "LifecycleTransition" $ \case
        "autoscaling:EC2_INSTANCE_LAUNCHING" -> pure InstanceLaunching
        "autoscaling:EC2_INSTANCE_TERMINATING" -> pure InstanceTerminating
        x -> fail $ "Unexpected Transition: " <> unpack x

data LifecycleHookDetails = LifecycleHookDetails
    { lhdHookName :: Text
    , lhdTransition :: LifecycleTransition
    , lhdScalingGroupName :: Text
    , lhdInstanceId :: Text
    , lhdLifecycleActionToken :: Text
    }
    deriving stock (Eq, Show)

-- brittany-disable-next-binding

instance FromJSON LifecycleHookDetails where
    parseJSON = withObject "LifecycleHookDetails" $ \o -> LifecycleHookDetails
        <$> o .: "LifecycleHookName"
        <*> o .: "LifecycleTransition"
        <*> o .: "AutoScalingGroupName"
        <*> o .: "EC2InstanceId"
        <*> o .: "LifecycleActionToken"

withPendingLifecycleHook
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HasOptions env
       , HasLogFunc env
       , HasAWS env
       )
    => m a
    -> m (Maybe a)
withPendingLifecycleHook = withLifecycleHook InstanceLaunching

withTerminatingLifecycleHook
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HasOptions env
       , HasLogFunc env
       , HasAWS env
       )
    => m a
    -> m (Maybe a)
withTerminatingLifecycleHook = withLifecycleHook InstanceTerminating

withLifecycleHook
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HasOptions env
       , HasLogFunc env
       , HasAWS env
       )
    => LifecycleTransition
    -> m a
    -> m (Maybe a)
withLifecycleHook transition act = do
    Options {..} <- view optionsL

    case (,) <$> oInstance <*> oLifecycleQueueUrl of
        Nothing -> do
            logInfo
                $ "No Lifecycle hooks configured, executing "
                <> display transition
                <> " immediately"
            Just <$> act
        Just (inst, queue) -> do
            logInfo
                $ "Awaiting "
                <> display transition
                <> " hook for "
                <> display inst
                <> " on "
                <> display queue
            decodedMessage <- awaitDecodedMessage queue $ predicate inst

            let
                finalize action = do
                    completeLifecycleAction (dmBody decodedMessage) action
                    deleteDecodedMessage decodedMessage

            eResult <- tryAny act

            case eResult of
                Left ex -> do
                    logError
                        $ "Error acting on "
                        <> display transition
                        <> " Lifecycle hook: "
                        <> displayShow ex
                    Nothing <$ finalize ActionResultAbandon
                Right result -> Just result <$ finalize ActionResultContinue
  where
    predicate instanceId LifecycleHookDetails {..} =
        lhdTransition == transition && lhdInstanceId == instanceId

data LifecycleHookActionResult
    = ActionResultContinue
    | ActionResultAbandon

completeLifecycleAction
    :: (MonadIO m, MonadReader env m, HasLogFunc env, HasAWS env)
    => LifecycleHookDetails
    -> LifecycleHookActionResult
    -> m ()
completeLifecycleAction LifecycleHookDetails {..} action = do
    logInfo
        $ "Completing LifecycleHook with token "
        <> display lhdLifecycleActionToken
        <> " and result "
        <> displayShow result
    resp <-
        runAWS
        $ AWS.completeLifecycleAction lhdHookName lhdScalingGroupName result
        & (AWS.claInstanceId ?~ lhdInstanceId)
        & (AWS.claLifecycleActionToken ?~ lhdLifecycleActionToken)
    logDebug $ "Status: " <> displayShow (resp ^. AWS.clarsResponseStatus)
  where
    result = case action of
        ActionResultContinue -> "CONTINUE"
        ActionResultAbandon -> "ABANDON"
