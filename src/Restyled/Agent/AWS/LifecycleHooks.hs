module Restyled.Agent.AWS.LifecycleHooks
    ( LifecycleHookActionResult(..)
    , withPendingLifecycleHook
    , withTerminatingLifecycleHook
    ) where

import Restyled.Agent.Prelude

import qualified Amazonka.AutoScaling.CompleteLifecycleAction as AWS
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
       , MonadResource m
       , MonadLogger m
       , MonadReader env m
       , HasOptions env
       , HasAWS env
       )
    => Text
    -> m a
    -> m (Maybe a)
withPendingLifecycleHook = withLifecycleHook InstanceLaunching

withTerminatingLifecycleHook
    :: ( MonadUnliftIO m
       , MonadResource m
       , MonadLogger m
       , MonadReader env m
       , HasOptions env
       , HasAWS env
       )
    => Text
    -> m a
    -> m (Maybe a)
withTerminatingLifecycleHook = withLifecycleHook InstanceTerminating

withLifecycleHook
    :: ( MonadUnliftIO m
       , MonadResource m
       , MonadLogger m
       , MonadReader env m
       , HasOptions env
       , HasAWS env
       )
    => LifecycleTransition
    -> Text
    -> m a
    -> m (Maybe a)
withLifecycleHook transition queue act = do
    Options {..} <- view optionsL

    logInfo $ "Awaiting " <> displayT transition
    decodedMessage <- awaitDecodedMessage queue $ predicate oInstance

    let
        finalize action = do
            completeLifecycleAction (dmBody decodedMessage) action
            deleteDecodedMessage decodedMessage

    eResult <- tryAny act

    case eResult of
        Left ex -> do
            logError $ "Error acting on " <> displayT transition <> ": " <> pack
                (displayException ex)
            Nothing <$ finalize ActionResultAbandon
        Right result -> Just result <$ finalize ActionResultContinue
  where
    predicate instanceId LifecycleHookDetails {..} =
        lhdTransition == transition && lhdInstanceId == instanceId

data LifecycleHookActionResult
    = ActionResultContinue
    | ActionResultAbandon

completeLifecycleAction
    :: ( MonadIO m
       , MonadResource m
       , MonadLogger m
       , MonadReader env m
       , HasAWS env
       )
    => LifecycleHookDetails
    -> LifecycleHookActionResult
    -> m ()
completeLifecycleAction LifecycleHookDetails {..} action = do
    logInfo $ "Completing " <> displayT lhdTransition <> " with " <> pack
        (show result)
    resp <-
        send
        $ AWS.newCompleteLifecycleAction lhdHookName lhdScalingGroupName result
        & (AWS.completeLifecycleAction_instanceId ?~ lhdInstanceId)
        & (AWS.completeLifecycleAction_lifecycleActionToken
          ?~ lhdLifecycleActionToken
          )
    logDebug $ "Status: " <> pack
        (show $ resp ^. AWS.completeLifecycleActionResponse_httpStatus)
  where
    result = case action of
        ActionResultContinue -> "CONTINUE"
        ActionResultAbandon -> "ABANDON"
