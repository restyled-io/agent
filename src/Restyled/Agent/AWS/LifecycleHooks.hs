module Restyled.Agent.AWS.LifecycleHooks
  ( LifecycleHookActionResult (..)
  , withPendingLifecycleHook
  , withTerminatingLifecycleHook
  ) where

import Restyled.Agent.Prelude

import Amazonka.AutoScaling.CompleteLifecycleAction
import Restyled.Agent.AWS
import Restyled.Agent.AWS.SQS.DecodedMessage
import Restyled.Agent.Options

data LifecycleTransition
  = InstanceLaunching
  | InstanceTerminating
  deriving stock (Eq, Show)

instance ToJSON LifecycleTransition where
  toJSON = \case
    InstanceLaunching -> String "LAUNCHING"
    InstanceTerminating -> String "TERMINATING"

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

instance FromJSON LifecycleHookDetails where
  parseJSON = withObject "LifecycleHookDetails" $ \o ->
    LifecycleHookDetails
      <$> (o .: "LifecycleHookName")
      <*> (o .: "LifecycleTransition")
      <*> (o .: "AutoScalingGroupName")
      <*> (o .: "EC2InstanceId")
      <*> (o .: "LifecycleActionToken")

withPendingLifecycleHook
  :: ( MonadUnliftIO m
     , MonadAWS m
     , MonadLogger m
     , MonadReader env m
     , HasOptions env
     )
  => Text
  -> m a
  -> m (Maybe a)
withPendingLifecycleHook = withLifecycleHook InstanceLaunching

withTerminatingLifecycleHook
  :: ( MonadUnliftIO m
     , MonadAWS m
     , MonadLogger m
     , MonadReader env m
     , HasOptions env
     )
  => Text
  -> m a
  -> m (Maybe a)
withTerminatingLifecycleHook = withLifecycleHook InstanceTerminating

withLifecycleHook
  :: ( MonadUnliftIO m
     , MonadAWS m
     , MonadLogger m
     , MonadReader env m
     , HasOptions env
     )
  => LifecycleTransition
  -> Text
  -> m a
  -> m (Maybe a)
withLifecycleHook transition queue act = do
  Options {..} <- view optionsL

  logInfo $ "Awaiting" :# ["transition" .= transition]
  decodedMessage <- awaitDecodedMessage queue $ predicate oInstance

  let finalize action = do
        completeLifecycleAction (dmBody decodedMessage) action
        deleteDecodedMessage decodedMessage

  eResult <- tryAny act

  case eResult of
    Left ex -> do
      logError
        $ "Error"
        :# [ "transition" .= transition
           , "exception" .= displayException ex
           ]
      Nothing <$ finalize ActionResultAbandon
    Right result -> Just result <$ finalize ActionResultContinue
 where
  predicate instanceId LifecycleHookDetails {..} =
    lhdTransition == transition && lhdInstanceId == instanceId

data LifecycleHookActionResult
  = ActionResultContinue
  | ActionResultAbandon

completeLifecycleAction
  :: (MonadIO m, MonadAWS m, MonadLogger m)
  => LifecycleHookDetails
  -> LifecycleHookActionResult
  -> m ()
completeLifecycleAction LifecycleHookDetails {..} action = do
  logInfo
    $ "Completing"
    :# ["transition" .= lhdTransition, "result" .= result]
  resp <-
    send
      $ newCompleteLifecycleAction lhdHookName lhdScalingGroupName result
      & (completeLifecycleAction_instanceId ?~ lhdInstanceId)
      & ( completeLifecycleAction_lifecycleActionToken
            ?~ lhdLifecycleActionToken
        )
  logDebug
    $ "Response"
    :# [ "status"
          .= show @String
            (resp ^. completeLifecycleActionResponse_httpStatus)
       ]
 where
  result = case action of
    ActionResultContinue -> "CONTINUE"
    ActionResultAbandon -> "ABANDON"
