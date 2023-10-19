module Restyled.Agent
  ( Agent
  , bootAgent
  , shutdownAgent
  ) where

import Restyled.Agent.Prelude

import qualified Control.Immortal as Immortal
import Data.Aeson.KeyMap (KeyMap)
import qualified Data.Aeson.KeyMap as KeyMap
import Restyled.Agent.Options
import Restyled.Agent.Queue
import Restyled.Agent.Redis
import Restyled.Agent.Restyler (processPullRequestEvent)

newtype Agent = Agent
  { _unAgent :: [Thread]
  }

data Thread = Thread
  { label :: Text
  , thread :: Immortal.Thread
  }

bootAgent
  :: ( MonadUnliftIO m
     , MonadMask m
     , MonadLogger m
     , MonadReader env m
     , HasOptions env
     , HasRedis env
     )
  => m Agent
bootAgent = do
  size <- views optionsL (.restylerPoolSize)
  Agent <$> traverse bootAgentThread [1 .. size]

bootAgentThread
  :: ( MonadUnliftIO m
     , MonadMask m
     , MonadLogger m
     , MonadReader env m
     , HasOptions env
     , HasRedis env
     )
  => Natural
  -> m Thread
bootAgentThread n = withThreadContext context $ do
  logInfo "creating"

  threadContext <- extendThreadContext context <$> myThreadContext
  thread <- Immortal.create $ \t ->
    Immortal.onUnexpectedFinish
      t
      (withThreadContext threadContext . logUnexpectedFinish)
      (withThreadContext threadContext loop)
  pure $ Thread label thread
 where
  label = pack $ "restyle-" <> show n
  context = ["thread" .= label]

  loop
    :: ( MonadUnliftIO m
       , MonadMask m
       , MonadLogger m
       , MonadReader env m
       , HasOptions env
       , HasRedis env
       )
    => m ()
  loop = do
    mEvent <- awaitWebhook
    traverse_ processPullRequestEvent mEvent

  logUnexpectedFinish
    :: (MonadLogger m, Exception ex) => Either ex () -> m ()
  logUnexpectedFinish = \case
    Left ex ->
      logError
        $ "Unexpected finish"
        :# ["exception" .= displayException ex]
    Right () -> pure ()

extendThreadContext :: [Pair] -> KeyMap Value -> [Pair]
extendThreadContext ps = KeyMap.toList . KeyMap.union (KeyMap.fromList ps)

shutdownAgent :: (MonadUnliftIO m, MonadLogger m) => Agent -> m ()
shutdownAgent (Agent threads) = do
  as <- traverse shutdownAgentThread threads
  traverse_ wait as

shutdownAgentThread
  :: (MonadUnliftIO m, MonadLogger m) => Thread -> m (Async ())
shutdownAgentThread thread = do
  logInfo $ "mortalizing" :# ["thread" .= thread.label]
  liftIO $ Immortal.mortalize thread.thread

  async $ do
    liftIO $ Immortal.wait thread.thread
    logInfo $ "done" :# ["thread" .= thread.label]
