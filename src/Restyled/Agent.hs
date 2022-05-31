module Restyled.Agent
    ( Agent
    , bootAgent
    , shutdownAgent
    ) where

import Restyled.Agent.Prelude

import qualified Control.Immortal as Immortal
import Restyled.Agent.Options
import Restyled.Agent.Queue
import Restyled.Agent.Redis
import Restyled.Agent.Restyler (processPullRequestEvent)

newtype Agent = Agent
    { _unAgent :: [Thread]
    }

data Thread = Thread
    { tLabel :: Text
    , tThread :: Immortal.Thread
    }

bootAgent
    :: ( MonadUnliftIO m
       , MonadLogger m
       , MonadReader env m
       , HasOptions env
       , HasRedis env
       )
    => m Agent
bootAgent = do
    size <- oRestylerPoolSize <$> view optionsL
    Agent <$> traverse bootAgentThread [1 .. size]

bootAgentThread
    :: ( MonadUnliftIO m
       , MonadLogger m
       , MonadReader env m
       , HasOptions env
       , HasRedis env
       )
    => Natural
    -> m Thread
bootAgentThread n = do
    logInfo "creating"
    thread <- Immortal.create
        $ \t -> Immortal.onUnexpectedFinish t logUnexpectedFinish loop
    pure $ Thread label thread
  where
    label = pack $ "restyle-" <> show n

    loop
        :: ( MonadUnliftIO m
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
            logError $ "Unexpected finish: " <> pack (displayException ex)
        Right () -> pure ()

shutdownAgent :: (MonadUnliftIO m, MonadLogger m) => Agent -> m ()
shutdownAgent (Agent threads) = do
    as <- traverse shutdownAgentThread threads
    traverse_ wait as

shutdownAgentThread
    :: (MonadUnliftIO m, MonadLogger m) => Thread -> m (Async ())
shutdownAgentThread Thread {..} = do
    logInfo "mortalizing"
    liftIO $ Immortal.mortalize tThread

    async $ do
        liftIO $ Immortal.wait tThread
        logInfo "done"
