module Restyled.Agent
    ( Agent
    , bootAgent
    , shutdownAgent
    ) where

import RIO

import qualified Control.Immortal as Immortal
import RIO.Process
import RIO.Text (pack)
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
       , MonadReader env m
       , HasLogFunc env
       , HasProcessContext env
       , HasOptions env
       , HasRedis env
       )
    => m Agent
bootAgent = do
    size <- oRestylerPoolSize <$> view optionsL
    Agent <$> traverse bootAgentThread [1 .. size]

bootAgentThread
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HasLogFunc env
       , HasProcessContext env
       , HasOptions env
       , HasRedis env
       )
    => Natural
    -> m Thread
bootAgentThread n = do
    logInfoS label "creating"
    thread <- Immortal.create
        $ \t -> Immortal.onUnexpectedFinish t logUnexpectedFinish loop
    pure $ Thread label thread
  where
    label = pack $ "restyle-" <> show n

    loop = do
        mEvent <- awaitWebhook
        traverse_ (processPullRequestEvent label) mEvent

    logUnexpectedFinish = \case
        Left ex -> logErrorS label $ "Unexpected finish: " <> displayShow ex
        Right () -> pure ()

shutdownAgent
    :: (MonadUnliftIO m, MonadReader env m, HasLogFunc env) => Agent -> m ()
shutdownAgent (Agent threads) = do
    as <- traverse shutdownAgentThread threads
    traverse_ wait as

shutdownAgentThread
    :: (MonadUnliftIO m, MonadReader env m, HasLogFunc env)
    => Thread
    -> m (Async ())
shutdownAgentThread Thread {..} = do
    logInfoS tLabel "mortalizing"
    liftIO $ Immortal.mortalize tThread

    async $ do
        liftIO $ Immortal.wait tThread
        logInfoS tLabel "done"
