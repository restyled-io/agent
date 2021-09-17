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
    Thread label <$> Immortal.create (\_ -> webhookLoop label)
    where label = pack $ "restyle-" <> show n

webhookLoop
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HasLogFunc env
       , HasProcessContext env
       , HasOptions env
       , HasRedis env
       )
    => LogSource
    -> m ()
webhookLoop label = do
    mEvent <- awaitWebhook
    traverse_ (processPullRequestEvent label) mEvent

shutdownAgent
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HasLogFunc env
       , HasProcessContext env
       , HasOptions env
       , HasRedis env
       )
    => Agent
    -> m ()
shutdownAgent (Agent threads) = do
    mDelay <- oTerminationDelay <$> view optionsL
    for_ mDelay $ \delay -> do
        logInfo $ "Delay termination " <> displayShow delay <> "s"
        threadDelay $ fromIntegral delay * 1000000
    traverse_ shutdownAgentThread threads

shutdownAgentThread
    :: (MonadIO m, MonadReader env m, HasLogFunc env) => Thread -> m ()
shutdownAgentThread Thread {..} = do
    logInfoS tLabel "mortalizing"
    liftIO $ Immortal.mortalize tThread
