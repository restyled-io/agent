{-# LANGUAGE NamedFieldPuns #-}

module Restyled.Agent
    ( Agent
    , createAgent
    , terminateAgent
    , monitorAgent
    , runAgent
    )
where

import RIO hiding (timeout)

import Control.Lens (_2)
import Data.Aeson
import Data.Functor.Syntax ((<$$>))
import Restyled.Agent.Options
import Restyled.Agent.Redis
import Restyled.Agent.Restyler
import Restyled.Agent.ThreadPool
import RIO.Process

data Agent = Agent
    { agentState :: IORef AgentState
    , agentPool :: Pool
    }

data AgentState
    = AgentRunning
    | AgentStopping
    | AgentStopped
    deriving stock Show

instance Display AgentState where
    display = displayShow

createAgent :: (MonadIO m, MonadReader env m, HasOptions env) => m Agent
createAgent = do
    size <- oRestylerPoolSize <$> view optionsL
    Agent <$> newIORef AgentRunning <*> createPool size

terminateAgent
    :: (MonadIO m, MonadReader env m, HasLogFunc env, HasOptions env)
    => Agent
    -> m ()
terminateAgent Agent { agentState } = do
    delay <- oTerminationDelay <$> view optionsL
    logInfo $ "Delaying termination " <> displayShow delay <> " second(s)"
    threadDelay $ fromIntegral delay * 1000000

    logInfo "Stopping Agent"
    writeIORef agentState AgentStopping

monitorAgent
    :: (MonadIO m, MonadReader env m, HasLogFunc env, HasOptions env)
    => Agent
    -> m ()
monitorAgent Agent { agentState } = forever $ do
    state <- readIORef agentState
    logInfo $ "Agent state: " <> display state
    interval <- oMonitorInterval <$> view optionsL
    threadDelay $ fromIntegral interval * 1000000

runAgent
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HasLogFunc env
       , HasProcessContext env
       , HasOptions env
       , HasRedis env
       )
    => Agent
    -> m ()
runAgent agent@Agent { agentState, agentPool } = do
    state <- readIORef agentState

    case state of
        AgentRunning -> do
            withWebhook $ withThread agentPool . processPullRequestEvent
            runAgent agent
        AgentStopping -> writeIORef agentState AgentStopped
        AgentStopped -> pure ()

withWebhook
    :: ( MonadIO m
       , MonadReader env m
       , HasLogFunc env
       , HasOptions env
       , HasRedis env
       , FromJSON a
       )
    => (a -> m ())
    -> m ()
withWebhook f = do
    timeout <- oWebhookTimeout <$> view optionsL
    queueName <- oRestyleQueue <$> view optionsL
    eresult <- runRedis $ brpop [queueName] $ fromIntegral timeout

    case over _2 eitherDecodeStrict <$$> eresult of
        Left err -> logWarn $ "Error Reply from Redis: " <> displayShow err
        Right Nothing -> pure ()
        Right (Just (_, Left err)) ->
            logWarn $ "Unexpected JSON: " <> fromString err
        Right (Just (_, Right event)) -> f event
