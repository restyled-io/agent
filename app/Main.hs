module Main
    ( main
    )
where

import RIO

import Restyled.Agent.App
import Restyled.Agent.AWS
import Restyled.Agent.AWS.LifecycleHooks
import Restyled.Agent.Options
import Restyled.Agent.RestyleMachine
import Restyled.Agent.Restylers
import Restyled.Agent.Statsd (HasStatsClient)
import qualified Restyled.Agent.Statsd as Statsd
import RIO.Process

main :: IO ()
main = do
    options <- parseOptions
    withApp options $ do
        Statsd.increment "agent.booted" []
        initializeOnPending >>= awaitTermination
        Statsd.increment "agent.terminated" []

initializeOnPending
    :: ( MonadUnliftIO m
       , MonadThrow m
       , MonadReader env m
       , MonadThrow m
       , HasLogFunc env
       , HasOptions env
       , HasAWS env
       , HasProcessContext env
       , HasStatsClient env
       )
    => m RestyleMachine
initializeOnPending = withPendingLifecycleHook $ do
    machine <- createRestyleMachine
    logInfo $ "Created machine: " <> display machine

    if restyleMachineEnabled machine
        then do
            Statsd.increment "agent.registered" []
            pure (ActionResultContinue, machine)
        else do
            logWarn "Created Machine was not enabled"
            pure (ActionResultAbandon, machine)

awaitTermination
    :: ( MonadUnliftIO m
       , MonadThrow m
       , MonadReader env m
       , MonadThrow m
       , HasLogFunc env
       , HasOptions env
       , HasAWS env
       , HasProcessContext env
       , HasStatsClient env
       )
    => RestyleMachine
    -> m ()
awaitTermination machine = withTerminatingLifecycleHook $ do
    handleAny (logError . displayShow) $ do
        logInfo "Draining machine"
        disableRestyleMachine machine
        awaitRestylers
        deleteRestyleMachine machine
    pure (ActionResultContinue, ())
