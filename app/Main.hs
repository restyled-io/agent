module Main
    ( main
    )
where

import RIO

import Restyled.Agent.AWS
import Restyled.Agent.AWS.LifecycleHooks
import Restyled.Agent.App
import Restyled.Agent.Options
import Restyled.Agent.RestyleMachine
import Restyled.Agent.Restylers
import RIO.Process

main :: IO ()
main = do
    opts@Options {..} <- parseOptions
    logOptions <- logOptionsHandle stdout (oDebug || oTrace)
    withLogFunc logOptions $ \lf -> do
        app <- loadApp opts lf
        runRIO app $ do
            logDebug $ "Options: " <> displayShow opts
            initializeOnPending >>= awaitTermination

initializeOnPending
    :: ( MonadUnliftIO m
       , MonadThrow m
       , MonadReader env m
       , MonadThrow m
       , HasLogFunc env
       , HasOptions env
       , HasAWS env
       , HasProcessContext env
       )
    => m RestyleMachine
initializeOnPending = withPendingLifecycleHook $ do
    machine <- createRestyleMachine
    logInfo $ "Created machine: " <> display machine

    if restyleMachineEnabled machine
        then pure (ActionResultContinue, machine)
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
