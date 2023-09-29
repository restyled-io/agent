module Main
  ( main
  ) where

import Restyled.Agent.Prelude

import Restyled.Agent
import Restyled.Agent.AWS.LifecycleHooks
import Restyled.Agent.App
import Restyled.Agent.Options

main :: IO ()
main = do
  options <- parseOptions
  withApp options $ do
    case options.lifecycleQueueUrl of
      Nothing -> do
        void bootAgent
        logInfo "No Lifecycle Hook queue, running forever"
        forever $ threadDelay maxBound
      Just queue -> do
        let m = options.shutdownTimeoutMinutes
        mAgent <- withPendingLifecycleHook queue bootAgent
        mmResult <- for mAgent $ \agent -> do
          withTerminatingLifecycleHook queue $ do
            race (threadDelayMinutes m) $ shutdownAgent agent

        case mmResult of
          Nothing -> logError "Agent failed to boot"
          Just Nothing ->
            logError "Failed to process Terminating LifecycleHook"
          Just (Just (Left ())) ->
            logError
              $ "Agent shutdown timed out after "
              <> pack (show m)
              <> " minute(s)"
              :# []
          Just (Just (Right ())) -> logInfo "Agent shutdown complete"

threadDelayMinutes :: MonadIO m => Natural -> m ()
threadDelayMinutes m = threadDelay $ fromIntegral m * 60 * 1000000
