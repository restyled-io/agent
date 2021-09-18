module Main
    ( main
    ) where

import RIO

import Restyled.Agent
import Restyled.Agent.AWS.LifecycleHooks
import Restyled.Agent.App
import Restyled.Agent.Options

main :: IO ()
main = do
    options <- parseOptions
    withApp options $ do
        case oLifecycleQueueUrl options of
            Nothing -> do
                void bootAgent
                logInfo "No Lifecycle Hook queue, running forever"
                forever $ threadDelay maxBound
            Just queue -> do
                mAgent <- withPendingLifecycleHook queue bootAgent
                traverse_
                    (withTerminatingLifecycleHook queue . shutdownAgent)
                    mAgent
