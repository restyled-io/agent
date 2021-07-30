module Main
    ( main
    )
where

import RIO

import Restyled.Agent
import Restyled.Agent.App
import Restyled.Agent.AWS.LifecycleHooks
import Restyled.Agent.Options

main :: IO ()
main = do
    options <- parseOptions
    withApp options $ do
        mAgent <- withPendingLifecycleHook createAgent

        for_ mAgent $ \agent -> do
            void $ async $ monitorAgent agent
            void $ async $ withTerminatingLifecycleHook $ terminateAgent agent
            runAgent agent
