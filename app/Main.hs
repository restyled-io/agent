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
        mAgent <- withPendingLifecycleHook bootAgent
        traverse_ (withTerminatingLifecycleHook . shutdownAgent) mAgent
