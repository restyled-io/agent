module Main
    ( main
    ) where

import RIO

import qualified Control.Immortal as Immortal
import Restyled.Agent (withWebhook)
import Restyled.Agent.AWS.LifecycleHooks
import Restyled.Agent.App
import Restyled.Agent.Options
import Restyled.Agent.Restyler (processPullRequestEvent)

main :: IO ()
main = do
    options <- parseOptions
    withApp options $ do
        mThreads <- withPendingLifecycleHook $ do
            size <- oRestylerPoolSize <$> view optionsL

            for [1 .. size] $ \n -> do
                let label = "restyle-" <> show n
                Immortal.createWithLabel label
                    $ \_ -> withWebhook processPullRequestEvent

        for_ mThreads $ \threads -> do
            withTerminatingLifecycleHook $ do
                mDelay <- oTerminationDelay <$> view optionsL
                for_ mDelay $ \delay -> do
                    logInfo $ "Delay termination " <> displayShow delay <> "s"
                    threadDelay $ fromIntegral delay * 1000000

                logInfo "Stopping Agent"
                liftIO $ traverse_ Immortal.stop threads
