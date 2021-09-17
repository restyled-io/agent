module Main
    ( main
    ) where

import RIO

import qualified Control.Immortal as Immortal
import RIO.Text (pack)
import Restyled.Agent.AWS.LifecycleHooks
import Restyled.Agent.App
import Restyled.Agent.Options
import Restyled.Agent.Queue
import Restyled.Agent.Restyler (processPullRequestEvent)

main :: IO ()
main = do
    options <- parseOptions
    withApp options $ do
        mThreads <- withPendingLifecycleHook $ do
            size <- oRestylerPoolSize <$> view optionsL

            for [1 .. size] $ \n -> do
                let label = "restyle-" <> show n
                    source = pack label

                logInfoS source "created"
                Immortal.createWithLabel label $ \_ -> do
                    mEvent <- awaitWebhook
                    traverse_ (processPullRequestEvent source) mEvent

        for_ mThreads $ \threads -> do
            withTerminatingLifecycleHook $ do
                mDelay <- oTerminationDelay <$> view optionsL
                for_ mDelay $ \delay -> do
                    logInfo $ "Delay termination " <> displayShow delay <> "s"
                    threadDelay $ fromIntegral delay * 1000000

                logInfo "Stopping Agent"
                liftIO $ traverse_ Immortal.mortalize threads
