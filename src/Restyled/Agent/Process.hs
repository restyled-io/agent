module Restyled.Agent.Process
    ( runProcessDevNull
    ) where

import Restyled.Agent.Prelude

import Conduit
import Data.Conduit.Process.Typed (createSource)

runProcessDevNull
    :: MonadUnliftIO m => ProcessConfig stdin stdout stderr -> m ExitCode
runProcessDevNull pc = withUnliftIO $ \u -> do
    let pc' = setStdout createSource $ setStderr createSource pc

    withProcessWait pc' $ \p -> do
        a <- async $ unliftIO u $ drain $ getStdout p
        b <- async $ unliftIO u $ drain $ getStderr p
        wait a *> wait b *> waitExitCode p
  where
    drain :: MonadUnliftIO m => ConduitT () ByteString (ResourceT m) () -> m ()
    drain source = runResourceT $ runConduit $ source .| sinkFile "/dev/null"
