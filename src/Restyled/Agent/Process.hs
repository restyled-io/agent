module Restyled.Agent.Process
    ( runProcessLogged
    ) where

import Restyled.Agent.Prelude

import Conduit
import qualified Data.Conduit.Binary as CB
import Data.Conduit.Process.Typed (createSource)

-- | Run a process and log its @stdout@ and @stderr@ using the given functions
--
-- @{ stream: "stdout|stderr" }@ will be pre-attached to the 'Message', though
-- you could add more by pattern-matching and re-building in your function.
--
runProcessLogged
    :: (MonadUnliftIO m, MonadLogger m)
    => (Message -> m ())
    -> (Message -> m ())
    -> ProcessConfig stdin stdout stderr
    -> m ExitCode
runProcessLogged logStdout logStderr = runProcessSinks
    (sinkLogger "stdout" logStdout)
    (sinkLogger "stderr" logStderr)

sinkLogger
    :: MonadLogger m
    => Text
    -> (Message -> m ())
    -> ConduitT ByteString Void m ()
sinkLogger stream logX = CB.lines .| mapM_C (logX . toMessage)
    where toMessage bs = decodeUtf8 bs :# ["stream" .= stream]

runProcessSinks
    :: MonadUnliftIO m
    => ConduitT ByteString Void m () -- ^ Sink for @stdout@
    -> ConduitT ByteString Void m () -- ^ Sink for @stderr@
    -> ProcessConfig stdin stdout stderr
    -> m ExitCode
runProcessSinks sinkStdout sinkStderr pc = withUnliftIO $ \u -> do
    let pc' = setStdout createSource $ setStderr createSource pc

    withProcessWait pc' $ \p -> do
        a <- async $ unliftIO u $ runConduit $ getStdout p .| sinkStdout
        b <- async $ unliftIO u $ runConduit $ getStderr p .| sinkStderr
        wait a *> wait b *> waitExitCode p
