module Restyled.Agent.ThreadPool
    ( Pool
    , createPool
    , withThread
    ) where

import RIO

newtype Pool = Pool
    { _poolSize :: IORef Natural
    }

createPool :: MonadIO m => Natural -> m Pool
createPool size = Pool <$> newIORef size

withThread
    :: (MonadUnliftIO m, MonadReader env m, HasLogFunc env)
    => Pool
    -> m ()
    -> m ()
withThread pool f = do
    available <- takeThread pool

    if available
        then void $ async $ f `finally` returnThread pool
        else do
            logWarn "All threads busy, waiting 5s"
            threadDelay $ 5 * 1000000
            withThread pool f

takeThread :: MonadIO m => Pool -> m Bool
takeThread (Pool ref) = atomicModifyIORef' ref
    $ \n -> if n <= 0 then (n, False) else (n `subtract` 1, True)

returnThread :: MonadIO m => Pool -> m ()
returnThread (Pool ref) = atomicModifyIORef' ref $ \n -> (n + 1, ())
