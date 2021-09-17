module Restyled.Agent.ThreadPool
    ( Pool
    , createPool
    , withThread
    ) where

import RIO

import qualified Data.Pool as Pool

newtype Pool = Pool (Pool.Pool ())

createPool
    :: (MonadIO m, MonadReader env m, HasLogFunc env) => Natural -> m Pool
createPool size = do
    logFunc <- view logFuncL

    let info :: MonadIO m => Utf8Builder -> m ()
        info msg = runRIO logFunc $ logInfo $ "[ThreadPool] " <> msg

    liftIO $ Pool <$> Pool.createPool
        (info "thread create")
        (\_ -> info "thread destroy")
        1
        (60 * 60 * 24)
        (fromIntegral size)

withThread
    :: (MonadUnliftIO m, MonadReader env m, HasLogFunc env)
    => Pool
    -> m ()
    -> m ()
withThread (Pool pool) f = do
    runInIO <- askRunInIO
    handleAny err $ liftIO $ Pool.withResource pool $ \() -> async_ $ runInIO f
    where err ex = logError $ "[withThread] " <> displayShow ex

async_ :: MonadUnliftIO m => m a -> m ()
async_ = void . async
