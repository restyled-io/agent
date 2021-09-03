module Restyled.Agent.ThreadPool
    ( Pool
    , createPool
    , withThread
    ) where

import RIO

import qualified Data.Pool as Pool

newtype Pool = Pool (Pool.Pool ())

createPool :: MonadIO m => Natural -> m Pool
createPool size = liftIO $ Pool <$> Pool.createPool
    (pure ())
    (\_ -> pure ())
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
    handleAny err $ liftIO $ Pool.withResource pool $ \() -> runInIO f
    where err ex = logError $ "Exception in withThread: " <> displayShow ex
