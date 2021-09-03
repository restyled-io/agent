module Restyled.Agent.Redis
    ( HasRedis(..)
    , runRedis

    -- * Re-export
    , Redis
    , Connection
    , checkedConnect
    , ConnectInfo
    , parseConnectInfo
    , defaultConnectInfo

    -- * Actions we use
    -- | FIXME: encapsulate our own push/pop?
    , llen
    , lpush
    , brpop

    -- * Convenience
    , encodeStrict
    ) where

import RIO

import Data.Aeson (ToJSON, encode)
import Data.ByteString.Lazy (toStrict)
import Database.Redis
    ( ConnectInfo
    , Connection
    , Redis
    , brpop
    , checkedConnect
    , defaultConnectInfo
    , llen
    , lpush
    , parseConnectInfo
    )
import qualified Database.Redis as Redis

class HasRedis env where
    redisConnectionL :: Lens' env Connection

instance HasRedis Connection where
    redisConnectionL = id

runRedis :: (HasRedis env, MonadReader env m, MonadIO m) => Redis a -> m a
runRedis action = do
    conn <- view redisConnectionL
    liftIO $ Redis.runRedis conn action

encodeStrict :: ToJSON a => a -> ByteString
encodeStrict = toStrict . encode
