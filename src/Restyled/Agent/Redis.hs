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
    ) where

import Restyled.Agent.Prelude

import Database.Redis
    ( ConnectInfo
    , Connection
    , Redis
    , brpop
    , checkedConnect
    , defaultConnectInfo
    , llen
    , lpush
    )
import qualified Database.Redis as Redis
import qualified Database.Redis.TLS as TLS

class HasRedis env where
    redisConnectionL :: Lens' env Connection

instance HasRedis Connection where
    redisConnectionL = id

runRedis :: (HasRedis env, MonadReader env m, MonadIO m) => Redis a -> m a
runRedis action = do
    conn <- view redisConnectionL
    liftIO $ Redis.runRedis conn action

parseConnectInfo :: String -> Either String ConnectInfo
parseConnectInfo url = TLS.parseConnectInfo TLS.clientParamsNoVerify url
    <|> Redis.parseConnectInfo url
