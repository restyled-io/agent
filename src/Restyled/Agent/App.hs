module Restyled.Agent.App
    ( App
    , withApp
    ) where

import RIO

import qualified Amazonka as AWS
import RIO.Orphans (HasResourceMap(..), ResourceMap, withResourceMap)
import RIO.Process
import Restyled.Agent.AWS
import Restyled.Agent.Logger
import Restyled.Agent.Options
import Restyled.Agent.Redis (HasRedis(..))
import qualified Restyled.Agent.Redis as Redis

data App = App
    { appOptions :: Options
    , appLogFunc :: LogFunc
    , appProcessContext :: ProcessContext
    , appResourceMap :: ResourceMap
    , appAWS :: AWS.Env
    , appRedisConn :: Redis.Connection
    }

instance HasOptions App where
    optionsL = lens appOptions $ \x y -> x { appOptions = y }

instance HasLogFunc App where
    logFuncL = lens appLogFunc $ \x y -> x { appLogFunc = y }

instance HasProcessContext App where
    processContextL =
        lens appProcessContext $ \x y -> x { appProcessContext = y }

instance HasResourceMap App where
    resourceMapL = lens appResourceMap $ \x y -> x { appResourceMap = y }

instance HasAWS App where
    awsEnvL = lens appAWS $ \x y -> x { appAWS = y }

instance HasRedis App where
    redisConnectionL = lens appRedisConn $ \x y -> x { appRedisConn = y }

withApp :: Options -> RIO App a -> IO a
withApp opts@Options {..} action = do
    withResourceMap $ \resourceMap -> do
        app <-
            App opts logFunc
            <$> mkDefaultProcessContext
            <*> pure resourceMap
            <*> discoverAWS oTrace
            <*> Redis.checkedConnect oRedisConnectInfo
        runRIO app action
  where
    logFunc = getLogFunc stdout logLevel $ "[" <> oInstance <> "] "

    logLevel
        | oDebug = LevelDebug
        | oTrace = LevelDebug
        | otherwise = LevelInfo
