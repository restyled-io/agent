module Restyled.Agent.App
    ( App
    , withApp
    )
where

import RIO

import qualified Network.AWS as AWS
import Restyled.Agent.AWS
import Restyled.Agent.Logger
import Restyled.Agent.Options
import Restyled.Agent.Redis (HasRedis(..))
import qualified Restyled.Agent.Redis as Redis
import RIO.Process

data App = App
    { appOptions :: Options
    , appLogFunc :: LogFunc
    , appProcessContext :: ProcessContext
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

instance HasAWS App where
    awsEnvL = lens appAWS $ \x y -> x { appAWS = y }

instance HasRedis App where
    redisConnectionL = lens appRedisConn $ \x y -> x { appRedisConn = y }

withApp :: Options -> RIO App a -> IO a
withApp opts@Options {..} action = do
    app <- loadApp opts logFunc
    runRIO app $ do
        logDebug $ "Options: " <> displayShow opts
        action
  where
    logFunc = getLogFunc stdout logLevel logPrefix

    logLevel
        | oDebug = LevelDebug
        | oTrace = LevelDebug
        | otherwise = LevelInfo

    logPrefix = maybe "" (\i -> "[" <> i <> "] ") oInstance

loadApp :: Options -> LogFunc -> IO App
loadApp opts@Options {..} lf =
    App opts lf
        <$> mkDefaultProcessContext
        <*> discoverAWS oTrace
        <*> Redis.checkedConnect oRedisConnectInfo
