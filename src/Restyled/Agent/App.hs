module Restyled.Agent.App
    ( App
    , withApp
    )
where

import RIO

import qualified Network.AWS as AWS
import Restyled.Agent.AWS
import Restyled.Agent.Options
import Restyled.Agent.Statsd
import RIO.Process

data App = App
    { appOptions :: Options
    , appLogFunc :: LogFunc
    , appProcessContext :: ProcessContext
    , appAWS :: AWS.Env
    , appStatsClient :: StatsClient
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

instance HasStatsClient App where
    statsClientL = lens appStatsClient $ \x y -> x { appStatsClient = y }

withApp :: Options -> RIO App a -> IO a
withApp opts@Options {..} action = do
    logOptions <- logOptionsHandle stdout (oDebug || oTrace)
    withStatsClient oStatsdHost oStatsdPort $ \sc -> do
        withLogFunc logOptions $ \lf -> do
            app <- loadApp opts lf sc
            runRIO app $ do
                logDebug $ "Options: " <> displayShow opts
                action

loadApp :: Options -> LogFunc -> StatsClient -> IO App
loadApp opts@Options {..} lf sc =
    App opts lf <$> mkDefaultProcessContext <*> discoverAWS oTrace <*> pure sc
