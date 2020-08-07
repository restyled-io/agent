module Restyled.Agent.App
    ( App
    , loadApp
    )
where

import RIO

import qualified Network.AWS as AWS
import Restyled.Agent.AWS
import Restyled.Agent.Options
import RIO.Process

data App = App
    { appOptions :: Options
    , appLogFunc :: LogFunc
    , appProcessContext :: ProcessContext
    , appAWS :: AWS.Env
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

loadApp :: Options -> LogFunc -> IO App
loadApp opts@Options {..} lf =
    App opts lf <$> mkDefaultProcessContext <*> discoverAWS oTrace
