module Restyled.Agent.App
    ( App
    , withApp
    ) where

import Restyled.Agent.Prelude

import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Restyled.Agent.AWS (HasAWS(..))
import qualified Restyled.Agent.AWS as AWS
import Restyled.Agent.Options
import Restyled.Agent.Redis (HasRedis(..))
import qualified Restyled.Agent.Redis as Redis

data App = App
    { appOptions :: Options
    , appAWS :: AWS.Env
    , appRedisConn :: Redis.Connection
    }

instance HasOptions App where
    optionsL = lens appOptions $ \x y -> x { appOptions = y }

instance HasAWS App where
    awsEnvL = lens appAWS $ \x y -> x { appAWS = y }

instance HasRedis App where
    redisConnectionL = lens appRedisConn $ \x y -> x { appRedisConn = y }

withApp :: Options -> ReaderT App (LoggingT (ResourceT IO)) a -> IO a
withApp opts@Options {..} action = do
    app <- App opts <$> AWS.discover oTrace <*> liftIO
        (Redis.checkedConnect oRedisConnectInfo)

    runResourceT
        $ runStdoutLoggingT
        $ filterLogger (const (>= logLevel))
        $ withThreadContext context
        $ runReaderT action app
  where
    logLevel
        | oDebug = LevelDebug
        | oTrace = LevelDebug
        | otherwise = LevelInfo

    context = ["instance" .= oInstance, "queue" .= decodeUtf8 oRestyleQueue]
