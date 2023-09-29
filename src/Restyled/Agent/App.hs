{-# LANGUAGE DerivingVia #-}

module Restyled.Agent.App
  ( App
  , withApp
  ) where

import Restyled.Agent.Prelude

import qualified Amazonka
import Control.Monad.AWS
import Control.Monad.AWS.ViaReader
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.Trans.Resource (MonadResource, ResourceT, runResourceT)
import qualified Restyled.Agent.AWS as AWS
import Restyled.Agent.Options
import Restyled.Agent.Redis (HasRedis (..))
import qualified Restyled.Agent.Redis as Redis

data App = App
  { appOptions :: Options
  , appLogger :: Logger
  , appAWS :: Amazonka.Env
  , appRedisConn :: Redis.Connection
  }

instance HasOptions App where
  optionsL = lens appOptions $ \x y -> x {appOptions = y}

instance HasLogger App where
  loggerL = lens appLogger $ \x y -> x {appLogger = y}

instance HasEnv App where
  envL = lens appAWS $ \x y -> x {appAWS = y}

instance HasRedis App where
  redisConnectionL = lens appRedisConn $ \x y -> x {appRedisConn = y}

newtype TestAppT a = TestAppT
  { unTestApp :: ReaderT App (LoggingT (ResourceT IO)) a
  }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadThrow
    , MonadCatch
    , MonadMask
    , MonadIO
    , MonadUnliftIO
    , MonadLogger
    , MonadResource
    , MonadReader App
    )
  deriving (MonadAWS) via (ReaderAWS TestAppT)

withApp :: Options -> TestAppT a -> IO a
withApp opts@Options {..} action = do
  logger <- newLogger oLoggerSettings
  app <-
    App opts logger
      <$> runLoggerLoggingT logger AWS.discover
      <*> liftIO (Redis.checkedConnect oRedisConnectInfo)

  runResourceT
    $ runLoggerLoggingT app
    $ withThreadContext context
    $ runReaderT (unTestApp action) app
 where
  context =
    ["instance" .= oInstance, "queue" .= decodeUtf8 @Text oRestyleQueue]
