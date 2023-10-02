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
  { options :: Options
  , logger :: Logger
  , awsEnv :: Amazonka.Env
  , redisConn :: Redis.Connection
  }

instance HasOptions App where
  optionsL = lens (. options) $ \x y -> x {options = y}

instance HasLogger App where
  loggerL = lens (. logger) $ \x y -> x {logger = y}

instance HasEnv App where
  envL = lens (. awsEnv) $ \x y -> x {awsEnv = y}

instance HasRedis App where
  redisConnectionL = lens (. redisConn) $ \x y -> x {redisConn = y}

newtype TestAppT a = TestAppT
  { _unTestApp :: ReaderT App (LoggingT (ResourceT IO)) a
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
withApp options action = do
  logger <- newLogger options . loggerSettings
  app <-
    App options logger
      <$> runLoggerLoggingT logger AWS.discover
      <*> liftIO (Redis.checkedConnect options . redisConnectInfo)

  runResourceT
    $ runLoggerLoggingT app
    $ withThreadContext context
    $ runReaderT (coerce action) app
 where
  context =
    [ "instance" .= options . instanceId
    , "queue" .= decodeUtf8 @Text options . restyleQueue
    ]
