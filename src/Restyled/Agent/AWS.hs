module Restyled.Agent.AWS
  ( module Control.Monad.AWS
  , discover
  ) where

import Restyled.Agent.Prelude

import qualified Amazonka
import Amazonka.Env (env_logger)
import Control.Monad.AWS
import Control.Monad.Logger (defaultLoc, toLogStr)

discover :: MonadLoggerIO m => m Amazonka.Env
discover = do
  loggerIO <- askLoggerIO

  let logFn level msg = do
        loggerIO
          defaultLoc
          "Amazonka"
          (fromLevel level)
          (toLogStr msg)

  env <- liftIO $ Amazonka.newEnv Amazonka.discover
  pure $ env & env_logger .~ logFn

fromLevel :: Amazonka.LogLevel -> LogLevel
fromLevel = \case
  Amazonka.Info -> LevelInfo
  Amazonka.Error -> LevelError
  Amazonka.Debug -> LevelDebug
  Amazonka.Trace -> LevelOther "trace"
