module Restyled.Agent.AWS
    ( Env
    , HasAWS(..)
    , AWSRequest
    , AWSResponse
    , discover
    , send

    -- * Re-export
    , MonadResource
    ) where

import Restyled.Agent.Prelude

import Amazonka (AWSRequest, AWSResponse, Env)
import qualified Amazonka as AWS
import Conduit
import qualified Control.Monad.Logger as Logger

class HasAWS env where
    awsEnvL :: Lens' env Env

instance HasAWS Env where
    awsEnvL = id

discover :: (MonadIO m, MonadLoggerIO m) => m AWS.Env
discover = do
    loggerIO <- Logger.askLoggerIO
    env <- liftIO $ AWS.newEnv AWS.discover
    pure $ env
        { AWS.envLogger = \level msg -> do
            loggerIO
                Logger.defaultLoc
                "Amazonka"
                (fromLevel level)
                (Logger.toLogStr msg)
        }

fromLevel :: AWS.LogLevel -> Logger.LogLevel
fromLevel = \case
    AWS.Info -> Logger.LevelInfo
    AWS.Error -> Logger.LevelError
    AWS.Debug -> Logger.LevelDebug
    AWS.Trace -> Logger.LevelDebug

send
    :: (MonadResource m, MonadReader env m, HasAWS env, AWSRequest a)
    => a
    -> m (AWSResponse a)
send req = do
    env <- view awsEnvL
    AWS.send env req
