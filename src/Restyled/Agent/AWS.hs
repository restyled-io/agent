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

discover :: MonadLoggerIO m => m AWS.Env
discover = do
    loggerIO <- askLoggerIO
    env <- liftIO $ AWS.newEnv AWS.discover
    pure $ env
        { AWS.envLogger = \level msg -> do
            loggerIO
                Logger.defaultLoc
                "Amazonka"
                (fromLevel level)
                (Logger.toLogStr msg)
        }

fromLevel :: AWS.LogLevel -> LogLevel
fromLevel = \case
    AWS.Info -> LevelInfo
    AWS.Error -> LevelError
    AWS.Debug -> LevelDebug
    AWS.Trace -> LevelOther "trace"

send
    :: (MonadResource m, MonadReader env m, HasAWS env, AWSRequest a)
    => a
    -> m (AWSResponse a)
send req = do
    env <- view awsEnvL
    AWS.send env req
