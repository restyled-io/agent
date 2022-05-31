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

class HasAWS env where
    awsEnvL :: Lens' env Env

instance HasAWS Env where
    awsEnvL = id

discover
    :: MonadIO m
    => Bool -- ^ Debug?
    -> m Env
discover debug = do
    let level = if debug then AWS.Debug else AWS.Info
    lgr <- AWS.newLogger level stdout
    env <- liftIO $ AWS.newEnv AWS.discover
    pure $ env { AWS.envLogger = lgr }

send
    :: (MonadResource m, MonadReader env m, HasAWS env, AWSRequest a)
    => a
    -> m (AWSResponse a)
send req = do
    env <- view awsEnvL
    AWS.send env req
