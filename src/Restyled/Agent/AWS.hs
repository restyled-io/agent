module Restyled.Agent.AWS
    ( HasAWS(..)
    , discoverAWS
    , send
    , MonadResource
    ) where

import RIO

import qualified Amazonka as AWS
import Conduit

class HasAWS env where
    awsEnvL :: Lens' env AWS.Env

instance HasAWS AWS.Env where
    awsEnvL = id

discoverAWS
    :: MonadIO m
    => Bool -- ^ Debug?
    -> m AWS.Env
discoverAWS debug = do
    let level = if debug then AWS.Debug else AWS.Info
    lgr <- AWS.newLogger level stdout
    env <- liftIO $ AWS.newEnv AWS.discover
    pure $ env { AWS.envLogger = lgr }

send
    :: (MonadResource m, MonadReader env m, HasAWS env, AWS.AWSRequest a)
    => a
    -> m (AWS.AWSResponse a)
send req = do
    env <- view awsEnvL
    AWS.send env req
