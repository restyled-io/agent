{-# LANGUAGE RecordWildCards #-}

module Restyled.Agent.Options
  ( Options (..)
  , HasOptions (..)
  , parseOptions
  ) where

import Restyled.Agent.Prelude

import qualified Restyled.Agent.GitHub as GitHub
import Restyled.Agent.Options.CLI
import Restyled.Agent.Options.Env
import qualified Restyled.Agent.Redis as Redis

data Options = Options
  { gitHubAppId :: GitHub.Id GitHub.App
  , gitHubAppKey :: GitHub.AppKey
  , restyledHost :: Text
  , restyledToken :: Text
  , instanceId :: Text
  , lifecycleQueueUrl :: Maybe Text
  , loggerSettings :: LogSettings
  , net :: Maybe Text
  , statsdHost :: Maybe String
  , statsdPort :: Maybe Int
  , redisConnectInfo :: Redis.ConnectInfo
  , restyleQueue :: ByteString
  , restylerPoolSize :: Natural
  , shutdownTimeoutMinutes :: Natural
  }

class HasOptions env where
  optionsL :: Lens' env Options

parseOptions :: IO Options
parseOptions = do
  OptionsEnv {..} <- parseEnv
  OptionsCLI {..} <- parseCLI
  pure Options {..}
