module Restyled.Agent.Options
    ( Options(..)
    , HasOptions(..)
    , parseOptions
    ) where

import RIO

import qualified Restyled.Agent.GitHub as GitHub
import Restyled.Agent.Options.CLI
import Restyled.Agent.Options.Env
import qualified Restyled.Agent.Redis as Redis

data Options = Options
    { oGitHubAppId :: GitHub.Id GitHub.App
    , oGitHubAppKey :: GitHub.AppKey
    , oRestyledHost :: Text
    , oRestyledToken :: Text
    , oInstance :: Text
    , oLifecycleQueueUrl :: Maybe Text
    , oDebug :: Bool
    , oTrace :: Bool
    , oNet :: Maybe Text
    , oStatsdHost :: Maybe String
    , oStatsdPort :: Maybe Int
    , oRedisConnectInfo :: Redis.ConnectInfo
    , oRestyleQueue :: ByteString
    , oRestylerPoolSize :: Natural
    }
    deriving stock Show

class HasOptions env where
    optionsL :: Lens' env Options

parseOptions :: IO Options
parseOptions = do
    OptionsEnv {..} <- parseEnv
    OptionsCLI {..} <- parseCLI
    pure Options { .. }
