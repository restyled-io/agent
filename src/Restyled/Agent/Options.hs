module Restyled.Agent.Options
    ( Options(..)
    , HasOptions(..)
    , parseOptions
    ) where

import RIO

import Options.Applicative
import qualified Restyled.Agent.GitHub as GitHub
import qualified Restyled.Agent.Redis as Redis

data Options = Options
    { oGitHubAppId :: GitHub.Id GitHub.App
    , oGitHubAppKey :: GitHub.AppKey
    , oRestyledHost :: Text
    , oRestyledToken :: Text
    , oInstance :: Maybe Text
    , oLifecycleQueueUrl :: Maybe Text
    , oTerminationDelay :: Natural
    , oMonitorInterval :: Natural
    , oWebhookTimeout :: Natural
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
parseOptions = execParser $ parse options "Run a RestyleMachine Agent"

-- brittany-disable-next-binding

options :: Parser Options
options = Options
    <$> (GitHub.mkId Proxy <$> option auto
        (  long "github-app-id"
        <> help "GitHub App Id"
        ))
    <*> option str
        (  long "github-app-key"
        <> help "GitHub App Key"
        )
    <*> option str
        (  long "restyled-host"
        <> help "Base URL for Restyled API"
        <> value "https://restyled.io")
    <*> option str
        (  long "restyled-token"
        <> help "Token for Restyled API"
        )
    <*> optional (option str
        (  long "instance"
        <> help "Instance Id for the instance"
        ))
    <*> optional (option str
        (  long "lifecycle-queue-url"
        <> help "SQS Queue URL for LifecycleHook notifications"
        ))
    <*> option auto
        (  long "termination-delay"
        <> help "Artificially delay termination by the given seconds"
        )
    <*> option auto
        (  long "monitor-interval"
        <> help "Interval in seconds to print Agent state"
        <> value 5
        )
    <*> option auto
        (  long "webhook-timeout"
        <> help "Timeout to wait for next Webhook"
        <> value 30
        )
    <*> switch
        (  long "debug"
        <> help "Log our own DEBUG messages"
        )
    <*> switch
        (  long "trace"
        <> help "Also log AWS DEBUG messages"
        )
    <*> optional (option str
        (  long "net"
        <> help "Docker --net option"
        ))
    <*> optional (option str
        (  long "statsd-host"
        <> help "Statsd Host"
        ))
    <*> optional (option auto
        (  long "statsd-port"
        <> help "Statsd Port"
        ))
    <*> option (eitherReader Redis.parseConnectInfo)
        (  long "redis-url"
        <> help "Redis URL"
        <> value Redis.defaultConnectInfo
        )
    <*> option str
        (  long "restyle-queue"
        <> help "Restyle queue"
        <> value "restyled:agent:webhooks"
        )
    <*> option auto
        (  long "restyler-pool-size"
        <> help "Restyler pool size"
        <> value 3
        )

parse :: Parser a -> String -> ParserInfo a
parse p d = info (p <**> helper) $ fullDesc <> progDesc d
