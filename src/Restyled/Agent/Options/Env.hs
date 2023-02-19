module Restyled.Agent.Options.Env
    ( OptionsEnv(..)
    , parseEnv
    ) where

import Restyled.Agent.Prelude

import qualified Blammo.Logging.LogSettings.Env as LoggingEnv
import qualified Database.Redis.TLS as TLS
import Env
import qualified Restyled.Agent.GitHub as GitHub
import qualified Restyled.Agent.Redis as Redis

data OptionsEnv = OptionsEnv
    { oGitHubAppId :: GitHub.Id GitHub.App
    , oGitHubAppKey :: GitHub.AppKey
    , oRestyledHost :: Text
    , oRestyledToken :: Text
    , oInstance :: Text
    , oLifecycleQueueUrl :: Maybe Text
    , oLoggerSettings :: LogSettings
    , oStatsdHost :: Maybe String
    , oStatsdPort :: Maybe Int
    , oRedisConnectInfo :: Redis.ConnectInfo
    , oRestyleQueue :: ByteString
    , oRestylerPoolSize :: Natural
    , oShutdownTimeoutMinutes :: Natural
    }

-- brittany-disable-next-binding

parseEnv :: IO OptionsEnv
parseEnv = Env.parse (header "Run a RestyleMachine Agent") $ OptionsEnv
    <$> var (fmap (GitHub.mkId Proxy) . auto) "GITHUB_APP_ID" (help "GitHub App Id")
    <*> var (str <=< nonempty) "GITHUB_APP_KEY" (help "GitHub App key")
    <*> var (str <=< nonempty) "RESTYLED_HOST" (help "Restyled API host" <> def "https://restyled.io")
    <*> var (str <=< nonempty) "RESTYLED_TOKEN" (help "Restyled API token")
    <*> var (str <=< nonempty) "INSTANCE_ID" (help "EC2 Instance Id" <> def "local")
    <*> optional (var (str <=< nonempty) "LIFECYCLE_HOOKS_URL" (help "URL for EC2 Instance Lifecycle Hooks"))
    <*> LoggingEnv.parser
    <*> optional (var (str <=< nonempty) "STATSD_HOST" (help "StatsD host"))
    <*> optional (var auto "STATSD_PORT" (help "StatsD port"))
    <*> var connectInfo "REDIS_URL" (help "Redis URL" <> def Redis.defaultConnectInfo)
    <*> var str "RESTYLER_QUEUE_NAME" (help "Queue to fetch Restyle webhooks" <> def "restyled:agent:webhooks")
    <*> var auto "RESTYLER_POOL_SIZE" (help "How many Restyle threads to run" <> def 1)
    <*> var auto "SHUTDOWN_TIMEOUT" (help "Minutes to wait for shutdown" <> def 15)

connectInfo :: Env.Reader Error Redis.ConnectInfo
connectInfo = eith $ \url -> TLS.parseConnectInfo TLS.clientParamsNoVerify url
    <|> Redis.parseConnectInfo url

eith :: (String -> Either String a) -> Env.Reader Error a
eith f = first UnreadError . f
