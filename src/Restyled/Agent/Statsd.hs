module Restyled.Agent.Statsd
    (
    -- * Setup
      HasStatsClient(..)
    , StatsClient
    , withStatsClient

    -- * Sending metrics
    , increment
    )
where

import RIO

import Network.StatsD.Datadog
    ( DogStatsSettings(..)
    , Metric
    , MetricName(..)
    , MetricType(..)
    , StatsClient
    , ToMetricValue
    , defaultSettings
    , withDogStatsD
    )
import qualified Network.StatsD.Datadog as DD

withStatsClient
    :: MonadUnliftIO m => String -> Int -> (StatsClient -> m a) -> m a
withStatsClient host port f = do
    withDogStatsD settings f
  where
    settings = defaultSettings
        { dogStatsSettingsHost = host
        , dogStatsSettingsPort = port
        }

class HasStatsClient env where
    statsClientL :: Lens' env StatsClient

instance HasStatsClient StatsClient where
    statsClientL = id

increment
    :: (MonadIO m, MonadReader env m, HasStatsClient env)
    => Text
    -> [(Text, Text)]
    -> m ()
increment name = send $ metric @Int name Counter 1

metric :: ToMetricValue a => Text -> MetricType -> a -> Metric
metric = DD.metric . MetricName

send
    :: (MonadIO m, MonadReader env m, HasStatsClient env)
    => Metric
    -> [(Text, Text)]
    -> m ()
send metric' tags = do
    client <- view statsClientL
    DD.send client $ metric' & DD.tags .~ ddTags
    where ddTags = map (uncurry DD.tag) tags
