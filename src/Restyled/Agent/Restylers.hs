module Restyled.Agent.Restylers
    ( awaitRestylers
    )
where

import RIO

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL8
import Restyled.Agent.Statsd (HasStatsClient)
import qualified Restyled.Agent.Statsd as Statsd
import RIO.Process

data Restyler = Restyler
    { containerId :: Text
    , status :: Text
    }
    deriving stock Generic
    deriving anyclass FromJSON

restylersWaitSeconds :: Int
restylersWaitSeconds = 3

restylersTimeoutSeconds :: Int
restylersTimeoutSeconds = 10 * 60


awaitRestylers
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HasLogFunc env
       , HasProcessContext env
       , HasStatsClient env
       )
    => m ()
awaitRestylers = race_
    (do
        untilM_ (== 0) $ do
            threadDelay $ restylersWaitSeconds * 1000000
            logDebug "Checking for Restyler containers"
            length <$> getRestylers
    )
    (do
        threadDelay $ restylersTimeoutSeconds * 1000000
        logWarn "Timeout reached"
        Statsd.increment "agent.drain.timeout" []
    )

getRestylers
    :: (MonadIO m, MonadReader env m, HasLogFunc env, HasProcessContext env)
    => m [Restyler]
getRestylers = do
    out <- proc
        "docker"
        [ "ps"
        , "--filter"
        , "label=restyler"
        , "--format"
        , "{\"containerId\":\"{{.ID}}\", \"status\":\"{{.Status}}\"}"
        ]
        readProcessStdout_
    either parseError pure
        $ eitherDecode
        $ "["
        <> BSL8.intercalate "," (BSL8.lines out)
        <> "]"
  where
    parseError err =
        [] <$ logWarn ("Unable to parse docker ps: " <> fromString err)

untilM :: Monad m => (a -> Bool) -> m a -> m a
untilM pred act = do
    result <- act
    if pred result then pure result else untilM pred act

untilM_ :: Monad m => (a -> Bool) -> m a -> m ()
untilM_ pred act = void $ untilM pred act
