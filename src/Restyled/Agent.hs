-- TODO: rename module
module Restyled.Agent
    ( withWebhook
    ) where

import RIO hiding (timeout)

import Control.Lens (_2)
import Data.Aeson
import Data.Functor.Syntax ((<$$>))
import Restyled.Agent.Options
import Restyled.Agent.Redis

withWebhook
    :: ( MonadIO m
       , MonadReader env m
       , HasLogFunc env
       , HasOptions env
       , HasRedis env
       , FromJSON a
       )
    => (a -> m ())
    -> m ()
withWebhook f = do
    timeout <- oWebhookTimeout <$> view optionsL
    queueName <- oRestyleQueue <$> view optionsL
    eresult <- runRedis $ brpop [queueName] $ fromIntegral timeout

    case over _2 eitherDecodeStrict <$$> eresult of
        Left err -> logWarn $ "Error Reply from Redis: " <> displayShow err
        Right Nothing -> pure ()
        Right (Just (_, Left err)) ->
            logWarn $ "Unexpected JSON: " <> fromString err
        Right (Just (_, Right event)) -> f event
