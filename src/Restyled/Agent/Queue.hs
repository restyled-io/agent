module Restyled.Agent.Queue
    ( awaitWebhook
    ) where

import RIO hiding (timeout)

import Control.Lens (_2)
import Data.Aeson
import Data.Functor.Syntax ((<$$>))
import Restyled.Agent.Options
import Restyled.Agent.Redis

awaitWebhook
    :: ( MonadIO m
       , MonadReader env m
       , HasLogFunc env
       , HasOptions env
       , HasRedis env
       , FromJSON a
       )
    => m (Maybe a)
awaitWebhook = do
    timeout <- oWebhookTimeout <$> view optionsL
    queueName <- oRestyleQueue <$> view optionsL
    eresult <- runRedis $ brpop [queueName] $ fromIntegral timeout

    case over _2 eitherDecodeStrict <$$> eresult of
        Left err ->
            Nothing <$ logWarn ("Error Reply from Redis: " <> displayShow err)
        Right Nothing -> pure Nothing
        Right (Just (_, Left err)) ->
            Nothing <$ logWarn ("Unexpected JSON: " <> fromString err)
        Right (Just (_, Right event)) -> pure $ Just event
