module Restyled.Agent.AWS.SQS.DecodedMessage
    ( DecodedMessage(..)
    , awaitDecodedMessage
    , receiveDecodedMessage
    , deleteDecodedMessage
    )
where

import RIO

import Control.Lens ((?~))
import Data.Aeson
import qualified Network.AWS.SQS.DeleteMessage as AWS
import qualified Network.AWS.SQS.ReceiveMessage as AWS
import qualified Network.AWS.SQS.Types as AWS
import Restyled.Agent.AWS
import qualified RIO.ByteString.Lazy as BSL

data DecodedMessage a = DecodedMessage
    { dmQueueUrl :: Text
    , dmReceiptHandle :: Text
    , dmBody :: a
    }

awaitDecodedMessage
    :: ( MonadIO m
       , MonadReader env m
       , HasLogFunc env
       , HasAWS env
       , FromJSON a
       , Show a
       )
    => Text
    -> (a -> Bool)
    -> m (DecodedMessage a)
awaitDecodedMessage queueUrl predicate = untilJustM $ do
    emDecodedMessage <- receiveDecodedMessage queueUrl

    case emDecodedMessage of
        Nothing -> Nothing <$ logDebug "No messages"
        Just (Left err) -> do
            logDebug $ "Message did not parse: " <> fromString err
            pure Nothing
        Just (Right decodedMessage) | predicate (dmBody decodedMessage) ->
            pure $ Just decodedMessage
        Just (Right decodedMessage) -> do
            logDebug $ "Message was not expected: " <> displayShow
                (dmBody decodedMessage)
            pure Nothing

receiveDecodedMessage
    :: (MonadIO m, MonadReader env m, HasLogFunc env, HasAWS env, FromJSON a)
    => Text
    -> m (Maybe (Either String (DecodedMessage a)))
receiveDecodedMessage queueUrl = do
    resp <-
        runAWS
        $ AWS.receiveMessage queueUrl
        & (AWS.rmMaxNumberOfMessages ?~ 1)
        & (AWS.rmWaitTimeSeconds ?~ 20)
    logDebug $ "Response: " <> displayShow resp

    pure $ do
        guard $ resp ^. AWS.rmrsResponseStatus == 200
        msg <- listToMaybe $ resp ^. AWS.rmrsMessages
        body <- msg ^. AWS.mBody
        recieptHandle <- msg ^. AWS.mReceiptHandle
        pure $ decodedMessage body recieptHandle
  where
    decodedMessage body recieptHandle = DecodedMessage queueUrl recieptHandle
        <$> eitherDecode (BSL.fromStrict $ encodeUtf8 body)

deleteDecodedMessage
    :: (MonadIO m, MonadReader env m, HasLogFunc env, HasAWS env)
    => DecodedMessage a
    -> m ()
deleteDecodedMessage DecodedMessage {..} = do
    resp <- runAWS $ AWS.deleteMessage dmQueueUrl dmReceiptHandle
    logDebug ("Response: " <> displayShow resp)

-- | Keep running an operation until it becomes a 'Just', then return the value
--   inside the 'Just' as the result of the overall loop.
--
--   From extra-1.7.4
--
untilJustM :: Monad m => m (Maybe a) -> m a
untilJustM act = do
    res <- act
    case res of
        Just r -> pure r
        Nothing -> untilJustM act
