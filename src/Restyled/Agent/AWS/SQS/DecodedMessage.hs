module Restyled.Agent.AWS.SQS.DecodedMessage
    ( DecodedMessage(..)
    , awaitDecodedMessage
    , receiveDecodedMessage
    , deleteDecodedMessage
    ) where

import RIO

import qualified Amazonka.SQS.DeleteMessage as AWS
import qualified Amazonka.SQS.ReceiveMessage as AWS
import qualified Amazonka.SQS.Types as AWS
import Control.Lens ((?~))
import Data.Aeson
import qualified RIO.ByteString.Lazy as BSL
import Restyled.Agent.AWS

data DecodedMessage a = DecodedMessage
    { dmQueueUrl :: Text
    , dmReceiptHandle :: Text
    , dmBody :: a
    }

awaitDecodedMessage
    :: ( MonadUnliftIO m
       , MonadResource m
       , MonadReader env m
       , HasLogFunc env
       , HasAWS env
       , FromJSON a
       , Show a
       )
    => Text
    -> (a -> Bool)
    -> m (DecodedMessage a)
awaitDecodedMessage queueUrl predicate = untilJustM $ handleAny onErr $ do
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
  where
    onErr ex =
        Nothing <$ logError ("Exception awaiting SQS Message: " <> display ex)

receiveDecodedMessage
    :: ( MonadIO m
       , MonadResource m
       , MonadReader env m
       , HasLogFunc env
       , HasAWS env
       , FromJSON a
       )
    => Text
    -> m (Maybe (Either String (DecodedMessage a)))
receiveDecodedMessage queueUrl = do
    resp <-
        send
        $ AWS.newReceiveMessage queueUrl
        & (AWS.receiveMessage_maxNumberOfMessages ?~ 1)
        & (AWS.receiveMessage_waitTimeSeconds ?~ 20)
    logDebug $ "Response: " <> displayShow resp

    pure $ do
        guard $ resp ^. AWS.receiveMessageResponse_httpStatus == 200
        msgs <- resp ^. AWS.receiveMessageResponse_messages
        msg <- listToMaybe msgs
        body <- msg ^. AWS.message_body
        recieptHandle <- msg ^. AWS.message_receiptHandle
        pure $ decodedMessage body recieptHandle
  where
    decodedMessage body recieptHandle = DecodedMessage queueUrl recieptHandle
        <$> eitherDecode (BSL.fromStrict $ encodeUtf8 body)

deleteDecodedMessage
    :: ( MonadIO m
       , MonadResource m
       , MonadReader env m
       , HasLogFunc env
       , HasAWS env
       )
    => DecodedMessage a
    -> m ()
deleteDecodedMessage DecodedMessage {..} = do
    let req = AWS.newDeleteMessage dmQueueUrl dmReceiptHandle
    resp <- send req
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
