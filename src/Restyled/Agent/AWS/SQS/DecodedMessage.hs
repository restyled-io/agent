module Restyled.Agent.AWS.SQS.DecodedMessage
  ( DecodedMessage (..)
  , awaitDecodedMessage
  , receiveDecodedMessage
  , deleteDecodedMessage
  ) where

import Restyled.Agent.Prelude

import Amazonka.SQS.DeleteMessage
import Amazonka.SQS.ReceiveMessage
import Amazonka.SQS.Types.Message (message_body, message_receiptHandle)
import Restyled.Agent.AWS

data DecodedMessage a = DecodedMessage
  { queueUrl :: Text
  , receiptHandle :: Text
  , body :: a
  }

awaitDecodedMessage
  :: ( MonadUnliftIO m
     , MonadAWS m
     , MonadLogger m
     , FromJSON a
     , Show a
     )
  => Text
  -> (a -> Bool)
  -> m (DecodedMessage a)
awaitDecodedMessage url predicate = untilJustM $ handleAny onErr $ do
  emDecodedMessage <- receiveDecodedMessage url

  case emDecodedMessage of
    Nothing -> Nothing <$ logDebug "No messages"
    Just (Left err) -> do
      logDebug $ "Message did not parse: " :# ["error" .= err]
      pure Nothing
    Just (Right decodedMessage)
      | predicate decodedMessage . body ->
          pure $ Just decodedMessage
    Just (Right decodedMessage) -> do
      logDebug
        $ "Message was not expected"
        :# ["body" .= show @String decodedMessage . body]
      pure Nothing
 where
  onErr :: (MonadLogger m, Exception e) => e -> m (Maybe a)
  onErr ex =
    Nothing
      <$ logError
        ( "Exception awaiting SQS Message"
            :# ["exception" .= displayException ex]
        )

receiveDecodedMessage
  :: ( MonadIO m
     , MonadAWS m
     , MonadLogger m
     , FromJSON a
     )
  => Text
  -> m (Maybe (Either String (DecodedMessage a)))
receiveDecodedMessage url = do
  resp <-
    send
      $ newReceiveMessage url
      & (receiveMessage_maxNumberOfMessages ?~ 1)
      & (receiveMessage_waitTimeSeconds ?~ 20)
  logDebug $ "Response" :# ["body" .= show @String resp]

  pure $ do
    guard $ resp ^. receiveMessageResponse_httpStatus == 200
    msgs <- resp ^. receiveMessageResponse_messages
    msg <- listToMaybe msgs
    body <- msg ^. message_body
    recieptHandle <- msg ^. message_receiptHandle
    pure $ decodedMessage body recieptHandle
 where
  decodedMessage body recieptHandle =
    DecodedMessage url recieptHandle <$> eitherDecodeText body

deleteDecodedMessage
  :: (MonadIO m, MonadAWS m, MonadLogger m)
  => DecodedMessage a
  -> m ()
deleteDecodedMessage decodedMessage = do
  let req =
        newDeleteMessage
          decodedMessage
          . queueUrl
            decodedMessage
          . receiptHandle
  resp <- send req
  logDebug $ "Response" :# ["body" .= show @String resp]

-- | Keep running an operation until it becomes a 'Just', then return the value
--   inside the 'Just' as the result of the overall loop.
--
--   From extra-1.7.4
untilJustM :: Monad m => m (Maybe a) -> m a
untilJustM act = do
  res <- act
  case res of
    Just r -> pure r
    Nothing -> untilJustM act
