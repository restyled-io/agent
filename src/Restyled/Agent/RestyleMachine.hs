{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Restyled.Agent.RestyleMachine
    ( RestyleMachine
    , restyleMachineEnabled
    , getRestyleMachineByName
    , createRestyleMachine
    , disableRestyleMachine
    , deleteRestyleMachine
    )
where

import RIO hiding (id)

import Data.Aeson
import Network.HTTP.Simple
import Network.HTTP.Types.Header (hAuthorization)
import Restyled.Agent.Options
import RIO.FilePath ((</>))
import RIO.List (find)
import RIO.Text (unpack)
import qualified RIO.Text as T
import System.Random (randomRIO)

data RestyleMachine = RestyleMachine
    { id :: Int
    , name :: Text
    , enabled :: Bool
    , host :: Text
    , jobCount :: Int
    }
    deriving stock Generic
    deriving anyclass FromJSON

restyleMachineName :: RestyleMachine -> Text
restyleMachineName RestyleMachine { name } = name

restyleMachineEnabled :: RestyleMachine -> Bool
restyleMachineEnabled RestyleMachine { enabled } = enabled

instance Display RestyleMachine where
    display RestyleMachine { id, name, host, enabled } =
        display id
            <> ": "
            <> display name
            <> " ("
            <> display host
            <> ")"
            <> if enabled then "" else " DISABLED"

getRestyleMachineByName
    :: (MonadIO m, MonadThrow m, MonadReader env m, HasOptions env)
    => Text
    -> m RestyleMachine
getRestyleMachineByName name = do
    req <- restyledRequest "GET" "/admin/machines"
    machines <- getResponseBody <$> httpJSON req
    maybe notFound pure $ find @[] ((== name) . restyleMachineName) machines
  where
    notFound =
        throwString
            $ unpack
            $ "RestyleMachine with name "
            <> name
            <> " not found"

data CreateRestyleMachine = CreateRestyleMachine
    { name :: Text
    , host :: Text
    , certificateAuthority :: Text
    , certificate :: Text
    , privateKey :: Text
    }
    deriving stock (Show, Generic)
    deriving anyclass ToJSON

createRestyleMachine
    :: ( MonadIO m
       , MonadThrow m
       , MonadReader env m
       , HasOptions env
       , HasLogFunc env
       )
    => m RestyleMachine
createRestyleMachine = do
    Options {..} <- view optionsL
    body <-
        CreateRestyleMachine
        <$> randomName
        <*> pure ("tcp://" <> oHost <> ":2376")
        <*> readFileUtf8 (oCerts </> "ca.pem")
        <*> readFileUtf8 (oCerts </> "cert.pem")
        <*> readFileUtf8 (oCerts </> "key.pem")

    logDebug $ displayShow body
    req <- setRequestBodyJSON body <$> restyledRequest "POST" "/admin/machines"
    getResponseBody <$> httpJSON req

disableRestyleMachine
    :: ( MonadIO m
       , MonadThrow m
       , MonadReader env m
       , HasOptions env
       , HasLogFunc env
       )
    => RestyleMachine
    -> m ()
disableRestyleMachine RestyleMachine { id } = do
    logDebug $ "Disabling id=" <> display id
    req <- restyledRequest "PATCH" $ "/admin/machines/" <> show id
    resp <- httpJSON $ setRequestBodyJSON (object ["enabled" .= False]) req
    logDebug $ displayShow @Value $ getResponseBody resp

deleteRestyleMachine
    :: ( MonadIO m
       , MonadThrow m
       , MonadReader env m
       , HasOptions env
       , HasLogFunc env
       )
    => RestyleMachine
    -> m ()
deleteRestyleMachine RestyleMachine { id } = do
    logDebug $ "Deleting id=" <> display id
    req <- restyledRequest "DELETE" $ "/admin/machines/" <> show id
    void $ httpNoBody req

restyledRequest
    :: (MonadThrow m, MonadReader env m, HasOptions env)
    => String
    -> String
    -> m Request
restyledRequest method path = do
    (host, token) <- (oRestyledHost &&& oRestyledToken) <$> view optionsL
    req <- parseRequest $ method <> " " <> unpack host <> path
    pure $ addAuthorization token req
  where
    addAuthorization token =
        addRequestHeader hAuthorization $ "token " <> encodeUtf8 token

randomName :: (MonadIO m, MonadReader env m, HasOptions env) => m Text
randomName = do
    env <- oEnvironment <$> view optionsL
    digits <- liftIO $ randomRIO (1000, 9999)
    pure $ T.intercalate "-" ["restyled", env, tshow @Int digits]
