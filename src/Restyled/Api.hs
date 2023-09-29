{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Restyled.Api
  ( upsertRepo
  , createJob
  , completeJob
  ) where

import Restyled.Agent.Prelude

import Network.HTTP.Simple
import Network.HTTP.Types (hAuthorization)
import Restyled.Agent.GitHub
import Restyled.Agent.Options
import Restyled.Api.Job
import Restyled.Api.Repo

data ApiUpsertRepo = ApiUpsertRepo
  { owner :: Name Owner
  , name :: Name Repo
  , isPrivate :: Bool
  , installationId :: Id Installation
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

upsertRepo
  :: (MonadIO m, MonadReader env m, HasOptions env)
  => Name Owner
  -> Name Repo
  -> Bool
  -- ^ Private?
  -> Id Installation
  -> m ApiRepo
upsertRepo owner name isPrivate installationId = do
  req <-
    restyledRequest "PUT" $
      unpack $
        "/gh/"
          <> toPathPart owner
          <> "/repos/"
          <> toPathPart name
  resp <- httpJSON $ setRequestBodyJSON body req
  pure $ getResponseBody resp
 where
  body = ApiUpsertRepo {owner, name, isPrivate, installationId}

data ApiCreateJob = ApiCreateJob
  { owner :: Name Owner
  , repo :: Name Repo
  , pullRequest :: IssueNumber
  , completedAt :: Maybe UTCTime
  , exitCode :: Maybe Int
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

createJob
  :: (MonadIO m, MonadReader env m, HasOptions env)
  => ApiRepo
  -> IssueNumber
  -> m ApiJob
createJob ApiRepo {owner, name} pullRequest = do
  req <- restyledRequest "POST" "/jobs"
  resp <- httpJSON $ setRequestBodyJSON body req
  pure $ getResponseBody resp
 where
  body =
    ApiCreateJob
      { owner
      , repo = name
      , pullRequest
      , completedAt = Nothing
      , exitCode = Nothing
      }

data ApiCompleteJob = ApiCompleteJob UTCTime ExitCode

-- brittany-disable-next-binding

instance ToJSON ApiCompleteJob where
  toJSON (ApiCompleteJob completedAt exitCode) =
    object
      [ "tag" .= ("Complete" :: Text)
      , "contents"
          .= object
            [ "completedAt" .= completedAt
            , "exitCode" .= exitCodeToInt exitCode
            ]
      ]

completeJob
  :: (MonadIO m, MonadReader env m, HasOptions env)
  => ApiJobId
  -> ExitCode
  -> m ApiJob
completeJob jobId exitCode = do
  now <- liftIO getCurrentTime
  req <- restyledRequest "PATCH" $ unpack $ "/jobs/" <> apiJobIdToText jobId
  let change = ApiCompleteJob now exitCode
  resp <- httpJSON $ setRequestBodyJSON [change] req
  pure $ getResponseBody resp

restyledRequest
  :: (MonadIO m, MonadReader env m, HasOptions env)
  => String
  -> String
  -> m Request
restyledRequest method path = do
  (host, token) <- ((. restyledHost) &&& (. restyledToken)) <$> view optionsL
  req <- liftIO $ parseRequest $ method <> " " <> unpack host <> path
  pure $ addAuthorization token $ setRequestCheckStatus req
 where
  addAuthorization :: Text -> Request -> Request
  addAuthorization token =
    addRequestHeader hAuthorization $ "token " <> encodeUtf8 token

exitCodeToInt :: ExitCode -> Int
exitCodeToInt = \case
  ExitSuccess -> 0
  ExitFailure n -> n
