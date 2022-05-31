{-# LANGUAGE NamedFieldPuns #-}

module Restyled.Api.Job
    ( ApiJob(..)
    , apiJobSpec
    , ApiJobId(..)
    , apiJobIdToText
    ) where

import Restyled.Agent.Prelude

import Restyled.Agent.GitHub

data ApiJob = ApiJob
    { id :: ApiJobId
    , owner :: Name Owner
    , repo :: Name Repo
    , pullRequest :: IssueNumber
    , url :: Text
    , awsLogGroup :: Text
    , awsLogStream :: Text
    }
    deriving stock Generic
    deriving anyclass (FromJSON, ToJSON)

apiJobSpec :: ApiJob -> Text
apiJobSpec ApiJob { owner, repo, pullRequest } =
    toPathPart owner <> "/" <> toPathPart repo <> "#" <> toPathPart pullRequest

newtype ApiJobId = ApiJobId
    { unApiJobId :: Int
    }
    deriving newtype (FromJSON, ToJSON)

apiJobIdToText :: ApiJobId -> Text
apiJobIdToText = pack . show . unApiJobId
