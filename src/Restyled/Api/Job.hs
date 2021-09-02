{-# LANGUAGE NamedFieldPuns #-}

module Restyled.Api.Job
    ( ApiJob(..)
    , apiJobSpec
    , ApiJobId(..)
    , apiJobIdToText
    )
where

import RIO hiding (id)

import Data.Aeson
import Restyled.Agent.GitHub
import RIO.Text (pack)

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
    deriving anyclass FromJSON

instance Display ApiJob where
    display job@ApiJob { url } = display (apiJobSpec job) <> " " <> display url

apiJobSpec :: ApiJob -> Text
apiJobSpec ApiJob { owner, repo, pullRequest } =
    toPathPart owner <> "/" <> toPathPart repo <> "#" <> toPathPart pullRequest

newtype ApiJobId = ApiJobId
    { unApiJobId :: Int
    }
    deriving newtype FromJSON

instance Display ApiJobId where
    display = display . apiJobIdToText

apiJobIdToText :: ApiJobId -> Text
apiJobIdToText = pack . show . unApiJobId