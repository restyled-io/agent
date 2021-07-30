-- | Extends "GitHub.Data.PullRequests" 'PullRequestEvent'
module GitHub.Data.PullRequests.Ext
    ( PullRequestEvent(..)
    , module GitHub.Data.PullRequests
    )
where

import RIO

import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import GitHub.Data.Definitions (SimpleUser(..))
import GitHub.Data.Installations
import GitHub.Data.PullRequests hiding (PullRequestEvent(..))
import qualified GitHub.Data.PullRequests as GitHub
import GitHub.Data.Repos

data PullRequestEvent = PullRequestEvent
    { pullRequestEventAction :: PullRequestEventType
    , pullRequestEventNumber :: Int
    , pullRequestEventPullRequest :: PullRequest
    , pullRequestRepository :: Repo
    , pullRequestSender :: SimpleUser
    , pullRequestInstallation :: Installation
    }

instance FromJSON PullRequestEvent where
    parseJSON v@(Object o) = do
        event <- parseJSON v
        installation <- o .: "installation"

        pure PullRequestEvent
            { pullRequestEventAction = GitHub.pullRequestEventAction event
            , pullRequestEventNumber = GitHub.pullRequestEventNumber event
            , pullRequestEventPullRequest = GitHub.pullRequestEventPullRequest
                event
            , pullRequestRepository = GitHub.pullRequestRepository event
            , pullRequestSender = GitHub.pullRequestSender event
            , pullRequestInstallation = installation
            }

    parseJSON v = typeMismatch "PullRequestEvent" v

--         pure $ Webhook
--             { webhookOwnerName = simpleOwnerLogin repoOwner
--             , webhookRepoName = repoName
--             , webhookInstallationId = installationId installation
--             , webhookRepoIsPrivate = repoPrivate
--             , webhookPullRequest = pullRequestNumber
--             , webhookAction = pullRequestEventAction event
--             , webhookAuthor = untagName $ simpleUserLogin pullRequestUser
--             }
