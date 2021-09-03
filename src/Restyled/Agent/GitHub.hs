module Restyled.Agent.GitHub
    ( getGitHubAppToken

    -- * Re-exports
    , App
    , AppKey
    , Id
    , Installation(..)
    , IssueNumber
    , Name
    , Owner
    , PullRequest(..)
    , PullRequestEvent(..)
    , PullRequestEventType(..)
    , Repo(..)
    , SimpleOwner(..)
    , SimpleUser(..)
    , mkId
    , toPathPart
    , untagName
    ) where

import RIO

import GitHub.Auth.JWT
import GitHub.Data hiding (PullRequestEvent(..))
import GitHub.Data.AccessTokens
import GitHub.Data.Apps
import GitHub.Data.Installations
import GitHub.Data.PullRequests.Ext
import GitHub.Endpoints.Installations.AccessTokens
import GitHub.Request

getGitHubAppToken :: MonadIO m => Id App -> AppKey -> Id Installation -> m Text
getGitHubAppToken appId appKey installationId = liftIO $ do
    auth <- authJWTMax appId appKey
    let req = accessTokenForR installationId
    either throwIO (pure . atToken) =<< github auth req
