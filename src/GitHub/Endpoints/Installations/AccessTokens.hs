module GitHub.Endpoints.Installations.AccessTokens
  ( accessTokenForR
  ) where

import Prelude

import GitHub.Data
import GitHub.Data.AccessTokens
import GitHub.Data.Installations hiding (installationId)
import GitHub.Request.Preview

accessTokenForR :: Id Installation -> PreviewRequest 'RW AccessToken
accessTokenForR installationId =
  Command
    Post
    ["app", "installations", toPathPart installationId, "access_tokens"]
    mempty
