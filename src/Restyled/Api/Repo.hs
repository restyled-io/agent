module Restyled.Api.Repo
    ( ApiRepo(..)
    ) where

import Restyled.Agent.Prelude

import Restyled.Agent.GitHub

data ApiRepo = ApiRepo
    { owner :: Name Owner
    , name :: Name Repo
    , installationId :: Id Installation
    , restylerImage :: Text
    , restylerEnv :: [Text]
    }
    deriving stock Generic
    deriving anyclass FromJSON
