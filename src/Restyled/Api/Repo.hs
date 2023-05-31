module Restyled.Api.Repo
    ( ApiRepo(..)
    ) where

import Restyled.Agent.Prelude

import Restyled.Agent.GitHub
import Restyled.Api.MarketplacePlanAllows

data ApiRepo = ApiRepo
    { owner :: Name Owner
    , name :: Name Repo
    , isPrivate :: Bool
    , isEnabled :: Bool
    , installationId :: Id Installation
    , marketplacePlanAllows :: MarketplacePlanAllows
    , restylerImage :: Text
    , restylerEnv :: Maybe [Text]
    }
    deriving stock Generic
    deriving anyclass FromJSON
