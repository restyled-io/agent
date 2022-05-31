{-# LANGUAGE NamedFieldPuns #-}

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
    , restylerLogLevel :: Text
    }
    deriving stock Generic
    deriving anyclass FromJSON

instance Display ApiRepo where
    display ApiRepo { owner, name, isPrivate, installationId } =
        display
            $ untagName owner
            <> "/"
            <> untagName name
            <> ", "
            <> (if isPrivate then "private" else "public")
            <> ", installationId:"
            <> toPathPart installationId
