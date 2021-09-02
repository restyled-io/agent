module Restyled.Api.MarketplacePlanAllows
    ( MarketplacePlanAllows(..)
    , MarketplacePlanLimitation(..)
    ) where

import RIO

import Data.Aeson
import Data.Time (UTCTime)

data MarketplacePlanAllows
    = MarketplacePlanAllows
    | MarketplacePlanForbids MarketplacePlanLimitation
    deriving stock Generic
    deriving anyclass FromJSON

data MarketplacePlanLimitation
    = MarketplacePlanNotFound
    | MarketplacePlanPublicOnly
    | MarketplacePlanMaxRepos
    | MarketplacePlanAccountExpired UTCTime
    deriving stock Generic
    deriving anyclass FromJSON
