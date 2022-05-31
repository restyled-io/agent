module Restyled.Api.MarketplacePlanAllows
    ( MarketplacePlanAllows(..)
    , MarketplacePlanLimitation(..)
    ) where

import Restyled.Agent.Prelude

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
