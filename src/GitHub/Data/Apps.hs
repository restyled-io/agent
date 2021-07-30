module GitHub.Data.Apps
    ( App
    , AppKey
    , unAppKey
    )
where

import Prelude

import Data.String

data App

newtype AppKey = AppKey { unAppKey :: String }
    deriving newtype (Show, IsString)
