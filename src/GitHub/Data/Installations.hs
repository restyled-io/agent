{-# LANGUAGE FieldSelectors #-}
{-# LANGUAGE RecordWildCards #-}

module GitHub.Data.Installations
  ( Installation (..)
  ) where

import Prelude

import Data.Aeson
import GitHub.Data

newtype Installation = Installation
  { installationId :: Id Installation
  }

instance FromJSON Installation where
  parseJSON = withObject "Installation" $ \o -> Installation <$> o .: "id"

instance ToJSON Installation where
  toJSON Installation {..} = object ["id" .= installationId]
  toEncoding Installation {..} = pairs $ "id" .= installationId
