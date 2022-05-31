module Restyled.Agent.Prelude
    ( module X
    , displayT
    , eitherDecodeText
    ) where

import RIO as X hiding
    ( LogLevel(..)
    , LogSource
    , logDebug
    , logDebugS
    , logError
    , logErrorS
    , logInfo
    , logInfoS
    , logOther
    , logOtherS
    , logWarn
    , logWarnS
    )

import Control.Lens as X (_1, _2, (?~))
import Control.Monad.Logger.CallStack as X
import Data.Aeson as X hiding (Options)
import Data.Functor.Syntax as X ((<$$>))
import RIO.Text as X (pack, unpack)
import RIO.Time as X (UTCTime, diffUTCTime, getCurrentTime)
import System.Process.Typed as X

import qualified RIO.ByteString.Lazy as BSL

displayT :: Display a => a -> Text
displayT = utf8BuilderToText . display

eitherDecodeText :: FromJSON a => Text -> Either String a
eitherDecodeText = eitherDecode . BSL.fromStrict . encodeUtf8
