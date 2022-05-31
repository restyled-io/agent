module Restyled.Agent.Prelude
    ( module X
    , eitherDecodeText
    , exitCodeInt
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
import Control.Monad.Logger.Aeson as X
import Data.Aeson as X hiding (Options)
import Data.Functor.Syntax as X ((<$$>))
import RIO.Text as X (pack, unpack)
import RIO.Time as X (UTCTime, diffUTCTime, getCurrentTime)
import System.Process.Typed as X

import qualified RIO.ByteString.Lazy as BSL

eitherDecodeText :: FromJSON a => Text -> Either String a
eitherDecodeText = eitherDecode . BSL.fromStrict . encodeUtf8

exitCodeInt :: ExitCode -> Int
exitCodeInt = \case
    ExitSuccess -> 0
    ExitFailure i -> i
