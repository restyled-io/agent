module Restyled.Agent.Prelude
  ( module X
  , eitherDecodeText
  , exitCodeInt
  ) where

import Relude as X

import Blammo.Logging as X
import Control.Monad.IO.Unlift as X (MonadUnliftIO (..), unliftIO, withUnliftIO)
import Data.Aeson as X hiding (One, Options)
import Data.Functor.Syntax as X ((<$$>))
import Data.Text as X (pack, unpack)
import Data.Time as X (UTCTime, diffUTCTime, getCurrentTime)
import Data.Traversable as X (for)
import GHC.Records as X
import Lens.Micro.Platform as X
  ( Lens'
  , lens
  , over
  , view
  , (.~)
  , (?~)
  , (^.)
  , _1
  , _2
  )
import System.Process.Typed as X
import UnliftIO.Async as X (Async, async, race, wait)
import UnliftIO.Concurrent as X (threadDelay)
import UnliftIO.Exception as X (handleAny, throwIO, tryAny)
import UnliftIO.Temporary as X (withSystemTempFile)

eitherDecodeText :: FromJSON a => Text -> Either String a
eitherDecodeText = eitherDecode . fromStrict . encodeUtf8

exitCodeInt :: ExitCode -> Int
exitCodeInt = \case
  ExitSuccess -> 0
  ExitFailure i -> i
