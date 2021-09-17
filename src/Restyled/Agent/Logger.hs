module Restyled.Agent.Logger
    ( getLogFunc
    ) where

import RIO

import Data.ByteString.Builder (byteString, toLazyByteString)
import qualified Data.ByteString.Char8 as BS8
import qualified RIO.Text as T

getLogFunc
    :: Handle
    -- ^ Handle for output
    -> LogLevel
    -- ^ Minimum @'LogLevel'@
    -> Text
    -- ^ Prefix
    -> LogFunc
getLogFunc h logLevel prefix = mkLogFunc $ \_cs source level msg -> do
    let
        source'
            | T.null source = source
            | otherwise = "[" <> source <> "] "
    when (level >= logLevel)
        $ BS8.hPutStrLn h
        $ toStrictBytes
        $ toLazyByteString
        $ levelBuilder level
        <> " "
        <> byteString (encodeUtf8 prefix)
        <> byteString (encodeUtf8 source')
        <> getUtf8Builder msg

levelBuilder :: LogLevel -> Builder
levelBuilder = byteString . encodeUtf8 . logLevelToText

logLevelToText :: LogLevel -> Text
logLevelToText = \case
    LevelDebug -> "DEBUG"
    LevelInfo -> "INFO"
    LevelWarn -> "WARN"
    LevelError -> "ERROR"
    LevelOther x -> x
