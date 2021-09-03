module Restyled.Agent.Logger
    ( getLogFunc
    )
where

import RIO

import Data.ByteString.Builder (byteString, toLazyByteString)
import qualified Data.ByteString.Char8 as BS8

getLogFunc
    :: Handle
    -- ^ Handle for output
    -> LogLevel
    -- ^ Minimum @'LogLevel'@
    -> Text
    -- ^ Prefix
    -> LogFunc
getLogFunc h logLevel prefix = mkLogFunc $ \_cs _source level msg ->
    when (level >= logLevel)
        $ BS8.hPutStrLn h
        $ toStrictBytes
        $ toLazyByteString
        $ levelBuilder level
        <> " "
        <> byteString (encodeUtf8 prefix)
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
