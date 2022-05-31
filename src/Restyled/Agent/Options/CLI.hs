module Restyled.Agent.Options.CLI
    ( OptionsCLI(..)
    , parseCLI
    ) where

import Restyled.Agent.Prelude

import Options.Applicative

data OptionsCLI = OptionsCLI
    { oDebug :: Bool
    , oTrace :: Bool
    , oNet :: Maybe Text
    }

parseCLI :: IO OptionsCLI
parseCLI = execParser $ parse options "Run a RestyleMachine Agent"

-- brittany-disable-next-binding

options :: Parser OptionsCLI
options = OptionsCLI
    <$> switch
        (  long "debug"
        <> help "Log our own DEBUG messages"
        )
    <*> switch
        (  long "trace"
        <> help "Also log AWS DEBUG messages"
        )
    <*> optional (option str
        (  long "net"
        <> help "Docker --net option"
        ))

parse :: Parser a -> String -> ParserInfo a
parse p d = info (p <**> helper) $ fullDesc <> progDesc d
