module Restyled.Agent.Options.CLI
    ( OptionsCLI(..)
    , parseCLI
    ) where

import Restyled.Agent.Prelude

import Options.Applicative

newtype OptionsCLI = OptionsCLI
    { oNet :: Maybe Text
    }

parseCLI :: IO OptionsCLI
parseCLI = execParser $ parse options "Run a RestyleMachine Agent"

-- brittany-disable-next-binding

options :: Parser OptionsCLI
options = OptionsCLI
    <$> optional (option str
        (  long "net"
        <> help "Docker --net option"
        ))

parse :: Parser a -> String -> ParserInfo a
parse p d = info (p <**> helper) $ fullDesc <> progDesc d
