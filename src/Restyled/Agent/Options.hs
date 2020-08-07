module Restyled.Agent.Options
    ( Options(..)
    , HasOptions(..)
    , parseOptions
    )
where

import RIO

import Options.Applicative

data Options = Options
    { oName :: Maybe Text
    , oHost :: Text
    , oInstance :: Text
    , oCerts :: FilePath
    , oRestyledHost :: Text
    , oRestyledToken :: Text
    , oEnvironment :: Text
    , oQueueUrl :: Text
    , oDebug :: Bool
    , oTrace :: Bool
    }
    deriving stock Show

class HasOptions env where
    optionsL :: Lens' env Options

parseOptions :: IO Options
parseOptions = execParser $ parse options "Run a RestyleMachine Agent"

-- brittany-disable-next-binding

options :: Parser Options
options = Options
    <$> optional (option str
        (  long "name"
        <> help "Machine Name, if already in service"
        ))
    <*> option str
        (  long "host"
        <> help "Public DNS IPv5 for the instance"
        )
    <*> option str
        (  long "instance"
        <> help "Instance Id for the instance"
        )
    <*> option str
        (  long "certs"
        <> help "Directory containing {ca,cert,key}.pem files"
        <> value "/certs")
    <*> option str
        (  long "restyled-host"
        <> help "Base URL for Restyled API"
        <> value "https://restyled.io")
    <*> option str
        (  long "restyled-token"
        <> help "Token for Restyled API"
        )
    <*> option str
        (  long "env"
        <> help "Environment we're operating in"
        <> value "dev"
        )
    <*> option str
        (  long "sqs-queue-url"
        <> help "SQS Queue URL for LifecycleHook notifications"
        )
    <*> switch
        (  long "debug"
        <> help "Log our own DEBUG messages"
        )
    <*> switch
        (  long "trace"
        <> help "Also log AWS DEBUG messages"
        )

parse :: Parser a -> String -> ParserInfo a
parse p d = info (p <**> helper) $ fullDesc <> progDesc d
