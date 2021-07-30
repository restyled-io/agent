module Restyled.Agent.Restyler
    ( processPullRequestEvent
    )
where

import RIO

import Control.Monad.Validate
import Restyled.Agent.GitHub
import Restyled.Agent.Options
import Restyled.Agent.Process
import qualified Restyled.Api as Api
import Restyled.Api.Job (ApiJob, apiJobIdToText, apiJobSpec)
import qualified Restyled.Api.Job as ApiJob
import Restyled.Api.MarketplacePlanAllows
import Restyled.Api.Repo (ApiRepo)
import qualified Restyled.Api.Repo as ApiRepo
import qualified RIO.NonEmpty as NE
import RIO.Process
import RIO.Text (pack, unpack)
import qualified RIO.Text as T
import RIO.Time

processPullRequestEvent
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HasLogFunc env
       , HasOptions env
       , HasProcessContext env
       )
    => PullRequestEvent
    -> m ()
processPullRequestEvent event
    | not $ pullRequestEventIsInteresting event
    = logDebug
        $ "Ignoring "
        <> displayShow (pullRequestEventAction event)
        <> " PR Event"
    | otherwise
    = do
        repo <- Api.upsertRepo
            (simpleOwnerLogin $ repoOwner $ pullRequestRepository event)
            (repoName $ pullRequestRepository event)
            (repoPrivate $ pullRequestRepository event)
            (installationId $ pullRequestInstallation event)
        job <- Api.createJob repo pullRequest
        result <- runValidateT $ validatePullRequestEvent repo event

        logInfo
            $ "Job "
            <> display job
            <> " validated, result="
            <> displayShow result

        start <- liftIO getCurrentTime
        exitCode <- case result of
            Left messages -> dockerRunSkippedJob repo job messages
            Right () -> dockerRunJob repo job

        finish <- liftIO getCurrentTime
        logInfo
            $ "Job "
            <> display job
            <> " complete, exitCode="
            <> displayShow exitCode
            <> ", duration="
            <> displayShow (diffUTCTime finish start)

        void $ Api.completeJob (ApiJob.id job) exitCode
    where pullRequest = pullRequestNumber $ pullRequestEventPullRequest event

dockerRunJob
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HasLogFunc env
       , HasOptions env
       , HasProcessContext env
       )
    => ApiRepo
    -> ApiJob
    -> m ExitCode
dockerRunJob repo job = handleAny (exceptionHandler repo job) $ do
    Options {..} <- view optionsL
    token <- getGitHubAppToken oGitHubAppId oGitHubAppKey
        $ ApiRepo.installationId repo

    runRestylerImage
        repo
        job
        (mconcat
            [ maybe [] (\n -> ["--net", n]) oNet
            , ["--env", "GITHUB_ACCESS_TOKEN=" <> token]
            , optionalEnv "STATSD_HOST" $ pack <$> oStatsdHost
            , optionalEnv "STATSD_PORT" $ pack . show <$> oStatsdPort
            , optionalEnv "DEBUG" $ "1" <$ guard
                (ApiRepo.restylerLogLevel repo == "DEBUG")
            ]
        )
        ["--job-url", ApiJob.url job, apiJobSpec job]
  where
    optionalEnv name = \case
        Nothing -> []
        Just x -> ["--env", name <> "=" <> x]

exceptionHandler
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HasOptions env
       , HasLogFunc env
       , HasProcessContext env
       )
    => ApiRepo
    -> ApiJob
    -> SomeException
    -> m ExitCode
exceptionHandler repo job ex = do
    logError $ "Exception running Job: " <> displayShow ex
    let messages = pure "Unexpected error running Job"
    ExitFailure 99 <$ dockerRunSkippedJob repo job messages

dockerRunSkippedJob
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HasOptions env
       , HasLogFunc env
       , HasProcessContext env
       )
    => ApiRepo
    -> ApiJob
    -> NonEmpty Text
    -> m ExitCode
dockerRunSkippedJob repo job messages = withSystemTempFile "" $ \tmp h -> do
    hPutBuilder h $ encodeUtf8Builder $ formatMessages messages
    hFlush h
    handleAny warn $ void $ runRestylerImage
        repo
        job
        ["--entrypoint", "cat"]
        [pack tmp]
    pure ExitSuccess
  where
    prefix = "Skipping Job:"
    formatMessages = \case
        (msg :| []) -> prefix <> " " <> msg
        _ -> T.unlines $ (prefix :) $ map ("  • " <>) $ NE.toList messages
    warn ex =
        logWarn
            $ "Ignoring exception during skipped-job-handling: "
            <> displayShow ex

runRestylerImage
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HasOptions env
       , HasLogFunc env
       , HasProcessContext env
       )
    => ApiRepo
    -> ApiJob
    -> [Text] -- ^ Arguments for docker-run
    -> [Text] -- ^ Arguments for restyled
    -> m ExitCode
runRestylerImage repo job dockerArgs restylerArgs = proc
    "docker"
    (map unpack $ mconcat
        [ ["run", "--rm"]
        , ["--label", "restyler"]
        , ["--label", "job-id=" <> apiJobIdToText (ApiJob.id job)]
        , ["--log-driver=awslogs"]
        , ["--log-opt", "awslogs-region=us-east-1"]
        , ["--log-opt", "awslogs-group=" <> ApiJob.awsLogGroup job]
        , ["--log-opt", "awslogs-stream=" <> ApiJob.awsLogStream job]
        , ["--volume", "/tmp:/tmp"]
        , ["--volume", "/var/run/docker.sock:/var/run/docker.sock"]
        , dockerArgs
        , [ApiRepo.restylerImage repo]
        , restylerArgs
        ]
    )
    runProcessDevNull

validatePullRequestEvent
    :: MonadValidate (NonEmpty Text) m => ApiRepo -> PullRequestEvent -> m ()
validatePullRequestEvent repo event =
    validateAgainstPullBot event
        *> validateAgainstSelf event
        *> validateAgainstDisabled repo
        *> validateMarketplacePlanAllows repo

validateAgainstPullBot
    :: MonadValidate (NonEmpty Text) m => PullRequestEvent -> m ()
validateAgainstPullBot event = refuteWhen messages $ author == "pull[bot]"
  where
    messages = pure "Ignoring pull[bot] Pull Request"
    author = pullRequestEventAuthor event

validateAgainstSelf
    :: MonadValidate (NonEmpty Text) m => PullRequestEvent -> m ()
validateAgainstSelf event = refuteWhen messages $ isRestyledBot author
  where
    messages = pure "Ignoring Restyled Pull Request"
    author = pullRequestEventAuthor event
    isRestyledBot =
        (&&) <$> ("restyled-io" `T.isPrefixOf`) <*> ("[bot]" `T.isSuffixOf`)

validateAgainstDisabled :: MonadValidate (NonEmpty Text) m => ApiRepo -> m ()
validateAgainstDisabled repo = refuteUnless (pure message)
    $ ApiRepo.isEnabled repo
  where
    message =
        "The repository "
            <> toPathPart (ApiRepo.owner repo)
            <> "/"
            <> toPathPart (ApiRepo.name repo)
            <> "has been disabled."
            <> " If you believe this is an error,"
            <> " please reach out to support@restyled.io"

validateMarketplacePlanAllows
    :: MonadValidate (NonEmpty Text) m => ApiRepo -> m ()
validateMarketplacePlanAllows repo = case ApiRepo.marketplacePlanAllows repo of
    MarketplacePlanAllows -> pure ()
    MarketplacePlanForbids limitation -> refute $ pure $ message limitation
  where
    message = \case
        MarketplacePlanNotFound ->
            "No Marketplace plan for the owner of this repository ("
                <> ownerName
                <> ")"
        MarketplacePlanPublicOnly ->
            "Marketplace plan for the owner of this repository ("
                <> ownerName
                <> ") only allows public repositories"
        MarketplacePlanMaxRepos ->
            "You have reached the maximum number of private repositories for the Marketplace plan for the owner of this repository ("
                <> ownerName
                <> ")"
        MarketplacePlanAccountExpired asOf ->
            "Marketplace plan for the owner of this repository ("
                <> ownerName
                <> ") expired at "
                <> pack (show asOf)

    ownerName = toPathPart $ ApiRepo.owner repo

pullRequestEventAuthor :: PullRequestEvent -> Text
pullRequestEventAuthor =
    untagName . simpleUserLogin . pullRequestUser . pullRequestEventPullRequest

pullRequestEventIsInteresting :: PullRequestEvent -> Bool
pullRequestEventIsInteresting =
    (`elem` interestingPullRequestEventActions) . pullRequestEventAction

interestingPullRequestEventActions :: [PullRequestEventType]
interestingPullRequestEventActions =
    [PullRequestOpened, PullRequestClosed, PullRequestSynchronized]

refuteWhen :: MonadValidate e m => e -> Bool -> m ()
refuteWhen e b = when b $ refute e

refuteUnless :: MonadValidate e m => e -> Bool -> m ()
refuteUnless e b = unless b $ refute e
