module Restyled.Agent.Restyler
  ( processPullRequestEvent
  ) where

import Restyled.Agent.Prelude

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Restyled.Agent.GitHub
import Restyled.Agent.Options
import Restyled.Agent.Process
import qualified Restyled.Api as Api
import Restyled.Api.Job (ApiJob, apiJobIdToText, apiJobSpec)
import qualified Restyled.Api.Job as ApiJob
import Restyled.Api.Repo (ApiRepo)
import qualified Restyled.Api.Repo as ApiRepo

processPullRequestEvent
  :: ( MonadUnliftIO m
     , MonadMask m
     , MonadLogger m
     , MonadReader env m
     , HasOptions env
     )
  => PullRequestEvent
  -> m ()
processPullRequestEvent event
  | not $ pullRequestEventIsInteresting event =
      logDebug $ "Ignoring uninteresting PR Event" :# ["event" .= event]
  | otherwise =
      withThreadContext context $ do
        repo <-
          Api.upsertRepo
            owner
            name
            (repoPrivate $ pullRequestRepository event)
            (installationId $ pullRequestInstallation event)

        Options {..} <- view optionsL
        token <-
          getGitHubAppToken oGitHubAppId oGitHubAppKey
            $ ApiRepo.installationId repo

        job <- Api.createJob repo pull

        withThreadContext ["job" .= ApiJob.url job] $ do
          exitCode <-
            handleAny (exceptionHandler repo job)
              $ runRestylerImage
                repo
                job
                ( mconcat
                    [ maybe [] (\n -> ["--net", n]) oNet
                    , ["--env", "GITHUB_ACCESS_TOKEN=" <> token]
                    , ["--env", "STATSD_HOST"]
                    , ["--env", "STATSD_PORT"]
                    , concatMap (\e -> ["--env", e]) $ ApiRepo.restylerEnv repo
                    ]
                )
                ["--job-url", ApiJob.url job, apiJobSpec job]

          logInfo $ "Job complete" :# ["exitCode" .= exitCodeInt exitCode]
          void $ Api.completeJob (ApiJob.id job) exitCode
 where
  owner = simpleOwnerLogin $ repoOwner $ pullRequestRepository event
  name = repoName $ pullRequestRepository event
  pull = pullRequestNumber $ pullRequestEventPullRequest event
  context = ["owner" .= owner, "repo" .= name, "pull" .= pull]

exceptionHandler
  :: (MonadUnliftIO m, MonadLogger m)
  => ApiRepo
  -> ApiJob
  -> SomeException
  -> m ExitCode
exceptionHandler repo job ex = do
  logError $ "Exception running Job" :# ["exception" .= displayException ex]
  let
    messages :: NonEmpty Text
    messages = pure "Unexpected error running Job"
  ExitFailure 99 <$ dockerRunSkippedJob repo job messages

dockerRunSkippedJob
  :: (MonadUnliftIO m, MonadLogger m)
  => ApiRepo
  -> ApiJob
  -> NonEmpty Text
  -> m ExitCode
dockerRunSkippedJob repo job messages = withSystemTempFile "" $ \tmp h -> do
  liftIO $ T.hPutStr h $ formatMessages messages
  hFlush h
  handleAny warn
    $ void
    $ runRestylerImage
      repo
      job
      ["--entrypoint", "sh"]
      [ "-c"
      , "fold --width 72 --spaces '"
          <> pack tmp
          <> "' | jo -d. time=\"$(date --utc --iso-8601=s)\" level=info message.text=@-"
      ]
  pure ExitSuccess
 where
  formatMessages :: NonEmpty Text -> Text
  formatMessages = T.intercalate "\n\n" . toList

  warn :: (MonadLogger m, Exception ex) => ex -> m ()
  warn ex =
    logWarn
      $ "Ignoring exception during skipped-job-handling"
      :# ["exception" .= displayException ex]

runRestylerImage
  :: (MonadUnliftIO m, MonadLogger m)
  => ApiRepo
  -> ApiJob
  -> [Text]
  -- ^ Arguments for docker-run
  -> [Text]
  -- ^ Arguments for restyled
  -> m ExitCode
runRestylerImage repo job dockerArgs restylerArgs = do
  ec <-
    runProcessLogged (logDebugNS "restyler") (logWarnNS "restyler")
      $ proc "docker" args
  when (ec == ExitFailure 125) $ do
    logError $ "Container failed to run (exit code 125)" :# ["args" .= args]
  pure ec
 where
  args =
    map unpack
      $ mconcat
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

pullRequestEventIsInteresting :: PullRequestEvent -> Bool
pullRequestEventIsInteresting =
  (`elem` interestingPullRequestEventActions) . pullRequestEventAction

interestingPullRequestEventActions :: [PullRequestEventType]
interestingPullRequestEventActions =
  [PullRequestOpened, PullRequestClosed, PullRequestSynchronized]
