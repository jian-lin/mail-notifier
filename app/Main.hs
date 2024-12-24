{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Colog (HasLog (..), LogAction, Message, Severity (Info))
import MailNotifier (app)
import MailNotifier.App (run)
import MailNotifier.Types
import MailNotifier.Utils (mkLogAction)
import Options.Applicative (Parser)
import Options.Applicative qualified as O
import Options.Applicative.NonEmpty (some1)
import Relude
import UnliftIO (newTBQueue)

timeoutReader :: (Timeout t) => String -> Either String t
timeoutReader s = do
  timeout <- first toString $ readEither s
  first show $ mkTimeout timeout

configParser :: Parser Config
configParser =
  Config
    <$> O.strOption
      ( O.long "account-name"
          <> O.metavar "ACCOUNT-NAME"
          <> O.help "Mail account name"
      )
    <*> O.strOption
      ( O.long "server"
          <> O.metavar "SERVER"
          <> O.help "IMAP server address without port"
      )
    <*> O.strOption
      ( O.long "username"
          <> O.metavar "USERNAME"
          <> O.help "Username"
      )
    <*> O.strOption
      ( O.long "password-file"
          <> O.metavar "PASSWORD-FILE"
          <> O.help "Password file"
      )
    <*> O.strOption
      ( O.long "mbsync-config-file"
          <> O.metavar "MBSYNC-CONFIG-FILE"
          <> O.help "mbsync config file"
      )
    <*> some1 -- TODO make its element unique
      ( O.argument
          O.str
          ( O.metavar "MAILBOXES"
              <> O.help "Mailboxes to watch"
          )
      )
    <*> O.option
      (O.eitherReader timeoutReader)
      ( O.long "idle-timeout"
          <> O.metavar "MILLISECOND"
          <> O.showDefaultWith (show . unTimeout)
          <> O.value $(mkTimeoutMilliSecondTH $ 2 * 60 * 1_000)
          <> O.help "Timeout for IMAP IDLE command"
      )
    <*> O.option
      (O.eitherReader timeoutReader)
      ( O.long "read-sync-jobs-timeout"
          <> O.metavar "MICROSECOND"
          <> O.showDefaultWith (show . unTimeout)
          <> O.value $(mkTimeoutMicroSecondTH 5_000_000)
          <> O.help "Timeout for reading following sync jobs before performing one sync"
      )
    <*> O.option
      (O.eitherReader timeoutReader)
      ( O.long "poll-interval"
          <> O.metavar "MICROSECOND"
          <> O.showDefaultWith (show . unTimeout)
          <> O.value $(mkTimeoutMicroSecondTH $ 2 * 60 * 1_000_000)
          <> O.help "Interval for polling new mails (fallback if IDLE is not supported)"
      )
    <*> O.option
      O.auto
      ( O.long "log-level"
          <> O.metavar "LOG-LEVEL"
          <> O.showDefault
          <> O.value Info
          <> O.help (show [minBound :: Severity ..])
      )

-- TODO use synopsis and version from PackageInfo_mail_notifier when cabal2nix supports cabal-version 3.12
parseConfig :: IO Config
parseConfig =
  O.execParser
    $ O.info
      (configParser <**> O.helper <**> O.simpleVersioner "0.1.0.0")
      (O.fullDesc <> O.progDesc "A tool to immediately run mbsync and notify your MUA for new mails")

data Env m = Env
  { envLogAction :: !(LogAction m Message),
    envSyncJobQueue :: !SyncJobQueue,
    envWatchdogState :: !WatchdogState,
    envConfig :: !Config
  }

instance HasLog (Env m) Message m where
  getLogAction = envLogAction
  {-# INLINE getLogAction #-}
  setLogAction newLogAction env = env {envLogAction = newLogAction}
  {-# INLINE setLogAction #-}

instance HasConfig (Env m) where
  getConfig = envConfig
  {-# INLINE getConfig #-}

instance HasSyncJobQueue (Env m) where
  getSyncJobQueue = envSyncJobQueue
  {-# INLINE getSyncJobQueue #-}

instance HasWatchdogState (Env m) where
  getWatchdogState = envWatchdogState
  {-# INLINE getWatchdogState #-}

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering -- print log while running under systemd
  config <- parseConfig
  -- Since mailboxNum >= 0, it can be safely converted to Natural.
  let mailboxNum = length $ mailboxes config
      queueSize = 3 * fromIntegral mailboxNum
  syncJobQueue <- atomically $ newTBQueue queueSize
  vs <- atomically $ replicateM mailboxNum newEmptyTMVar
  let env =
        Env
          { envLogAction = mkLogAction $ logLevel config,
            envSyncJobQueue = SyncJobQueue syncJobQueue,
            envWatchdogState = WatchdogState $ fromList $ zip (toList $ mailboxes config) vs,
            envConfig = config
          }
  foldMap absurd <$> run app env
