module Main (main) where

import Colog (Severity (Info))
import Data.HashMap.Strict qualified as HM
import MailNotifier (app)
import MailNotifier.App
import MailNotifier.Types
import MailNotifier.Utils (mkLogAction)
import Options.Applicative (Parser)
import Options.Applicative qualified as O
import Options.Applicative.NonEmpty (some1)
import Relude
import UnliftIO (newTBQueue)

-- TODO split this big parser into smaller ones and then compose those small ones
-- TODO parse this more strict: positive integer within maxBound :: Int
-- TODO maybe parse name more strict: non-empty
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
    <*> O.option -- TODO warn if too long since conn may be cut by middle boxes
      O.auto
      ( O.long "idle-timeout"
          <> O.metavar "MILLISECOND"
          <> O.showDefault
          <> O.value (2 * 60 * 1_000)
          <> O.help "Timeout for IMAP IDLE command"
      )
    <*> O.option
      O.auto
      ( O.long "read-sync-jobs-timeout"
          <> O.metavar "MICROSECOND"
          <> O.showDefault
          <> O.value 5_000_000
          <> O.help "Timeout for reading following sync jobs before performing one sync"
      )
    <*> O.option -- TODO warn if too long since conn may be cut by middle boxes
      O.auto
      ( O.long "poll-interval"
          <> O.metavar "MICROSECOND"
          <> O.showDefault
          <> O.value (2 * 60 * 1_000_000)
          <> O.help "Interval for polling new mails (fallback if IDLE is not supported)"
      )
    <*> O.option -- TODO find a way to show valid values
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

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering -- print log while running under systemd
  config <- parseConfig
  -- no exception raised from Integer -> Natural since mailboxes is NonEmpty
  let mailboxNum = length $ mailboxes config
      queueSize = 3 * fromInteger (toInteger mailboxNum)
  syncJobQueue <- atomically $ newTBQueue queueSize
  vs <- atomically $ replicateM mailboxNum newEmptyTMVar
  let env =
        Env
          { envLogAction = mkLogAction $ logLevel config,
            envSyncJobQueue = syncJobQueue,
            envWatchdogState = HM.fromList $ zip (toList $ mailboxes config) vs,
            envConfig = config
          }
  foldMap absurd <$> run app env
