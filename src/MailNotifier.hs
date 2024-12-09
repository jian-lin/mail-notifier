module MailNotifier (app) where

import Colog (Message, WithLog, logDebug, logInfo, logWarning)
import Data.List.NonEmpty ((<|))
import MailNotifier.DBus (sync)
import MailNotifier.Exception
import MailNotifier.Mail (watch)
import MailNotifier.Types
import MailNotifier.Utils (busName, interface, objectPath, withDBus, withImap)
import MailNotifier.Watchdog (watchdog)
import Network.HaskellNet.IMAP.SSL (Settings (..), defaultSettingsIMAPSSL)
import Relude hiding (getArgs) -- FIXME
import UnliftIO (MonadUnliftIO, mapConcurrently, throwIO)

-- TODO patch HaskellNetSSL to replace connection with crypton-connection
-- https://github.com/dpwright/HaskellNet-SSL/pull/34/files
-- https://github.com/dpwright/HaskellNet-SSL/pull/33/files

-- TODO handle exceptions (or not? do they worth being handling?)
-- TODO add some doc string

warnArgs :: (WithLog env Message m, MonadIO m, HasArgs env) => m ()
warnArgs = do
  args <- asks getArgs

  -- running an unbounded numbder of threads concurrently is a bad pattern
  -- since too many threads can cause resouces issues
  --   a server may limit number of connections from a single client
  --   a client may use up its memory (less likely in this case)
  --   a client may increase the overhead of its sheduler
  let mailboxNum = length $ mailboxes args
  when (mailboxNum > 10) $ logWarning $ "too many mailboxes: " <> show mailboxNum

  watchdogTimeoutStringMaybe <- liftIO $ lookupEnv "WATCHDOG_USEC"
  let watchdogTimeoutIntMaybe :: Maybe Int
      watchdogTimeoutIntMaybe = watchdogTimeoutStringMaybe >>= readMaybe
  case watchdogTimeoutIntMaybe of
    Just watchdogTimeout ->
      when (watchdogTimeout <= pollInterval args || watchdogTimeout <= (idleTimeout args * 1_000))
        $ logWarning
        $ "systemd WatchdogSec ("
        <> show watchdogTimeout
        <> ") is smaller than poll interval ("
        <> show (pollInterval args)
        <> ") or idle timeout ("
        <> show (idleTimeout args * 1_000)
        <> ") (unit: us)"
    Nothing -> pure ()

-- TODO split env in ReaderT of sync and watch according to the "Next Level MTL" video
app ::
  ( HasArgs env,
    HasWatchdogState env,
    HasSyncJobQueue env,
    WithLog env Message m,
    MonadReader env m,
    MonadUnliftIO m
  ) =>
  m (NonEmpty Void)
app = do
  args <- asks getArgs
  logDebug $ show args
  -- TODO is it better to: bracket openFile hClose $ \h -> ...
  -- what happens if readFile fails in the process of reading an opened file? will file be closed?
  ePassword <- decodeUtf8' <$> readFileBS (passwordFile args)
  case ePassword of
    Left err -> throwIO $ PasswordDecodeException err
    Right password -> do
      warnArgs
      logInfo
        $ "DBus: "
        <> show busName
        <> " "
        <> show objectPath
        <> " "
        <> show interface
      let imapSettings =
            defaultSettingsIMAPSSL
              { sslMaxLineLength = 100_000,
                -- NOTE setting sslLogToConsole to True will print your password in clear text!
                sslLogToConsole = False
              }
          watchOneMailbox mailbox =
            withImap (server args) imapSettings (watch (toString password) mailbox)
      mapConcurrently id $ withDBus sync <| watchdog <| fmap watchOneMailbox (mailboxes args)
