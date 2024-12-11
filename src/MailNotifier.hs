module MailNotifier (app, appDBusBroker) where

import Colog (Message, WithLog, logDebug, logInfo, logWarning)
import Data.List.NonEmpty ((<|))
import MailNotifier.DBus (sync)
import MailNotifier.Mail (watch)
import MailNotifier.Types
import MailNotifier.Utils
  ( busName,
    interface,
    muaSyncSignalName,
    objectPath,
    syncNotificationMethodName,
    withDBus,
    withImap,
  )
import MailNotifier.Watchdog (watchdog)
import Network.HaskellNet.IMAP.SSL (Settings (..), defaultSettingsIMAPSSL)
import Relude
import UnliftIO (writeTBQueue)

-- TODO patch HaskellNetSSL to replace connection with crypton-connection
-- https://github.com/dpwright/HaskellNet-SSL/pull/34/files
-- https://github.com/dpwright/HaskellNet-SSL/pull/33/files

-- TODO handle exceptions (or not? do they worth being handling?)
-- TODO add some doc string

warnConfig :: (WithLog env Message m, HasConfig env) => Maybe Text -> m ()
warnConfig mWatchdogTimeoutString = do
  config <- asks getConfig

  -- running an unbounded numbder of threads concurrently is a bad pattern
  -- since too many threads can cause resouces issues
  --   a server may limit number of connections from a single client
  --   a client may use up its memory (less likely in this case)
  --   a client may increase the overhead of its sheduler
  let mailboxNum = length $ mailboxes config
  when (mailboxNum > 10) $ logWarning $ "too many mailboxes: " <> show mailboxNum

  case mWatchdogTimeoutString >>= (readMaybe . toString) of
    Just watchdogTimeout ->
      when
        ( (watchdogTimeout <= unTimeout (pollInterval config))
            || (watchdogTimeout <= unTimeout (idleTimeout config * 1_000))
        )
        $ logWarning
        $ "systemd WatchdogSec ("
        <> show watchdogTimeout
        <> ") is smaller than poll interval ("
        <> show (pollInterval config)
        <> ") or idle timeout ("
        <> show (idleTimeout config * 1_000)
        <> ") (unit: us)"
    Nothing -> pure ()

app ::
  ( HasConfig env,
    HasWatchdogState env,
    MonadWatchdog m,
    HasSyncJobQueue env,
    MonadSync m,
    WithLog env Message m,
    MonadReader env m,
    MonadAsync m,
    MonadIORead m,
    MonadMailRead m
  ) =>
  m (NonEmpty Void)
app = do
  config <- asks getConfig
  logDebug $ show config
  password <- readFileM (passwordFile config)
  warnConfig =<< lookupEnvM "WATCHDOG_USEC"
  logInfo $ "DBus: " <> show busName <> " " <> show objectPath <> " " <> show interface
  let imapSettings =
        defaultSettingsIMAPSSL
          { sslMaxLineLength = 100_000,
            -- NOTE setting sslLogToConsole to True will print your password in clear text!
            sslLogToConsole = False
          }
      watchOneMailbox mailbox =
        withImap (server config) imapSettings (watch (Password password) mailbox)
  concurrentlyManyM $ withDBus sync <| watchdog <| fmap watchOneMailbox (mailboxes config)

emitSignal :: (WithLog env Message m, HasSyncJobQueue env, MonadDBus m) => DBusClient -> m Void
emitSignal client = infinitely $ do
  queue <- asks getSyncJobQueue
  waitSyncJobsM queue 1_000_000
  emitM client objectPath interface muaSyncSignalName
  logInfo $ "emitted signal: " <> show muaSyncSignalName

-- TODO try to add some log
getSyncNotification :: SyncJobQueue -> IO ()
getSyncNotification (SyncJobQueue queue) = atomically $ writeTBQueue queue ()

appDBusBroker :: (WithLog env Message m, HasSyncJobQueue env, MonadDBus m) => DBusClient -> m Void
appDBusBroker client = do
  logInfo $ "try to request " <> show busName
  requestNameM client busName
  logInfo $ "requested " <> show busName
  queue <- asks getSyncJobQueue
  exportM client objectPath interface syncNotificationMethodName (getSyncNotification queue)
  logInfo
    $ "exported method "
    <> show syncNotificationMethodName
    <> " at "
    <> show objectPath
    <> " "
    <> show interface
  logInfo "wait for sync notifications"
  emitSignal client
