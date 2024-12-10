module MailNotifier.App where

import DBus (methodCall, methodCallDestination)
import DBus.Client (call_)
import Data.HashMap.Strict (elems, lookup)
import MailNotifier.Exception (PasswordDecodeException (..), WatchdogMailboxError (..))
import MailNotifier.Types
import MailNotifier.Utils
  ( atomicallyTimeoutUntilFail_,
    busName,
    interface,
    objectPath,
    syncNotificationMethodName,
  )
import Network.HaskellNet.IMAP.Connection (exists)
import Network.HaskellNet.IMAP.SSL (capability, idle, list, login, select)
import Relude
import System.Systemd.Daemon (notifyWatchdog)
import UnliftIO (MonadUnliftIO, mapConcurrently, throwIO)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Process (readProcess)
import UnliftIO.STM (readTBQueue, writeTBQueue)

newtype App a = App {runApp :: ReaderT (Env App) IO a}
  deriving newtype (Functor, Applicative, Monad, MonadReader (Env App), MonadIO, MonadUnliftIO)

run :: App a -> Env App -> IO a
run app = runReaderT (runApp app)

instance MonadMailRead App where
  loginM (ImapConnection conn) (Username username) (Password password) =
    liftIO $ login conn (toString username) (toString password)
  getCapabilitiesM (ImapConnection conn) = liftIO $ (Capability . toText) <<$>> capability conn
  listMailboxesM (ImapConnection conn) = liftIO $ (Mailbox . toText . snd) <<$>> list conn
  selectMailboxM (ImapConnection conn) (Mailbox mailbox) = liftIO $ select conn (toString mailbox)
  getMailNumM (ImapConnection conn) = liftIO $ exists conn
  idleOrSleepM (ImapConnection conn) (Timeout timeout) mode =
    liftIO
      $ if mode == Idle
        then idle conn (fromInteger timeout)
        else threadDelay (fromInteger timeout)

instance MonadSync App where
  addSyncJobM (SyncJobQueue queue) = atomically $ writeTBQueue queue ()
  waitForSyncJobsM (SyncJobQueue queue) timeout = do
    _ <- atomically $ readTBQueue queue
    atomicallyTimeoutUntilFail_ timeout $ readTBQueue queue
  syncM program args = toText <$> readProcess program (toString <$> args) ""

  -- DBus tutrial: https://dbus.freedesktop.org/doc/dbus-tutorial.html
  signalSyncDoneM (DBusClient client) =
    liftIO
      $ void
      $ call_
        client
        ( methodCall
            (unDBusObjectPath objectPath)
            (unDBusInterfaceName interface)
            (unDBusMemberName syncNotificationMethodName)
        )
          { methodCallDestination = Just (unDBusBusName busName)
          }

instance MonadWatchdog App where
  signalCheckedMailboxM mailbox (WatchdogState watchdogState) =
    case lookup mailbox watchdogState of
      Just stateOfThisMailbox -> void $ atomically $ tryPutTMVar stateOfThisMailbox ()
      Nothing -> throwIO $ WatchdogMailboxError mailbox
  notiftyWatchdogWhenAllMailboxesAreCheckedM (WatchdogState watchdogState) = do
    atomically $ mapM_ takeTMVar $ elems watchdogState
    liftIO notifyWatchdog

instance MonadIORead App where
  readFileM filePath = do
    eContent <- decodeUtf8' <$> readFileBS filePath
    case eContent of
      Left err -> throwIO $ PasswordDecodeException err
      Right content -> pure content
  lookupEnvM = (fmap . fmap) toText . lookupEnv . toString

instance MonadAsync App where
  concurrentlyManyM = mapConcurrently id
