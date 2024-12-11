module MailNotifier.App where

import DBus (methodCall, methodCallDestination)
import DBus.Client
  ( RequestNameReply (NamePrimaryOwner),
    autoMethod,
    call_,
    defaultInterface,
    emit,
    export,
    interfaceMethods,
    interfaceName,
    nameDoNotQueue,
    requestName,
  )
import DBus.Internal.Message (Signal (..))
import Data.HashMap.Strict (elems, lookup)
import MailNotifier.Exception
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

newtype App (env :: (Type -> Type) -> Type) a = App {runApp :: ReaderT (env (App env)) IO a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadReader (env (App env)),
      MonadIO,
      MonadUnliftIO
    )

run :: App env a -> env (App env) -> IO a
run app = runReaderT (runApp app)

instance MonadMailRead (App env) where
  loginM (ImapConnection conn) (Username username) (Password password) =
    liftIO $ login conn (toString username) (toString password)
  getCapabilitiesM (ImapConnection conn) = liftIO $ (Capability . toText) <<$>> capability conn
  listMailboxesM (ImapConnection conn) = liftIO $ (Mailbox . toText . snd) <<$>> list conn
  selectMailboxM (ImapConnection conn) (Mailbox mailbox) = liftIO $ select conn (toString mailbox)
  getMailNumM (ImapConnection conn) = liftIO $ exists conn
  idleOrSleepM (ImapConnection conn) timeout mode =
    liftIO
      $ if mode == Idle
        then idle conn (fromInteger $ unTimeout timeout)
        else threadDelay (fromInteger $ unTimeout timeout)

instance MonadSync (App env) where
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

instance MonadWatchdog (App env) where
  signalCheckedMailboxM mailbox (WatchdogState watchdogState) =
    case lookup mailbox watchdogState of
      Just stateOfThisMailbox -> void $ atomically $ tryPutTMVar stateOfThisMailbox ()
      Nothing -> throwIO $ WatchdogMailboxError mailbox
  notiftyWatchdogWhenAllMailboxesAreCheckedM (WatchdogState watchdogState) = do
    atomically $ mapM_ takeTMVar $ elems watchdogState
    liftIO notifyWatchdog

instance MonadIORead (App env) where
  readFileM filePath = do
    eContent <- decodeUtf8' <$> readFileBS filePath
    case eContent of
      Left err -> throwIO $ PasswordDecodeException err
      Right content -> pure content
  lookupEnvM = (fmap . fmap) toText . lookupEnv . toString

instance MonadAsync (App env) where
  concurrentlyManyM = mapConcurrently id

instance MonadDBus (App env) where
  requestNameM (DBusClient client) busName' = do
    reply <- liftIO $ requestName client (unDBusBusName busName') [nameDoNotQueue]
    when (reply /= NamePrimaryOwner)
      $ throwIO
      $ DBusRequestNameError busName'
      $ DBusRequestNameReply reply
  exportM (DBusClient client) (DBusObjectPath objPath) (DBusInterfaceName ifName) (DBusMemberName methodName) action =
    liftIO
      $ export
        client
        objPath
        defaultInterface
          { interfaceName = ifName,
            interfaceMethods = [autoMethod methodName action]
          }
  waitSyncJobsM (SyncJobQueue queue) timeout = do
    atomically $ readTBQueue queue
    atomicallyTimeoutUntilFail_ timeout $ readTBQueue queue
  emitM (DBusClient client) (DBusObjectPath objPath) (DBusInterfaceName ifName) (DBusMemberName signalName) = do
    let signal =
          Signal
            { signalPath = objPath,
              signalInterface = ifName,
              signalMember = signalName,
              signalSender = Nothing,
              signalDestination = Nothing,
              signalBody = []
            }
    liftIO $ emit client signal
