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
import MailNotifier.Utils (atomicallyTimeoutUntilFail_)
import Network.HaskellNet.IMAP (capability, idle, list, login, select)
import Network.HaskellNet.IMAP.Connection (exists)
import Relude
import System.Exit (ExitCode (ExitSuccess))
import System.Systemd.Daemon (notifyReady, notifyWatchdog)
import UnliftIO (MonadUnliftIO, handle, handleAny, mapConcurrently, throwIO)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Process (readProcessWithExitCode)
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
  loginM (ImapConnection conn) username password =
    handleAny (throwIO . MailLoginError username)
      $ liftIO
      $ login conn (toString username) (toString password)
  getCapabilitiesM (ImapConnection conn) =
    handleAny (throwIO . MailGetCapabilityError)
      $ liftIO (Capability . toText <<$>> capability conn)
  listMailboxesM (ImapConnection conn) =
    handleAny (throwIO . MailListMailboxError)
      $ liftIO (Mailbox . toText . snd <<$>> list conn)
  selectMailboxM (ImapConnection conn) mailbox =
    handleAny (throwIO . MailSelectMailboxError mailbox)
      $ liftIO (select conn (toString mailbox))
  getMailNumM (ImapConnection conn) = liftIO $ MailNumber <$> exists conn
  idleM (ImapConnection conn) timeout =
    handleAny (throwIO . MailIdleError timeout)
      $ liftIO
      $ idle conn (fromInteger $ unTimeout timeout)
  sleepM timeout =
    handleAny (throwIO . MailSleepError timeout)
      $ liftIO
      $ threadDelay (fromInteger $ unTimeout timeout)

instance MonadSync (App env) where
  addSyncJobM (SyncJobQueue queue) = atomically $ writeTBQueue queue ()
  waitForSyncJobsM (SyncJobQueue queue) timeout = do
    atomically $ readTBQueue queue
    atomicallyTimeoutUntilFail_ timeout $ readTBQueue queue
  syncM program args = do
    (exitCode, out, err) <- readProcessWithExitCode program (toString <$> args) ""
    let out' = ProcessStdoutOutput $ toText out
        err' = ProcessStderrOutput $ toText err
    when (exitCode /= ExitSuccess)
      $ throwIO
      $ SyncExternalProcessError program args exitCode out' err'
    pure (out', err')

  -- DBus tutrial: https://dbus.freedesktop.org/doc/dbus-tutorial.html
  signalSyncDoneM client busName objectPath interfaceName signalName =
    handle (throwIO . SyncDBusError busName objectPath interfaceName signalName . DBusClientError)
      $ liftIO
      $ void
      $ call_
        (unDBusClient client)
        ( methodCall
            (unDBusObjectPath objectPath)
            (unDBusInterfaceName interfaceName)
            (unDBusMemberName signalName)
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
  lookupEnvM = (fmap . fmap) (EnvVar . toText) . lookupEnv . toString

instance MonadAsync (App env) where
  concurrentlyManyM = mapConcurrently id

instance MonadDBus (App env) where
  requestNameM client busName = do
    reply <-
      handle (throwIO . DBusRequestNameCallError busName . DBusClientError)
        $ liftIO
        $ requestName (unDBusClient client) (unDBusBusName busName) [nameDoNotQueue]
    when (reply /= NamePrimaryOwner)
      $ throwIO
      $ DBusRequestNameFailed busName
      $ DBusRequestNameReply reply
  exportM client objectPath interfaceName methodName action =
    liftIO
      $ export
        (unDBusClient client)
        (unDBusObjectPath objectPath)
        defaultInterface
          { interfaceName = unDBusInterfaceName interfaceName,
            interfaceMethods = [autoMethod (unDBusMemberName methodName) action]
          }
  notifySystemdReadyM = liftIO $ void notifyReady
  waitSyncJobsM (SyncJobQueue queue) timeout = do
    atomically $ readTBQueue queue
    atomicallyTimeoutUntilFail_ timeout $ readTBQueue queue
  emitM client objectPath interfaceName signalName = do
    let signal =
          Signal
            { signalPath = unDBusObjectPath objectPath,
              signalInterface = unDBusInterfaceName interfaceName,
              signalMember = unDBusMemberName signalName,
              signalSender = Nothing,
              signalDestination = Nothing,
              signalBody = []
            }
    handle (throwIO . DBusEmitError objectPath interfaceName signalName . DBusClientError)
      $ liftIO
      $ emit (unDBusClient client) signal
