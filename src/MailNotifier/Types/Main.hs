module MailNotifier.Types.Main where

import Colog (Severity)
import DBus (BusName, InterfaceName, MemberName, ObjectPath)
import DBus.Client (Client, RequestNameReply)
import Data.Text (toLower)
import MailNotifier.Types.Timeout (Timeout)
import Network.HaskellNet.IMAP.Connection (IMAPConnection)
import Relude
import UnliftIO (MonadUnliftIO, TBQueue)

newtype SyncJobQueue = SyncJobQueue (TBQueue ())

newtype WatchdogState = WatchdogState (HashMap Mailbox (TMVar ()))

newtype Username = Username Text
  deriving stock (Show)
  deriving newtype (IsString)

newtype Password = Password Text

newtype AccountName = AccountName {unAccountName :: Text}
  deriving stock (Show)
  deriving newtype (IsString)

newtype Server = Server Text
  deriving stock (Show)
  deriving newtype (IsString, ToString)

data Config = Config
  { accountName :: !AccountName,
    server :: !Server,
    username :: !Username,
    passwordFile :: !FilePath,
    mbsyncConfigFile :: !FilePath,
    mailboxes :: !(NonEmpty Mailbox),
    idleTimeout :: !Timeout,
    readSyncJobsTimeout :: !Timeout,
    pollInterval :: !Timeout,
    logLevel :: !Severity
  }
  deriving stock (Show)

-- TODO optional: use lens to simplify instance definitions for HashFoo
class HasConfig env where
  getConfig :: env -> Config

class HasSyncJobQueue env where
  getSyncJobQueue :: env -> SyncJobQueue

class HasWatchdogState env where
  getWatchdogState :: env -> WatchdogState

newtype Capability = Capability Text
  deriving stock (Show)
  deriving newtype (IsString)

-- | Assume case-insensitive
instance Eq Capability where
  Capability x == Capability y = toLower x == toLower y

newtype ImapConnection = ImapConnection IMAPConnection

newtype Mailbox = Mailbox Text
  deriving stock (Show, Eq)
  deriving newtype (Hashable, IsString)

data IdleMode = Idle | Sleep deriving stock (Eq)

newtype MailNumber = MailNumber Integer
  deriving stock (Show, Eq)
  deriving newtype (Num)

class (Monad m) => MonadMailRead m where
  loginM :: ImapConnection -> Username -> Password -> m ()
  getCapabilitiesM :: ImapConnection -> m [Capability]
  listMailboxesM :: ImapConnection -> m [Mailbox]
  selectMailboxM :: ImapConnection -> Mailbox -> m ()
  getMailNumM :: ImapConnection -> m MailNumber
  idleOrSleepM :: ImapConnection -> Timeout -> IdleMode -> m ()

newtype DBusClient = DBusClient {unDBusClient :: Client}

class (Monad m) => MonadSync m where
  addSyncJobM :: SyncJobQueue -> m ()

  -- | Wait infinitely for one sync job.  After getting one, continue waiting for
  -- following jobs which appears within 'Timeout' seconds from the last one.
  waitForSyncJobsM :: SyncJobQueue -> Timeout -> m ()

  syncM :: FilePath -> [Text] -> m Text
  signalSyncDoneM :: DBusClient -> m ()

class (Monad m) => MonadWatchdog m where
  signalCheckedMailboxM :: Mailbox -> WatchdogState -> m ()
  notiftyWatchdogWhenAllMailboxesAreCheckedM :: WatchdogState -> m (Maybe ())

newtype EnvVar = EnvVar Text
  deriving newtype (ToString, ToText)

class (Monad m) => MonadIORead m where
  readFileM :: FilePath -> m Text
  lookupEnvM :: Text -> m (Maybe EnvVar)

class (Monad m, MonadUnliftIO m) => MonadAsync m where
  concurrentlyManyM :: (Traversable t) => t (m a) -> m (t a)

newtype DBusBusName = DBusBusName {unDBusBusName :: BusName}
  deriving stock (Show)

newtype DBusObjectPath = DBusObjectPath {unDBusObjectPath :: ObjectPath}
  deriving stock (Show)

newtype DBusInterfaceName = DBusInterfaceName {unDBusInterfaceName :: InterfaceName}
  deriving stock (Show)

newtype DBusMemberName = DBusMemberName {unDBusMemberName :: MemberName}
  deriving stock (Show)
  deriving newtype (IsString)

newtype DBusRequestNameReply = DBusRequestNameReply RequestNameReply
  deriving stock (Show)

class (Monad m) => MonadDBus m where
  requestNameM :: DBusClient -> DBusBusName -> m ()
  exportM :: DBusClient -> DBusObjectPath -> DBusInterfaceName -> DBusMemberName -> IO () -> m ()
  waitSyncJobsM :: SyncJobQueue -> Timeout -> m ()
  emitM :: DBusClient -> DBusObjectPath -> DBusInterfaceName -> DBusMemberName -> m ()
