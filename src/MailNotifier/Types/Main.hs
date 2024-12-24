module MailNotifier.Types.Main where

import Colog (Severity)
import Control.Exception.Safe (MonadCatch, MonadThrow)
import DBus (BusName, InterfaceName, MemberName, ObjectPath)
import DBus.Client (AutoMethod, Client, ClientError, RequestNameReply)
import Data.Text (toLower)
import MailNotifier.Types.Timeout (TimeoutMicroSecond, TimeoutMilliSecond)
import Network.HaskellNet.IMAP.Connection (IMAPConnection)
import Relude
import UnliftIO (MonadUnliftIO, TBQueue)

-- TODO use OsPath and OsString when optparse-applicative, process (and relude) support them

newtype SyncJobQueue = SyncJobQueue (TBQueue ())

newtype WatchdogState = WatchdogState (HashMap Mailbox (TMVar ()))

newtype Username = Username Text
  deriving stock (Show)
  deriving newtype (IsString, ToString)

newtype Password = Password Text
  deriving newtype (ToString)

newtype AccountName = AccountName Text
  deriving stock (Show)
  deriving newtype (IsString, ToText)

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
    idleTimeout :: !TimeoutMilliSecond,
    readSyncJobsTimeout :: !TimeoutMicroSecond,
    pollInterval :: !TimeoutMicroSecond,
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

data ImapConfig = ImapConfig
  { sslMaxLineLength' :: Word16,
    -- | Setting this to True may /print your password in clear text/.
    sslLogToConsole' :: Bool
  }

newtype Mailbox = Mailbox Text
  deriving stock (Show, Eq)
  deriving newtype (Hashable, IsString, ToString)

newtype MailNumber = MailNumber Integer
  deriving stock (Show, Eq)
  deriving newtype (Num)

class (MonadThrow m, MonadCatch m) => MonadMailRead m where
  loginM :: ImapConnection -> Username -> Password -> m ()
  getCapabilitiesM :: ImapConnection -> m [Capability]
  listMailboxesM :: ImapConnection -> m [Mailbox]
  selectMailboxM :: ImapConnection -> Mailbox -> m ()
  getMailNumM :: ImapConnection -> m MailNumber
  idleM :: ImapConnection -> TimeoutMilliSecond -> m ()
  sleepM :: TimeoutMicroSecond -> m ()

newtype DBusClient = DBusClient {unDBusClient :: Client}

class (MonadThrow m, MonadCatch m) => MonadSync m where
  addSyncJobM :: SyncJobQueue -> m ()

  -- | Wait infinitely for one sync job.  After getting one, continue waiting for
  -- following jobs which appears within 'TimeoutMicroSecond' from the last one.
  waitForSyncJobsM :: SyncJobQueue -> TimeoutMicroSecond -> m ()

  syncM :: FilePath -> [Text] -> m (ProcessStdoutOutput, ProcessStderrOutput)
  signalSyncDoneM ::
    DBusClient ->
    DBusBusName ->
    DBusObjectPath ->
    DBusInterfaceName ->
    DBusMemberName ->
    m ()

class (MonadThrow m) => MonadWatchdog m where
  signalCheckedMailboxM :: Mailbox -> WatchdogState -> m ()
  notiftyWatchdogWhenAllMailboxesAreCheckedM :: WatchdogState -> m (Maybe ())

newtype EnvVar = EnvVar Text
  deriving newtype (ToString)

class (MonadThrow m) => MonadIORead m where
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

newtype DBusRequestNameReply = DBusRequestNameReply RequestNameReply
  deriving stock (Show)

newtype DBusClientError = DBusClientError ClientError
  deriving stock (Show)

newtype DBusExportedAction = DBusExportedAction (IO ())
  deriving newtype (AutoMethod)

class (MonadThrow m, MonadCatch m) => MonadDBus m where
  requestNameM :: DBusClient -> DBusBusName -> m ()
  exportM ::
    DBusClient ->
    DBusObjectPath ->
    DBusInterfaceName ->
    DBusMemberName ->
    DBusExportedAction ->
    m ()
  notifySystemdReadyM :: m ()
  waitSyncJobsM :: SyncJobQueue -> TimeoutMicroSecond -> m ()
  emitM :: DBusClient -> DBusObjectPath -> DBusInterfaceName -> DBusMemberName -> m ()

newtype ProcessStdoutOutput = ProcessStdoutOutput Text
  deriving stock (Show)
  deriving newtype (ToText)

newtype ProcessStderrOutput = ProcessStderrOutput Text
  deriving stock (Show)
  deriving newtype (ToText)
