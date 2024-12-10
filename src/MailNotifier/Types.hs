module MailNotifier.Types where

import Colog (HasLog (..), LogAction, Message, Severity)
import DBus.Client (Client)
import Data.Text (toLower)
import Network.HaskellNet.IMAP.Connection (IMAPConnection)
import Relude
import UnliftIO (TBQueue)

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
    idleTimeout :: !Integer,
    readSyncJobsTimeout :: !Integer,
    pollInterval :: !Integer,
    logLevel :: !Severity
  }
  deriving stock (Show)

data Env m = Env
  { envLogAction :: !(LogAction m Message),
    envSyncJobQueue :: !SyncJobQueue,
    envWatchdogState :: !WatchdogState,
    envConfig :: !Config
  }

instance HasLog (Env m) Message m where
  getLogAction :: Env m -> LogAction m Message
  getLogAction = envLogAction
  {-# INLINE getLogAction #-}

  setLogAction :: LogAction m Message -> Env m -> Env m
  setLogAction newLogAction env = env {envLogAction = newLogAction}
  {-# INLINE setLogAction #-}

-- TODO are there better ways to do HasConfig, HasSyncJobQueue and HasWatchdogQueue?
class HasConfig env where
  getConfig :: env -> Config

instance HasConfig (Env m) where
  getConfig :: Env m -> Config
  getConfig = envConfig
  {-# INLINE getConfig #-}

class HasSyncJobQueue env where
  getSyncJobQueue :: env -> SyncJobQueue

instance HasSyncJobQueue (Env m) where
  getSyncJobQueue :: Env m -> SyncJobQueue
  getSyncJobQueue = envSyncJobQueue
  {-# INLINE getSyncJobQueue #-}

class HasWatchdogState env where
  getWatchdogState :: env -> WatchdogState

instance HasWatchdogState (Env m) where
  getWatchdogState :: Env m -> WatchdogState
  getWatchdogState = envWatchdogState
  {-# INLINE getWatchdogState #-}

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

newtype Timeout = Timeout Integer -- TODO make sure it is positive

data IdleMode = Idle | Sleep deriving stock (Eq)

class (Monad m) => MonadMailRead m where
  loginM :: ImapConnection -> Username -> Password -> m ()
  getCapabilitiesM :: ImapConnection -> m [Capability]
  listMailboxesM :: ImapConnection -> m [Mailbox]
  selectMailboxM :: ImapConnection -> Mailbox -> m ()
  getMailNumM :: ImapConnection -> m Integer
  idleOrSleepM :: ImapConnection -> Timeout -> IdleMode -> m ()

newtype DBusClient = DBusClient Client

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
