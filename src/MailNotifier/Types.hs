module MailNotifier.Types where

import Colog (HasLog (..), LogAction, Message, Severity)
import Network.HaskellNet.IMAP.Types (MailboxName)
import Relude
import UnliftIO (TBQueue)

newtype SyncJobQueue = SyncJobQueue {unSyncJobQueue :: TBQueue ()}

newtype WatchdogState = WatchdogState {unWatchdogState :: HashMap MailboxName (TMVar ())}

newtype Username = Username {unUsername :: Text}
  deriving stock (Show)
  deriving newtype (IsString)

newtype Password = Password {unPassword :: Text}

newtype AccountName = AccountName {unAccountName :: Text}
  deriving stock (Show)
  deriving newtype (IsString)

newtype Server = Server {unServer :: Text}
  deriving stock (Show)
  deriving newtype (IsString)

data Config = Config
  { accountName :: !AccountName,
    server :: !Server,
    username :: !Username,
    passwordFile :: !FilePath,
    mbsyncConfigFile :: !FilePath,
    mailboxes :: !(NonEmpty MailboxName),
    idleTimeout :: !Int,
    readSyncJobsTimeout :: !Int,
    pollInterval :: !Int,
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
