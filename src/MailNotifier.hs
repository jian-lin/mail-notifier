module MailNotifier
  ( main,
  )
where

import Colog
  ( HasLog (..),
    LogAction,
    Message,
    Severity (..),
    WithLog,
    logDebug,
    logInfo,
    logWarning,
  )
import DBus (MethodCall (methodCallDestination), methodCall)
import DBus.Client (Client, call_)
import Data.Char (toUpper)
import Data.HashMap.Strict qualified as HM (elems, fromList, (!))
import Data.List.NonEmpty ((<|))
import MailNotifier.Utils
  ( AccountName,
    Server,
    atomicallyTimeoutUntilFail_,
    busName,
    interface,
    mkLogAction,
    objectPath,
    syncNotificationMethodName,
    withDBus,
    withImap,
  )
import Network.HaskellNet.IMAP.Connection (IMAPConnection, exists)
import Network.HaskellNet.IMAP.SSL
  ( Settings (..),
    capability,
    defaultSettingsIMAPSSL,
    idle,
    list,
    login,
    select,
  )
import Network.HaskellNet.IMAP.Types (MailboxName)
import Options.Applicative
  ( Parser,
    argument,
    auto,
    execParser,
    fullDesc,
    help,
    helper,
    info,
    long,
    metavar,
    option,
    progDesc,
    showDefault,
    simpleVersioner,
    str,
    strOption,
    value,
  )
import Options.Applicative.NonEmpty (some1)
import Relude hiding (getArgs) -- FIXME
import System.Systemd.Daemon (notifyWatchdog)
import UnliftIO (MonadUnliftIO, TBQueue, mapConcurrently, newTBQueue, readTBQueue, writeTBQueue)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Process (readProcess)

-- TODO patch HaskellNetSSL to replace connection with crypton-connection
-- https://github.com/dpwright/HaskellNet-SSL/pull/34/files
-- https://github.com/dpwright/HaskellNet-SSL/pull/33/files

-- TODO handle exceptions (or not? do they worth being handling?)
-- TODO add some doc string

type SyncJobQueue = TBQueue ()

type WatchdogState = HashMap MailboxName (TMVar ())

type Username = String

type Password = String

-- TODO exit with different exit code to indicate if restarting this program by system is desirable
-- e.g., if password is wrong, do not restart
--       if network connection is unavailable, restart
-- is it worth it?  Doesn't almost all "no need to restart" cases lead to a quick crash?
-- So if it runs fine for a while, it should be restarted if crashes.
-- Two exceptions I can think of are (1) the password is changed and (2) the mailbox is deleted.
watch ::
  (WithLog env Message m, MonadIO m, HasArgs env, HasSyncJobQueue env, HasWatchdogState env) =>
  Password ->
  MailboxName ->
  IMAPConnection ->
  m Void
watch password accountMailbox conn = do
  logInfo $ "watch " <> toText accountMailbox
  args <- asks getArgs
  liftIO $ login conn (username args) password
  logDebug "logged in "
  capabilities <- liftIO $ capability conn
  logDebug $ "capabilities: " <> show capabilities
  allMailboxes <- liftIO $ list conn
  logDebug $ "mailboxes: " <> show allMailboxes
  liftIO $ select conn accountMailbox
  logDebug $ "selected " <> toText accountMailbox
  let getMailNum :: IO Integer
      getMailNum = exists conn
  mailNum <- liftIO getMailNum
  logInfo $ show mailNum <> " mails in " <> toText accountMailbox
  syncJobQueue <- asks getSyncJobQueue
  liftIO $ atomically $ writeTBQueue syncJobQueue () -- initial sync job
  let supportIdle :: Bool
      supportIdle = "IDLE" `elem` (toUpper <<$>> capabilities) -- assume case-insensitive
      watchAwhile :: IO ()
      watchAwhile =
        if supportIdle
          then idle conn (idleTimeout args)
          else threadDelay (pollInterval args)
  logInfo $ "enter watchLoop, " <> if supportIdle then "use IDLE" else "fallback to poll"
  watchLoop mailNum (liftIO watchAwhile) (liftIO getMailNum) accountMailbox

watchLoop ::
  (WithLog env Message m, MonadIO m, HasSyncJobQueue env, HasWatchdogState env) =>
  Integer ->
  m () ->
  m Integer ->
  MailboxName ->
  m Void
watchLoop mailNum watchAwhile getMailNum accountMailbox = do
  watchdogState <- asks ((HM.! accountMailbox) . getWatchdogState)
  _ <- liftIO $ atomically $ tryPutTMVar watchdogState ()
  watchAwhile
  newMailNum <- getMailNum
  syncJobQueue <- asks getSyncJobQueue
  if newMailNum == mailNum
    then do
      logDebug
        $ toText accountMailbox
        <> " has no new mail, still "
        <> show mailNum
      watchLoop mailNum watchAwhile getMailNum accountMailbox
    else do
      logInfo
        $ toText accountMailbox
        <> " has "
        <> show (newMailNum - mailNum)
        <> " new mail(s), total "
        <> show mailNum
      liftIO $ atomically $ writeTBQueue syncJobQueue ()
      watchLoop newMailNum watchAwhile getMailNum accountMailbox

sync ::
  (WithLog env Message m, MonadIO m, HasArgs env, HasSyncJobQueue env) =>
  Client ->
  m Void
sync client = infinitely $ do
  args <- asks getArgs
  logInfo "wait for sync jobs"
  syncJobQueue <- asks getSyncJobQueue
  _ <- liftIO $ atomically $ readTBQueue syncJobQueue
  liftIO $ atomicallyTimeoutUntilFail_ (readSyncJobsTimeout args) $ readTBQueue syncJobQueue
  logInfo "got sync jobs, start to sync"
  output <-
    liftIO
      $ readProcess
        "/run/wrappers/bin/mbsyncSetuid"
        [ "--config",
          mbsyncConfigFile args,
          accountName args
        ]
        ""
  unless (null output)
    $ logWarning ("sync output: " <> toText output) -- has warnings
  notify client
  logInfo "sent a synced notification"

-- DBus tutrial: https://dbus.freedesktop.org/doc/dbus-tutorial.html
notify ::
  (WithLog env Message m, MonadIO m, HasArgs env) =>
  Client ->
  m ()
notify client = do
  _ <-
    liftIO
      $ call_
        client
        ( methodCall
            objectPath
            interface
            syncNotificationMethodName
        )
          { methodCallDestination = Just busName
          }

  logDebug
    $ "DBus: called method "
    <> show syncNotificationMethodName
    <> " at "
    <> show busName
    <> " "
    <> show objectPath
    <> " "
    <> show interface

watchdog ::
  (WithLog env Message m, MonadIO m, HasWatchdogState env, HasArgs env) =>
  m Void
watchdog = infinitely $ do
  args <- asks getArgs
  let mailboxNum = length $ mailboxes args
  watchdogState <- asks getWatchdogState
  liftIO $ atomically $ mapM_ takeTMVar $ HM.elems watchdogState
  reply <- liftIO notifyWatchdog
  logDebug
    $ "all "
    <> show mailboxNum
    <> " mailboxes are under watch, "
    <> if isNothing reply
      then "but watchdog is not enabled by systemd"
      else "sent watchdog to systemd"

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
    MonadIO m,
    MonadUnliftIO m
  ) =>
  m (NonEmpty Void)
app = do
  args <- asks getArgs
  logDebug $ show args
  -- TODO is it better to: bracket openFile hClose $ \h -> ...
  -- what happens if readFile fails in the process of reading an opened file? will file be closed?
  password <- liftIO $ readFile $ passwordFile args
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
        withImap (server args) imapSettings (watch password mailbox)
  mapConcurrently id $ withDBus sync <| watchdog <| fmap watchOneMailbox (mailboxes args)

data Args = Args
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
  deriving (Show)

data Env m = Env
  { envLogAction :: !(LogAction m Message),
    envSyncJobQueue :: !SyncJobQueue,
    envWatchdogState :: !WatchdogState,
    envArgs :: !Args
  }

newtype App a = App {runApp :: ReaderT (Env App) IO a}
  deriving (Functor, Applicative, Monad, MonadReader (Env App), MonadIO, MonadUnliftIO)

instance HasLog (Env m) Message m where
  getLogAction :: Env m -> LogAction m Message
  getLogAction = envLogAction
  {-# INLINE getLogAction #-}

  setLogAction :: LogAction m Message -> Env m -> Env m
  setLogAction newLogAction env = env {envLogAction = newLogAction}
  {-# INLINE setLogAction #-}

-- TODO are there better ways to do HasArgs, HasSyncJobQueue and HasWatchdogQueue?
class HasArgs env where
  getArgs :: env -> Args

instance HasArgs (Env m) where
  getArgs :: Env m -> Args
  getArgs = envArgs
  {-# INLINE getArgs #-}

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

run :: Env App -> App a -> IO a
run env application = runReaderT (runApp application) env

-- TODO split this big parser into smaller ones and then compose those small ones
-- TODO parse this more strict: positive integer within maxBound :: Int
-- TODO maybe parse name more strict: non-empty
argsParser :: Parser Args
argsParser =
  Args
    <$> strOption
      ( long "account-name"
          <> metavar "ACCOUNT-NAME"
          <> help "Mail account name"
      )
    <*> strOption
      ( long "server"
          <> metavar "SERVER"
          <> help "IMAP server address without port"
      )
    <*> strOption
      ( long "username"
          <> metavar "USERNAME"
          <> help "Username"
      )
    <*> strOption
      ( long "password-file"
          <> metavar "PASSWORD-FILE"
          <> help "Password file"
      )
    <*> strOption
      ( long "mbsync-config-file"
          <> metavar "MBSYNC-CONFIG-FILE"
          <> help "mbsync config file"
      )
    <*> some1 -- TODO make its element unique
      ( argument
          str
          ( metavar "MAILBOXES"
              <> help "Mailboxes to watch"
          )
      )
    <*> option -- TODO warn if too long since conn may be cut by middle boxes
      auto
      ( long "idle-timeout"
          <> metavar "MILLISECOND"
          <> showDefault
          <> value (2 * 60 * 1_000)
          <> help "Timeout for IMAP IDLE command"
      )
    <*> option
      auto
      ( long "read-sync-jobs-timeout"
          <> metavar "MICROSECOND"
          <> showDefault
          <> value 5_000_000
          <> help "Timeout for reading following sync jobs before performing one sync"
      )
    <*> option -- TODO warn if too long since conn may be cut by middle boxes
      auto
      ( long "poll-interval"
          <> metavar "MICROSECOND"
          <> showDefault
          <> value (2 * 60 * 1_000_000)
          <> help "Interval for polling new mails (fallback if IDLE is not supported)"
      )
    <*> option -- TODO find a way to show valid values
      auto
      ( long "log-level"
          <> metavar "LOG-LEVEL"
          <> showDefault
          <> value Info
          <> help "Log level"
      )

-- TODO use synopsis and version from PackageInfo_mail_notifier when cabal2nix supports cabal-version 3.12
parseArgs :: IO Args
parseArgs =
  execParser
    $ info
      (argsParser <**> helper <**> simpleVersioner "0.1.0.0")
      (fullDesc <> progDesc "A tool to immediately run mbsync and notify your MUA for new mails")

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering -- print log while running under systemd
  args <- parseArgs
  -- no exception raised from Integer -> Natural since mailboxes is NonEmpty
  let mailboxNum = length $ mailboxes args
      queueSize = 3 * fromInteger (toInteger mailboxNum)
  syncJobQueue <- atomically $ newTBQueue queueSize
  vs <- atomically $ replicateM mailboxNum newEmptyTMVar
  let env :: Env App
      env =
        Env
          { envLogAction = mkLogAction $ logLevel args,
            envSyncJobQueue = syncJobQueue,
            envWatchdogState = HM.fromList $ zip (toList $ mailboxes args) vs,
            envArgs = args
          }
  foldMap absurd <$> run env app
