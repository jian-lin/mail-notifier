{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.MailNotifier
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
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
  ( TBQueue,
    TMVar,
    atomically,
    newEmptyTMVar,
    newTBQueue,
    readTBQueue,
    takeTMVar,
    tryPutTMVar,
    writeTBQueue,
  )
import Control.Monad (replicateM, when)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader (ask), ReaderT (..), asks)
import DBus (ObjectPath)
import DBus.Client (Client, emit, nameDoNotQueue, requestName)
import DBus.Internal.Message (Signal (..))
import Data.Char (toUpper)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM (elems, fromList, (!))
import Data.List.NonEmpty (NonEmpty, (<|))
import qualified Data.List.NonEmpty as NL (map, toList)
import Data.Maybe (isNothing)
import Data.Text (pack)
import Data.Version (showVersion)
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
import Network.MailNotifier.Utils
  ( AccountName,
    Server,
    atomicallyTimeoutUntilFail_,
    interface,
    mkBusName,
    mkLogAction,
    mkObjectPath,
    raceMany,
    withDBus,
    withImap,
  )
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
    (<**>),
  )
import Options.Applicative.NonEmpty (some1)
import PackageInfo_mail_notifier (synopsis, version)
import System.IO (BufferMode (..), hSetBuffering, stdout)
import System.Process (readProcess)
import System.Systemd.Daemon (notifyWatchdog)

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
  IMAPConnection ->
  Password ->
  MailboxName ->
  m ()
watch conn password accountMailbox = do
  logInfo $ "watch " <> pack accountMailbox
  args <- asks getArgs
  liftIO $ login conn args.username password
  logDebug "logged in "
  capabilities <- liftIO $ capability conn
  logDebug $ "capabilities: " <> pack (show capabilities)
  allMailboxes <- liftIO $ list conn
  logDebug $ "mailboxes: " <> pack (show allMailboxes)
  liftIO $ select conn accountMailbox
  logDebug $ "selected " <> pack accountMailbox
  let getMailNum :: IO Integer
      getMailNum = exists conn
  mailNum <- liftIO getMailNum
  logInfo $ pack (show mailNum) <> " mails in " <> pack accountMailbox
  syncJobQueue <- asks getSyncJobQueue
  liftIO $ atomically $ writeTBQueue syncJobQueue () -- initial sync job
  let supportIdle :: Bool
      supportIdle = "IDLE" `elem` map (map toUpper) capabilities -- assume case-insensitive
      watchAwhile :: IO ()
      watchAwhile =
        if supportIdle
          then idle conn args.idleTimeout
          else threadDelay args.pollInterval
  logInfo $ "enter watchLoop, " <> if supportIdle then "use IDLE" else "fallback to poll"
  watchLoop mailNum watchAwhile getMailNum accountMailbox

watchLoop ::
  (WithLog env Message m, MonadIO m, HasSyncJobQueue env, HasWatchdogState env) =>
  Integer ->
  IO () ->
  IO Integer ->
  MailboxName ->
  m ()
watchLoop mailNum watchAwhile getMailNum accountMailbox = do
  watchdogState <- asks ((HM.! accountMailbox) . getWatchdogState)
  _ <- liftIO $ atomically $ tryPutTMVar watchdogState ()
  liftIO watchAwhile
  newMailNum <- liftIO getMailNum
  syncJobQueue <- asks getSyncJobQueue
  if newMailNum == mailNum
    then do
      logDebug $
        pack accountMailbox <> " has no new mail, still " <> pack (show mailNum)
      watchLoop mailNum watchAwhile getMailNum accountMailbox
    else do
      logInfo $
        pack accountMailbox
          <> " has "
          <> pack (show (newMailNum - mailNum))
          <> " new mail(s), total "
          <> pack (show mailNum)
      liftIO $ atomically $ writeTBQueue syncJobQueue ()
      watchLoop newMailNum watchAwhile getMailNum accountMailbox

sync ::
  (WithLog env Message m, MonadIO m, HasArgs env, HasSyncJobQueue env) =>
  Client ->
  ObjectPath ->
  m ()
sync client objectPath = do
  args <- asks getArgs
  logInfo "wait for sync jobs"
  syncJobQueue <- asks getSyncJobQueue
  _ <- liftIO $ atomically $ readTBQueue syncJobQueue
  liftIO $ atomicallyTimeoutUntilFail_ args.readSyncJobsTimeout $ readTBQueue syncJobQueue
  logInfo "got sync jobs, start to sync"
  output <-
    liftIO $
      readProcess
        "/run/wrappers/bin/mbsyncSetuid"
        [ "--config",
          args.mbsyncConfigFile,
          args.accountName
        ]
        ""
  when (output /= "") $ -- has warnings
    logWarning ("sync output: " <> pack output)
  notify client objectPath
  logInfo "emitted a synced signal"
  sync client objectPath

-- DBus tutrial: https://dbus.freedesktop.org/doc/dbus-tutorial.html
notify ::
  (WithLog env Message m, MonadIO m, HasArgs env) =>
  Client ->
  ObjectPath ->
  m ()
notify client objectPath = do
  let signal =
        Signal
          { signalPath = objectPath,
            signalInterface = interface,
            signalMember = "Synced",
            signalSender = Nothing,
            signalDestination = Nothing,
            signalBody = []
          }
  liftIO $ emit client signal
  logDebug $ "DBus: " <> pack (show signal)

watchdog ::
  (WithLog env Message m, MonadIO m, HasWatchdogState env, HasArgs env) =>
  m ()
watchdog = do
  args <- asks getArgs
  let mailboxNum = length args.mailboxes
  watchdogState <- asks getWatchdogState
  liftIO $ atomically $ mapM_ takeTMVar $ HM.elems watchdogState
  reply <- liftIO notifyWatchdog
  logDebug $
    "all "
      <> pack (show mailboxNum)
      <> " mailboxes are under watch, "
      <> if isNothing reply
        then "but watchdog is not enabled by systemd"
        else "sent watchdog to systemd"
  watchdog

-- TODO split env in ReaderT of sync and watch according to the "Next Level MTL" video
-- TODO can we put withDBus into sync function? (also withImap)
app :: App ()
app = do
  args <- asks envArgs
  logDebug $ pack (show args)
  -- TODO is it better to: bracket openFile hClose $ \h -> ...
  -- what happens if readFile fails in the process of reading an opened file? will file be closed?
  password <- liftIO $ readFile args.passwordFile
  -- running an unbounded numbder of threads concurrently is a bad pattern
  -- since too many threads can cause resouces issues
  --   a server may limit number of connections from a single client
  --   a client may use up its memory (less likely in this case)
  --   a client may increase the overhead of its sheduler
  let mailboxNum = length args.mailboxes
  when (mailboxNum > 10) $ logWarning $ "too many mailboxes: " <> pack (show mailboxNum)
  let busName = mkBusName args.accountName
      objectPath = mkObjectPath args.accountName
  logInfo $ "DBus: " <> pack (show busName) <> " " <> pack (show objectPath)
  env <- ask
  let syncThread :: IO ()
      syncThread = withDBus $ \client -> do
        _reply <- requestName client busName [nameDoNotQueue]
        -- TODO logDebug reply
        run env (sync client objectPath)
      -- NOTE setting sslLogToConsole to True will print your password in clear text!
      imapSettings :: Settings
      imapSettings = defaultSettingsIMAPSSL {sslMaxLineLength = 100000, sslLogToConsole = False}
      watchThreads :: NonEmpty (IO ())
      watchThreads =
        NL.map
          ( \mailbox ->
              withImap
                args.server
                imapSettings
                (\conn -> run env (watch conn password mailbox))
          )
          args.mailboxes
      watchdogThread :: IO ()
      watchdogThread = run env watchdog
  liftIO $ raceMany $ syncThread <| watchdogThread <| watchThreads

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
  deriving (Functor, Applicative, Monad, MonadReader (Env App), MonadIO)

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
          <> value (2 * 60 * 1000)
          <> help "Timeout for IMAP IDLE command"
      )
    <*> option
      auto
      ( long "read-sync-jobs-timeout"
          <> metavar "MICROSECOND"
          <> showDefault
          <> value (5 * 1000 * 1000)
          <> help "Timeout for reading following sync jobs before performing one sync"
      )
    <*> option -- TODO warn if too long since conn may be cut by middle boxes
      auto
      ( long "poll-interval"
          <> metavar "MICROSECOND"
          <> showDefault
          <> value (2 * 60 * 1000 * 1000)
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

parseArgs :: IO Args
parseArgs =
  execParser $
    info
      (argsParser <**> helper <**> simpleVersioner (showVersion version))
      (fullDesc <> progDesc synopsis)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering -- print log while running under systemd
  args <- parseArgs
  -- no exception raised from Integer -> Natural since mailboxes is NonEmpty
  let mailboxNum = length args.mailboxes
      queueSize = 3 * fromInteger (toInteger mailboxNum)
  syncJobQueue <- atomically $ newTBQueue queueSize
  vs <- atomically $ replicateM mailboxNum newEmptyTMVar
  let env :: Env App
      env =
        Env
          { envLogAction = mkLogAction args.logLevel,
            envSyncJobQueue = syncJobQueue,
            envWatchdogState = HM.fromList $ zip (NL.toList args.mailboxes) vs,
            envArgs = args
          }
  run env app
