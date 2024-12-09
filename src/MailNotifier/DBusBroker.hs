module MailNotifier.DBusBroker
  ( main,
  )
where

import Colog
  ( HasLog (..),
    LogAction,
    Message,
    Severity (..),
    WithLog,
    logInfo,
  )
import DBus (MemberName)
import DBus.Client
  ( Client,
    RequestNameReply (NamePrimaryOwner),
    autoMethod,
    clientError,
    defaultInterface,
    emit,
    export,
    interfaceMethods,
    interfaceName,
    nameDoNotQueue,
    requestName,
  )
import DBus.Internal.Message (Signal (..))
import MailNotifier.Utils
  ( atomicallyTimeoutUntilFail_,
    busName,
    interface,
    mkLogAction,
    objectPath,
    withDBus,
  )
import Relude
import UnliftIO (TBQueue, newTBQueue, readTBQueue, throwIO, writeTBQueue)

type Queue = TBQueue ()

emitSignal :: (WithLog env Message m, MonadIO m, HasQueue env) => Client -> m ()
emitSignal client = forever $ do
  queue <- asks getQueue
  _ <- liftIO $ atomically $ readTBQueue queue
  liftIO $ atomicallyTimeoutUntilFail_ 1_000_000 $ readTBQueue queue
  let signalName :: MemberName
      signalName = "Synced"
      signal :: Signal
      signal =
        Signal
          { signalPath = objectPath,
            signalInterface = interface,
            signalMember = signalName,
            signalSender = Nothing,
            signalDestination = Nothing,
            signalBody = []
          }
  liftIO $ emit client signal
  logInfo $ "emitted a " <> show signalName <> " signal: " <> show signal

-- TODO try to add some log
getSyncNotification :: Queue -> IO ()
getSyncNotification queue = atomically $ writeTBQueue queue ()

app :: (WithLog env Message m, MonadIO m, HasQueue env) => Client -> m ()
app client = do
  logInfo $ "try to request " <> show busName
  reply <- liftIO $ requestName client busName [nameDoNotQueue]
  when (reply /= NamePrimaryOwner) $ do
    throwIO $ clientError $ "failed to request " <> show busName <> ": " <> show reply
  logInfo $ "requested " <> show busName
  queue <- asks getQueue
  let methodName :: MemberName
      methodName = "Notify"
  liftIO
    $ export
      client
      objectPath
      defaultInterface
        { interfaceName = interface,
          interfaceMethods = [autoMethod methodName (getSyncNotification queue)]
        }
  logInfo
    $ "exported method "
    <> show methodName
    <> " at "
    <> show objectPath
    <> " "
    <> show interface
  logInfo "wait for sync notifications"
  emitSignal client

data Env m = Env
  { envLogAction :: !(LogAction m Message),
    envQueue :: !Queue
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

class HasQueue env where
  getQueue :: env -> Queue

instance HasQueue (Env m) where
  getQueue :: Env m -> Queue
  getQueue = envQueue
  {-# INLINE getQueue #-}

run :: Env App -> App a -> IO a
run env application = runReaderT (runApp application) env

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering -- print log while running under systemd
  queue <- atomically $ newTBQueue 10
  let env :: Env App
      env =
        Env
          { envLogAction = mkLogAction Info,
            envQueue = queue
          }
  withDBus $ \client -> run env (app client)
