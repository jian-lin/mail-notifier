module MailNotifier.Utils
  ( atomicallyTimeoutUntilFail_,
    busName,
    interfaceName,
    mkLogAction,
    objectPath,
    syncNotificationMethodName,
    muaSyncSignalName,
    withDBus,
    withImap,
  )
where

import Colog (LogAction, Message, Msg (msgSeverity), Severity, filterBySeverity, richMessageAction)
import DBus.Client (connectSystem, disconnect)
import MailNotifier.Types
import Network.HaskellNet.IMAP.SSL
  ( Settings (..),
    connectIMAPSSLWithSettings,
    defaultSettingsIMAPSSL,
    logout,
  )
import Relude
import UnliftIO (MonadUnliftIO, bracket, checkSTM, orElse, registerDelay, withRunInIO)

atomicallyTimeout :: (MonadIO m) => Timeout -> STM a -> m (Maybe a)
atomicallyTimeout microsecond action = do
  timer <- registerDelay $ fromInteger $ unTimeout microsecond
  atomically $ (Just <$> action) `orElse` (Nothing <$ (checkSTM <=< readTVar) timer)

atomicallyTimeoutUntilFail_ :: (MonadIO m) => Timeout -> STM a -> m ()
atomicallyTimeoutUntilFail_ microsecond action = do
  result <- atomicallyTimeout microsecond action
  case result of
    Just _ -> atomicallyTimeoutUntilFail_ microsecond action
    Nothing -> pure ()

withImap :: (MonadUnliftIO m) => Server -> ImapConfig -> (ImapConnection -> m a) -> m a
withImap server config action = withRunInIO $ \runInIO ->
  bracket
    (connectIMAPSSLWithSettings (toString server) (fromImapConfig config))
    logout
    (runInIO . action . ImapConnection)

withDBus :: (MonadUnliftIO m) => (DBusClient -> m a) -> m a
withDBus action = withRunInIO $ \runInIO ->
  bracket connectSystem disconnect (runInIO . action . DBusClient)

busName :: DBusBusName
busName = DBusBusName "tech.linj.MailNotifier"

objectPath :: DBusObjectPath
objectPath = DBusObjectPath "/tech/linj/MailNotifier"

interfaceName :: DBusInterfaceName
interfaceName = DBusInterfaceName "tech.linj.MailNotifier"

mkLogAction :: (MonadIO m) => Severity -> LogAction m Message
mkLogAction severity = filterBySeverity severity msgSeverity richMessageAction

syncNotificationMethodName :: DBusMemberName
syncNotificationMethodName = DBusMemberName "Notify"

muaSyncSignalName :: DBusMemberName
muaSyncSignalName = DBusMemberName "Synced"

fromImapConfig :: ImapConfig -> Settings
fromImapConfig config =
  defaultSettingsIMAPSSL
    { sslMaxLineLength = fromMaybe 10_000 $ toIntegralSized $ sslMaxLineLength' config,
      sslLogToConsole = sslLogToConsole' config
    }
