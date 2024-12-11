{-# LANGUAGE ViewPatterns #-}

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

import Colog
  ( LogAction,
    Message,
    Msg (..),
    RichMessage,
    Severity (Debug),
    cmapM,
    defaultFieldMap,
    filterBySeverity,
    fmtRichMessageCustomDefault,
    logByteStringStdout,
    showSeverity,
    showSourceLoc,
    showThreadId,
    showTime,
    upgradeMessageAction,
  )
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
mkLogAction severity =
  let -- modified from fmtRichMessageDefault
      fmtRichMessage :: (MonadIO m) => RichMessage m -> m Text
      fmtRichMessage richMsg =
        let formatRichMessage (maybe "" showThreadId -> thread) (maybe "" showTime -> time) msg =
              showSeverity (msgSeverity msg)
                <> (if severity <= Debug then time else mempty)
                <> (if severity <= Debug then showSourceLoc (msgStack msg) else mempty)
                <> thread
                <> msgText msg
         in fmtRichMessageCustomDefault richMsg formatRichMessage
      -- modified from richMessageAction
      logAction :: (MonadIO m) => LogAction m Message
      logAction =
        upgradeMessageAction defaultFieldMap
          $ cmapM (fmap encodeUtf8 . fmtRichMessage) logByteStringStdout
   in filterBySeverity severity msgSeverity logAction

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
