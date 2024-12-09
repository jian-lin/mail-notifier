{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module MailNotifier.Utils
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
where

import Chronos (Time)
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
import DBus (BusName, MemberName, ObjectPath)
import DBus.Client (Client, connectSystem, disconnect)
import DBus.Internal.Types (InterfaceName)
import Network.HaskellNet.IMAP.Connection (IMAPConnection)
import Network.HaskellNet.IMAP.SSL (Settings, connectIMAPSSLWithSettings, logout)
import Relude
import UnliftIO
  ( MonadUnliftIO,
    bracket,
    checkSTM,
    orElse,
    registerDelay,
    withRunInIO,
  )
import UnliftIO.Concurrent (ThreadId)

atomicallyTimeout :: Int -> STM a -> IO (Maybe a)
atomicallyTimeout microsecond action = do
  timer <- registerDelay microsecond
  atomically $ (Just <$> action) `orElse` (Nothing <$ (checkSTM <=< readTVar) timer)

atomicallyTimeoutUntilFail_ :: Int -> STM a -> IO ()
atomicallyTimeoutUntilFail_ microsecond action = do
  result <- atomicallyTimeout microsecond action
  case result of
    Just _ -> atomicallyTimeoutUntilFail_ microsecond action
    Nothing -> pure ()

type Server = String

withImap :: (MonadUnliftIO m) => Server -> Settings -> (IMAPConnection -> m a) -> m a
withImap server settings action = withRunInIO $ \runInIO ->
  bracket
    (connectIMAPSSLWithSettings server settings)
    logout
    (runInIO . action)

withDBus :: (MonadUnliftIO m) => (Client -> m a) -> m a
withDBus action = withRunInIO $ \runInIO ->
  bracket connectSystem disconnect (runInIO . action)

busName :: BusName
busName = "tech.linj.MailNotifier"

objectPath :: ObjectPath
objectPath = "/tech/linj/MailNotifier"

interface :: InterfaceName
interface = "tech.linj.MailNotifier"

type AccountName = String

mkLogAction :: (MonadIO m) => Severity -> LogAction m Message
mkLogAction severity =
  let -- modified from fmtRichMessageDefault
      fmtRichMessage :: (MonadIO m) => RichMessage m -> m Text
      fmtRichMessage richMsg =
        let formatRichMessage :: Maybe ThreadId -> Maybe Time -> Message -> Text
            formatRichMessage (maybe "" showThreadId -> thread) (maybe "" showTime -> time) msg =
              showSeverity msg.msgSeverity
                <> (if severity <= Debug then time else mempty)
                <> (if severity <= Debug then showSourceLoc msg.msgStack else mempty)
                <> thread
                <> msg.msgText
         in fmtRichMessageCustomDefault richMsg formatRichMessage
      -- modified from richMessageAction
      logAction :: (MonadIO m) => LogAction m Message
      logAction =
        upgradeMessageAction defaultFieldMap
          $ cmapM (fmap encodeUtf8 . fmtRichMessage) logByteStringStdout
   in filterBySeverity severity msgSeverity logAction

syncNotificationMethodName :: MemberName
syncNotificationMethodName = "Notify"
