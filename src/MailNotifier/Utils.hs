{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module MailNotifier.Utils
  ( AccountName,
    Server,
    atomicallyTimeoutUntilFail_,
    busName,
    interface,
    mkLogAction,
    objectPath,
    raceMany,
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
import Control.Concurrent (ThreadId)
import Control.Concurrent.Async (Concurrently (..))
import Control.Concurrent.STM (STM, atomically, check, orElse, readTVar, registerDelay)
import Control.Exception (bracket)
import Control.Monad ((<=<))
import Control.Monad.IO.Class (MonadIO)
import DBus (BusName, MemberName, ObjectPath)
import DBus.Client (Client, connectSystem, disconnect)
import DBus.Internal.Types (InterfaceName)
import Data.Foldable (asum, toList)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Network.HaskellNet.IMAP.Connection (IMAPConnection)
import Network.HaskellNet.IMAP.SSL (Settings, connectIMAPSSLWithSettings, logout)

atomicallyTimeout :: Int -> STM a -> IO (Maybe a)
atomicallyTimeout microsecond action = do
  timer <- registerDelay microsecond
  atomically $ (Just <$> action) `orElse` (Nothing <$ (check <=< readTVar) timer)

atomicallyTimeoutUntilFail_ :: Int -> STM a -> IO ()
atomicallyTimeoutUntilFail_ microsecond action = do
  result <- atomicallyTimeout microsecond action
  case result of
    Just _ -> atomicallyTimeoutUntilFail_ microsecond action
    Nothing -> return ()

type Server = String

withImap :: Server -> Settings -> (IMAPConnection -> IO ()) -> IO ()
withImap server settings = bracket (connectIMAPSSLWithSettings server settings) logout

withDBus :: (Client -> IO ()) -> IO ()
withDBus = bracket connectSystem disconnect

busName :: BusName
busName = "tech.linj.MailNotifier"

objectPath :: ObjectPath
objectPath = "/tech/linj/MailNotifier"

interface :: InterfaceName
interface = "tech.linj.MailNotifier"

type AccountName = String

raceMany :: (Foldable t) => t (IO a) -> IO a
raceMany actions = runConcurrently $ asum $ map Concurrently $ toList actions

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
        upgradeMessageAction defaultFieldMap $
          cmapM (fmap encodeUtf8 . fmtRichMessage) logByteStringStdout
   in filterBySeverity severity msgSeverity logAction

syncNotificationMethodName :: MemberName
syncNotificationMethodName = "Notify"
