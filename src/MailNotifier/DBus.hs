module MailNotifier.DBus (sync) where

import Colog (Message, WithLog, logDebug, logInfo, logWarning)
import DBus (methodCall, methodCallDestination)
import DBus.Client (Client, call_)
import MailNotifier.Types
import MailNotifier.Utils
  ( atomicallyTimeoutUntilFail_,
    busName,
    interface,
    objectPath,
    syncNotificationMethodName,
  )
import Relude
import UnliftIO (readTBQueue)
import UnliftIO.Process (readProcess)

sync ::
  (WithLog env Message m, MonadIO m, HasConfig env, HasSyncJobQueue env) =>
  Client ->
  m Void
sync client = infinitely $ do
  config <- asks getConfig
  logInfo "wait for sync jobs"
  syncJobQueue <- asks getSyncJobQueue
  _ <- atomically $ readTBQueue syncJobQueue
  atomicallyTimeoutUntilFail_ (readSyncJobsTimeout config) $ readTBQueue syncJobQueue
  logInfo "got sync jobs, start to sync"
  output <-
    readProcess
      "/run/wrappers/bin/mbsyncSetuid"
      [ "--config",
        mbsyncConfigFile config,
        accountName config
      ]
      ""
  unless (null output)
    $ logWarning ("sync output: " <> toText output) -- has warnings
  notify client
  logInfo "sent a synced notification"

-- DBus tutrial: https://dbus.freedesktop.org/doc/dbus-tutorial.html
notify ::
  (WithLog env Message m, MonadIO m, HasConfig env) =>
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
