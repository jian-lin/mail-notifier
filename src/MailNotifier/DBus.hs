module MailNotifier.DBus (sync) where

import Colog (Message, WithLog, logDebug, logInfo, logWarning)
import Data.Text qualified as T
import MailNotifier.Types
import MailNotifier.Utils (busName, interface, objectPath, syncNotificationMethodName)
import Relude

sync ::
  (WithLog env Message m, HasConfig env, HasSyncJobQueue env, MonadSync m) =>
  DBusClient ->
  m Void
sync client = infinitely $ do
  config <- asks getConfig
  logInfo "wait for sync jobs"
  syncJobQueue <- asks getSyncJobQueue
  waitForSyncJobsM syncJobQueue (readSyncJobsTimeout config)
  logInfo "got sync jobs, start to sync"
  output <-
    syncM
      "/run/wrappers/bin/mbsyncSetuid"
      [ "--config",
        toText $ mbsyncConfigFile config,
        unAccountName $ accountName config
      ]
  unless (T.null output) $ logWarning ("sync output: " <> output) -- has warnings
  signalSyncDoneM client
  logDebug
    $ "DBus: called method "
    <> show syncNotificationMethodName
    <> " at "
    <> show busName
    <> " "
    <> show objectPath
    <> " "
    <> show interface
  logInfo "sent a synced notification"
