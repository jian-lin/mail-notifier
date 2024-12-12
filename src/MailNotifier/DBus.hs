module MailNotifier.DBus (sync) where

import Colog (Message, WithLog, logDebug, logInfo, logWarning)
import Data.Text qualified as T
import MailNotifier.Types
import MailNotifier.Utils (busName, interfaceName, objectPath, syncNotificationMethodName)
import Relude

sync ::
  (WithLog env Message m, HasConfig env, HasSyncJobQueue env, MonadSync m) => DBusClient -> m Void
sync client = infinitely $ do
  config <- asks getConfig
  logInfo "wait for sync jobs"
  syncJobQueue <- asks getSyncJobQueue
  waitForSyncJobsM syncJobQueue (readSyncJobsTimeout config)
  logInfo "got sync jobs, start to sync"
  (processStdoutOutput, processStderrOutput) <-
    syncM
      "/run/wrappers/bin/mbsyncSetuid"
      [ "--config",
        toText $ mbsyncConfigFile config,
        toText $ accountName config
      ]
  unless (T.null $ toText processStdoutOutput) $ logInfo $ toText processStdoutOutput
  unless (T.null $ toText processStderrOutput) $ logWarning $ toText processStderrOutput
  signalSyncDoneM client busName objectPath interfaceName syncNotificationMethodName
  logDebug
    $ "DBus: called method "
    <> show syncNotificationMethodName
    <> " at "
    <> show busName
    <> " "
    <> show objectPath
    <> " "
    <> show interfaceName
  logInfo "sent a synced notification"
