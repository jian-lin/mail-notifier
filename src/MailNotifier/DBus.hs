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
import Relude hiding (getArgs) -- FIXME
import UnliftIO (readTBQueue)
import UnliftIO.Process (readProcess)

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
