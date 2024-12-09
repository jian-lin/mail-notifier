module MailNotifier.Mail (watch) where

import Colog (Message, WithLog, logDebug, logInfo)
import Data.Char (toUpper)
import Data.HashMap.Strict qualified as HM
import MailNotifier.Types
import Network.HaskellNet.IMAP.Connection (IMAPConnection, exists)
import Network.HaskellNet.IMAP.SSL (capability, idle, list, login, select)
import Network.HaskellNet.IMAP.Types (MailboxName)
import Relude
import UnliftIO (writeTBQueue)
import UnliftIO.Concurrent (threadDelay)

-- TODO exit with different exit code to indicate if restarting this program by system is desirable
-- e.g., if password is wrong, do not restart
--       if network connection is unavailable, restart
-- is it worth it?  Doesn't almost all "no need to restart" cases lead to a quick crash?
-- So if it runs fine for a while, it should be restarted if crashes.
-- Two exceptions I can think of are (1) the password is changed and (2) the mailbox is deleted.
watch ::
  (WithLog env Message m, MonadIO m, HasConfig env, HasSyncJobQueue env, HasWatchdogState env) =>
  Password ->
  MailboxName ->
  IMAPConnection ->
  m Void
watch password accountMailbox conn = do
  logInfo $ "watch " <> toText accountMailbox
  config <- asks getConfig
  liftIO $ login conn (toString . unUsername $ username config) (toString . unPassword $ password)
  logDebug "logged in "
  capabilities <- liftIO $ capability conn
  logDebug $ "capabilities: " <> show capabilities
  allMailboxes <- liftIO $ list conn
  logDebug $ "mailboxes: " <> show allMailboxes
  liftIO $ select conn accountMailbox
  logDebug $ "selected " <> toText accountMailbox
  let getMailNum = exists conn
  mailNum <- liftIO getMailNum
  logInfo $ show mailNum <> " mails in " <> toText accountMailbox
  syncJobQueue <- asks getSyncJobQueue
  atomically $ writeTBQueue (unSyncJobQueue syncJobQueue) () -- initial sync job
  let supportIdle = "IDLE" `elem` (toUpper <<$>> capabilities) -- assume case-insensitive
      watchAwhile =
        if supportIdle
          then idle conn (idleTimeout config)
          else threadDelay (pollInterval config)
  logInfo $ "enter watchLoop, " <> if supportIdle then "use IDLE" else "fallback to poll"
  watchLoop mailNum (liftIO watchAwhile) (liftIO getMailNum) accountMailbox

watchLoop ::
  (WithLog env Message m, MonadIO m, HasSyncJobQueue env, HasWatchdogState env) =>
  Integer ->
  m () ->
  m Integer ->
  MailboxName ->
  m Void
watchLoop mailNum watchAwhile getMailNum accountMailbox = do
  watchdogState <- asks $ (HM.! accountMailbox) . unWatchdogState . getWatchdogState
  _ <- atomically $ tryPutTMVar watchdogState ()
  watchAwhile
  newMailNum <- getMailNum
  syncJobQueue <- asks getSyncJobQueue
  if newMailNum == mailNum
    then logDebug $ toText accountMailbox <> " has no new mail, still " <> show mailNum
    else do
      logInfo
        $ toText accountMailbox
        <> " has "
        <> show (newMailNum - mailNum)
        <> " new mail(s), total "
        <> show mailNum
      atomically $ writeTBQueue (unSyncJobQueue syncJobQueue) ()
  watchLoop newMailNum watchAwhile getMailNum accountMailbox
