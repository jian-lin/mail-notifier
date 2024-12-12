module MailNotifier.Mail (watch) where

import Colog (Message, WithLog, logDebug, logInfo)
import MailNotifier.Types
import Relude

watch ::
  ( WithLog env Message m,
    HasConfig env,
    HasSyncJobQueue env,
    MonadSync m,
    HasWatchdogState env,
    MonadWatchdog m,
    MonadMailRead m
  ) =>
  Password ->
  Mailbox ->
  ImapConnection ->
  m Void
watch password mailbox conn = do
  logInfo $ "watch " <> show mailbox
  config <- asks getConfig
  loginM conn (username config) password
  logDebug "logged in "
  capabilities <- getCapabilitiesM conn
  logDebug $ "capabilities: " <> show capabilities
  allMailboxes <- listMailboxesM conn
  logDebug $ "mailboxes: " <> show allMailboxes
  selectMailboxM conn mailbox
  logDebug $ "selected " <> show mailbox
  mailNum <- getMailNumM conn
  logInfo $ show mailNum <> " mails in " <> show mailbox
  syncJobQueue <- asks getSyncJobQueue
  addSyncJobM syncJobQueue -- initial sync job
  let supportIdle = "IDLE" `elem` capabilities
      watchAwhile =
        if supportIdle
          then idleOrSleepM conn (idleTimeout config) Idle
          else idleOrSleepM conn (pollInterval config) Sleep
  logInfo $ "enter watchLoop, " <> if supportIdle then "use IDLE" else "fallback to poll"
  watchLoop mailNum watchAwhile (getMailNumM conn) mailbox

watchLoop ::
  ( WithLog env Message m,
    HasSyncJobQueue env,
    MonadSync m,
    HasWatchdogState env,
    MonadWatchdog m
  ) =>
  MailNumber ->
  m () ->
  m MailNumber ->
  Mailbox ->
  m Void
watchLoop mailNum watchAwhile getMailNum mailbox = do
  watchdogState <- asks getWatchdogState
  signalCheckedMailboxM mailbox watchdogState
  watchAwhile
  newMailNum <- getMailNum
  if newMailNum == mailNum
    then logDebug $ show mailbox <> " has no new mail, still " <> show mailNum
    else do
      logInfo
        $ show mailbox
        <> " has "
        <> show (newMailNum - mailNum)
        <> " new mail(s), total "
        <> show newMailNum
      syncJobQueue <- asks getSyncJobQueue
      addSyncJobM syncJobQueue
  watchLoop newMailNum watchAwhile getMailNum mailbox
