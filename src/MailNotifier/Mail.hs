{-# LANGUAGE QuasiQuotes #-}

module MailNotifier.Mail (watch) where

import Colog (Message, WithLog, logDebug, logInfo)
import Data.String.Interpolate (i)
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
  logInfo [i|watch #{mailbox}|]
  config <- asks getConfig
  loginM conn (username config) password
  logDebug "logged in"
  capabilities <- getCapabilitiesM conn
  logDebug [i|"capabilities: #{capabilities}|]
  allMailboxes <- listMailboxesM conn
  logDebug [i|mailboxes: #{allMailboxes}|]
  selectMailboxM conn mailbox
  logDebug [i|"selected #{mailbox}|]
  mailNum <- getMailNumM conn
  logInfo [i|#{mailNum} mails in #{mailbox}|]
  syncJobQueue <- asks getSyncJobQueue
  addSyncJobM syncJobQueue -- initial sync job
  let supportIdle = "IDLE" `elem` capabilities
      watchAwhile =
        if supportIdle
          then idleM conn (idleTimeout config)
          else sleepM (pollInterval config)
      watchMethod :: Text
      watchMethod = if supportIdle then "use IDLE" else "fallback to poll"
  logInfo [i|enter watchLoop, #{watchMethod}|]
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
    then logDebug [i|#{mailbox} has no new mail, still #{mailNum}|]
    else do
      logInfo [i|#{mailbox} has #{newMailNum - mailNum} new mail(s), total #{newMailNum}|]
      syncJobQueue <- asks getSyncJobQueue
      addSyncJobM syncJobQueue
  watchLoop newMailNum watchAwhile getMailNum mailbox
