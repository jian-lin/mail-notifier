module MailNotifier.Watchdog (watchdog) where

import Colog (Message, WithLog, logDebug)
import MailNotifier.Types
import Relude

watchdog ::
  (WithLog env Message m, HasWatchdogState env, MonadWatchdog m, HasConfig env) => m Void
watchdog = infinitely $ do
  config <- asks getConfig
  let mailboxNum = length $ mailboxes config
  watchdogState <- asks getWatchdogState
  mReply <- notiftyWatchdogWhenAllMailboxesAreCheckedM watchdogState
  logDebug
    $ "all "
    <> show mailboxNum
    <> " mailboxes are under watch, "
    <> if isNothing mReply
      then "but watchdog is not enabled by systemd"
      else "sent watchdog to systemd"
