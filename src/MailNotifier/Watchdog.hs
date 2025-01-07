{-# LANGUAGE QuasiQuotes #-}

module MailNotifier.Watchdog (watchdog) where

import Colog (Message, WithLog, logDebug)
import Data.String.Interpolate (i)
import MailNotifier.Types
import Relude

watchdog ::
  (WithLog env Message m, HasWatchdogState env, MonadWatchdog m, HasConfig env) => m Void
watchdog = infinitely $ do
  config <- asks getConfig
  let mailboxNum = length $ mailboxes config
  watchdogState <- asks getWatchdogState
  mReply <- notiftyWatchdogWhenAllMailboxesAreCheckedM watchdogState
  let whatIsDone :: Text
      whatIsDone =
        if isNothing mReply
          then "but watchdog is not enabled by systemd"
          else "sent watchdog to systemd"
  logDebug [i|all #{mailboxNum} mailboxes are under watch, #{whatIsDone}|]
