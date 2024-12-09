module MailNotifier.Watchdog (watchdog) where

import Colog (Message, WithLog, logDebug)
import Data.HashMap.Strict qualified as HM
import MailNotifier.Types
import Relude
import System.Systemd.Daemon (notifyWatchdog)

watchdog ::
  (WithLog env Message m, MonadIO m, HasWatchdogState env, HasConfig env) =>
  m Void
watchdog = infinitely $ do
  config <- asks getConfig
  let mailboxNum = length $ mailboxes config
  watchdogState <- asks getWatchdogState
  atomically $ mapM_ takeTMVar $ HM.elems (unWatchdogState watchdogState)
  reply <- liftIO notifyWatchdog
  logDebug
    $ "all "
    <> show mailboxNum
    <> " mailboxes are under watch, "
    <> if isNothing reply
      then "but watchdog is not enabled by systemd"
      else "sent watchdog to systemd"
