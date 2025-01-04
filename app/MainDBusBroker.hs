module Main (main) where

import Colog (HasLog (..), LogAction, Message, Severity (Info))
import Control.Exception.Uncaught (setDisplayExceptionHandler)
import MailNotifier (appDBusBroker)
import MailNotifier.App (run)
import MailNotifier.Types
import MailNotifier.Utils (mkLogAction, withDBus)
import Relude
import UnliftIO (newTBQueue)

data Env m = Env
  { envLogAction :: !(LogAction m Message),
    envSyncJobQueue :: !SyncJobQueue
  }

instance HasLog (Env m) Message m where
  getLogAction = envLogAction
  {-# INLINE getLogAction #-}
  setLogAction newLogAction env = env {envLogAction = newLogAction}
  {-# INLINE setLogAction #-}

instance HasSyncJobQueue (Env m) where
  getSyncJobQueue = envSyncJobQueue
  {-# INLINE getSyncJobQueue #-}

main :: IO ()
main = do
  setDisplayExceptionHandler
  hSetBuffering stdout LineBuffering -- print log while running under systemd
  queue <- atomically $ newTBQueue 10
  let env =
        Env
          { envLogAction = mkLogAction Info,
            envSyncJobQueue = SyncJobQueue queue
          }
  fmap absurd $ withDBus $ \client -> run (appDBusBroker client) env
