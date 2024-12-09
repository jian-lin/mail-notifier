module Main (main) where

import Colog (Severity (Info))
import MailNotifier.DBusBroker (App, Env (..), app, run)
import MailNotifier.Utils (mkLogAction, withDBus)
import Relude
import UnliftIO (newTBQueue)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering -- print log while running under systemd
  queue <- atomically $ newTBQueue 10
  let env :: Env App
      env =
        Env
          { envLogAction = mkLogAction Info,
            envQueue = queue
          }
  fmap absurd $ withDBus $ \client -> run env (app client)
