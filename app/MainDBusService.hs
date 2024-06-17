module Main where

import qualified Network.MailNotifier.DBusService (main)

main :: IO ()
main = Network.MailNotifier.DBusService.main
