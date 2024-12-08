module Main where

import qualified MailNotifier.DBusBroker (main)

main :: IO ()
main = MailNotifier.DBusBroker.main
