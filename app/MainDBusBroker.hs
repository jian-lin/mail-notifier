module Main where

import MailNotifier.DBusBroker qualified (main)
import Relude

main :: IO ()
main = MailNotifier.DBusBroker.main
