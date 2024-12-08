module Main where

import MailNotifier qualified (main)
import Relude

main :: IO ()
main = MailNotifier.main
