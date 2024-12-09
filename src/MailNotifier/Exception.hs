{-# LANGUAGE DeriveAnyClass #-}

module MailNotifier.Exception where

import Relude

newtype PasswordDecodeException = PasswordDecodeException UnicodeException
  deriving stock (Show)
  deriving anyclass (Exception)
