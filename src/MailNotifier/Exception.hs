{-# LANGUAGE DeriveAnyClass #-}

module MailNotifier.Exception where

import MailNotifier.Types
import Relude

newtype PasswordDecodeException = PasswordDecodeException UnicodeException
  deriving stock (Show)
  deriving anyclass (Exception)

newtype WatchdogMailboxError = WatchdogMailboxError Mailbox
  deriving stock (Show)
  deriving anyclass (Exception)

data DBusRequestNameError = DBusRequestNameError DBusBusName DBusRequestNameReply
  deriving stock (Show)
  deriving anyclass (Exception)
