{-# LANGUAGE DeriveAnyClass #-}

module MailNotifier.Exception where

import MailNotifier.Types
import Relude
import UnliftIO (IOException)

newtype PasswordDecodeException = PasswordDecodeException UnicodeException
  deriving stock (Show)
  deriving anyclass (Exception)

newtype WatchdogMailboxError = WatchdogMailboxError Mailbox
  deriving stock (Show)
  deriving anyclass (Exception)

data DBusRequestNameFailed = DBusRequestNameFailed DBusBusName DBusRequestNameReply
  deriving stock (Show)
  deriving anyclass (Exception)

data DBusRequestNameCallError = DBusRequestNameCallError DBusBusName DBusClientError
  deriving stock (Show)
  deriving anyclass (Exception)

data DBusEmitError = DBusEmitError DBusObjectPath DBusInterfaceName DBusMemberName DBusClientError
  deriving stock (Show)
  deriving anyclass (Exception)

data MailLoginError = MailLoginError Username SomeException
  deriving stock (Show)
  deriving anyclass (Exception)

newtype MailGetCapabilityError = MailGetCapabilityError SomeException
  deriving stock (Show)
  deriving anyclass (Exception)

newtype MailListMailboxError = MailListMailboxError SomeException
  deriving stock (Show)
  deriving anyclass (Exception)

data MailSelectMailboxError = MailSelectMailboxError Mailbox SomeException
  deriving stock (Show)
  deriving anyclass (Exception)

data MailIdleOrSleepError = MailIdleOrSleepError Timeout SomeException
  deriving stock (Show)
  deriving anyclass (Exception)

data SyncExternalProcessError = SyncExternalProcessError FilePath [Text] IOException
  deriving stock (Show)
  deriving anyclass (Exception)

data SyncDBusError
  = SyncDBusError DBusBusName DBusObjectPath DBusInterfaceName DBusMemberName DBusClientError
  deriving stock (Show)
  deriving anyclass (Exception)
