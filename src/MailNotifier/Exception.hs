{-# LANGUAGE QuasiQuotes #-}

module MailNotifier.Exception where

import Data.String.Interpolate (__i)
import MailNotifier.Types
import Relude
import System.Exit (ExitCode)

data PasswordDecodeException = PasswordDecodeException FilePath UnicodeException
  deriving stock (Show)

instance Exception PasswordDecodeException where
  displayException (PasswordDecodeException filePath unicodeException) =
    [__i|
      failed to decode #{filePath} with exception:
        #{displayException unicodeException}
    |]

newtype WatchdogMailboxError = WatchdogMailboxError Mailbox
  deriving stock (Show)

instance Exception WatchdogMailboxError where
  displayException (WatchdogMailboxError mailbox) =
    [__i|
      failed to signal watchdog that we have done a watch for #{mailbox}:
        missing watchdog state
    |]

data DBusRequestNameFailed = DBusRequestNameFailed DBusBusName DBusRequestNameReply
  deriving stock (Show)

instance Exception DBusRequestNameFailed where
  displayException (DBusRequestNameFailed busName requestNameReply) =
    [__i|
      failed to request the well-known #{busName} with reply:
        #{requestNameReply}
    |]

data DBusRequestNameCallError = DBusRequestNameCallError DBusBusName DBusClientError
  deriving stock (Show)

instance Exception DBusRequestNameCallError where
  displayException (DBusRequestNameCallError busName (DBusClientError clientError)) =
    [__i|
      failed to try to request the well-knwon #{busName} with dbus client error:
        #{displayException clientError}
    |]

data DBusEmitError = DBusEmitError DBusObjectPath DBusInterfaceName DBusMemberName DBusClientError
  deriving stock (Show)

instance Exception DBusEmitError where
  displayException (DBusEmitError objectPath interfaceName memberName (DBusClientError clientError)) =
    [__i|
      failed to emit a dbus signal #{memberName}
        at:
          #{objectPath} #{interfaceName}
        with dbus client error:
          #{displayException clientError}
    |]

-- TODO optional add the mail server info to mail exceptions

data MailLoginError = MailLoginError Username SomeException
  deriving stock (Show)

instance Exception MailLoginError where
  displayException (MailLoginError username exception) =
    [__i|
      failed to login to mail server as #{username} with exception:
        #{displayException exception}
    |]

newtype MailGetCapabilityError = MailGetCapabilityError SomeException
  deriving stock (Show)

instance Exception MailGetCapabilityError where
  displayException (MailGetCapabilityError exception) =
    [__i|
      failed to get mail capabilities with exception:
        #{displayException exception}
    |]

newtype MailListMailboxError = MailListMailboxError SomeException
  deriving stock (Show)

instance Exception MailListMailboxError where
  displayException (MailListMailboxError exception) =
    [__i|
      failed to list mailboxes with exception:
        #{displayException exception}
    |]

data MailSelectMailboxError = MailSelectMailboxError Mailbox SomeException
  deriving stock (Show)

instance Exception MailSelectMailboxError where
  displayException (MailSelectMailboxError mailbox exception) =
    [__i|
      failed to select #{mailbox} with exception:
        #{displayException exception}
    |]

data MailIdleError = MailIdleError TimeoutMilliSecond SomeException
  deriving stock (Show)

instance Exception MailIdleError where
  displayException (MailIdleError timeoutMilliSecond exception) =
    [__i|
      failed to run mail IDLE command
        in:
          #{timeoutMilliSecond}
        with exception:
          #{displayException exception}
    |]

data MailSleepError = MailSleepError TimeoutMicroSecond SomeException
  deriving stock (Show)

instance Exception MailSleepError where
  displayException (MailSleepError timeoutMicroSecond exception) =
    [__i|
      failed to sleep
        in:
          #{timeoutMicroSecond}
        with exception:
          #{displayException exception}
    |]

data SyncExternalProcessError
  = SyncExternalProcessError FilePath [Text] ExitCode ProcessStdoutOutput ProcessStderrOutput
  deriving stock (Show)

instance Exception SyncExternalProcessError where
  displayException (SyncExternalProcessError filePath args exitCode output err) =
    let command = unwords (toText filePath : args)
     in [__i|
          failed to run external command to sync mails
            command:
              #{command}
            exit code:
              #{exitCode}
            output message:
              #{output}'
            error message:
              #{err}
        |]

data SyncDBusError
  = SyncDBusError DBusBusName DBusObjectPath DBusInterfaceName DBusMemberName DBusClientError
  deriving stock (Show)

instance Exception SyncDBusError where
  displayException (SyncDBusError busName objectPath interfaceName memberName (DBusClientError clientError)) =
    [__i|
      failed to tell dbus broker that a sync is done
        method:
          #{memberName}
        destionation:
          #{busName} #{objectPath} #{interfaceName}
        with exception:
          #{displayException clientError}
    |]
