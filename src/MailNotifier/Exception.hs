module MailNotifier.Exception where

import Relude

newtype PasswordDecodeException = PasswordDecodeException UnicodeException
  deriving (Show, Exception)
