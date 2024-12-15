module MailNotifier.Types.Timeout
  ( Timeout,
    mkTimeout,
    unTimeout,
  )
where

import Language.Haskell.TH.Syntax (Lift)
import Relude

data TimeoutError = NonPositiveTimeout | TooLargeTimeout
  deriving stock (Show)

newtype Timeout = Timeout Integer
  deriving stock (Show, Lift)

mkTimeout :: Integer -> Either TimeoutError Timeout
mkTimeout timeout
  | timeout <= 0 = Left NonPositiveTimeout
  | timeout > toInteger (maxBound :: Int) = Left TooLargeTimeout
  | otherwise = Right $ Timeout timeout

unTimeout :: Timeout -> Integer
unTimeout (Timeout timeout) = timeout
