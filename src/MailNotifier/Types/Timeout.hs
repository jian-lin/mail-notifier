module MailNotifier.Types.Timeout
  ( Timeout,
    mkTimeout,
    unTimeout,
  )
where

import Relude

data TimeoutError = NonPositiveTimeout | TooLargeTimeout
  deriving stock (Show)

newtype Timeout = Timeout Integer
  deriving stock (Show, Eq, Ord)
  deriving newtype (Num, Real, Enum, Integral)

mkTimeout :: Integer -> Either TimeoutError Timeout
mkTimeout timeout
  | timeout <= 0 = Left NonPositiveTimeout
  | timeout > toInteger (maxBound :: Int) = Left TooLargeTimeout
  | otherwise = Right $ Timeout timeout

unTimeout :: Timeout -> Integer
unTimeout (Timeout timeout) = timeout
