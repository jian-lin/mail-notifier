module MailNotifier.Types.Timeout
  ( Timeout,
    mkTimeout,
    unTimeout,
    unsafeMkTimeout,
  )
where

import Relude

data TimeoutError = NonPositiveTimeout | TooLargeTimeout
  deriving stock (Show)

newtype Timeout = Timeout Integer
  deriving stock (Show)

mkTimeout :: Integer -> Either TimeoutError Timeout
mkTimeout timeout
  | timeout <= 0 = Left NonPositiveTimeout
  | timeout > toInteger (maxBound :: Int) = Left TooLargeTimeout
  | otherwise = Right $ Timeout timeout

unTimeout :: Timeout -> Integer
unTimeout (Timeout timeout) = timeout

-- TODO find a way to remove this
unsafeMkTimeout :: Integer -> Timeout
unsafeMkTimeout timeout = either (error . show) id (mkTimeout timeout)
