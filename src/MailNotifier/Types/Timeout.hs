module MailNotifier.Types.Timeout
  ( TimeoutMicroSecond,
    TimeoutMilliSecond,
    Timeout (mkTimeout, unTimeout),
  )
where

import Language.Haskell.TH.Syntax (Lift)
import Relude

data TimeoutError = NonPositiveTimeout | TooLargeTimeout
  deriving stock (Show)

class Timeout t where
  unsafeMkTimeout :: Integer -> t
  mkTimeout :: Integer -> Either TimeoutError t
  mkTimeout timeout
    | timeout <= 0 = Left NonPositiveTimeout
    | timeout > toInteger (maxBound :: Int) = Left TooLargeTimeout
    | otherwise = Right $ unsafeMkTimeout timeout
  unTimeout :: t -> Integer

newtype TimeoutMicroSecond = TimeoutMicroSecond Integer
  deriving stock (Show, Lift)

instance Timeout TimeoutMicroSecond where
  unsafeMkTimeout = TimeoutMicroSecond
  unTimeout (TimeoutMicroSecond timeout) = timeout

newtype TimeoutMilliSecond = TimeoutMilliSecond Integer
  deriving stock (Show, Lift)

instance Timeout TimeoutMilliSecond where
  unsafeMkTimeout = TimeoutMilliSecond
  unTimeout (TimeoutMilliSecond timeout) = timeout
