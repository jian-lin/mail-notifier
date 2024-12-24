module MailNotifier.Types.Timeout.TH
  ( mkTimeoutMicroSecondTH,
    mkTimeoutMilliSecondTH,
  )
where

import Language.Haskell.TH (Exp, Q)
import Language.Haskell.TH.Syntax (lift)
import MailNotifier.Types.Timeout (TimeoutMicroSecond, TimeoutMilliSecond, mkTimeout)
import Relude hiding (lift)

-- | A type-safe way to write literal TimeoutMicroSecond values
mkTimeoutMicroSecondTH :: Integer -> Q Exp
mkTimeoutMicroSecondTH timeout =
  either (fail . show) lift $ mkTimeout @TimeoutMicroSecond timeout

-- | A type-safe way to write literal TimeoutMilliSecond values
mkTimeoutMilliSecondTH :: Integer -> Q Exp
mkTimeoutMilliSecondTH timeout =
  either (fail . show) lift $ mkTimeout @TimeoutMilliSecond timeout
