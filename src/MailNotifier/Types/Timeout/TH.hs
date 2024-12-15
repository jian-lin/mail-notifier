module MailNotifier.Types.Timeout.TH (mkTimeoutTH) where

import Language.Haskell.TH (Exp, Q)
import Language.Haskell.TH.Syntax (lift)
import MailNotifier.Types.Timeout (mkTimeout)
import Relude hiding (lift)

-- | A type-safe way to write literal Timeout values
mkTimeoutTH :: Integer -> Q Exp
mkTimeoutTH timeout =
  case mkTimeout timeout of
    Left err -> fail $ show err
    Right timeout' -> lift timeout'
