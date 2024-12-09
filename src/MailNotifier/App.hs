module MailNotifier.App where

import MailNotifier.Types
import Relude
import UnliftIO (MonadUnliftIO)

newtype App a = App {runApp :: ReaderT (Env App) IO a}
  deriving (Functor, Applicative, Monad, MonadReader (Env App), MonadIO, MonadUnliftIO)

run :: App a -> Env App -> IO a
run app = runReaderT (runApp app)
