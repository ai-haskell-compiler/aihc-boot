-- | Monads in which IO computations can run.
module Control.Monad.IO.Class (MonadIO (..)) where

import GHC.Base
import GHC.Types

class (Monad m) => MonadIO m where
  liftIO :: IO a -> m a

instance MonadIO IO where
  liftIO = id
