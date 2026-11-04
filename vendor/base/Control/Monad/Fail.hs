-- | Monads with a failure that pattern-match failure in @do@ uses.
module Control.Monad.Fail (MonadFail (..)) where

import Control.Exception (ErrorCall (..), throwIO)
import GHC.Base
import GHC.Maybe
import GHC.Types

class (Monad m) => MonadFail m where
  fail :: String -> m a

instance MonadFail Maybe where
  fail _ = Nothing

instance MonadFail [] where
  fail _ = []

-- | GHC throws a user error. The boot base has no IOError yet, so the
-- failure is an ErrorCall.
instance MonadFail IO where
  fail s = throwIO (ErrorCall s)
