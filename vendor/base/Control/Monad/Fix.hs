-- | Monads with a fixed point operation.
module Control.Monad.Fix (MonadFix (..), fix) where

import Data.Either
import Data.Function (fix)
import GHC.Base
import GHC.Err
import GHC.List (head, tail)
import GHC.Maybe

class (Monad m) => MonadFix m where
  mfix :: (a -> m a) -> m a

instance MonadFix Maybe where
  mfix f = let a = f (unJust a) in a
    where
      unJust (Just x) = x
      unJust Nothing = errorWithoutStackTrace "mfix Maybe: Nothing"

instance MonadFix [] where
  mfix f = case fix (f . head) of
    [] -> []
    (x : _) -> x : mfix (tail . f)

instance MonadFix (Either e) where
  mfix f = let a = f (unRight a) in a
    where
      unRight (Right x) = x
      unRight (Left _) = errorWithoutStackTrace "mfix Either: Left"
