-- | Functors of two arguments.
module Data.Bifunctor (Bifunctor (..)) where

import Data.Either
import GHC.Base

class Bifunctor p where
  bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
  first :: (a -> b) -> p a c -> p b c
  second :: (b -> c) -> p a b -> p a c
  bimap f g = first f . second g
  first f = bimap f id
  second = bimap id

instance Bifunctor (,) where
  bimap f g (a, b) = (f a, g b)

instance Bifunctor (Either) where
  bimap f _ (Left a) = Left (f a)
  bimap _ g (Right b) = Right (g b)
