-- | The identity functor and monad.
module Data.Functor.Identity (Identity (..)) where

import Control.Monad.Fix
import Data.Foldable
import Data.Traversable
import GHC.Base
import GHC.Classes
import GHC.Show

newtype Identity a = Identity {runIdentity :: a}
  deriving (Eq, Ord)

-- | As in GHC's base, without the record syntax.
instance (Show a) => Show (Identity a) where
  showsPrec d (Identity x) = showParen (d >= 11) (showString "Identity " . showsPrec 11 x)

instance (Semigroup a) => Semigroup (Identity a) where
  Identity a <> Identity b = Identity (a <> b)

instance (Monoid a) => Monoid (Identity a) where
  mempty = Identity mempty

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity a = Identity (f a)

instance Monad Identity where
  Identity a >>= k = k a

instance MonadFix Identity where
  mfix f = Identity (fix (runIdentity . f))

instance Foldable Identity where
  foldMap f (Identity a) = f a
  foldr f z (Identity a) = f a z
  toList (Identity a) = [a]

instance Traversable Identity where
  traverse f (Identity a) = fmap Identity (f a)
