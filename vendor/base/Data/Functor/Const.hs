-- | The constant functor.
module Data.Functor.Const (Const (..)) where

import Data.Foldable
import Data.Traversable
import GHC.Base
import GHC.Classes
import GHC.Show

newtype Const a b = Const {getConst :: a}
  deriving (Eq, Ord)

-- | As in GHC's base, without the record syntax.
instance (Show a) => Show (Const a b) where
  showsPrec d (Const x) = showParen (d >= 11) (showString "Const " . showsPrec 11 x)

instance (Semigroup a) => Semigroup (Const a b) where
  Const a <> Const b = Const (a <> b)

instance (Monoid a) => Monoid (Const a b) where
  mempty = Const mempty

instance Functor (Const m) where
  fmap _ (Const v) = Const v

instance Foldable (Const m) where
  foldMap _ _ = mempty
  foldr _ z _ = z

instance Traversable (Const m) where
  traverse _ (Const v) = pure (Const v)

instance (Monoid m) => Applicative (Const m) where
  pure _ = Const mempty
  Const f <*> Const v = Const (f <> v)
