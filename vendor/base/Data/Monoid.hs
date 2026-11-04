-- | Monoids, and newtypes with the monoid of a choice.
module Data.Monoid
  ( Monoid (..),
    (<>),
    Dual (..),
    Endo (..),
    All (..),
    Any (..),
    Sum (..),
    Product (..),
    First (..),
    Last (..),
    Alt (..),
  )
where

import Data.Foldable (Endo (..))
import GHC.Base
import GHC.Classes
import GHC.Maybe
import GHC.Num
import GHC.Show
import GHC.Types

-- | The dual of a monoid: the arguments of '<>' swap.
newtype Dual a = Dual {getDual :: a}
  deriving (Eq, Ord, Show)

instance (Semigroup a) => Semigroup (Dual a) where
  Dual a <> Dual b = Dual (b <> a)

instance (Monoid a) => Monoid (Dual a) where
  mempty = Dual mempty

-- | Booleans under conjunction.
newtype All = All {getAll :: Bool}
  deriving (Eq, Ord, Show)

instance Semigroup All where
  All a <> All b = All (a && b)

instance Monoid All where
  mempty = All True

-- | Booleans under disjunction.
newtype Any = Any {getAny :: Bool}
  deriving (Eq, Ord, Show)

instance Semigroup Any where
  Any a <> Any b = Any (a || b)

instance Monoid Any where
  mempty = Any False

-- | Numbers under addition.
newtype Sum a = Sum {getSum :: a}
  deriving (Eq, Ord, Show)

instance (Num a) => Semigroup (Sum a) where
  Sum a <> Sum b = Sum (a + b)

instance (Num a) => Monoid (Sum a) where
  mempty = Sum 0

-- | Numbers under multiplication.
newtype Product a = Product {getProduct :: a}
  deriving (Eq, Ord, Show)

instance (Num a) => Semigroup (Product a) where
  Product a <> Product b = Product (a * b)

instance (Num a) => Monoid (Product a) where
  mempty = Product 1

-- | The leftmost Just.
newtype First a = First {getFirst :: Maybe a}
  deriving (Eq, Ord, Show)

instance Semigroup (First a) where
  First Nothing <> b = b
  a <> _ = a

instance Monoid (First a) where
  mempty = First Nothing

-- | The rightmost Just.
newtype Last a = Last {getLast :: Maybe a}
  deriving (Eq, Ord, Show)

instance Semigroup (Last a) where
  a <> Last Nothing = a
  _ <> b = b

instance Monoid (Last a) where
  mempty = Last Nothing

-- | An Alternative under '<|>'.
newtype Alt f a = Alt {getAlt :: f a}

instance (Alternative f) => Semigroup (Alt f a) where
  Alt a <> Alt b = Alt (a <|> b)

instance (Alternative f) => Monoid (Alt f a) where
  mempty = Alt empty
