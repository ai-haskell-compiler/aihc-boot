-- | Orderings.
module Data.Ord
  ( Ord (..),
    Ordering (..),
    Down (..),
    comparing,
    clamp,
  )
where

import GHC.Base
import GHC.Classes
import GHC.Show
import GHC.Types

-- | The dual ordering.
newtype Down a = Down {getDown :: a}
  deriving (Eq)

-- | As in GHC's base, without the record syntax.
instance (Show a) => Show (Down a) where
  showsPrec d (Down x) = showParen (d >= 11) (showString "Down " . showsPrec 11 x)

instance (Ord a) => Ord (Down a) where
  compare (Down x) (Down y) = compare y x

comparing :: (Ord a) => (b -> a) -> b -> b -> Ordering
comparing p x y = compare (p x) (p y)

clamp :: (Ord a) => (a, a) -> a -> a
clamp (low, high) a = min high (max a low)
