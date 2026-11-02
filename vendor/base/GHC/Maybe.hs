-- | The Maybe type.
module GHC.Maybe (Maybe (..)) where

import GHC.Classes
import GHC.Types

data Maybe a = Nothing | Just a

instance (Eq a) => Eq (Maybe a) where
  Nothing == Nothing = True
  Just x == Just y = x == y
  _ == _ = False

instance (Ord a) => Ord (Maybe a) where
  compare Nothing Nothing = EQ
  compare Nothing (Just _) = LT
  compare (Just _) Nothing = GT
  compare (Just x) (Just y) = compare x y
