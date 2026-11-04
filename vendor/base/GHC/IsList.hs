{-# LANGUAGE TypeFamilies #-}

-- | Types that list syntax can build, with OverloadedLists.
module GHC.IsList (IsList (..)) where

import GHC.Base
import GHC.List (length)
import GHC.Types

class IsList l where
  type Item l
  fromList :: [Item l] -> l
  fromListN :: Int -> [Item l] -> l
  toList :: l -> [Item l]
  fromListN _ = fromList

instance IsList [a] where
  type Item [a] = a
  fromList xs = xs
  toList xs = xs
