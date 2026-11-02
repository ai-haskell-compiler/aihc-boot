-- | Non-empty lists.
module Data.List.NonEmpty
  ( NonEmpty (..),
    toList,
    head,
  )
where

import GHC.Base

toList :: NonEmpty a -> [a]
toList (a :| as) = a : as

head :: NonEmpty a -> a
head (a :| _) = a
