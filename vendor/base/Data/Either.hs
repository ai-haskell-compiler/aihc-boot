-- | The Either type.
module Data.Either
  ( Either (..),
    either,
    lefts,
    rights,
    partitionEithers,
  )
where

import GHC.Base
import GHC.Types

data Either a b = Left a | Right b

either :: (a -> c) -> (b -> c) -> Either a b -> c
either f _ (Left x) = f x
either _ g (Right y) = g y

lefts :: [Either a b] -> [a]
lefts xs = foldr (\e r -> either (\x -> x : r) (\_ -> r) e) [] xs

rights :: [Either a b] -> [b]
rights xs = foldr (\e r -> either (\_ -> r) (\y -> y : r) e) [] xs

partitionEithers :: [Either a b] -> ([a], [b])
partitionEithers xs = (lefts xs, rights xs)
