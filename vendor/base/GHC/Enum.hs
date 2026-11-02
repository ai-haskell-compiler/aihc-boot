-- | Enumeration of Int. Arithmetic sequences desugar to these functions.
module GHC.Enum
  ( succ,
    pred,
    enumFrom,
    enumFromThen,
    enumFromTo,
    enumFromThenTo,
  )
where

import GHC.Classes
import GHC.Num
import GHC.Types

succ :: Int -> Int
succ x = x + 1

pred :: Int -> Int
pred x = x - 1

enumFrom :: Int -> [Int]
enumFrom x = x : enumFrom (x + 1)

enumFromThen :: Int -> Int -> [Int]
enumFromThen x y = go x
  where
    step = y - x
    go n = n : go (n + step)

enumFromTo :: Int -> Int -> [Int]
enumFromTo x y = if x > y then [] else x : enumFromTo (x + 1) y

enumFromThenTo :: Int -> Int -> Int -> [Int]
enumFromThenTo x y z
  | step >= 0 = up x
  | True = down x
  where
    step = y - x
    up n = if n > z then [] else n : up (n + step)
    down n = if n < z then [] else n : down (n + step)
