-- | Arithmetic on Int. The boot base has no classes yet.
module GHC.Num
  ( (+),
    (-),
    (*),
    negate,
    abs,
    signum,
    subtract,
  )
where

import GHC.Prim
import GHC.Types

infixl 6 +, -

infixl 7 *

(+) :: Int -> Int -> Int
(+) = primIntAdd

(-) :: Int -> Int -> Int
(-) = primIntSub

(*) :: Int -> Int -> Int
(*) = primIntMul

negate :: Int -> Int
negate = primIntNegate

abs :: Int -> Int
abs x = if primIntLt x 0 then primIntNegate x else x

signum :: Int -> Int
signum x
  | primIntLt x 0 = primIntNegate 1
  | primIntEq x 0 = 0
  | True = 1

subtract :: Int -> Int -> Int
subtract x y = primIntSub y x
