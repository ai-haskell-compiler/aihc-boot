-- | The 'Num' class, with its instances for Int and Integer.
module GHC.Num
  ( Num (..),
    subtract,
    Integer,
  )
where

import GHC.Classes
import GHC.Num.Integer
import GHC.Prim
import GHC.Show
import GHC.Types

infixl 6 +, -

infixl 7 *

class Num a where
  (+), (-), (*) :: a -> a -> a
  negate, abs, signum :: a -> a
  fromInteger :: Integer -> a
  x - y = x + negate y
  negate x = fromInteger 0 - x

subtract :: (Num a) => a -> a -> a
subtract x y = y - x

instance Num Int where
  (+) = primIntAdd
  (-) = primIntSub
  (*) = primIntMul
  negate = primIntNegate
  abs x = if primIntLt x 0 then primIntNegate x else x
  signum x
    | primIntLt x 0 = primIntNegate 1
    | primIntEq x 0 = 0
    | True = 1
  fromInteger = integerToInt

instance Num Integer where
  (+) = integerAdd
  (-) = integerSub
  (*) = integerMul
  negate = integerNegate
  abs = integerAbs
  signum = integerSignum
  fromInteger x = x

instance Show Integer where
  showsPrec d n s
    | integerIsNegative n =
        if d > 6
          then '(' : '-' : digits (integerNegate n) (')' : s)
          else '-' : digits (integerNegate n) s
    | True = digits n s
    where
      ten = integerFromInt 10
      digits m acc =
        let (q, r) = integerQuotRem m ten
            acc' = primChr (primOrd '0' + integerToInt r) : acc
         in if integerIsZero q then acc' else digits q acc'
