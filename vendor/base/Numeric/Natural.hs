-- | Natural numbers: non-negative integers of any size.
module Numeric.Natural (Natural) where

import GHC.Base
import GHC.Classes
import GHC.Enum
import GHC.Exception.Type
import GHC.Num
import GHC.Num.Integer
import GHC.Prim
import GHC.Real
import GHC.Show
import GHC.Types

-- | A natural number. The Integer is never negative.
newtype Natural = Natural Integer

unNatural :: Natural -> Integer
unNatural (Natural n) = n

-- | A natural number, or 'Underflow' for a negative Integer.
natural :: Integer -> Natural
natural n = if n < 0 then primRaise (toException Underflow) else Natural n

instance Eq Natural where
  a == b = unNatural a == unNatural b

instance Ord Natural where
  compare a b = compare (unNatural a) (unNatural b)

instance Show Natural where
  showsPrec d a = showsPrec d (unNatural a)

instance Num Natural where
  a + b = Natural (unNatural a + unNatural b)
  a - b = natural (unNatural a - unNatural b)
  a * b = Natural (unNatural a * unNatural b)
  negate a = natural (negate (unNatural a))
  abs a = a
  signum a = Natural (signum (unNatural a))
  fromInteger = natural

instance Enum Natural where
  succ a = a + 1
  pred a = a - 1
  toEnum i = natural (integerFromInt i)
  fromEnum a = integerToInt (unNatural a)
  enumFrom a = a : enumFrom (a + 1)
  enumFromTo a b = map Natural (enumFromTo (unNatural a) (unNatural b))
  enumFromThenTo a b c = map Natural (enumFromThenTo (unNatural a) (unNatural b) (unNatural c))

instance Real Natural

instance Integral Natural where
  quot a b = Natural (quot (unNatural a) (unNatural b))
  rem a b = Natural (rem (unNatural a) (unNatural b))
  div a b = Natural (div (unNatural a) (unNatural b))
  mod a b = Natural (mod (unNatural a) (unNatural b))
  quotRem a b = (quot a b, rem a b)
  divMod a b = (div a b, mod a b)
  toInteger = unNatural
