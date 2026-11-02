-- | The classes 'Real' and 'Integral', and integer functions.
module GHC.Real
  ( Real,
    Integral (..),
    fromIntegral,
    even,
    odd,
    (^),
    gcd,
    lcm,
    divZeroError,
  )
where

import GHC.Classes
import GHC.Enum
import GHC.Err
import GHC.Exception.Type
import GHC.Num
import GHC.Num.Integer
import GHC.Prim
import GHC.Types

infixl 7 `div`, `mod`, `quot`, `rem`

infixr 8 ^

-- | The boot base has no Rational, so 'Real' has no methods.
class (Num a, Ord a) => Real a

class (Real a, Enum a) => Integral a where
  quot, rem, div, mod :: a -> a -> a
  quotRem, divMod :: a -> a -> (a, a)
  toInteger :: a -> Integer
  n `quot` d = case quotRem n d of (q, _) -> q
  n `rem` d = case quotRem n d of (_, r) -> r
  n `div` d = case divMod n d of (q, _) -> q
  n `mod` d = case divMod n d of (_, r) -> r
  divMod n d =
    let qr = quotRem n d
     in case qr of
          (q, r) -> if signum r == negate (signum d) then (q - 1, r + d) else qr
  quotRem n d = (quot n d, rem n d)

instance Real Int

instance Integral Int where
  div x y = if primIntEq y 0 then divZeroError else primIntDiv x y
  mod x y = if primIntEq y 0 then divZeroError else primIntMod x y
  quot x y = if primIntEq y 0 then divZeroError else primIntQuot x y
  rem x y = if primIntEq y 0 then divZeroError else primIntRem x y
  divMod x y = (div x y, mod x y)
  quotRem x y = (quot x y, rem x y)
  toInteger = integerFromInt

instance Real Integer

instance Integral Integer where
  div = integerDiv
  mod = integerMod
  quot = integerQuot
  rem = integerRem
  divMod = integerDivMod
  quotRem = integerQuotRem
  toInteger x = x

fromIntegral :: (Integral a, Num b) => a -> b
fromIntegral x = fromInteger (toInteger x)

even :: (Integral a) => a -> Bool
even n = n `rem` 2 == 0

odd :: (Integral a) => a -> Bool
odd n = not (even n)

-- | Exponentiation with a non-negative exponent.
(^) :: (Num a, Integral b) => a -> b -> a
x ^ n
  | n < 0 = errorWithoutStackTrace "Negative exponent"
  | n == 0 = 1
  | even n = half * half
  | otherwise = x * half * half
  where
    half = x ^ (n `quot` 2)
    otherwise = True

gcd :: (Integral a) => a -> a -> a
gcd x y = go (abs x) (abs y)
  where
    go a b = if b == 0 then a else go b (a `rem` b)

lcm :: (Integral a) => a -> a -> a
lcm _ 0 = 0
lcm 0 _ = 0
lcm x y = abs ((x `quot` gcd x y) * y)
