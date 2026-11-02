-- | Integer division on Int. The boot base has no classes yet.
module GHC.Real
  ( div,
    mod,
    quot,
    rem,
    divMod,
    quotRem,
    even,
    odd,
    (^),
    gcd,
    lcm,
  )
where

import GHC.Exception.Type
import GHC.Num
import GHC.Prim
import GHC.Types

infixl 7 `div`, `mod`, `quot`, `rem`

infixr 8 ^

-- | The division functions raise 'DivideByZero' when the divisor is
-- zero, as in GHC.
div :: Int -> Int -> Int
div x y = if primIntEq y 0 then divZeroError else primIntDiv x y

mod :: Int -> Int -> Int
mod x y = if primIntEq y 0 then divZeroError else primIntMod x y

quot :: Int -> Int -> Int
quot x y = if primIntEq y 0 then divZeroError else primIntQuot x y

rem :: Int -> Int -> Int
rem x y = if primIntEq y 0 then divZeroError else primIntRem x y

divMod :: Int -> Int -> (Int, Int)
divMod x y = (div x y, mod x y)

quotRem :: Int -> Int -> (Int, Int)
quotRem x y = (quot x y, rem x y)

even :: Int -> Bool
even x = primIntEq (primIntRem x 2) 0

odd :: Int -> Bool
odd x = primIntNe (primIntRem x 2) 0

-- | Exponentiation with a non-negative exponent.
(^) :: Int -> Int -> Int
x ^ n
  | primIntLt n 0 = error "Negative exponent"
  | primIntEq n 0 = 1
  | even n = half * half
  | True = x * (half * half)
  where
    half = x ^ primIntQuot n 2

gcd :: Int -> Int -> Int
gcd x y = go (abs x) (abs y)
  where
    go a b = if primIntEq b 0 then a else go b (primIntRem a b)

lcm :: Int -> Int -> Int
lcm x y
  | primIntEq x 0 = 0
  | primIntEq y 0 = 0
  | True = abs (primIntQuot x (gcd x y) * y)
