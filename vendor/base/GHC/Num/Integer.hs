{-# LANGUAGE GHCForeignImportPrim #-}

-- | Arbitrary-precision integers. The names follow the module
-- GHC.Num.Integer of GHC. The boot base has no classes yet, so these
-- functions are the only way to compute with an Integer.
--
-- The representation is primitive. The evaluator keeps an Integer that
-- fits in an Int as an Int, so 'integerFromInt' costs nothing.
module GHC.Num.Integer
  ( Integer,
    integerZero,
    integerOne,
    integerFromInt,
    integerToInt,
    integerAdd,
    integerSub,
    integerMul,
    integerNegate,
    integerAbs,
    integerSignum,
    integerQuot,
    integerRem,
    integerDiv,
    integerMod,
    integerQuotRem,
    integerDivMod,
    integerGcd,
    integerLcm,
    integerEq,
    integerNe,
    integerLt,
    integerLe,
    integerGt,
    integerGe,
    integerCompare,
    integerIsZero,
    integerIsNegative,
  )
where

import GHC.Types

data Integer

foreign import prim "integerFromInt" integerFromInt :: Int -> Integer

-- | The low bits of an Integer, in two's complement.
foreign import prim "integerToInt" integerToInt :: Integer -> Int

foreign import prim "integerAdd" integerAdd :: Integer -> Integer -> Integer

foreign import prim "integerSub" integerSub :: Integer -> Integer -> Integer

foreign import prim "integerMul" integerMul :: Integer -> Integer -> Integer

foreign import prim "integerNegate" integerNegate :: Integer -> Integer

-- | Division that rounds toward zero.
foreign import prim "integerQuot" integerQuot :: Integer -> Integer -> Integer

foreign import prim "integerRem" integerRem :: Integer -> Integer -> Integer

-- | Division that rounds toward negative infinity.
foreign import prim "integerDiv" integerDiv :: Integer -> Integer -> Integer

foreign import prim "integerMod" integerMod :: Integer -> Integer -> Integer

-- | The greatest common divisor. It is never negative.
foreign import prim "integerGcd" integerGcd :: Integer -> Integer -> Integer

foreign import prim "integerEq" integerEq :: Integer -> Integer -> Bool

foreign import prim "integerNe" integerNe :: Integer -> Integer -> Bool

foreign import prim "integerLt" integerLt :: Integer -> Integer -> Bool

foreign import prim "integerLe" integerLe :: Integer -> Integer -> Bool

foreign import prim "integerGt" integerGt :: Integer -> Integer -> Bool

foreign import prim "integerGe" integerGe :: Integer -> Integer -> Bool

integerZero :: Integer
integerZero = integerFromInt 0

integerOne :: Integer
integerOne = integerFromInt 1

integerIsZero :: Integer -> Bool
integerIsZero x = integerEq x integerZero

integerIsNegative :: Integer -> Bool
integerIsNegative x = integerLt x integerZero

integerAbs :: Integer -> Integer
integerAbs x = if integerIsNegative x then integerNegate x else x

integerSignum :: Integer -> Integer
integerSignum x
  | integerIsNegative x = integerNegate integerOne
  | integerIsZero x = integerZero
  | True = integerOne

integerQuotRem :: Integer -> Integer -> (Integer, Integer)
integerQuotRem x y = (integerQuot x y, integerRem x y)

integerDivMod :: Integer -> Integer -> (Integer, Integer)
integerDivMod x y = (integerDiv x y, integerMod x y)

-- | The least common multiple. It is never negative.
integerLcm :: Integer -> Integer -> Integer
integerLcm x y
  | integerIsZero x = integerZero
  | integerIsZero y = integerZero
  | True = integerAbs (integerMul (integerQuot x (integerGcd x y)) y)

integerCompare :: Integer -> Integer -> Ordering
integerCompare x y
  | integerLt x y = LT
  | integerEq x y = EQ
  | True = GT
