-- | Signed integer types of fixed size.
--
-- This file comes from scripts/gen_base_ints.py. Do not edit it.
module GHC.Int
  ( Int8,
    toInt8,
    fromInt8,
    Int16,
    toInt16,
    fromInt16,
    Int32,
    toInt32,
    fromInt32,
    Int64,
    toInt64,
    fromInt64
  )
where

import GHC.Base
import GHC.Bits
import GHC.Classes
import GHC.Enum
import GHC.Err
import GHC.Maybe
import GHC.Num
import GHC.Num.Integer
import GHC.Prim
import GHC.Real
import GHC.Show
import GHC.Types

data Int8

toInt8 :: Int8 -> Int
toInt8 = primUnsafeCoerce

fromInt8 :: Int -> Int8
fromInt8 x = primUnsafeCoerce (primIntShra (primIntShl x 56) 56)

instance Eq Int8 where
  a == b = primIntEq (toInt8 a) (toInt8 b)
  a /= b = primIntNe (toInt8 a) (toInt8 b)

instance Ord Int8 where
  compare a b = if primIntLt (toInt8 a) (toInt8 b) then LT else if primIntEq (toInt8 a) (toInt8 b) then EQ else GT
  a < b = primIntLt (toInt8 a) (toInt8 b)
  a <= b = primIntLe (toInt8 a) (toInt8 b)
  a > b = primIntGt (toInt8 a) (toInt8 b)
  a >= b = primIntGe (toInt8 a) (toInt8 b)

instance Show Int8 where
  showsPrec d a = showsPrec d (toInt8 a)

instance Num Int8 where
  a + b = fromInt8 (primIntAdd (toInt8 a) (toInt8 b))
  a - b = fromInt8 (primIntSub (toInt8 a) (toInt8 b))
  a * b = fromInt8 (primIntMul (toInt8 a) (toInt8 b))
  negate a = fromInt8 (primIntNegate (toInt8 a))
  abs a = if a < 0 then negate a else a
  signum a = if a == 0 then 0 else if a < 0 then negate 1 else 1
  fromInteger i = fromInt8 (integerToInt i)

instance Bounded Int8 where
  minBound = fromInt8 (primIntShl 1 7)
  maxBound = fromInt8 (primIntSub (primIntShl 1 7) 1)

instance Enum Int8 where
  succ x = x + 1
  pred x = x - 1
  toEnum = fromInt8
  fromEnum = toInt8
  enumFrom x = enumFromTo x maxBound
  enumFromThen x y = enumFromThenTo x y (if y >= x then maxBound else minBound)
  enumFromTo x y = if x > y then [] else go x
    where
      go i = i : (if i == y then [] else go (i + 1))
  enumFromThenTo x y z
    | y >= x = if x > z then [] else up x
    | True = if x < z then [] else down x
    where
      step = y - x
      back = x - y
      up i = i : (if i > z - step then [] else up (i + step))
      down i = i : (if i < z + back then [] else down (i - back))

instance Real Int8

instance Integral Int8 where
  quot a b = if b == 0 then divZeroError else fromInt8 (primIntQuot (toInt8 a) (toInt8 b))
  rem a b = if b == 0 then divZeroError else fromInt8 (primIntRem (toInt8 a) (toInt8 b))
  div a b = if b == 0 then divZeroError else fromInt8 (primIntDiv (toInt8 a) (toInt8 b))
  mod a b = if b == 0 then divZeroError else fromInt8 (primIntMod (toInt8 a) (toInt8 b))
  divMod a b = (div a b, mod a b)
  quotRem a b = (quot a b, rem a b)
  toInteger a = integerFromInt (toInt8 a)

instance Bits Int8 where
  a .&. b = fromInt8 (primIntAnd (toInt8 a) (toInt8 b))
  a .|. b = fromInt8 (primIntOr (toInt8 a) (toInt8 b))
  xor a b = fromInt8 (primIntXor (toInt8 a) (toInt8 b))
  complement a = fromInt8 (primIntXor (toInt8 a) (negate 1))
  zeroBits = 0
  bit i = fromInt8 (primIntShl 1 i)
  testBit a i = primIntAnd (primIntShrl (toInt8 a) i) 1 /= 0
  shiftL a i = fromInt8 (primIntShl (toInt8 a) i)
  unsafeShiftL a i = fromInt8 (primIntShl (toInt8 a) i)
  shiftR a i = fromInt8 (primIntShra (toInt8 a) i)
  unsafeShiftR a i = fromInt8 (primIntShra (toInt8 a) i)
  rotateL a i =
    let r = i `mod` 8
        u = primIntAnd (toInt8 a) 255
     in if r == 0 then a else fromInt8 (primIntOr (primIntShl u r) (primIntShrl u (8 - r)))
  rotateR a i = rotateL a (8 - (i `mod` 8))
  bitSizeMaybe _ = Just 8
  bitSize _ = 8
  isSigned _ = True
  popCount a = primIntPopCount (primIntAnd (toInt8 a) 255)

instance FiniteBits Int8 where
  finiteBitSize _ = 8
  countLeadingZeros a = primIntClz (primIntAnd (toInt8 a) 255) - 56
  countTrailingZeros a = min 8 (primIntCtz (toInt8 a))

data Int16

toInt16 :: Int16 -> Int
toInt16 = primUnsafeCoerce

fromInt16 :: Int -> Int16
fromInt16 x = primUnsafeCoerce (primIntShra (primIntShl x 48) 48)

instance Eq Int16 where
  a == b = primIntEq (toInt16 a) (toInt16 b)
  a /= b = primIntNe (toInt16 a) (toInt16 b)

instance Ord Int16 where
  compare a b = if primIntLt (toInt16 a) (toInt16 b) then LT else if primIntEq (toInt16 a) (toInt16 b) then EQ else GT
  a < b = primIntLt (toInt16 a) (toInt16 b)
  a <= b = primIntLe (toInt16 a) (toInt16 b)
  a > b = primIntGt (toInt16 a) (toInt16 b)
  a >= b = primIntGe (toInt16 a) (toInt16 b)

instance Show Int16 where
  showsPrec d a = showsPrec d (toInt16 a)

instance Num Int16 where
  a + b = fromInt16 (primIntAdd (toInt16 a) (toInt16 b))
  a - b = fromInt16 (primIntSub (toInt16 a) (toInt16 b))
  a * b = fromInt16 (primIntMul (toInt16 a) (toInt16 b))
  negate a = fromInt16 (primIntNegate (toInt16 a))
  abs a = if a < 0 then negate a else a
  signum a = if a == 0 then 0 else if a < 0 then negate 1 else 1
  fromInteger i = fromInt16 (integerToInt i)

instance Bounded Int16 where
  minBound = fromInt16 (primIntShl 1 15)
  maxBound = fromInt16 (primIntSub (primIntShl 1 15) 1)

instance Enum Int16 where
  succ x = x + 1
  pred x = x - 1
  toEnum = fromInt16
  fromEnum = toInt16
  enumFrom x = enumFromTo x maxBound
  enumFromThen x y = enumFromThenTo x y (if y >= x then maxBound else minBound)
  enumFromTo x y = if x > y then [] else go x
    where
      go i = i : (if i == y then [] else go (i + 1))
  enumFromThenTo x y z
    | y >= x = if x > z then [] else up x
    | True = if x < z then [] else down x
    where
      step = y - x
      back = x - y
      up i = i : (if i > z - step then [] else up (i + step))
      down i = i : (if i < z + back then [] else down (i - back))

instance Real Int16

instance Integral Int16 where
  quot a b = if b == 0 then divZeroError else fromInt16 (primIntQuot (toInt16 a) (toInt16 b))
  rem a b = if b == 0 then divZeroError else fromInt16 (primIntRem (toInt16 a) (toInt16 b))
  div a b = if b == 0 then divZeroError else fromInt16 (primIntDiv (toInt16 a) (toInt16 b))
  mod a b = if b == 0 then divZeroError else fromInt16 (primIntMod (toInt16 a) (toInt16 b))
  divMod a b = (div a b, mod a b)
  quotRem a b = (quot a b, rem a b)
  toInteger a = integerFromInt (toInt16 a)

instance Bits Int16 where
  a .&. b = fromInt16 (primIntAnd (toInt16 a) (toInt16 b))
  a .|. b = fromInt16 (primIntOr (toInt16 a) (toInt16 b))
  xor a b = fromInt16 (primIntXor (toInt16 a) (toInt16 b))
  complement a = fromInt16 (primIntXor (toInt16 a) (negate 1))
  zeroBits = 0
  bit i = fromInt16 (primIntShl 1 i)
  testBit a i = primIntAnd (primIntShrl (toInt16 a) i) 1 /= 0
  shiftL a i = fromInt16 (primIntShl (toInt16 a) i)
  unsafeShiftL a i = fromInt16 (primIntShl (toInt16 a) i)
  shiftR a i = fromInt16 (primIntShra (toInt16 a) i)
  unsafeShiftR a i = fromInt16 (primIntShra (toInt16 a) i)
  rotateL a i =
    let r = i `mod` 16
        u = primIntAnd (toInt16 a) 65535
     in if r == 0 then a else fromInt16 (primIntOr (primIntShl u r) (primIntShrl u (16 - r)))
  rotateR a i = rotateL a (16 - (i `mod` 16))
  bitSizeMaybe _ = Just 16
  bitSize _ = 16
  isSigned _ = True
  popCount a = primIntPopCount (primIntAnd (toInt16 a) 65535)

instance FiniteBits Int16 where
  finiteBitSize _ = 16
  countLeadingZeros a = primIntClz (primIntAnd (toInt16 a) 65535) - 48
  countTrailingZeros a = min 16 (primIntCtz (toInt16 a))

data Int32

toInt32 :: Int32 -> Int
toInt32 = primUnsafeCoerce

fromInt32 :: Int -> Int32
fromInt32 x = primUnsafeCoerce (primIntShra (primIntShl x 32) 32)

instance Eq Int32 where
  a == b = primIntEq (toInt32 a) (toInt32 b)
  a /= b = primIntNe (toInt32 a) (toInt32 b)

instance Ord Int32 where
  compare a b = if primIntLt (toInt32 a) (toInt32 b) then LT else if primIntEq (toInt32 a) (toInt32 b) then EQ else GT
  a < b = primIntLt (toInt32 a) (toInt32 b)
  a <= b = primIntLe (toInt32 a) (toInt32 b)
  a > b = primIntGt (toInt32 a) (toInt32 b)
  a >= b = primIntGe (toInt32 a) (toInt32 b)

instance Show Int32 where
  showsPrec d a = showsPrec d (toInt32 a)

instance Num Int32 where
  a + b = fromInt32 (primIntAdd (toInt32 a) (toInt32 b))
  a - b = fromInt32 (primIntSub (toInt32 a) (toInt32 b))
  a * b = fromInt32 (primIntMul (toInt32 a) (toInt32 b))
  negate a = fromInt32 (primIntNegate (toInt32 a))
  abs a = if a < 0 then negate a else a
  signum a = if a == 0 then 0 else if a < 0 then negate 1 else 1
  fromInteger i = fromInt32 (integerToInt i)

instance Bounded Int32 where
  minBound = fromInt32 (primIntShl 1 31)
  maxBound = fromInt32 (primIntSub (primIntShl 1 31) 1)

instance Enum Int32 where
  succ x = x + 1
  pred x = x - 1
  toEnum = fromInt32
  fromEnum = toInt32
  enumFrom x = enumFromTo x maxBound
  enumFromThen x y = enumFromThenTo x y (if y >= x then maxBound else minBound)
  enumFromTo x y = if x > y then [] else go x
    where
      go i = i : (if i == y then [] else go (i + 1))
  enumFromThenTo x y z
    | y >= x = if x > z then [] else up x
    | True = if x < z then [] else down x
    where
      step = y - x
      back = x - y
      up i = i : (if i > z - step then [] else up (i + step))
      down i = i : (if i < z + back then [] else down (i - back))

instance Real Int32

instance Integral Int32 where
  quot a b = if b == 0 then divZeroError else fromInt32 (primIntQuot (toInt32 a) (toInt32 b))
  rem a b = if b == 0 then divZeroError else fromInt32 (primIntRem (toInt32 a) (toInt32 b))
  div a b = if b == 0 then divZeroError else fromInt32 (primIntDiv (toInt32 a) (toInt32 b))
  mod a b = if b == 0 then divZeroError else fromInt32 (primIntMod (toInt32 a) (toInt32 b))
  divMod a b = (div a b, mod a b)
  quotRem a b = (quot a b, rem a b)
  toInteger a = integerFromInt (toInt32 a)

instance Bits Int32 where
  a .&. b = fromInt32 (primIntAnd (toInt32 a) (toInt32 b))
  a .|. b = fromInt32 (primIntOr (toInt32 a) (toInt32 b))
  xor a b = fromInt32 (primIntXor (toInt32 a) (toInt32 b))
  complement a = fromInt32 (primIntXor (toInt32 a) (negate 1))
  zeroBits = 0
  bit i = fromInt32 (primIntShl 1 i)
  testBit a i = primIntAnd (primIntShrl (toInt32 a) i) 1 /= 0
  shiftL a i = fromInt32 (primIntShl (toInt32 a) i)
  unsafeShiftL a i = fromInt32 (primIntShl (toInt32 a) i)
  shiftR a i = fromInt32 (primIntShra (toInt32 a) i)
  unsafeShiftR a i = fromInt32 (primIntShra (toInt32 a) i)
  rotateL a i =
    let r = i `mod` 32
        u = primIntAnd (toInt32 a) 4294967295
     in if r == 0 then a else fromInt32 (primIntOr (primIntShl u r) (primIntShrl u (32 - r)))
  rotateR a i = rotateL a (32 - (i `mod` 32))
  bitSizeMaybe _ = Just 32
  bitSize _ = 32
  isSigned _ = True
  popCount a = primIntPopCount (primIntAnd (toInt32 a) 4294967295)

instance FiniteBits Int32 where
  finiteBitSize _ = 32
  countLeadingZeros a = primIntClz (primIntAnd (toInt32 a) 4294967295) - 32
  countTrailingZeros a = min 32 (primIntCtz (toInt32 a))

data Int64

toInt64 :: Int64 -> Int
toInt64 = primUnsafeCoerce

fromInt64 :: Int -> Int64
fromInt64 x = primUnsafeCoerce (x)

instance Eq Int64 where
  a == b = primIntEq (toInt64 a) (toInt64 b)
  a /= b = primIntNe (toInt64 a) (toInt64 b)

instance Ord Int64 where
  compare a b = if primIntLt (toInt64 a) (toInt64 b) then LT else if primIntEq (toInt64 a) (toInt64 b) then EQ else GT
  a < b = primIntLt (toInt64 a) (toInt64 b)
  a <= b = primIntLe (toInt64 a) (toInt64 b)
  a > b = primIntGt (toInt64 a) (toInt64 b)
  a >= b = primIntGe (toInt64 a) (toInt64 b)

instance Show Int64 where
  showsPrec d a = showsPrec d (toInt64 a)

instance Num Int64 where
  a + b = fromInt64 (primIntAdd (toInt64 a) (toInt64 b))
  a - b = fromInt64 (primIntSub (toInt64 a) (toInt64 b))
  a * b = fromInt64 (primIntMul (toInt64 a) (toInt64 b))
  negate a = fromInt64 (primIntNegate (toInt64 a))
  abs a = if a < 0 then negate a else a
  signum a = if a == 0 then 0 else if a < 0 then negate 1 else 1
  fromInteger i = fromInt64 (integerToInt i)

instance Bounded Int64 where
  minBound = fromInt64 (primIntShl 1 63)
  maxBound = fromInt64 (primIntSub (primIntShl 1 63) 1)

instance Enum Int64 where
  succ x = x + 1
  pred x = x - 1
  toEnum = fromInt64
  fromEnum = toInt64
  enumFrom x = enumFromTo x maxBound
  enumFromThen x y = enumFromThenTo x y (if y >= x then maxBound else minBound)
  enumFromTo x y = if x > y then [] else go x
    where
      go i = i : (if i == y then [] else go (i + 1))
  enumFromThenTo x y z
    | y >= x = if x > z then [] else up x
    | True = if x < z then [] else down x
    where
      step = y - x
      back = x - y
      up i = i : (if i > z - step then [] else up (i + step))
      down i = i : (if i < z + back then [] else down (i - back))

instance Real Int64

instance Integral Int64 where
  quot a b = if b == 0 then divZeroError else fromInt64 (primIntQuot (toInt64 a) (toInt64 b))
  rem a b = if b == 0 then divZeroError else fromInt64 (primIntRem (toInt64 a) (toInt64 b))
  div a b = if b == 0 then divZeroError else fromInt64 (primIntDiv (toInt64 a) (toInt64 b))
  mod a b = if b == 0 then divZeroError else fromInt64 (primIntMod (toInt64 a) (toInt64 b))
  divMod a b = (div a b, mod a b)
  quotRem a b = (quot a b, rem a b)
  toInteger a = integerFromInt (toInt64 a)

instance Bits Int64 where
  a .&. b = fromInt64 (primIntAnd (toInt64 a) (toInt64 b))
  a .|. b = fromInt64 (primIntOr (toInt64 a) (toInt64 b))
  xor a b = fromInt64 (primIntXor (toInt64 a) (toInt64 b))
  complement a = fromInt64 (primIntXor (toInt64 a) (negate 1))
  zeroBits = 0
  bit i = fromInt64 (primIntShl 1 i)
  testBit a i = primIntAnd (primIntShrl (toInt64 a) i) 1 /= 0
  shiftL a i = fromInt64 (primIntShl (toInt64 a) i)
  unsafeShiftL a i = fromInt64 (primIntShl (toInt64 a) i)
  shiftR a i = fromInt64 (primIntShra (toInt64 a) i)
  unsafeShiftR a i = fromInt64 (primIntShra (toInt64 a) i)
  rotateL a i =
    let r = i `mod` 64
        u = toInt64 a
     in if r == 0 then a else fromInt64 (primIntOr (primIntShl u r) (primIntShrl u (64 - r)))
  rotateR a i = rotateL a (64 - (i `mod` 64))
  bitSizeMaybe _ = Just 64
  bitSize _ = 64
  isSigned _ = True
  popCount a = primIntPopCount (toInt64 a)

instance FiniteBits Int64 where
  finiteBitSize _ = 64
  countLeadingZeros a = primIntClz (toInt64 a) - 0
  countTrailingZeros a = min 64 (primIntCtz (toInt64 a))
