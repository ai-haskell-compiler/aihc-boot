-- | Unsigned integer types of fixed size.
--
-- This file comes from scripts/gen_base_ints.py. Do not edit it.
module GHC.Word
  ( Word8,
    toWord8,
    fromWord8,
    Word16,
    toWord16,
    fromWord16,
    Word32,
    toWord32,
    fromWord32,
    Word64,
    toWord64,
    fromWord64,
    Word,
    toWord,
    fromWord
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

data Word8

toWord8 :: Word8 -> Int
toWord8 = primUnsafeCoerce

fromWord8 :: Int -> Word8
fromWord8 x = primUnsafeCoerce (primIntAnd x 255)

instance Eq Word8 where
  a == b = primIntEq (toWord8 a) (toWord8 b)
  a /= b = primIntNe (toWord8 a) (toWord8 b)

instance Ord Word8 where
  compare a b = if primIntLt (toWord8 a) (toWord8 b) then LT else if primIntEq (toWord8 a) (toWord8 b) then EQ else GT
  a < b = primIntLt (toWord8 a) (toWord8 b)
  a <= b = primIntLe (toWord8 a) (toWord8 b)
  a > b = primIntGt (toWord8 a) (toWord8 b)
  a >= b = primIntGe (toWord8 a) (toWord8 b)

instance Show Word8 where
  showsPrec d a = showsPrec d (toWord8 a)

instance Num Word8 where
  a + b = fromWord8 (primIntAdd (toWord8 a) (toWord8 b))
  a - b = fromWord8 (primIntSub (toWord8 a) (toWord8 b))
  a * b = fromWord8 (primIntMul (toWord8 a) (toWord8 b))
  negate a = fromWord8 (primIntNegate (toWord8 a))
  abs a = a
  signum a = if a == 0 then 0 else 1
  fromInteger i = fromWord8 (integerToInt i)

instance Bounded Word8 where
  minBound = 0
  maxBound = fromWord8 (negate 1)

instance Enum Word8 where
  succ x = x + 1
  pred x = x - 1
  toEnum = fromWord8
  fromEnum = toWord8
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

instance Real Word8

instance Integral Word8 where
  quot a b = if b == 0 then divZeroError else fromWord8 (primIntQuot (toWord8 a) (toWord8 b))
  rem a b = if b == 0 then divZeroError else fromWord8 (primIntRem (toWord8 a) (toWord8 b))
  div = quot
  mod = rem
  divMod a b = (quot a b, rem a b)
  quotRem a b = (quot a b, rem a b)
  toInteger a = integerFromInt (toWord8 a)

instance Bits Word8 where
  a .&. b = fromWord8 (primIntAnd (toWord8 a) (toWord8 b))
  a .|. b = fromWord8 (primIntOr (toWord8 a) (toWord8 b))
  xor a b = fromWord8 (primIntXor (toWord8 a) (toWord8 b))
  complement a = fromWord8 (primIntXor (toWord8 a) (negate 1))
  zeroBits = 0
  bit i = fromWord8 (primIntShl 1 i)
  testBit a i = primIntAnd (primIntShrl (toWord8 a) i) 1 /= 0
  shiftL a i = fromWord8 (primIntShl (toWord8 a) i)
  unsafeShiftL a i = fromWord8 (primIntShl (toWord8 a) i)
  shiftR a i = fromWord8 (primIntShrl (toWord8 a) i)
  unsafeShiftR a i = fromWord8 (primIntShrl (toWord8 a) i)
  rotateL a i =
    let r = i `mod` 8
        u = toWord8 a
     in if r == 0 then a else fromWord8 (primIntOr (primIntShl u r) (primIntShrl u (8 - r)))
  rotateR a i = rotateL a (8 - (i `mod` 8))
  bitSizeMaybe _ = Just 8
  bitSize _ = 8
  isSigned _ = False
  popCount a = primIntPopCount (toWord8 a)

instance FiniteBits Word8 where
  finiteBitSize _ = 8
  countLeadingZeros a = primIntClz (toWord8 a) - 56
  countTrailingZeros a = min 8 (primIntCtz (toWord8 a))

data Word16

toWord16 :: Word16 -> Int
toWord16 = primUnsafeCoerce

fromWord16 :: Int -> Word16
fromWord16 x = primUnsafeCoerce (primIntAnd x 65535)

instance Eq Word16 where
  a == b = primIntEq (toWord16 a) (toWord16 b)
  a /= b = primIntNe (toWord16 a) (toWord16 b)

instance Ord Word16 where
  compare a b = if primIntLt (toWord16 a) (toWord16 b) then LT else if primIntEq (toWord16 a) (toWord16 b) then EQ else GT
  a < b = primIntLt (toWord16 a) (toWord16 b)
  a <= b = primIntLe (toWord16 a) (toWord16 b)
  a > b = primIntGt (toWord16 a) (toWord16 b)
  a >= b = primIntGe (toWord16 a) (toWord16 b)

instance Show Word16 where
  showsPrec d a = showsPrec d (toWord16 a)

instance Num Word16 where
  a + b = fromWord16 (primIntAdd (toWord16 a) (toWord16 b))
  a - b = fromWord16 (primIntSub (toWord16 a) (toWord16 b))
  a * b = fromWord16 (primIntMul (toWord16 a) (toWord16 b))
  negate a = fromWord16 (primIntNegate (toWord16 a))
  abs a = a
  signum a = if a == 0 then 0 else 1
  fromInteger i = fromWord16 (integerToInt i)

instance Bounded Word16 where
  minBound = 0
  maxBound = fromWord16 (negate 1)

instance Enum Word16 where
  succ x = x + 1
  pred x = x - 1
  toEnum = fromWord16
  fromEnum = toWord16
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

instance Real Word16

instance Integral Word16 where
  quot a b = if b == 0 then divZeroError else fromWord16 (primIntQuot (toWord16 a) (toWord16 b))
  rem a b = if b == 0 then divZeroError else fromWord16 (primIntRem (toWord16 a) (toWord16 b))
  div = quot
  mod = rem
  divMod a b = (quot a b, rem a b)
  quotRem a b = (quot a b, rem a b)
  toInteger a = integerFromInt (toWord16 a)

instance Bits Word16 where
  a .&. b = fromWord16 (primIntAnd (toWord16 a) (toWord16 b))
  a .|. b = fromWord16 (primIntOr (toWord16 a) (toWord16 b))
  xor a b = fromWord16 (primIntXor (toWord16 a) (toWord16 b))
  complement a = fromWord16 (primIntXor (toWord16 a) (negate 1))
  zeroBits = 0
  bit i = fromWord16 (primIntShl 1 i)
  testBit a i = primIntAnd (primIntShrl (toWord16 a) i) 1 /= 0
  shiftL a i = fromWord16 (primIntShl (toWord16 a) i)
  unsafeShiftL a i = fromWord16 (primIntShl (toWord16 a) i)
  shiftR a i = fromWord16 (primIntShrl (toWord16 a) i)
  unsafeShiftR a i = fromWord16 (primIntShrl (toWord16 a) i)
  rotateL a i =
    let r = i `mod` 16
        u = toWord16 a
     in if r == 0 then a else fromWord16 (primIntOr (primIntShl u r) (primIntShrl u (16 - r)))
  rotateR a i = rotateL a (16 - (i `mod` 16))
  bitSizeMaybe _ = Just 16
  bitSize _ = 16
  isSigned _ = False
  popCount a = primIntPopCount (toWord16 a)

instance FiniteBits Word16 where
  finiteBitSize _ = 16
  countLeadingZeros a = primIntClz (toWord16 a) - 48
  countTrailingZeros a = min 16 (primIntCtz (toWord16 a))

data Word32

toWord32 :: Word32 -> Int
toWord32 = primUnsafeCoerce

fromWord32 :: Int -> Word32
fromWord32 x = primUnsafeCoerce (primIntAnd x 4294967295)

instance Eq Word32 where
  a == b = primIntEq (toWord32 a) (toWord32 b)
  a /= b = primIntNe (toWord32 a) (toWord32 b)

instance Ord Word32 where
  compare a b = if primIntLt (toWord32 a) (toWord32 b) then LT else if primIntEq (toWord32 a) (toWord32 b) then EQ else GT
  a < b = primIntLt (toWord32 a) (toWord32 b)
  a <= b = primIntLe (toWord32 a) (toWord32 b)
  a > b = primIntGt (toWord32 a) (toWord32 b)
  a >= b = primIntGe (toWord32 a) (toWord32 b)

instance Show Word32 where
  showsPrec d a = showsPrec d (toWord32 a)

instance Num Word32 where
  a + b = fromWord32 (primIntAdd (toWord32 a) (toWord32 b))
  a - b = fromWord32 (primIntSub (toWord32 a) (toWord32 b))
  a * b = fromWord32 (primIntMul (toWord32 a) (toWord32 b))
  negate a = fromWord32 (primIntNegate (toWord32 a))
  abs a = a
  signum a = if a == 0 then 0 else 1
  fromInteger i = fromWord32 (integerToInt i)

instance Bounded Word32 where
  minBound = 0
  maxBound = fromWord32 (negate 1)

instance Enum Word32 where
  succ x = x + 1
  pred x = x - 1
  toEnum = fromWord32
  fromEnum = toWord32
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

instance Real Word32

instance Integral Word32 where
  quot a b = if b == 0 then divZeroError else fromWord32 (primIntQuot (toWord32 a) (toWord32 b))
  rem a b = if b == 0 then divZeroError else fromWord32 (primIntRem (toWord32 a) (toWord32 b))
  div = quot
  mod = rem
  divMod a b = (quot a b, rem a b)
  quotRem a b = (quot a b, rem a b)
  toInteger a = integerFromInt (toWord32 a)

instance Bits Word32 where
  a .&. b = fromWord32 (primIntAnd (toWord32 a) (toWord32 b))
  a .|. b = fromWord32 (primIntOr (toWord32 a) (toWord32 b))
  xor a b = fromWord32 (primIntXor (toWord32 a) (toWord32 b))
  complement a = fromWord32 (primIntXor (toWord32 a) (negate 1))
  zeroBits = 0
  bit i = fromWord32 (primIntShl 1 i)
  testBit a i = primIntAnd (primIntShrl (toWord32 a) i) 1 /= 0
  shiftL a i = fromWord32 (primIntShl (toWord32 a) i)
  unsafeShiftL a i = fromWord32 (primIntShl (toWord32 a) i)
  shiftR a i = fromWord32 (primIntShrl (toWord32 a) i)
  unsafeShiftR a i = fromWord32 (primIntShrl (toWord32 a) i)
  rotateL a i =
    let r = i `mod` 32
        u = toWord32 a
     in if r == 0 then a else fromWord32 (primIntOr (primIntShl u r) (primIntShrl u (32 - r)))
  rotateR a i = rotateL a (32 - (i `mod` 32))
  bitSizeMaybe _ = Just 32
  bitSize _ = 32
  isSigned _ = False
  popCount a = primIntPopCount (toWord32 a)

instance FiniteBits Word32 where
  finiteBitSize _ = 32
  countLeadingZeros a = primIntClz (toWord32 a) - 32
  countTrailingZeros a = min 32 (primIntCtz (toWord32 a))

data Word64

toWord64 :: Word64 -> Int
toWord64 = primUnsafeCoerce

fromWord64 :: Int -> Word64
fromWord64 x = primUnsafeCoerce (x)

instance Eq Word64 where
  a == b = primIntEq (toWord64 a) (toWord64 b)
  a /= b = primIntNe (toWord64 a) (toWord64 b)

instance Ord Word64 where
  compare a b = if primWordLt (toWord64 a) (toWord64 b) then LT else if primIntEq (toWord64 a) (toWord64 b) then EQ else GT
  a < b = primWordLt (toWord64 a) (toWord64 b)
  a <= b = primWordLe (toWord64 a) (toWord64 b)
  a > b = primWordGt (toWord64 a) (toWord64 b)
  a >= b = primWordGe (toWord64 a) (toWord64 b)

instance Show Word64 where
  showsPrec _ a = showString (primWordShow (toWord64 a))

instance Num Word64 where
  a + b = fromWord64 (primIntAdd (toWord64 a) (toWord64 b))
  a - b = fromWord64 (primIntSub (toWord64 a) (toWord64 b))
  a * b = fromWord64 (primIntMul (toWord64 a) (toWord64 b))
  negate a = fromWord64 (primIntNegate (toWord64 a))
  abs a = a
  signum a = if a == 0 then 0 else 1
  fromInteger i = fromWord64 (integerToInt i)

instance Bounded Word64 where
  minBound = 0
  maxBound = fromWord64 (negate 1)

instance Enum Word64 where
  succ x = x + 1
  pred x = x - 1
  toEnum = fromWord64
  fromEnum = toWord64
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

instance Real Word64

instance Integral Word64 where
  quot a b = if b == 0 then divZeroError else fromWord64 (primWordQuot (toWord64 a) (toWord64 b))
  rem a b = if b == 0 then divZeroError else fromWord64 (primWordRem (toWord64 a) (toWord64 b))
  div = quot
  mod = rem
  divMod a b = (quot a b, rem a b)
  quotRem a b = (quot a b, rem a b)
  toInteger a = if primIntGe (toWord64 a) 0 then integerFromInt (toWord64 a) else integerAdd (integerFromInt (toWord64 a)) (integerMul (integerFromInt 4294967296) (integerFromInt 4294967296))

instance Bits Word64 where
  a .&. b = fromWord64 (primIntAnd (toWord64 a) (toWord64 b))
  a .|. b = fromWord64 (primIntOr (toWord64 a) (toWord64 b))
  xor a b = fromWord64 (primIntXor (toWord64 a) (toWord64 b))
  complement a = fromWord64 (primIntXor (toWord64 a) (negate 1))
  zeroBits = 0
  bit i = fromWord64 (primIntShl 1 i)
  testBit a i = primIntAnd (primIntShrl (toWord64 a) i) 1 /= 0
  shiftL a i = fromWord64 (primIntShl (toWord64 a) i)
  unsafeShiftL a i = fromWord64 (primIntShl (toWord64 a) i)
  shiftR a i = fromWord64 (primIntShrl (toWord64 a) i)
  unsafeShiftR a i = fromWord64 (primIntShrl (toWord64 a) i)
  rotateL a i =
    let r = i `mod` 64
        u = toWord64 a
     in if r == 0 then a else fromWord64 (primIntOr (primIntShl u r) (primIntShrl u (64 - r)))
  rotateR a i = rotateL a (64 - (i `mod` 64))
  bitSizeMaybe _ = Just 64
  bitSize _ = 64
  isSigned _ = False
  popCount a = primIntPopCount (toWord64 a)

instance FiniteBits Word64 where
  finiteBitSize _ = 64
  countLeadingZeros a = primIntClz (toWord64 a) - 0
  countTrailingZeros a = min 64 (primIntCtz (toWord64 a))

-- Word is in GHC.Types.

toWord :: Word -> Int
toWord = primUnsafeCoerce

fromWord :: Int -> Word
fromWord x = primUnsafeCoerce (x)

instance Eq Word where
  a == b = primIntEq (toWord a) (toWord b)
  a /= b = primIntNe (toWord a) (toWord b)

instance Ord Word where
  compare a b = if primWordLt (toWord a) (toWord b) then LT else if primIntEq (toWord a) (toWord b) then EQ else GT
  a < b = primWordLt (toWord a) (toWord b)
  a <= b = primWordLe (toWord a) (toWord b)
  a > b = primWordGt (toWord a) (toWord b)
  a >= b = primWordGe (toWord a) (toWord b)

instance Show Word where
  showsPrec _ a = showString (primWordShow (toWord a))

instance Num Word where
  a + b = fromWord (primIntAdd (toWord a) (toWord b))
  a - b = fromWord (primIntSub (toWord a) (toWord b))
  a * b = fromWord (primIntMul (toWord a) (toWord b))
  negate a = fromWord (primIntNegate (toWord a))
  abs a = a
  signum a = if a == 0 then 0 else 1
  fromInteger i = fromWord (integerToInt i)

instance Bounded Word where
  minBound = 0
  maxBound = fromWord (negate 1)

instance Enum Word where
  succ x = x + 1
  pred x = x - 1
  toEnum = fromWord
  fromEnum = toWord
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

instance Real Word

instance Integral Word where
  quot a b = if b == 0 then divZeroError else fromWord (primWordQuot (toWord a) (toWord b))
  rem a b = if b == 0 then divZeroError else fromWord (primWordRem (toWord a) (toWord b))
  div = quot
  mod = rem
  divMod a b = (quot a b, rem a b)
  quotRem a b = (quot a b, rem a b)
  toInteger a = if primIntGe (toWord a) 0 then integerFromInt (toWord a) else integerAdd (integerFromInt (toWord a)) (integerMul (integerFromInt 4294967296) (integerFromInt 4294967296))

instance Bits Word where
  a .&. b = fromWord (primIntAnd (toWord a) (toWord b))
  a .|. b = fromWord (primIntOr (toWord a) (toWord b))
  xor a b = fromWord (primIntXor (toWord a) (toWord b))
  complement a = fromWord (primIntXor (toWord a) (negate 1))
  zeroBits = 0
  bit i = fromWord (primIntShl 1 i)
  testBit a i = primIntAnd (primIntShrl (toWord a) i) 1 /= 0
  shiftL a i = fromWord (primIntShl (toWord a) i)
  unsafeShiftL a i = fromWord (primIntShl (toWord a) i)
  shiftR a i = fromWord (primIntShrl (toWord a) i)
  unsafeShiftR a i = fromWord (primIntShrl (toWord a) i)
  rotateL a i =
    let r = i `mod` 64
        u = toWord a
     in if r == 0 then a else fromWord (primIntOr (primIntShl u r) (primIntShrl u (64 - r)))
  rotateR a i = rotateL a (64 - (i `mod` 64))
  bitSizeMaybe _ = Just 64
  bitSize _ = 64
  isSigned _ = False
  popCount a = primIntPopCount (toWord a)

instance FiniteBits Word where
  finiteBitSize _ = 64
  countLeadingZeros a = primIntClz (toWord a) - 0
  countTrailingZeros a = min 64 (primIntCtz (toWord a))
