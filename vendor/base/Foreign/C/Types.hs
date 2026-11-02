-- | The C integer types, as newtypes of the fixed-size types.
--
-- This file comes from scripts/gen_base_ints.py. Do not edit it.
module Foreign.C.Types
  ( CChar (..),
    CSChar (..),
    CUChar (..),
    CShort (..),
    CUShort (..),
    CInt (..),
    CUInt (..),
    CLong (..),
    CULong (..),
    CLLong (..),
    CULLong (..),
    CSize (..)
  )
where

import Foreign.Storable
import GHC.Base
import GHC.Bits
import GHC.Classes
import GHC.Enum
import GHC.Int
import GHC.Num
import GHC.Real
import GHC.Show
import GHC.Types
import GHC.Word

newtype CChar = CChar Int8

unCChar :: CChar -> Int8
unCChar (CChar x) = x

instance Eq CChar where
  a == b = unCChar a == unCChar b

instance Ord CChar where
  compare a b = compare (unCChar a) (unCChar b)

instance Show CChar where
  showsPrec d a = showsPrec d (unCChar a)

instance Num CChar where
  a + b = CChar (unCChar a + unCChar b)
  a - b = CChar (unCChar a - unCChar b)
  a * b = CChar (unCChar a * unCChar b)
  negate a = CChar (negate (unCChar a))
  abs a = CChar (abs (unCChar a))
  signum a = CChar (signum (unCChar a))
  fromInteger i = CChar (fromInteger i)

instance Bounded CChar where
  minBound = CChar minBound
  maxBound = CChar maxBound

instance Enum CChar where
  toEnum i = CChar (toEnum i)
  fromEnum a = fromEnum (unCChar a)
  enumFromTo a b = map CChar (enumFromTo (unCChar a) (unCChar b))
  enumFromThenTo a b c = map CChar (enumFromThenTo (unCChar a) (unCChar b) (unCChar c))

instance Real CChar

instance Integral CChar where
  quot a b = CChar (quot (unCChar a) (unCChar b))
  rem a b = CChar (rem (unCChar a) (unCChar b))
  div a b = CChar (div (unCChar a) (unCChar b))
  mod a b = CChar (mod (unCChar a) (unCChar b))
  quotRem a b = (quot a b, rem a b)
  divMod a b = (div a b, mod a b)
  toInteger a = toInteger (unCChar a)

instance Bits CChar where
  a .&. b = CChar (unCChar a .&. unCChar b)
  a .|. b = CChar (unCChar a .|. unCChar b)
  xor a b = CChar (xor (unCChar a) (unCChar b))
  complement a = CChar (complement (unCChar a))
  zeroBits = CChar zeroBits
  bit i = CChar (bit i)
  testBit a i = testBit (unCChar a) i
  shiftL a i = CChar (shiftL (unCChar a) i)
  shiftR a i = CChar (shiftR (unCChar a) i)
  rotateL a i = CChar (rotateL (unCChar a) i)
  rotateR a i = CChar (rotateR (unCChar a) i)
  bitSizeMaybe a = bitSizeMaybe (unCChar a)
  bitSize a = bitSize (unCChar a)
  isSigned a = isSigned (unCChar a)
  popCount a = popCount (unCChar a)

instance FiniteBits CChar where
  finiteBitSize a = finiteBitSize (unCChar a)

instance Storable CChar where
  sizeOf a = sizeOf (unCChar a)
  alignment a = alignment (unCChar a)
  peekByteOff p off = fmap CChar (peekByteOff p off)
  pokeByteOff p off a = pokeByteOff p off (unCChar a)

newtype CSChar = CSChar Int8

unCSChar :: CSChar -> Int8
unCSChar (CSChar x) = x

instance Eq CSChar where
  a == b = unCSChar a == unCSChar b

instance Ord CSChar where
  compare a b = compare (unCSChar a) (unCSChar b)

instance Show CSChar where
  showsPrec d a = showsPrec d (unCSChar a)

instance Num CSChar where
  a + b = CSChar (unCSChar a + unCSChar b)
  a - b = CSChar (unCSChar a - unCSChar b)
  a * b = CSChar (unCSChar a * unCSChar b)
  negate a = CSChar (negate (unCSChar a))
  abs a = CSChar (abs (unCSChar a))
  signum a = CSChar (signum (unCSChar a))
  fromInteger i = CSChar (fromInteger i)

instance Bounded CSChar where
  minBound = CSChar minBound
  maxBound = CSChar maxBound

instance Enum CSChar where
  toEnum i = CSChar (toEnum i)
  fromEnum a = fromEnum (unCSChar a)
  enumFromTo a b = map CSChar (enumFromTo (unCSChar a) (unCSChar b))
  enumFromThenTo a b c = map CSChar (enumFromThenTo (unCSChar a) (unCSChar b) (unCSChar c))

instance Real CSChar

instance Integral CSChar where
  quot a b = CSChar (quot (unCSChar a) (unCSChar b))
  rem a b = CSChar (rem (unCSChar a) (unCSChar b))
  div a b = CSChar (div (unCSChar a) (unCSChar b))
  mod a b = CSChar (mod (unCSChar a) (unCSChar b))
  quotRem a b = (quot a b, rem a b)
  divMod a b = (div a b, mod a b)
  toInteger a = toInteger (unCSChar a)

instance Bits CSChar where
  a .&. b = CSChar (unCSChar a .&. unCSChar b)
  a .|. b = CSChar (unCSChar a .|. unCSChar b)
  xor a b = CSChar (xor (unCSChar a) (unCSChar b))
  complement a = CSChar (complement (unCSChar a))
  zeroBits = CSChar zeroBits
  bit i = CSChar (bit i)
  testBit a i = testBit (unCSChar a) i
  shiftL a i = CSChar (shiftL (unCSChar a) i)
  shiftR a i = CSChar (shiftR (unCSChar a) i)
  rotateL a i = CSChar (rotateL (unCSChar a) i)
  rotateR a i = CSChar (rotateR (unCSChar a) i)
  bitSizeMaybe a = bitSizeMaybe (unCSChar a)
  bitSize a = bitSize (unCSChar a)
  isSigned a = isSigned (unCSChar a)
  popCount a = popCount (unCSChar a)

instance FiniteBits CSChar where
  finiteBitSize a = finiteBitSize (unCSChar a)

instance Storable CSChar where
  sizeOf a = sizeOf (unCSChar a)
  alignment a = alignment (unCSChar a)
  peekByteOff p off = fmap CSChar (peekByteOff p off)
  pokeByteOff p off a = pokeByteOff p off (unCSChar a)

newtype CUChar = CUChar Word8

unCUChar :: CUChar -> Word8
unCUChar (CUChar x) = x

instance Eq CUChar where
  a == b = unCUChar a == unCUChar b

instance Ord CUChar where
  compare a b = compare (unCUChar a) (unCUChar b)

instance Show CUChar where
  showsPrec d a = showsPrec d (unCUChar a)

instance Num CUChar where
  a + b = CUChar (unCUChar a + unCUChar b)
  a - b = CUChar (unCUChar a - unCUChar b)
  a * b = CUChar (unCUChar a * unCUChar b)
  negate a = CUChar (negate (unCUChar a))
  abs a = CUChar (abs (unCUChar a))
  signum a = CUChar (signum (unCUChar a))
  fromInteger i = CUChar (fromInteger i)

instance Bounded CUChar where
  minBound = CUChar minBound
  maxBound = CUChar maxBound

instance Enum CUChar where
  toEnum i = CUChar (toEnum i)
  fromEnum a = fromEnum (unCUChar a)
  enumFromTo a b = map CUChar (enumFromTo (unCUChar a) (unCUChar b))
  enumFromThenTo a b c = map CUChar (enumFromThenTo (unCUChar a) (unCUChar b) (unCUChar c))

instance Real CUChar

instance Integral CUChar where
  quot a b = CUChar (quot (unCUChar a) (unCUChar b))
  rem a b = CUChar (rem (unCUChar a) (unCUChar b))
  div a b = CUChar (div (unCUChar a) (unCUChar b))
  mod a b = CUChar (mod (unCUChar a) (unCUChar b))
  quotRem a b = (quot a b, rem a b)
  divMod a b = (div a b, mod a b)
  toInteger a = toInteger (unCUChar a)

instance Bits CUChar where
  a .&. b = CUChar (unCUChar a .&. unCUChar b)
  a .|. b = CUChar (unCUChar a .|. unCUChar b)
  xor a b = CUChar (xor (unCUChar a) (unCUChar b))
  complement a = CUChar (complement (unCUChar a))
  zeroBits = CUChar zeroBits
  bit i = CUChar (bit i)
  testBit a i = testBit (unCUChar a) i
  shiftL a i = CUChar (shiftL (unCUChar a) i)
  shiftR a i = CUChar (shiftR (unCUChar a) i)
  rotateL a i = CUChar (rotateL (unCUChar a) i)
  rotateR a i = CUChar (rotateR (unCUChar a) i)
  bitSizeMaybe a = bitSizeMaybe (unCUChar a)
  bitSize a = bitSize (unCUChar a)
  isSigned a = isSigned (unCUChar a)
  popCount a = popCount (unCUChar a)

instance FiniteBits CUChar where
  finiteBitSize a = finiteBitSize (unCUChar a)

instance Storable CUChar where
  sizeOf a = sizeOf (unCUChar a)
  alignment a = alignment (unCUChar a)
  peekByteOff p off = fmap CUChar (peekByteOff p off)
  pokeByteOff p off a = pokeByteOff p off (unCUChar a)

newtype CShort = CShort Int16

unCShort :: CShort -> Int16
unCShort (CShort x) = x

instance Eq CShort where
  a == b = unCShort a == unCShort b

instance Ord CShort where
  compare a b = compare (unCShort a) (unCShort b)

instance Show CShort where
  showsPrec d a = showsPrec d (unCShort a)

instance Num CShort where
  a + b = CShort (unCShort a + unCShort b)
  a - b = CShort (unCShort a - unCShort b)
  a * b = CShort (unCShort a * unCShort b)
  negate a = CShort (negate (unCShort a))
  abs a = CShort (abs (unCShort a))
  signum a = CShort (signum (unCShort a))
  fromInteger i = CShort (fromInteger i)

instance Bounded CShort where
  minBound = CShort minBound
  maxBound = CShort maxBound

instance Enum CShort where
  toEnum i = CShort (toEnum i)
  fromEnum a = fromEnum (unCShort a)
  enumFromTo a b = map CShort (enumFromTo (unCShort a) (unCShort b))
  enumFromThenTo a b c = map CShort (enumFromThenTo (unCShort a) (unCShort b) (unCShort c))

instance Real CShort

instance Integral CShort where
  quot a b = CShort (quot (unCShort a) (unCShort b))
  rem a b = CShort (rem (unCShort a) (unCShort b))
  div a b = CShort (div (unCShort a) (unCShort b))
  mod a b = CShort (mod (unCShort a) (unCShort b))
  quotRem a b = (quot a b, rem a b)
  divMod a b = (div a b, mod a b)
  toInteger a = toInteger (unCShort a)

instance Bits CShort where
  a .&. b = CShort (unCShort a .&. unCShort b)
  a .|. b = CShort (unCShort a .|. unCShort b)
  xor a b = CShort (xor (unCShort a) (unCShort b))
  complement a = CShort (complement (unCShort a))
  zeroBits = CShort zeroBits
  bit i = CShort (bit i)
  testBit a i = testBit (unCShort a) i
  shiftL a i = CShort (shiftL (unCShort a) i)
  shiftR a i = CShort (shiftR (unCShort a) i)
  rotateL a i = CShort (rotateL (unCShort a) i)
  rotateR a i = CShort (rotateR (unCShort a) i)
  bitSizeMaybe a = bitSizeMaybe (unCShort a)
  bitSize a = bitSize (unCShort a)
  isSigned a = isSigned (unCShort a)
  popCount a = popCount (unCShort a)

instance FiniteBits CShort where
  finiteBitSize a = finiteBitSize (unCShort a)

instance Storable CShort where
  sizeOf a = sizeOf (unCShort a)
  alignment a = alignment (unCShort a)
  peekByteOff p off = fmap CShort (peekByteOff p off)
  pokeByteOff p off a = pokeByteOff p off (unCShort a)

newtype CUShort = CUShort Word16

unCUShort :: CUShort -> Word16
unCUShort (CUShort x) = x

instance Eq CUShort where
  a == b = unCUShort a == unCUShort b

instance Ord CUShort where
  compare a b = compare (unCUShort a) (unCUShort b)

instance Show CUShort where
  showsPrec d a = showsPrec d (unCUShort a)

instance Num CUShort where
  a + b = CUShort (unCUShort a + unCUShort b)
  a - b = CUShort (unCUShort a - unCUShort b)
  a * b = CUShort (unCUShort a * unCUShort b)
  negate a = CUShort (negate (unCUShort a))
  abs a = CUShort (abs (unCUShort a))
  signum a = CUShort (signum (unCUShort a))
  fromInteger i = CUShort (fromInteger i)

instance Bounded CUShort where
  minBound = CUShort minBound
  maxBound = CUShort maxBound

instance Enum CUShort where
  toEnum i = CUShort (toEnum i)
  fromEnum a = fromEnum (unCUShort a)
  enumFromTo a b = map CUShort (enumFromTo (unCUShort a) (unCUShort b))
  enumFromThenTo a b c = map CUShort (enumFromThenTo (unCUShort a) (unCUShort b) (unCUShort c))

instance Real CUShort

instance Integral CUShort where
  quot a b = CUShort (quot (unCUShort a) (unCUShort b))
  rem a b = CUShort (rem (unCUShort a) (unCUShort b))
  div a b = CUShort (div (unCUShort a) (unCUShort b))
  mod a b = CUShort (mod (unCUShort a) (unCUShort b))
  quotRem a b = (quot a b, rem a b)
  divMod a b = (div a b, mod a b)
  toInteger a = toInteger (unCUShort a)

instance Bits CUShort where
  a .&. b = CUShort (unCUShort a .&. unCUShort b)
  a .|. b = CUShort (unCUShort a .|. unCUShort b)
  xor a b = CUShort (xor (unCUShort a) (unCUShort b))
  complement a = CUShort (complement (unCUShort a))
  zeroBits = CUShort zeroBits
  bit i = CUShort (bit i)
  testBit a i = testBit (unCUShort a) i
  shiftL a i = CUShort (shiftL (unCUShort a) i)
  shiftR a i = CUShort (shiftR (unCUShort a) i)
  rotateL a i = CUShort (rotateL (unCUShort a) i)
  rotateR a i = CUShort (rotateR (unCUShort a) i)
  bitSizeMaybe a = bitSizeMaybe (unCUShort a)
  bitSize a = bitSize (unCUShort a)
  isSigned a = isSigned (unCUShort a)
  popCount a = popCount (unCUShort a)

instance FiniteBits CUShort where
  finiteBitSize a = finiteBitSize (unCUShort a)

instance Storable CUShort where
  sizeOf a = sizeOf (unCUShort a)
  alignment a = alignment (unCUShort a)
  peekByteOff p off = fmap CUShort (peekByteOff p off)
  pokeByteOff p off a = pokeByteOff p off (unCUShort a)

newtype CInt = CInt Int32

unCInt :: CInt -> Int32
unCInt (CInt x) = x

instance Eq CInt where
  a == b = unCInt a == unCInt b

instance Ord CInt where
  compare a b = compare (unCInt a) (unCInt b)

instance Show CInt where
  showsPrec d a = showsPrec d (unCInt a)

instance Num CInt where
  a + b = CInt (unCInt a + unCInt b)
  a - b = CInt (unCInt a - unCInt b)
  a * b = CInt (unCInt a * unCInt b)
  negate a = CInt (negate (unCInt a))
  abs a = CInt (abs (unCInt a))
  signum a = CInt (signum (unCInt a))
  fromInteger i = CInt (fromInteger i)

instance Bounded CInt where
  minBound = CInt minBound
  maxBound = CInt maxBound

instance Enum CInt where
  toEnum i = CInt (toEnum i)
  fromEnum a = fromEnum (unCInt a)
  enumFromTo a b = map CInt (enumFromTo (unCInt a) (unCInt b))
  enumFromThenTo a b c = map CInt (enumFromThenTo (unCInt a) (unCInt b) (unCInt c))

instance Real CInt

instance Integral CInt where
  quot a b = CInt (quot (unCInt a) (unCInt b))
  rem a b = CInt (rem (unCInt a) (unCInt b))
  div a b = CInt (div (unCInt a) (unCInt b))
  mod a b = CInt (mod (unCInt a) (unCInt b))
  quotRem a b = (quot a b, rem a b)
  divMod a b = (div a b, mod a b)
  toInteger a = toInteger (unCInt a)

instance Bits CInt where
  a .&. b = CInt (unCInt a .&. unCInt b)
  a .|. b = CInt (unCInt a .|. unCInt b)
  xor a b = CInt (xor (unCInt a) (unCInt b))
  complement a = CInt (complement (unCInt a))
  zeroBits = CInt zeroBits
  bit i = CInt (bit i)
  testBit a i = testBit (unCInt a) i
  shiftL a i = CInt (shiftL (unCInt a) i)
  shiftR a i = CInt (shiftR (unCInt a) i)
  rotateL a i = CInt (rotateL (unCInt a) i)
  rotateR a i = CInt (rotateR (unCInt a) i)
  bitSizeMaybe a = bitSizeMaybe (unCInt a)
  bitSize a = bitSize (unCInt a)
  isSigned a = isSigned (unCInt a)
  popCount a = popCount (unCInt a)

instance FiniteBits CInt where
  finiteBitSize a = finiteBitSize (unCInt a)

instance Storable CInt where
  sizeOf a = sizeOf (unCInt a)
  alignment a = alignment (unCInt a)
  peekByteOff p off = fmap CInt (peekByteOff p off)
  pokeByteOff p off a = pokeByteOff p off (unCInt a)

newtype CUInt = CUInt Word32

unCUInt :: CUInt -> Word32
unCUInt (CUInt x) = x

instance Eq CUInt where
  a == b = unCUInt a == unCUInt b

instance Ord CUInt where
  compare a b = compare (unCUInt a) (unCUInt b)

instance Show CUInt where
  showsPrec d a = showsPrec d (unCUInt a)

instance Num CUInt where
  a + b = CUInt (unCUInt a + unCUInt b)
  a - b = CUInt (unCUInt a - unCUInt b)
  a * b = CUInt (unCUInt a * unCUInt b)
  negate a = CUInt (negate (unCUInt a))
  abs a = CUInt (abs (unCUInt a))
  signum a = CUInt (signum (unCUInt a))
  fromInteger i = CUInt (fromInteger i)

instance Bounded CUInt where
  minBound = CUInt minBound
  maxBound = CUInt maxBound

instance Enum CUInt where
  toEnum i = CUInt (toEnum i)
  fromEnum a = fromEnum (unCUInt a)
  enumFromTo a b = map CUInt (enumFromTo (unCUInt a) (unCUInt b))
  enumFromThenTo a b c = map CUInt (enumFromThenTo (unCUInt a) (unCUInt b) (unCUInt c))

instance Real CUInt

instance Integral CUInt where
  quot a b = CUInt (quot (unCUInt a) (unCUInt b))
  rem a b = CUInt (rem (unCUInt a) (unCUInt b))
  div a b = CUInt (div (unCUInt a) (unCUInt b))
  mod a b = CUInt (mod (unCUInt a) (unCUInt b))
  quotRem a b = (quot a b, rem a b)
  divMod a b = (div a b, mod a b)
  toInteger a = toInteger (unCUInt a)

instance Bits CUInt where
  a .&. b = CUInt (unCUInt a .&. unCUInt b)
  a .|. b = CUInt (unCUInt a .|. unCUInt b)
  xor a b = CUInt (xor (unCUInt a) (unCUInt b))
  complement a = CUInt (complement (unCUInt a))
  zeroBits = CUInt zeroBits
  bit i = CUInt (bit i)
  testBit a i = testBit (unCUInt a) i
  shiftL a i = CUInt (shiftL (unCUInt a) i)
  shiftR a i = CUInt (shiftR (unCUInt a) i)
  rotateL a i = CUInt (rotateL (unCUInt a) i)
  rotateR a i = CUInt (rotateR (unCUInt a) i)
  bitSizeMaybe a = bitSizeMaybe (unCUInt a)
  bitSize a = bitSize (unCUInt a)
  isSigned a = isSigned (unCUInt a)
  popCount a = popCount (unCUInt a)

instance FiniteBits CUInt where
  finiteBitSize a = finiteBitSize (unCUInt a)

instance Storable CUInt where
  sizeOf a = sizeOf (unCUInt a)
  alignment a = alignment (unCUInt a)
  peekByteOff p off = fmap CUInt (peekByteOff p off)
  pokeByteOff p off a = pokeByteOff p off (unCUInt a)

newtype CLong = CLong Int64

unCLong :: CLong -> Int64
unCLong (CLong x) = x

instance Eq CLong where
  a == b = unCLong a == unCLong b

instance Ord CLong where
  compare a b = compare (unCLong a) (unCLong b)

instance Show CLong where
  showsPrec d a = showsPrec d (unCLong a)

instance Num CLong where
  a + b = CLong (unCLong a + unCLong b)
  a - b = CLong (unCLong a - unCLong b)
  a * b = CLong (unCLong a * unCLong b)
  negate a = CLong (negate (unCLong a))
  abs a = CLong (abs (unCLong a))
  signum a = CLong (signum (unCLong a))
  fromInteger i = CLong (fromInteger i)

instance Bounded CLong where
  minBound = CLong minBound
  maxBound = CLong maxBound

instance Enum CLong where
  toEnum i = CLong (toEnum i)
  fromEnum a = fromEnum (unCLong a)
  enumFromTo a b = map CLong (enumFromTo (unCLong a) (unCLong b))
  enumFromThenTo a b c = map CLong (enumFromThenTo (unCLong a) (unCLong b) (unCLong c))

instance Real CLong

instance Integral CLong where
  quot a b = CLong (quot (unCLong a) (unCLong b))
  rem a b = CLong (rem (unCLong a) (unCLong b))
  div a b = CLong (div (unCLong a) (unCLong b))
  mod a b = CLong (mod (unCLong a) (unCLong b))
  quotRem a b = (quot a b, rem a b)
  divMod a b = (div a b, mod a b)
  toInteger a = toInteger (unCLong a)

instance Bits CLong where
  a .&. b = CLong (unCLong a .&. unCLong b)
  a .|. b = CLong (unCLong a .|. unCLong b)
  xor a b = CLong (xor (unCLong a) (unCLong b))
  complement a = CLong (complement (unCLong a))
  zeroBits = CLong zeroBits
  bit i = CLong (bit i)
  testBit a i = testBit (unCLong a) i
  shiftL a i = CLong (shiftL (unCLong a) i)
  shiftR a i = CLong (shiftR (unCLong a) i)
  rotateL a i = CLong (rotateL (unCLong a) i)
  rotateR a i = CLong (rotateR (unCLong a) i)
  bitSizeMaybe a = bitSizeMaybe (unCLong a)
  bitSize a = bitSize (unCLong a)
  isSigned a = isSigned (unCLong a)
  popCount a = popCount (unCLong a)

instance FiniteBits CLong where
  finiteBitSize a = finiteBitSize (unCLong a)

instance Storable CLong where
  sizeOf a = sizeOf (unCLong a)
  alignment a = alignment (unCLong a)
  peekByteOff p off = fmap CLong (peekByteOff p off)
  pokeByteOff p off a = pokeByteOff p off (unCLong a)

newtype CULong = CULong Word64

unCULong :: CULong -> Word64
unCULong (CULong x) = x

instance Eq CULong where
  a == b = unCULong a == unCULong b

instance Ord CULong where
  compare a b = compare (unCULong a) (unCULong b)

instance Show CULong where
  showsPrec d a = showsPrec d (unCULong a)

instance Num CULong where
  a + b = CULong (unCULong a + unCULong b)
  a - b = CULong (unCULong a - unCULong b)
  a * b = CULong (unCULong a * unCULong b)
  negate a = CULong (negate (unCULong a))
  abs a = CULong (abs (unCULong a))
  signum a = CULong (signum (unCULong a))
  fromInteger i = CULong (fromInteger i)

instance Bounded CULong where
  minBound = CULong minBound
  maxBound = CULong maxBound

instance Enum CULong where
  toEnum i = CULong (toEnum i)
  fromEnum a = fromEnum (unCULong a)
  enumFromTo a b = map CULong (enumFromTo (unCULong a) (unCULong b))
  enumFromThenTo a b c = map CULong (enumFromThenTo (unCULong a) (unCULong b) (unCULong c))

instance Real CULong

instance Integral CULong where
  quot a b = CULong (quot (unCULong a) (unCULong b))
  rem a b = CULong (rem (unCULong a) (unCULong b))
  div a b = CULong (div (unCULong a) (unCULong b))
  mod a b = CULong (mod (unCULong a) (unCULong b))
  quotRem a b = (quot a b, rem a b)
  divMod a b = (div a b, mod a b)
  toInteger a = toInteger (unCULong a)

instance Bits CULong where
  a .&. b = CULong (unCULong a .&. unCULong b)
  a .|. b = CULong (unCULong a .|. unCULong b)
  xor a b = CULong (xor (unCULong a) (unCULong b))
  complement a = CULong (complement (unCULong a))
  zeroBits = CULong zeroBits
  bit i = CULong (bit i)
  testBit a i = testBit (unCULong a) i
  shiftL a i = CULong (shiftL (unCULong a) i)
  shiftR a i = CULong (shiftR (unCULong a) i)
  rotateL a i = CULong (rotateL (unCULong a) i)
  rotateR a i = CULong (rotateR (unCULong a) i)
  bitSizeMaybe a = bitSizeMaybe (unCULong a)
  bitSize a = bitSize (unCULong a)
  isSigned a = isSigned (unCULong a)
  popCount a = popCount (unCULong a)

instance FiniteBits CULong where
  finiteBitSize a = finiteBitSize (unCULong a)

instance Storable CULong where
  sizeOf a = sizeOf (unCULong a)
  alignment a = alignment (unCULong a)
  peekByteOff p off = fmap CULong (peekByteOff p off)
  pokeByteOff p off a = pokeByteOff p off (unCULong a)

newtype CLLong = CLLong Int64

unCLLong :: CLLong -> Int64
unCLLong (CLLong x) = x

instance Eq CLLong where
  a == b = unCLLong a == unCLLong b

instance Ord CLLong where
  compare a b = compare (unCLLong a) (unCLLong b)

instance Show CLLong where
  showsPrec d a = showsPrec d (unCLLong a)

instance Num CLLong where
  a + b = CLLong (unCLLong a + unCLLong b)
  a - b = CLLong (unCLLong a - unCLLong b)
  a * b = CLLong (unCLLong a * unCLLong b)
  negate a = CLLong (negate (unCLLong a))
  abs a = CLLong (abs (unCLLong a))
  signum a = CLLong (signum (unCLLong a))
  fromInteger i = CLLong (fromInteger i)

instance Bounded CLLong where
  minBound = CLLong minBound
  maxBound = CLLong maxBound

instance Enum CLLong where
  toEnum i = CLLong (toEnum i)
  fromEnum a = fromEnum (unCLLong a)
  enumFromTo a b = map CLLong (enumFromTo (unCLLong a) (unCLLong b))
  enumFromThenTo a b c = map CLLong (enumFromThenTo (unCLLong a) (unCLLong b) (unCLLong c))

instance Real CLLong

instance Integral CLLong where
  quot a b = CLLong (quot (unCLLong a) (unCLLong b))
  rem a b = CLLong (rem (unCLLong a) (unCLLong b))
  div a b = CLLong (div (unCLLong a) (unCLLong b))
  mod a b = CLLong (mod (unCLLong a) (unCLLong b))
  quotRem a b = (quot a b, rem a b)
  divMod a b = (div a b, mod a b)
  toInteger a = toInteger (unCLLong a)

instance Bits CLLong where
  a .&. b = CLLong (unCLLong a .&. unCLLong b)
  a .|. b = CLLong (unCLLong a .|. unCLLong b)
  xor a b = CLLong (xor (unCLLong a) (unCLLong b))
  complement a = CLLong (complement (unCLLong a))
  zeroBits = CLLong zeroBits
  bit i = CLLong (bit i)
  testBit a i = testBit (unCLLong a) i
  shiftL a i = CLLong (shiftL (unCLLong a) i)
  shiftR a i = CLLong (shiftR (unCLLong a) i)
  rotateL a i = CLLong (rotateL (unCLLong a) i)
  rotateR a i = CLLong (rotateR (unCLLong a) i)
  bitSizeMaybe a = bitSizeMaybe (unCLLong a)
  bitSize a = bitSize (unCLLong a)
  isSigned a = isSigned (unCLLong a)
  popCount a = popCount (unCLLong a)

instance FiniteBits CLLong where
  finiteBitSize a = finiteBitSize (unCLLong a)

instance Storable CLLong where
  sizeOf a = sizeOf (unCLLong a)
  alignment a = alignment (unCLLong a)
  peekByteOff p off = fmap CLLong (peekByteOff p off)
  pokeByteOff p off a = pokeByteOff p off (unCLLong a)

newtype CULLong = CULLong Word64

unCULLong :: CULLong -> Word64
unCULLong (CULLong x) = x

instance Eq CULLong where
  a == b = unCULLong a == unCULLong b

instance Ord CULLong where
  compare a b = compare (unCULLong a) (unCULLong b)

instance Show CULLong where
  showsPrec d a = showsPrec d (unCULLong a)

instance Num CULLong where
  a + b = CULLong (unCULLong a + unCULLong b)
  a - b = CULLong (unCULLong a - unCULLong b)
  a * b = CULLong (unCULLong a * unCULLong b)
  negate a = CULLong (negate (unCULLong a))
  abs a = CULLong (abs (unCULLong a))
  signum a = CULLong (signum (unCULLong a))
  fromInteger i = CULLong (fromInteger i)

instance Bounded CULLong where
  minBound = CULLong minBound
  maxBound = CULLong maxBound

instance Enum CULLong where
  toEnum i = CULLong (toEnum i)
  fromEnum a = fromEnum (unCULLong a)
  enumFromTo a b = map CULLong (enumFromTo (unCULLong a) (unCULLong b))
  enumFromThenTo a b c = map CULLong (enumFromThenTo (unCULLong a) (unCULLong b) (unCULLong c))

instance Real CULLong

instance Integral CULLong where
  quot a b = CULLong (quot (unCULLong a) (unCULLong b))
  rem a b = CULLong (rem (unCULLong a) (unCULLong b))
  div a b = CULLong (div (unCULLong a) (unCULLong b))
  mod a b = CULLong (mod (unCULLong a) (unCULLong b))
  quotRem a b = (quot a b, rem a b)
  divMod a b = (div a b, mod a b)
  toInteger a = toInteger (unCULLong a)

instance Bits CULLong where
  a .&. b = CULLong (unCULLong a .&. unCULLong b)
  a .|. b = CULLong (unCULLong a .|. unCULLong b)
  xor a b = CULLong (xor (unCULLong a) (unCULLong b))
  complement a = CULLong (complement (unCULLong a))
  zeroBits = CULLong zeroBits
  bit i = CULLong (bit i)
  testBit a i = testBit (unCULLong a) i
  shiftL a i = CULLong (shiftL (unCULLong a) i)
  shiftR a i = CULLong (shiftR (unCULLong a) i)
  rotateL a i = CULLong (rotateL (unCULLong a) i)
  rotateR a i = CULLong (rotateR (unCULLong a) i)
  bitSizeMaybe a = bitSizeMaybe (unCULLong a)
  bitSize a = bitSize (unCULLong a)
  isSigned a = isSigned (unCULLong a)
  popCount a = popCount (unCULLong a)

instance FiniteBits CULLong where
  finiteBitSize a = finiteBitSize (unCULLong a)

instance Storable CULLong where
  sizeOf a = sizeOf (unCULLong a)
  alignment a = alignment (unCULLong a)
  peekByteOff p off = fmap CULLong (peekByteOff p off)
  pokeByteOff p off a = pokeByteOff p off (unCULLong a)

newtype CSize = CSize Word64

unCSize :: CSize -> Word64
unCSize (CSize x) = x

instance Eq CSize where
  a == b = unCSize a == unCSize b

instance Ord CSize where
  compare a b = compare (unCSize a) (unCSize b)

instance Show CSize where
  showsPrec d a = showsPrec d (unCSize a)

instance Num CSize where
  a + b = CSize (unCSize a + unCSize b)
  a - b = CSize (unCSize a - unCSize b)
  a * b = CSize (unCSize a * unCSize b)
  negate a = CSize (negate (unCSize a))
  abs a = CSize (abs (unCSize a))
  signum a = CSize (signum (unCSize a))
  fromInteger i = CSize (fromInteger i)

instance Bounded CSize where
  minBound = CSize minBound
  maxBound = CSize maxBound

instance Enum CSize where
  toEnum i = CSize (toEnum i)
  fromEnum a = fromEnum (unCSize a)
  enumFromTo a b = map CSize (enumFromTo (unCSize a) (unCSize b))
  enumFromThenTo a b c = map CSize (enumFromThenTo (unCSize a) (unCSize b) (unCSize c))

instance Real CSize

instance Integral CSize where
  quot a b = CSize (quot (unCSize a) (unCSize b))
  rem a b = CSize (rem (unCSize a) (unCSize b))
  div a b = CSize (div (unCSize a) (unCSize b))
  mod a b = CSize (mod (unCSize a) (unCSize b))
  quotRem a b = (quot a b, rem a b)
  divMod a b = (div a b, mod a b)
  toInteger a = toInteger (unCSize a)

instance Bits CSize where
  a .&. b = CSize (unCSize a .&. unCSize b)
  a .|. b = CSize (unCSize a .|. unCSize b)
  xor a b = CSize (xor (unCSize a) (unCSize b))
  complement a = CSize (complement (unCSize a))
  zeroBits = CSize zeroBits
  bit i = CSize (bit i)
  testBit a i = testBit (unCSize a) i
  shiftL a i = CSize (shiftL (unCSize a) i)
  shiftR a i = CSize (shiftR (unCSize a) i)
  rotateL a i = CSize (rotateL (unCSize a) i)
  rotateR a i = CSize (rotateR (unCSize a) i)
  bitSizeMaybe a = bitSizeMaybe (unCSize a)
  bitSize a = bitSize (unCSize a)
  isSigned a = isSigned (unCSize a)
  popCount a = popCount (unCSize a)

instance FiniteBits CSize where
  finiteBitSize a = finiteBitSize (unCSize a)

instance Storable CSize where
  sizeOf a = sizeOf (unCSize a)
  alignment a = alignment (unCSize a)
  peekByteOff p off = fmap CSize (peekByteOff p off)
  pokeByteOff p off a = pokeByteOff p off (unCSize a)
