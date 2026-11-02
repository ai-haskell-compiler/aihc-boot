-- | The classes 'Bits' and 'FiniteBits', with their instances for Int.
module GHC.Bits
  ( Bits (..),
    FiniteBits (..),
    toIntegralSized,
  )
where

import GHC.Base
import GHC.Classes
import GHC.Err
import GHC.Maybe
import GHC.Num
import GHC.Prim
import GHC.Real
import GHC.Types

infixl 8 `shift`, `rotate`, `shiftL`, `shiftR`, `unsafeShiftL`, `unsafeShiftR`, `rotateL`, `rotateR`

infixl 7 .&.

infixl 6 `xor`

infixl 5 .|.

class (Eq a) => Bits a where
  (.&.), (.|.), xor :: a -> a -> a
  complement :: a -> a
  shift :: a -> Int -> a
  rotate :: a -> Int -> a
  zeroBits :: a
  bit :: Int -> a
  setBit, clearBit, complementBit :: a -> Int -> a
  testBit :: a -> Int -> Bool
  bitSizeMaybe :: a -> Maybe Int
  bitSize :: a -> Int
  isSigned :: a -> Bool
  shiftL, unsafeShiftL, shiftR, unsafeShiftR, rotateL, rotateR :: a -> Int -> a
  popCount :: a -> Int
  x `shift` i
    | i < 0 = x `shiftR` negate i
    | i > 0 = x `shiftL` i
    | True = x
  x `rotate` i
    | i < 0 = x `rotateR` negate i
    | i > 0 = x `rotateL` i
    | True = x
  zeroBits = clearBit (bit 0) 0
  setBit x i = x .|. bit i
  clearBit x i = x .&. complement (bit i)
  complementBit x i = x `xor` bit i
  testBit x i = (x .&. bit i) /= zeroBits
  bitSize b = case bitSizeMaybe b of
    Just n -> n
    Nothing -> error "Data.Bits.bitSize: no size"
  x `shiftL` i = x `shift` i
  x `unsafeShiftL` i = x `shiftL` i
  x `shiftR` i = x `shift` negate i
  x `unsafeShiftR` i = x `shiftR` i
  x `rotateL` i = x `rotate` i
  x `rotateR` i = x `rotate` negate i

class (Bits b) => FiniteBits b where
  finiteBitSize :: b -> Int
  countLeadingZeros, countTrailingZeros :: b -> Int
  countLeadingZeros x = go (finiteBitSize x - 1)
    where
      go i
        | i < 0 = finiteBitSize x
        | testBit x i = finiteBitSize x - 1 - i
        | True = go (i - 1)
  countTrailingZeros x = go 0
    where
      go i
        | i >= finiteBitSize x = i
        | testBit x i = i
        | True = go (i + 1)

-- | The value at another integral type, when it fits.
toIntegralSized :: (Integral a, Integral b, Bits a, Bits b) => a -> Maybe b
toIntegralSized x =
  let y = fromIntegral x
   in if toInteger y == toInteger x then Just y else Nothing

instance Bits Int where
  (.&.) = primIntAnd
  (.|.) = primIntOr
  xor = primIntXor
  complement x = primIntXor x (negate 1)
  zeroBits = 0
  bit i = primIntShl 1 i
  testBit x i = primIntAnd (primIntShrl x i) 1 /= 0
  shiftL = primIntShl
  unsafeShiftL = primIntShl
  shiftR = primIntShra
  unsafeShiftR = primIntShra
  rotateL x i =
    let r = i `mod` 64
     in if r == 0 then x else primIntOr (primIntShl x r) (primIntShrl x (64 - r))
  rotateR x i = rotateL x (64 - (i `mod` 64))
  bitSizeMaybe _ = Just 64
  bitSize _ = 64
  isSigned _ = True
  popCount = primIntPopCount

instance FiniteBits Int where
  finiteBitSize _ = 64
  countLeadingZeros = primIntClz
  countTrailingZeros = primIntCtz
