{-# LANGUAGE BangPatterns #-}

-- |
-- Module      : Data.Text.Internal.Encoding.Utf8
-- Copyright   : (c) 2008, 2009 Tom Harper,
--               (c) 2009, 2010 Bryan O'Sullivan,
--               (c) 2009 Duncan Coutts
--               (c) 2021 Andrew Lelechenko
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- /Warning/: this is an internal module, and does not have a stable
-- API or name. Functions in this module may not check or enforce
-- preconditions expected by public modules. Use at your own risk!
--
-- Basic UTF-8 validation and character manipulation.
--
-- Cut down for aihc-boot: the module uses only the lifted API of base.
-- Upstream uses primitive operations and 'Addr#' tables.
module Data.Text.Internal.Encoding.Utf8 (
    utf8Length,
    utf8LengthByLeader,
    ord2,
    ord3,
    ord4,
    chr2,
    chr3,
    chr4,
    DecoderState(..),
    utf8AcceptState,
    utf8RejectState,
    updateDecoderState,
  ) where

import Data.Bits (Bits(..))
import Data.Char (chr, ord)
import Data.Word (Word8)
import qualified Data.Text.Array as A

-- | @since 2.0
utf8Length :: Char -> Int
utf8Length c
  | c < '\x80' = 1
  | c < '\x800' = 2
  | c < '\x10000' = 3
  | otherwise = 4
{-# INLINE utf8Length #-}

-- | @since 2.0
utf8LengthByLeader :: Word8 -> Int
utf8LengthByLeader w
  | w < 0x80  = 1
  | w < 0xE0  = 2
  | w < 0xF0  = 3
  | otherwise = 4
{-# INLINE utf8LengthByLeader #-}

ord2 ::

  Char -> (Word8,Word8)
ord2 c =

    (x1,x2)
    where
      n  = ord c
      x1 = intToWord8 $ (n `shiftR` 6) + 0xC0
      x2 = intToWord8 $ (n .&. 0x3F)   + 0x80
{-# INLINE ord2 #-}

ord3 ::

  Char -> (Word8,Word8,Word8)
ord3 c =

    (x1,x2,x3)
    where
      n  = ord c
      x1 = intToWord8 $ (n `shiftR` 12) + 0xE0
      x2 = intToWord8 $ ((n `shiftR` 6) .&. 0x3F) + 0x80
      x3 = intToWord8 $ (n .&. 0x3F) + 0x80
{-# INLINE ord3 #-}

ord4 ::

  Char -> (Word8,Word8,Word8,Word8)
ord4 c =

    (x1,x2,x3,x4)
    where
      n  = ord c
      x1 = intToWord8 $ (n `shiftR` 18) + 0xF0
      x2 = intToWord8 $ ((n `shiftR` 12) .&. 0x3F) + 0x80
      x3 = intToWord8 $ ((n `shiftR` 6) .&. 0x3F) + 0x80
      x4 = intToWord8 $ (n .&. 0x3F) + 0x80
{-# INLINE ord4 #-}

chr2 :: Word8 -> Word8 -> Char
chr2 x1 x2 = chr (z1 + z2)
    where
      !y1 = word8ToInt x1
      !y2 = word8ToInt x2
      !z1 = (y1 - 0xC0) `shiftL` 6
      !z2 = y2 - 0x80
{-# INLINE chr2 #-}

chr3 :: Word8 -> Word8 -> Word8 -> Char
chr3 x1 x2 x3 = chr (z1 + z2 + z3)
    where
      !y1 = word8ToInt x1
      !y2 = word8ToInt x2
      !y3 = word8ToInt x3
      !z1 = (y1 - 0xE0) `shiftL` 12
      !z2 = (y2 - 0x80) `shiftL` 6
      !z3 = y3 - 0x80
{-# INLINE chr3 #-}

chr4 :: Word8 -> Word8 -> Word8 -> Word8 -> Char
chr4 x1 x2 x3 x4 = chr (z1 + z2 + z3 + z4)
    where
      !y1 = word8ToInt x1
      !y2 = word8ToInt x2
      !y3 = word8ToInt x3
      !y4 = word8ToInt x4
      !z1 = (y1 - 0xF0) `shiftL` 18
      !z2 = (y2 - 0x80) `shiftL` 12
      !z3 = (y3 - 0x80) `shiftL` 6
      !z4 = y4 - 0x80
{-# INLINE chr4 #-}

intToWord8 :: Int -> Word8
intToWord8 = fromIntegral

word8ToInt :: Word8 -> Int
word8ToInt = fromIntegral

newtype ByteClass = ByteClass Word8

byteToClass :: Word8 -> ByteClass
byteToClass n = ByteClass (A.unsafeIndex byteClassTable (word8ToInt n))

-- | The byte classes of the state machine, as an array of bytes.
byteClassTable :: A.Array
byteClassTable = tableFromString
  "\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\SOH\SOH\SOH\SOH\SOH\SOH\SOH\SOH\SOH\SOH\SOH\SOH\SOH\SOH\SOH\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\a\a\a\a\a\a\a\a\a\a\a\a\a\a\a\a\a\a\a\a\a\a\a\a\a\a\a\a\a\a\a\a\b\b\STX\STX\STX\STX\STX\STX\STX\STX\STX\STX\STX\STX\STX\STX\STX\STX\STX\STX\STX\STX\STX\STX\STX\STX\STX\STX\STX\STX\STX\STX\n\ETX\ETX\ETX\ETX\ETX\ETX\ETX\ETX\ETX\ETX\ETX\ETX\EOT\ETX\ETX\v\ACK\ACK\ACK\ENQ\b\b\b\b\b\b\b\b\b\b\b"
{-# NOINLINE byteClassTable #-}

-- | An array of the code points of a string, as bytes.
tableFromString :: String -> A.Array
tableFromString str = A.run (do
  arr <- A.new (length str)
  let fill _ [] = return ()
      fill i (c : cs) = A.unsafeWrite arr i (intToWord8 (ord c)) >> fill (i + 1) cs
  fill 0 str
  return arr)

newtype DecoderState = DecoderState Word8
  deriving (Eq, Show)

utf8AcceptState :: DecoderState
utf8AcceptState = DecoderState 0

utf8RejectState :: DecoderState
utf8RejectState = DecoderState 12

updateState :: ByteClass -> DecoderState -> DecoderState
updateState (ByteClass c) (DecoderState s) =
  DecoderState (A.unsafeIndex stateTable (word8ToInt (c + s)))

-- | The transitions of the state machine, as an array of bytes.
stateTable :: A.Array
stateTable = tableFromString
  "\NUL\f\CAN$<`T\f\f\f0H\f\f\f\f\f\f\f\f\f\f\f\f\f\NUL\f\f\f\f\f\NUL\f\NUL\f\f\f\CAN\f\f\f\f\f\CAN\f\CAN\f\f\f\f\f\f\f\f\f\CAN\f\f\f\f\f\CAN\f\f\f\f\f\f\f\CAN\f\f\f\f\f\f\f\f\f$\f$\f\f\f$\f\f\f\f\f$\f$\f\f\f$\f\f\f\f\f\f\f\f\f\f"
{-# NOINLINE stateTable #-}

updateDecoderState :: Word8 -> DecoderState -> DecoderState
updateDecoderState b s = updateState (byteToClass b) s

