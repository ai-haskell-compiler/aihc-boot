-- Enable yields to make `isValidUtf8` safe to use on large inputs.
{-# OPTIONS_GHC -fno-omit-yields #-}

-- | Haskell implementation of C bits
module Data.ByteString.Internal.Pure (
    memchr,
    memcmp,
    isValidUtf8,
  ) where

import Prelude
import GHC.Exts (Ptr(..), Word8#, Int#, indexWord8OffAddr#)
import GHC.Types                (Int (..))
import GHC.Word                 (Word8(..))
import GHC.Int                  (Int8(..))
import Data.Word
import Foreign.Ptr              (plusPtr, nullPtr)
import Foreign.Storable         (Storable(..))
import Control.Exception        (assert)

memchr :: Ptr Word8 -> Word8 -> Int -> IO (Ptr Word8)
memchr !p !target !len
  | len == 0 = pure nullPtr
  | otherwise = assert (len > 0) $ do
      c <- peek p
      if c == target
        then pure p
        else memchr (p `plusPtr` 1) target (len - 1)

-- | Compare two buffers one byte at a time.
memcmp :: Ptr Word8 -> Ptr Word8 -> Int -> IO Int
memcmp !p1 !p2 !len
  | len == 0 = pure 0
  | otherwise = assert (len > 0) $ do
      c1 <- peek p1
      c2 <- peek p2
      if | c1 == c2 -> memcmp (p1 `plusPtr` 1) (p2 `plusPtr` 1) (len - 1)
         | c1 < c2   -> pure (0-1)
         | otherwise -> pure 1

isValidUtf8 :: Ptr Word8 -> Int -> IO Bool
isValidUtf8 !(Ptr a) !len' = isValidUtf8' (indexWord8OffAddr# a) len'

isValidUtf8' :: (Int# -> Word8#) -> Int -> IO Bool
isValidUtf8' idx !len = go 0
  where
    indexWord8 (I# i) = W8# (idx i)

    indexIsCont :: Int -> Bool
    indexIsCont i =
        -- We use a signed comparison to avoid an extra comparison with 0x80,
        -- since _signed_ 0x80 is -128.
        let
           v :: Int8
           v = fromIntegral (indexWord8 i)
        in v <= (fromIntegral (0xBF :: Word8))

    go !i
      | i >= len  = pure True -- done
      | otherwise = do
            let !b0 = indexWord8 i
            if | b0 <= 0x7F -> go (i+1) -- ASCII
               | b0 >= 0xC2 && b0 <= 0xDF -> go2 (i+1)
               | b0 >= 0xE0 && b0 <= 0xEF -> go3 (i+1) b0
               | otherwise                -> go4 (i+1) b0

    go2 !i
      | i >= len  = pure False
      | indexIsCont i
      = go (i+1)
      | otherwise
      = pure False

    go3 !i !b0
      | i >= len - 1  = pure False -- Be careful: i+1 might overflow!
      | indexIsCont i
      , indexIsCont (i+1)
      , b1 <- indexWord8 i
      ,    (b0 == 0xE0 && b1 >= 0xA0)  -- E0, A0..BF, 80..BF
        || (b0 >= 0xE1 && b0 <= 0xEC)  -- E1..EC, 80..BF, 80..BF
        || (b0 == 0xED && b1 <= 0x9F)  -- ED, 80..9F, 80..BF
        || (b0 >= 0xEE && b0 <= 0xEF)  -- EE..EF, 80..BF, 80..BF
      = go (i+2)
      | otherwise
      = pure False

    go4 !i !b0
      | i >= len - 2  = pure False -- Be careful: i+2 might overflow!
      | indexIsCont i
      , indexIsCont (i+1)
      , indexIsCont (i+2)
      , b1 <- indexWord8 i
      ,    (b0 == 0xF0 && b1 >= 0x90) -- F0, 90..BF, 80..BF, 80..BF
        || (b0 >= 0xF1 && b0 <= 0xF3) -- F1..F3, 80..BF, 80..BF, 80..BF
        || (b0 == 0xF4 && b1 <= 0x8F) -- F4, 80..8F, 80..BF, 80..BF
      = go (i+3)

      | otherwise
      = pure False

