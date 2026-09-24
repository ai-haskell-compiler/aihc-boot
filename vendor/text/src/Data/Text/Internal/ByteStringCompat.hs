{-# LANGUAGE BangPatterns #-}

module Data.Text.Internal.ByteStringCompat (
    withBS,
  ) where

import Data.ByteString.Internal (ByteString (..))
import Data.Word (Word8)
import Foreign.ForeignPtr (ForeignPtr)

withBS :: ByteString -> (ForeignPtr Word8 -> Int -> r) -> r
withBS (BS !sfp !slen)       kont = kont sfp slen
{-# INLINE withBS #-}

