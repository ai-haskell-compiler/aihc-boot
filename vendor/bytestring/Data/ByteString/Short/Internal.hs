{-# OPTIONS_HADDOCK not-home #-}
{-# OPTIONS_GHC -fexpose-all-unfoldings #-}

-- |
-- Module      : Data.ByteString.Short.Internal
-- Copyright   : (c) Duncan Coutts 2012-2013, Julian Ospald 2022
-- License     : BSD-style
--
-- Maintainer  : hasufell@posteo.de
-- Stability   : stable
-- Portability : ghc only
--
-- Internal representation of ShortByteString
--
module Data.ByteString.Short.Internal (
    ShortByteString(.., SBS),
    toShort,
  ) where

import Data.ByteString.Internal.Type (ByteString(..), unsafeDupablePerformIO)
import Data.Array.Byte
  ( ByteArray(..), MutableByteArray(..) )
import Control.Exception
  ( assert )
import GHC.Exts (Int(I#), Int#, Ptr(Ptr), Addr#, State#, RealWorld, ByteArray#, MutableByteArray#, newByteArray#, unsafeFreezeByteArray#, unsafeFreezeByteArray#)
import GHC.IO hiding ( unsafeDupablePerformIO )
import GHC.ST (ST(ST))
import Prelude (Ord(..), ($), (<$>))
import qualified Data.ByteString.Internal.Type as BS
import qualified GHC.Exts

-- | A compact representation of a 'Word8' vector.
--
-- It has a lower memory overhead than a 'ByteString' and does not
-- contribute to heap fragmentation. It can be converted to or from a
-- 'ByteString' (at the cost of copying the string data). It supports very few
-- other operations.
--
newtype ShortByteString =
  -- | @since 0.12.0.0
  ShortByteString
  { unShortByteString :: ByteArray
  -- ^ @since 0.12.0.0
  }

-- | Prior to @bytestring-0.12@ 'SBS' was a genuine constructor of 'ShortByteString',
-- but now it is a bundled pattern synonym, provided as a compatibility shim.
pattern SBS :: ByteArray# -> ShortByteString
pattern SBS x = ShortByteString (ByteArray x)

-- | /O(n)/. Convert a 'ByteString' into a 'ShortByteString'.
--
-- This makes a copy, so does not retain the input string.
--
toShort :: ByteString -> ShortByteString
toShort !bs = unsafeDupablePerformIO (toShortIO bs)

toShortIO :: ByteString -> IO ShortByteString
toShortIO (BS fptr len) = do
    mba <- stToIO (newByteArray len)
    BS.unsafeWithForeignPtr fptr $ \ptr ->
      stToIO (copyAddrToByteArray ptr mba 0 len)
    ShortByteString <$> stToIO (unsafeFreezeByteArray mba)

newByteArray :: Int -> ST s (MutableByteArray s)
newByteArray len@(I# len#) =
  assert (len >= 0) $
    ST $ \s -> case newByteArray# len# s of
                 (# s', mba# #) -> (# s', MutableByteArray mba# #)

unsafeFreezeByteArray :: MutableByteArray s -> ST s ByteArray
unsafeFreezeByteArray (MutableByteArray mba#) =
    ST $ \s -> case unsafeFreezeByteArray# mba# s of
                 (# s', ba# #) -> (# s', ByteArray ba# #)

copyAddrToByteArray :: Ptr a -> MutableByteArray RealWorld -> Int -> Int -> ST RealWorld ()
copyAddrToByteArray (Ptr src#) (MutableByteArray dst#) (I# dst_off#) (I# len#) =
    ST $ \s -> case copyAddrToByteArray# src# dst# dst_off# len# s of
                 s' -> (# s', () #)

copyAddrToByteArray# :: Addr#
                     -> MutableByteArray# RealWorld -> Int#
                     -> Int#
                     -> State# RealWorld -> State# RealWorld

copyAddrToByteArray# = GHC.Exts.copyAddrToByteArray#

