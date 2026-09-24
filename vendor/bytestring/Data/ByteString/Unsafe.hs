-- |
-- Module      : Data.ByteString.Unsafe
-- Copyright   : (c) Don Stewart 2006-2008
--               (c) Duncan Coutts 2006-2011
-- License     : BSD-style
-- Maintainer  : dons00@gmail.com, duncan@community.haskell.org
-- Stability   : provisional
-- Portability : non-portable
--
-- A module containing unsafe 'ByteString' operations.
--
-- While these functions have a stable API and you may use these functions in
-- applications, do carefully consider the documented pre-conditions;
-- incorrect use can break referential transparency or worse.
--
module Data.ByteString.Unsafe (
    unsafeHead,
    unsafeIndex,
    unsafeTake,
    unsafeDrop,
  ) where

import Data.ByteString.Internal
import Foreign.Storable         (Storable(..))
import Control.Exception        (assert)
import Data.Word                (Word8)

-- | A variety of 'head' for non-empty ByteStrings. 'unsafeHead' omits the
-- check for the empty case, so there is an obligation on the programmer
-- to provide a proof that the ByteString is non-empty.
unsafeHead :: ByteString -> Word8
unsafeHead (BS x l) = assert (l > 0) $
    accursedUnutterablePerformIO $ unsafeWithForeignPtr x $ \p -> peek p
{-# INLINE unsafeHead #-}

-- | Unsafe 'ByteString' index (subscript) operator, starting from 0, returning a 'Word8'
-- This omits the bounds check, which means there is an accompanying
-- obligation on the programmer to ensure the bounds are checked in some
-- other way.
unsafeIndex :: ByteString -> Int -> Word8
unsafeIndex (BS x l) i = assert (i >= 0 && i < l) $
    accursedUnutterablePerformIO $ unsafeWithForeignPtr x $ \p -> peekByteOff p i
{-# INLINE unsafeIndex #-}

-- | A variety of 'take' which omits the checks on @n@ so there is an
-- obligation on the programmer to provide a proof that @0 <= n <= 'length' xs@.
unsafeTake :: Int -> ByteString -> ByteString
unsafeTake n (BS x l) = assert (0 <= n && n <= l) $ BS x n
{-# INLINE unsafeTake #-}

-- | A variety of 'drop' which omits the checks on @n@ so there is an
-- obligation on the programmer to provide a proof that @0 <= n <= 'length' xs@.
unsafeDrop  :: Int -> ByteString -> ByteString
unsafeDrop n (BS x l) = assert (0 <= n && n <= l) $ BS (plusForeignPtr x n) (l-n)
{-# INLINE unsafeDrop #-}

