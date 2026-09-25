{-# LANGUAGE RankNTypes #-}

-- |
-- Module      : Data.Text.Array
-- Copyright   : (c) 2009, 2010, 2011 Bryan O'Sullivan
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Portability : portable
--
-- Packed, unboxed, heap-resident arrays.  Suitable for performance
-- critical use, both in terms of large data quantities and high
-- speed.
--
-- This module is intended to be imported @qualified@, to avoid name
-- clashes with "Prelude" functions, e.g.
--
-- > import qualified Data.Text.Array as A
--
-- The names in this module resemble those in the 'Data.Array' family
-- of modules, but are shorter due to the assumption of qualified
-- naming.
--
-- Cut down for aihc-boot: the arrays are 'ForeignPtr' buffers, so the
-- module uses only the lifted API of base. Upstream uses 'ByteArray#'
-- and unboxed tuples.
module Data.Text.Array (
    Array,
    MArray,
    resizeM,
    shrinkM,
    copyI,
    copyFromPointer,
    empty,
    equal,
    run,
    unsafeFreeze,
    unsafeIndex,
    new,
    unsafeWrite,
  ) where

import Control.Monad.ST (ST, runST)
import Control.Monad.ST.Unsafe (unsafeIOToST)
import Data.Word (Word8)
import Foreign.ForeignPtr (ForeignPtr, mallocForeignPtrBytes, withForeignPtr)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (peekByteOff, pokeByteOff)
import System.IO.Unsafe (unsafeDupablePerformIO)

-- | Immutable array type.
newtype Array = Array (ForeignPtr Word8)

-- | Mutable array type, for use in the ST monad. The 'Int' is the size
-- of the buffer in bytes.
data MArray s = MArray !(ForeignPtr Word8) !Int

-- | Create an uninitialized mutable array.
new :: forall s. Int -> ST s (MArray s)
new len = unsafeIOToST $ do
  fp <- mallocForeignPtrBytes len
  pure (MArray fp len)
{-# INLINE new #-}

-- | Freeze a mutable array. Do not mutate the 'MArray' afterwards!
unsafeFreeze :: MArray s -> ST s Array
unsafeFreeze (MArray fp _) = pure (Array fp)
{-# INLINE unsafeFreeze #-}

-- | Unchecked read of an immutable array.  May return garbage or
-- crash on an out-of-bounds access.
unsafeIndex :: Array -> Int -> Word8
unsafeIndex (Array fp) i =
  unsafeDupablePerformIO $ withForeignPtr fp $ \p -> peekByteOff p i
{-# INLINE unsafeIndex #-}

-- | Unchecked write of a mutable array.  May return garbage or crash
-- on an out-of-bounds access.
unsafeWrite :: MArray s -> Int -> Word8 -> ST s ()
unsafeWrite (MArray fp _) i e =
  unsafeIOToST $ withForeignPtr fp $ \p -> pokeByteOff p i e
{-# INLINE unsafeWrite #-}

-- | An empty immutable array.
empty :: Array
empty = runST (new 0 >>= unsafeFreeze)

-- | Run an action in the ST monad and return an immutable array of
-- its result.
run :: (forall s. ST s (MArray s)) -> Array
run k = runST (k >>= unsafeFreeze)

-- | Resize a mutable array. The new array keeps the contents of the old
-- array up to the smaller of the two sizes.
--
-- @since 2.0
resizeM :: MArray s -> Int -> ST s (MArray s)
resizeM (MArray src srcLen) len = unsafeIOToST $ do
  dst <- mallocForeignPtrBytes len
  withForeignPtr src $ \sp -> withForeignPtr dst $ \dp ->
    copyBytes dp sp (min srcLen len)
  pure (MArray dst len)
{-# INLINE resizeM #-}

-- | Shrink a mutable array. This version keeps the whole buffer: the
-- callers only freeze the array after they shrink it, and a 'Text'
-- records its own length.
--
-- @since 2.0
shrinkM :: MArray s -> Int -> ST s ()
shrinkM _ _ = pure ()
{-# INLINE shrinkM #-}

-- | Copy some elements of an immutable array.
copyI :: Int                    -- ^ Count
      -> MArray s               -- ^ Destination
      -> Int                    -- ^ Destination offset
      -> Array                  -- ^ Source
      -> Int                    -- ^ Source offset
      -> ST s ()
copyI count (MArray dst _) dstOff (Array src) srcOff =
  unsafeIOToST $ withForeignPtr src $ \sp -> withForeignPtr dst $ \dp ->
    copyBytes (dp `plusPtr` dstOff) (sp `plusPtr` srcOff) count
{-# INLINE copyI #-}

-- | Copy from pointer.
--
-- @since 2.0
copyFromPointer
  :: MArray s               -- ^ Destination
  -> Int                    -- ^ Destination offset
  -> Ptr Word8              -- ^ Source
  -> Int                    -- ^ Count
  -> ST s ()
copyFromPointer (MArray dst _) dstOff src count =
  unsafeIOToST $ withForeignPtr dst $ \dp ->
    copyBytes (dp `plusPtr` dstOff) src count
{-# INLINE copyFromPointer #-}

-- | Compare portions of two arrays for equality.  No bounds checking
-- is performed.
equal :: Array -> Int -> Array -> Int -> Int -> Bool
equal (Array fp1) off1 (Array fp2) off2 count =
  unsafeDupablePerformIO $ withForeignPtr fp1 $ \p1 -> withForeignPtr fp2 $ \p2 ->
    let go i
          | i >= count = pure True
          | otherwise = do
              a <- peekByteOff p1 (off1 + i) :: IO Word8
              b <- peekByteOff p2 (off2 + i)
              if a == b then go (i + 1) else pure False
     in go 0
{-# INLINE equal #-}
