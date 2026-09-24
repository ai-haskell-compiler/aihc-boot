{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UnboxedTuples #-}

{-# OPTIONS_GHC -fno-warn-unused-matches #-}
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
module Data.Text.Array (
    Array,
    pattern ByteArray,
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

import GHC.Exts hiding (toList)
import GHC.ST (ST(..), runST)
import GHC.Word (Word8(..))
import Prelude hiding (length, read, compare)
import Data.Array.Byte (ByteArray(..), MutableByteArray(..))

-- | Immutable array type.
type Array = ByteArray

-- | Mutable array type, for use in the ST monad.
type MArray = MutableByteArray

-- | Create an uninitialized mutable array.
new :: forall s. Int -> ST s (MArray s)
new (I# len#)

  | otherwise = ST $ \s1# ->
    case newByteArray# len# s1# of
      (# s2#, marr# #) -> (# s2#, MutableByteArray marr# #)
{-# INLINE new #-}

-- | Freeze a mutable array. Do not mutate the 'MArray' afterwards!
unsafeFreeze :: MArray s -> ST s Array
unsafeFreeze (MutableByteArray marr) = ST $ \s1# ->
    case unsafeFreezeByteArray# marr s1# of
        (# s2#, ba# #) -> (# s2#, ByteArray ba# #)
{-# INLINE unsafeFreeze #-}

-- | Unchecked read of an immutable array.  May return garbage or
-- crash on an out-of-bounds access.
unsafeIndex ::

  Array -> Int -> Word8
unsafeIndex (ByteArray arr) i@(I# i#) =

  case indexWord8Array# arr i# of r# -> (W8# r#)
{-# INLINE unsafeIndex #-}

-- | Unchecked write of a mutable array.  May return garbage or crash
-- on an out-of-bounds access.
unsafeWrite ::

  MArray s -> Int -> Word8 -> ST s ()
unsafeWrite ma@(MutableByteArray marr) i@(I# i#) (W8# e#) =

  (ST $ \s1# -> case writeWord8Array# marr i# e# s1# of
    s2# -> (# s2#, () #))
{-# INLINE unsafeWrite #-}

-- | An empty immutable array.
empty :: Array
empty = runST (new 0 >>= unsafeFreeze)

-- | Run an action in the ST monad and return an immutable array of
-- its result.
run :: (forall s. ST s (MArray s)) -> Array
run k = runST (k >>= unsafeFreeze)

-- | @since 2.0
resizeM :: MArray s -> Int -> ST s (MArray s)
resizeM (MutableByteArray ma) i@(I# i#) = ST $ \s1# ->
  case resizeMutableByteArray# ma i# s1# of
    (# s2#, newArr #) -> (# s2#, MutableByteArray newArr #)
{-# INLINE resizeM #-}

-- | @since 2.0
shrinkM ::

  MArray s -> Int -> ST s ()
shrinkM (MutableByteArray marr) i@(I# newSize) = do

  ST $ \s1# ->
    case shrinkMutableByteArray# marr newSize s1# of
      s2# -> (# s2#, () #)
{-# INLINE shrinkM #-}

-- | Copy some elements of an immutable array.
copyI :: Int                    -- ^ Count
      -> MArray s               -- ^ Destination
      -> Int                    -- ^ Destination offset
      -> Array                  -- ^ Source
      -> Int                    -- ^ Source offset
      -> ST s ()
copyI count@(I# count#) (MutableByteArray dst#) dstOff@(I# dstOff#) (ByteArray src#) (I# srcOff#)

  | otherwise = ST $ \s1# ->
    case copyByteArray# src# srcOff# dst# dstOff# count# s1# of
      s2# -> (# s2#, () #)
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
copyFromPointer (MutableByteArray dst#) dstOff@(I# dstOff#) (Ptr src#) count@(I# count#)

  | otherwise = ST $ \s1# ->
    case copyAddrToByteArray# src# dst# dstOff# count# s1# of
      s2# -> (# s2#, () #)
{-# INLINE copyFromPointer #-}

-- | Compare portions of two arrays for equality.  No bounds checking
-- is performed.
equal :: Array -> Int -> Array -> Int -> Int -> Bool
equal src1 off1 src2 off2 count = compareInternal src1 off1 src2 off2 count == 0
{-# INLINE equal #-}

compareInternal
      :: Array                  -- ^ First
      -> Int                    -- ^ Offset into first
      -> Array                  -- ^ Second
      -> Int                    -- ^ Offset into second
      -> Int                    -- ^ Count
      -> Int
compareInternal (ByteArray src1#) (I# off1#) (ByteArray src2#) (I# off2#) (I# count#) = i
  where

    i = I# (compareByteArrays# src1# off1# src2# off2# count#)
{-# INLINE compareInternal #-}

