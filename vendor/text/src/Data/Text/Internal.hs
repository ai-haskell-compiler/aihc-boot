{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}

{-# OPTIONS_HADDOCK not-home #-}

-- |
-- Module      : Data.Text.Internal
-- Copyright   : (c) 2008, 2009 Tom Harper,
--               (c) 2009, 2010 Bryan O'Sullivan,
--               (c) 2009 Duncan Coutts
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- A module containing private 'Text' internals. This exposes the
-- 'Text' representation and low level construction functions.
-- Modules which extend the 'Text' system may need to use this module.
--
-- You should not use this module unless you are determined to monkey
-- with the internals, as the functions here do just about nothing to
-- preserve data invariants.  You have been warned!

module Data.Text.Internal (
    Text(..),
    safe,
    empty,
    append,
    pack,
  ) where

import Control.Monad.ST (ST, runST)
import Data.Bits
import Data.Text.Internal.Unsafe.Char (unsafeWrite, ord)
import qualified Data.Text.Array as A

-- | A space efficient, packed, unboxed Unicode text type.
data Text = Text
    {-# UNPACK #-} !A.Array -- ^ bytearray encoded as UTF-8
    {-# UNPACK #-} !Int     -- ^ offset in bytes (not in Char!), pointing to a start of UTF-8 sequence
    {-# UNPACK #-} !Int     -- ^ length in bytes (not in Char!), pointing to an end of UTF-8 sequence

-- | /O(1)/ The empty 'Text'.
empty :: Text
empty = Text A.empty 0 0
{-# NOINLINE empty #-}

-- | /O(n)/ Appends one 'Text' to the other by copying both of them
-- into a new 'Text'.
append :: Text -> Text -> Text
append a@(Text arr1 off1 len1) b@(Text arr2 off2 len2)
    | len1 == 0 = b
    | len2 == 0 = a
    | len > 0   = Text (A.run x) 0 len
    | otherwise = error $ "Data.Text.append: size overflow"
    where
      len = len1+len2
      x :: ST s (A.MArray s)
      x = do
        arr <- A.new len
        A.copyI len1 arr 0 arr1 off1
        A.copyI len2 arr len1 arr2 off2
        return arr
{-# NOINLINE append #-}

-- | Map a 'Char' to a 'Text'-safe value.
--
-- Unicode 'Data.Char.Surrogate' code points are not included in the set of Unicode
-- scalar values, but are unfortunately admitted as valid 'Char'
-- values by Haskell.  They cannot be represented in a 'Text'.  This
-- function remaps those code points to the Unicode replacement
-- character (U+FFFD, \'&#xfffd;\'), and leaves other code points
-- unchanged.
safe :: Char -> Char
safe c
    | ord c .&. 0x1ff800 /= 0xd800 = c
    | otherwise                    = '\xfffd'
{-# INLINE [0] safe #-}

-- | /O(n)/ Convert a 'String' into a 'Text'.
-- Performs replacement on invalid scalar values, so @'Data.Text.unpack' . 'pack'@ is not 'id':
--
-- >>> Data.Text.unpack (pack "\55555")
-- "\65533"
pack :: String -> Text
pack [] = empty
pack xs = runST $ do
  -- It's tempting to allocate a buffer of 4 * length xs bytes,
  -- but not only it's wasteful for predominantly ASCII arguments,
  -- the computation of length xs would force allocation of the entire xs at once.
  let dstLen = 64
  dst <- A.new dstLen
  outer dst dstLen 0 xs
  where
    outer :: forall s. A.MArray s -> Int -> Int -> String -> ST s Text
    outer !dst !dstLen = inner
      where
        inner !dstOff [] = do
          A.shrinkM dst dstOff
          arr <- A.unsafeFreeze dst
          return (Text arr 0 dstOff)
        inner !dstOff ccs@(c : cs)
          -- Each 'Char' takes up to 4 bytes
          | dstOff + 4 > dstLen = do
            -- Double size of the buffer
            let !dstLen' = dstLen * 2
            dst' <- A.resizeM dst dstLen'
            outer dst' dstLen' dstOff ccs
          | otherwise = do
            d <- unsafeWrite dst dstOff (safe c)
            inner (dstOff + d) cs
{-# NOINLINE [0] pack #-}

