{-# LANGUAGE MagicHash #-}

-- |
-- Module      : Data.Text.Internal.Unsafe.Char
-- Copyright   : (c) 2008, 2009 Tom Harper,
--               (c) 2009, 2010 Bryan O'Sullivan,
--               (c) 2009 Duncan Coutts
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
-- Fast character manipulation functions.
module Data.Text.Internal.Unsafe.Char (
    ord,
    unsafeChr8,
    unsafeWrite,
  ) where

import Control.Monad.ST (ST)
import Data.Text.Internal.Encoding.Utf8
import GHC.Exts (Char(..), Int(..), chr#, ord#, word2Int#, word8ToWord#)
import GHC.Word (Word8(..))
import qualified Data.Text.Array as A

ord :: Char -> Int
ord (C# c#) = I# (ord# c#)
{-# INLINE ord #-}

unsafeChr8 :: Word8 -> Char
unsafeChr8 (W8# w#) = C# (chr# (word2Int# (word8ToWord# w#)))
{-# INLINE unsafeChr8 #-}

-- | Write a character into the array at the given offset.  Returns
-- the number of 'Word8's written.
unsafeWrite ::

    A.MArray s -> Int -> Char -> ST s Int
unsafeWrite marr i c = case utf8Length c of
    1 -> do
        let n0 = intToWord8 (ord c)
        A.unsafeWrite marr i n0
        return 1
    2 -> do
        let (n0, n1) = ord2 c
        A.unsafeWrite marr i     n0
        A.unsafeWrite marr (i+1) n1
        return 2
    3 -> do
        let (n0, n1, n2) = ord3 c
        A.unsafeWrite marr i     n0
        A.unsafeWrite marr (i+1) n1
        A.unsafeWrite marr (i+2) n2
        return 3
    _ -> do
        let (n0, n1, n2, n3) = ord4 c
        A.unsafeWrite marr i     n0
        A.unsafeWrite marr (i+1) n1
        A.unsafeWrite marr (i+2) n2
        A.unsafeWrite marr (i+3) n3
        return 4
{-# INLINE unsafeWrite #-}

intToWord8 :: Int -> Word8
intToWord8 = fromIntegral

