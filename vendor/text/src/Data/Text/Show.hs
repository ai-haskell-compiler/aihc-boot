{-# LANGUAGE BangPatterns #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module      : Data.Text.Show
-- Copyright   : (c) 2009-2015 Bryan O'Sullivan
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC

module Data.Text.Show (
    unpack,
  ) where

import Data.Text.Internal (Text(..))
import Data.Text.Internal.Encoding.Utf8 ()
import Data.Text.Internal.Unsafe.Char ()
import Data.Text.Unsafe (Iter(..), iterArray)

-- | /O(n)/ Convert a 'Text' into a 'String'.
unpack ::

  Text -> String
unpack t = foldrText (:) [] t
{-# NOINLINE unpack #-}

foldrText :: (Char -> b -> b) -> b -> Text -> b
foldrText f z (Text arr off len) = go off
  where
    go !i
      | i >= off + len = z
      | otherwise = let !(Iter c l) = iterArray arr i in f c (go (i + l))
{-# INLINE foldrText #-}

instance Show Text where
    showsPrec p ps r = showsPrec p (unpack ps) r

