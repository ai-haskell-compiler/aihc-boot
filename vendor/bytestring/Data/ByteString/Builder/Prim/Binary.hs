-- | Copyright   : (c) 2010-2011 Simon Meier
-- License       : BSD3-style (see LICENSE)
--
-- Maintainer    : Simon Meier <iridcode@gmail.com>
-- Portability   : GHC
--
module Data.ByteString.Builder.Prim.Binary (
    word8,
  ) where

import Data.ByteString.Builder.Prim.Internal
import Foreign

{-# INLINE word8 #-}
-- | Encoding single unsigned bytes as-is.
--
word8 :: FixedPrim Word8
word8 = fixedPrim 1 (flip poke) -- Word8 is always aligned

