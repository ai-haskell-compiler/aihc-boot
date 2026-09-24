{-# LANGUAGE BangPatterns #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

-- |
-- Module      : Data.Text
-- Copyright   : (c) 2009, 2010, 2011, 2012 Bryan O'Sullivan,
--               (c) 2009 Duncan Coutts,
--               (c) 2008, 2009 Tom Harper
--               (c) 2021 Andrew Lelechenko
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Portability : GHC
--
-- A time and space-efficient implementation of Unicode text.
-- Suitable for performance critical use, both in terms of large data
-- quantities and high speed.
--
-- /Note/: Read below the synopsis for important notes on the use of
-- this module.
--
-- This module is intended to be imported @qualified@, to avoid name
-- clashes with "Prelude" functions, e.g.
--
-- > import qualified Data.Text as T
--
-- To use an extended and very rich family of functions for working
-- with Unicode text (including normalization, regular expressions,
-- non-standard encodings, text breaking, and locales), see the
-- <http://hackage.haskell.org/package/text-icu text-icu package >.
--

module Data.Text
    (
    -- * Types
      Text
    ) where

import Control.DeepSeq (NFData(rnf))
import Data.String (IsString(..))
import qualified Data.Text.Array as A
import Data.Text.Internal (Text(..), append, empty, pack)
import Data.Text.Show ()

instance Eq Text where
    Text arrA offA lenA == Text arrB offB lenB
        | lenA == lenB = A.equal arrA offA arrB offB lenA
        | otherwise    = False
    {-# INLINE (==) #-}

-- | aihc-boot keeps only '(<>)'. The other methods use their defaults.
instance Semigroup Text where
    (<>) = append

-- | aihc-boot keeps only 'mempty'. The other methods use their defaults.
instance Monoid Text where
    mempty  = empty

-- | Performs replacement on invalid scalar values:
--
-- >>> :set -XOverloadedStrings
-- >>> "\55555" :: Text
-- "\65533"
instance IsString Text where
    fromString = pack

instance NFData Text where rnf !_ = ()

