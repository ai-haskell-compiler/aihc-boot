{-# OPTIONS_HADDOCK prune #-}

-- |
-- Module      : Data.ByteString.Lazy
-- Copyright   : (c) Don Stewart 2006
--               (c) Duncan Coutts 2006-2011
-- License     : BSD-style
--
-- Maintainer  : dons00@gmail.com, duncan@community.haskell.org
-- Stability   : stable
-- Portability : portable
--
-- A time and space-efficient implementation of lazy byte vectors
-- using lists of packed 'Word8' arrays, suitable for high performance
-- use, both in terms of large data quantities, or high speed
-- requirements. Lazy ByteStrings are encoded as lazy lists of strict chunks
-- of bytes.
--
-- A key feature of lazy ByteStrings is the means to manipulate large or
-- unbounded streams of data without requiring the entire sequence to be
-- resident in memory. To take advantage of this you have to write your
-- functions in a lazy streaming style, e.g. classic pipeline composition. The
-- default I\/O chunk size is 32k, which should be good in most circumstances.
--
-- Some operations, such as 'concat', 'append', 'reverse' and 'cons', have
-- better complexity than their "Data.ByteString" equivalents, due to
-- optimisations resulting from the list spine structure. For other
-- operations lazy ByteStrings are usually within a few percent of
-- strict ones.
--
-- The recomended way to assemble lazy ByteStrings from smaller parts
-- is to use the builder monoid from "Data.ByteString.Builder".
--
-- This module is intended to be imported @qualified@, to avoid name
-- clashes with "Prelude" functions.  eg.
--
-- > import qualified Data.ByteString.Lazy as B
--
-- Original GHC implementation by Bryan O\'Sullivan.
-- Rewritten to use 'Data.Array.Unboxed.UArray' by Simon Marlow.
-- Rewritten to support slices and use 'Foreign.ForeignPtr.ForeignPtr'
-- by David Roundy.
-- Rewritten again and extended by Don Stewart and Duncan Coutts.
-- Lazy variant by Duncan Coutts and Don Stewart.
--

module Data.ByteString.Lazy (
        -- * Lazy @ByteString@
        ByteString,
        LazyByteString,
        -- * Conversions with strict ByteString
        toStrict,
  ) where

import Prelude hiding
    (reverse,head,tail,last,init,Foldable(..),map,lines,unlines
    ,concat,any,take,drop,splitAt,takeWhile,dropWhile,span,break,filter
    ,all,concatMap,scanl, scanl1, scanr, scanr1
    ,repeat, cycle, interact, iterate,readFile,writeFile,appendFile,replicate
    ,getContents,getLine,putStr,putStrLn ,zip,zipWith,unzip,notElem)
import Data.ByteString.Lazy.Internal

