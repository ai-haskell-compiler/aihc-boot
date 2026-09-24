{-# OPTIONS_HADDOCK prune #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
  -- We use the deprecated Data.ByteString.{hGetLine,getLine} to
  -- define the not-deprecated Char8 versions of the same functions.

-- |
-- Module      : Data.ByteString.Char8
-- Copyright   : (c) Don Stewart 2006-2008
--               (c) Duncan Coutts 2006-2011
-- License     : BSD-style
--
-- Maintainer  : dons00@gmail.com, duncan@community.haskell.org
-- Stability   : stable
-- Portability : portable
--
-- Manipulate 'ByteString's using 'Char' operations. All Chars will be
-- truncated to 8 bits. It can be expected that these functions will run
-- at identical speeds to their 'Word8' equivalents in "Data.ByteString".
--
-- More specifically these byte strings are taken to be in the
-- subset of Unicode covered by code points 0-255. This covers
-- Unicode Basic Latin, Latin-1 Supplement and C0+C1 Controls.
--
-- See:
--
--  * <http://www.unicode.org/charts/>
--
--  * <http://www.unicode.org/charts/PDF/U0000.pdf>
--
--  * <http://www.unicode.org/charts/PDF/U0080.pdf>
--
-- This module is intended to be imported @qualified@, to avoid name
-- clashes with "Prelude" functions.  eg.
--
-- > import qualified Data.ByteString.Char8 as C
--
-- The Char8 interface to bytestrings provides an instance of IsString
-- for the ByteString type, enabling you to use string literals, and
-- have them implicitly packed to ByteStrings.
-- Use @{-\# LANGUAGE OverloadedStrings \#-}@ to enable this.
--

module Data.ByteString.Char8 (
        -- * The @ByteString@ type
        ByteString,
    singleton,
    pack,
    unpack,
    cons,
    snoc,
    uncons,
    concatMap,
    all,
    replicate,
    takeWhile,
    takeWhileEnd,
    dropWhile,
    dropWhileEnd,
    span,
    break,
    split,
    splitWith,
    elemIndexEnd,
        -- * Re-exports from "Data.ByteString"
        null, length, concat, intercalate, isPrefixOf, isSuffixOf, take, drop,
        -- * Reading from ByteStrings
        readInt, readInteger,
  ) where

import Prelude hiding           (reverse,head,tail,last,init,Foldable(..)
                                ,map,lines,unlines
                                ,concat,any,take,drop,splitAt,takeWhile
                                ,dropWhile,span,break,filter,unwords
                                ,words,all,concatMap
                                ,scanl,scanl1,scanr,scanr1
                                ,appendFile,readFile,writeFile
                                ,replicate
                                ,getContents,getLine,putStr,putStrLn,interact
                                ,zip,zipWith,unzip,notElem)
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import Data.ByteString (null,length,concat,take,drop,intercalate,isPrefixOf,isSuffixOf)
import Data.ByteString.Internal.Type
import Data.ByteString.ReadInt
import Data.ByteString.ReadNat

-- | /O(1)/ Convert a 'Char' into a 'ByteString'
singleton :: Char -> ByteString
singleton = B.singleton . c2w
{-# INLINE singleton #-}

-- | /O(n)/ Convert a 'String' into a 'ByteString'
--
-- For applications with large numbers of string literals, pack can be a
-- bottleneck.
pack :: String -> ByteString
pack = packChars
{-# INLINE pack #-}

-- | /O(n)/ Converts a 'ByteString' to a 'String'.
unpack :: ByteString -> [Char]
unpack = B.unpackChars
{-# INLINE unpack #-}

-- | /O(n)/ 'cons' is analogous to (:) for lists, but of different
-- complexity, as it requires a memcpy.
cons :: Char -> ByteString -> ByteString
cons = B.cons . c2w
{-# INLINE cons #-}

-- | /O(n)/ Append a Char to the end of a 'ByteString'. Similar to
-- 'cons', this function performs a memcpy.
snoc :: ByteString -> Char -> ByteString
snoc p = B.snoc p . c2w
{-# INLINE snoc #-}

-- | /O(1)/ Extract the head and tail of a ByteString, returning Nothing
-- if it is empty.
uncons :: ByteString -> Maybe (Char, ByteString)
uncons bs = case B.uncons bs of
                  Nothing -> Nothing
                  Just (w, bs') -> Just (w2c w, bs')
{-# INLINE uncons #-}

-- | Map a function over a 'ByteString' and concatenate the results
concatMap :: (Char -> ByteString) -> ByteString -> ByteString
concatMap f = B.concatMap (f . w2c)
{-# INLINE concatMap #-}

-- | Applied to a predicate and a 'ByteString', 'all' determines if
-- all elements of the 'ByteString' satisfy the predicate.
all :: (Char -> Bool) -> ByteString -> Bool
all f = B.all (f . w2c)
{-# INLINE all #-}

-- | /O(n)/ 'replicate' @n x@ is a ByteString of length @n@ with @x@
-- the value of every element. The following holds:
--
-- > replicate w c = unfoldr w (\u -> Just (u,u)) c
--
-- This implementation uses @memset(3)@
replicate :: Int -> Char -> ByteString
replicate n = B.replicate n . c2w
{-# INLINE replicate #-}

-- | 'takeWhile', applied to a predicate @p@ and a ByteString @xs@,
-- returns the longest prefix (possibly empty) of @xs@ of elements that
-- satisfy @p@.
takeWhile :: (Char -> Bool) -> ByteString -> ByteString
takeWhile f = B.takeWhile (f . w2c)
{-# INLINE takeWhile #-}

-- | 'takeWhileEnd', applied to a predicate @p@ and a ByteString @xs@,
-- returns the longest suffix (possibly empty) of @xs@ of elements that
-- satisfy @p@.
--
-- @since 0.10.12.0
takeWhileEnd :: (Char -> Bool) -> ByteString -> ByteString
takeWhileEnd f = B.takeWhileEnd (f . w2c)
{-# INLINE takeWhileEnd #-}

-- | 'dropWhile' @p xs@ returns the suffix remaining after 'takeWhile' @p xs@.
dropWhile :: (Char -> Bool) -> ByteString -> ByteString
dropWhile f = B.dropWhile (f . w2c)
{-# INLINE [1] dropWhile #-}

-- | 'dropWhileEnd' @p xs@ returns the prefix remaining after 'takeWhileEnd' @p
-- xs@.
--
-- @since 0.10.12.0
dropWhileEnd :: (Char -> Bool) -> ByteString -> ByteString
dropWhileEnd f = B.dropWhileEnd (f . w2c)
{-# INLINE dropWhileEnd #-}

-- | 'break' @p@ is equivalent to @'span' ('not' . p)@.
break :: (Char -> Bool) -> ByteString -> (ByteString, ByteString)
break f = B.break (f . w2c)
{-# INLINE [1] break #-}

-- | 'span' @p xs@ breaks the ByteString into two segments. It is
-- equivalent to @('takeWhile' p xs, 'dropWhile' p xs)@
span :: (Char -> Bool) -> ByteString -> (ByteString, ByteString)
span f = B.span (f . w2c)
{-# INLINE span #-}

-- | /O(n)/ Break a 'ByteString' into pieces separated by the byte
-- argument, consuming the delimiter. I.e.
--
-- > split '\n' "a\nb\nd\ne" == ["a","b","d","e"]
-- > split 'a'  "aXaXaXa"    == ["","X","X","X",""]
-- > split 'x'  "x"          == ["",""]
-- > split undefined ""      == []  -- and not [""]
--
-- and
--
-- > intercalate [c] . split c == id
-- > split == splitWith . (==)
--
-- As for all splitting functions in this library, this function does
-- not copy the substrings, it just constructs new 'ByteString's that
-- are slices of the original.
--
split :: Char -> ByteString -> [ByteString]
split = B.split . c2w
{-# INLINE split #-}

-- | /O(n)/ Splits a 'ByteString' into components delimited by
-- separators, where the predicate returns True for a separator element.
-- The resulting components do not contain the separators.  Two adjacent
-- separators result in an empty component in the output.  eg.
--
-- > splitWith (=='a') "aabbaca" == ["","","bb","c",""]
-- > splitWith undefined ""      == []  -- and not [""]
--
splitWith :: (Char -> Bool) -> ByteString -> [ByteString]
splitWith f = B.splitWith (f . w2c)
{-# INLINE splitWith #-}

-- | /O(n)/ The 'elemIndexEnd' function returns the last index of the
-- element in the given 'ByteString' which is equal to the query
-- element, or 'Nothing' if there is no such element. The following
-- holds:
--
-- > elemIndexEnd c xs = case elemIndex c (reverse xs) of
-- >   Nothing -> Nothing
-- >   Just i  -> Just (length xs - 1 - i)
--
elemIndexEnd :: Char -> ByteString -> Maybe Int
elemIndexEnd = B.elemIndexEnd . c2w
{-# INLINE elemIndexEnd #-}

