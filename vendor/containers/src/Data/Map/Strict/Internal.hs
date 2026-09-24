{-# LANGUAGE BangPatterns #-}

{-# OPTIONS_HADDOCK not-home #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Map.Strict.Internal
-- Copyright   :  (c) Daan Leijen 2002
--                (c) Andriy Palamarchuk 2008
-- License     :  BSD-style
-- Maintainer  :  libraries@haskell.org
-- Portability :  portable
--
-- = WARNING
--
-- This module is considered __internal__.
--
-- = Description
--
-- An efficient implementation of ordered maps from keys to values
-- (dictionaries). The functions in this module are strict in the values.
--
-- aihc-boot keeps only the parts that the vendored tree uses.
-----------------------------------------------------------------------------

module Data.Map.Strict.Internal
    (
    -- * Map type
      Map(..)

    -- * Construction
    , empty
    , fromList

    -- * Insertion
    , insert

    -- * Deletion
    , delete

    -- * Query
    , lookup
    , member

    -- * Traversal
    , map

    -- * Folds
    , foldrWithKey
    ) where

import Prelude hiding (lookup, map)

import Data.Map.Internal
  ( Map (..)
  , balanceL
  , balanceR
  , delete
  , empty
  , foldrWithKey
  , insertMax
  , link
  , lookup
  , member
  , singleton
  )
import Data.Bits (shiftL, shiftR)
import qualified Data.Foldable as Foldable

{--------------------------------------------------------------------
  Insertion
--------------------------------------------------------------------}
-- See Map.Internal.Note: Type of local 'go' function
insert :: Ord k => k -> a -> Map k a -> Map k a
insert = go
  where
    go :: Ord k => k -> a -> Map k a -> Map k a
    go !kx !x Tip = singleton kx x
    go kx x (Bin sz ky y l r) =
        case compare kx ky of
            LT -> balanceL ky y (go kx x l) r
            GT -> balanceR ky y l (go kx x r)
            EQ -> Bin sz kx x l r
{-# INLINABLE insert #-}

{--------------------------------------------------------------------
  Mapping
--------------------------------------------------------------------}
map :: (a -> b) -> Map k a -> Map k b
map f = go
  where
    go Tip = Tip
    go (Bin sx kx x l r) = let !x' = f x in Bin sx kx x' (go l) (go r)
{-# NOINLINE [1] map #-}

{--------------------------------------------------------------------
  Lists
--------------------------------------------------------------------}
-- For some reason, when 'singleton' is used in fromList or in
-- create, it is not inlined, so we inline it manually.
fromList :: Ord k => [(k,a)] -> Map k a
fromList [] = Tip
fromList [(kx, x)] = x `seq` Bin 1 kx x Tip Tip
fromList ((kx0, x0) : xs0) | not_ordered kx0 xs0 = x0 `seq` fromList' (Bin 1 kx0 x0 Tip Tip) xs0
                           | otherwise = x0 `seq` go (1::Int) (Bin 1 kx0 x0 Tip Tip) xs0
  where
    not_ordered _ [] = False
    not_ordered kx ((ky,_) : _) = kx >= ky
    {-# INLINE not_ordered #-}

    fromList' t0 xs = Foldable.foldl' ins t0 xs
      where ins t (k,x) = insert k x t

    go !_ t [] = t
    go _ t [(kx, x)] = x `seq` insertMax kx x t
    go s l xs@((kx, x) : xss) | not_ordered kx xss = fromList' l xs
                              | otherwise = case create s xss of
                                  (r, ys, []) -> x `seq` go (s `shiftL` 1) (link kx x l r) ys
                                  (r, _,  ys) -> x `seq` fromList' (link kx x l r) ys

    -- The create is returning a triple (tree, xs, ys). Both xs and ys
    -- represent not yet processed elements and only one of them can be nonempty.
    -- If ys is nonempty, the keys in ys are not ordered with respect to tree
    -- and must be inserted using fromList'. Otherwise the keys have been
    -- ordered so far.
    create !_ [] = (Tip, [], [])
    create s xs@(xp : xss)
      | s == 1 = case xp of (kx, x) | not_ordered kx xss -> x `seq` (Bin 1 kx x Tip Tip, [], xss)
                                    | otherwise -> x `seq` (Bin 1 kx x Tip Tip, xss, [])
      | otherwise = case create (s `shiftR` 1) xs of
                      res@(_, [], _) -> res
                      (l, [(ky, y)], zs) -> y `seq` (insertMax ky y l, [], zs)
                      (l, ys@((ky, y):yss), _) | not_ordered ky yss -> (l, [], ys)
                                               | otherwise -> case create (s `shiftR` 1) yss of
                                                   (r, zs, ws) -> y `seq` (link ky y l r, zs, ws)
{-# INLINABLE fromList #-}

