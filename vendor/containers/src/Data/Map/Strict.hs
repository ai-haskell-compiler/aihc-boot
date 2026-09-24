-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Map.Strict
-- Copyright   :  (c) Daan Leijen 2002
--                (c) Andriy Palamarchuk 2008
-- License     :  BSD-style
-- Maintainer  :  libraries@haskell.org
-- Portability :  portable
--
--
-- = Finite Maps (strict interface)
--
-- The @'Map' k v@ type represents a finite map (sometimes called a dictionary)
-- from keys of type @k@ to values of type @v@.
--
-- Each function in this module is careful to force values before installing
-- them in a 'Map'.
--
-- aihc-boot keeps only the parts that the vendored tree uses.
-----------------------------------------------------------------------------

module Data.Map.Strict
    (
    -- * Map type
    Map

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

import Data.Map.Strict.Internal
import Prelude ()
