-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Set
-- Copyright   :  (c) Daan Leijen 2002
-- License     :  BSD-style
-- Maintainer  :  libraries@haskell.org
-- Portability :  portable
--
--
-- = Finite Sets
--
-- The @'Set' e@ type represents a set of elements of type @e@.
--
-- aihc-boot keeps only the parts that the vendored tree uses.
-----------------------------------------------------------------------------

module Data.Set (
            -- * Set type
              Set

            -- * Construction
            , empty
            , insert

            -- * Query
            , member
            ) where

import Data.Set.Internal
