{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

-----------------------------------------------------------------------------

-- |
-- Module      :  Control.DeepSeq
-- Copyright   :  (c) The University of Glasgow 2001-2009
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
-- Portability :  portable
--
-- This module provides overloaded functions, such as 'deepseq' and
-- 'rnf', for fully evaluating data structures (that is, evaluating to
-- \"Normal Form\").
--
-- aihc-boot keeps only the parts that the vendored tree uses. The
-- generic default of 'rnf' has no arity parameter, because the tree
-- does not use 'NFData1'.
module Control.DeepSeq (
  -- * 'NFData' class
  NFData (rnf),

  -- * Helper functions
  rwhnf,
) where

import GHC.Generics

-- | Hidden internal type-class
class GNFData f where
  grnf :: f a -> ()

instance GNFData U1 where
  grnf U1 = ()

instance NFData a => GNFData (K1 i a) where
  grnf = rnf . unK1
  {-# INLINEABLE grnf #-}

instance GNFData a => GNFData (M1 i c a) where
  grnf = grnf . unM1
  {-# INLINEABLE grnf #-}

instance (GNFData a, GNFData b) => GNFData (a :*: b) where
  grnf (x :*: y) = grnf x `seq` grnf y
  {-# INLINEABLE grnf #-}

instance (GNFData a, GNFData b) => GNFData (a :+: b) where
  grnf (L1 x) = grnf x
  grnf (R1 x) = grnf x
  {-# INLINEABLE grnf #-}

-- | Reduce to weak head normal form
--
-- Equivalent to @\\x -> 'seq' x ()@.
--
-- Useful for defining 'NFData' for types for which NF=WHNF holds.
--
-- > data T = C1 | C2 | C3
-- > instance NFData T where rnf = rwhnf
--
-- @since 1.4.3.0
rwhnf :: a -> ()
rwhnf = (`seq` ())
{-# INLINE rwhnf #-}

-- | A class of types that can be fully evaluated.
--
-- @since 1.1.0.0
class NFData a where
  -- | 'rnf' should reduce its argument to normal form (that is, fully
  -- evaluate all sub-components), and then return '()'.
  rnf :: a -> ()
  default rnf :: (Generic a, GNFData (Rep a)) => a -> ()
  rnf = grnf . from

instance NFData Int where rnf = rwhnf

instance NFData Char where rnf = rwhnf

instance NFData a => NFData [a] where
  rnf = foldr (\x r -> rnf x `seq` r) ()
  {-# INLINABLE rnf #-}
