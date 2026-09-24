{- | Copyright : (c) 2010-2011 Simon Meier
                 (c) 2010      Jasper van der Jeugt
License        : BSD3-style (see LICENSE)
Maintainer     : Simon Meier <iridcode@gmail.com>
Portability    : GHC

This module provides 'Builder' /primitives/, which are lower level building
blocks for constructing 'Builder's. You don't need to go down to this level but
it can be slightly faster.

aihc-boot keeps only the parts that the vendored tree uses.
-}

module Data.ByteString.Builder.Prim (
    primBounded,
    primMapListBounded,
    primFixed,
    primMapListFixed,
    char8,
  ) where

import           Data.ByteString.Builder.Internal
import           Data.Char (ord)
import           Data.ByteString.Builder.Prim.Internal hiding (sizeBound)
import qualified Data.ByteString.Builder.Prim.Internal as I
import           Data.ByteString.Builder.Prim.Binary
import           Foreign

{-# INLINE primFixed #-}
-- | Encode a value with a 'FixedPrim'.
primFixed :: FixedPrim a -> (a -> Builder)
primFixed = primBounded . toB

{-# INLINE primMapListFixed #-}
-- | Encode a list of values from left-to-right with a 'FixedPrim'.
primMapListFixed :: FixedPrim a -> ([a] -> Builder)
primMapListFixed = primMapListBounded . toB

-- | Create a 'Builder' that encodes values with the given 'BoundedPrim'.
--
-- We rewrite consecutive uses of 'primBounded' such that the bound-checks are
-- fused. For example,
--
-- > primBounded (word32 c1) `mappend` primBounded (word32 c2)
--
-- is rewritten such that the resulting 'Builder' checks only once, if ther are
-- at 8 free bytes, instead of checking twice, if there are 4 free bytes. This
-- optimization is not observationally equivalent in a strict sense, as it
-- influences the boundaries of the generated chunks. However, for a user of
-- this library it is observationally equivalent, as chunk boundaries of a
-- 'L.LazyByteString' can only be observed through the internal interface.
-- Moreover, we expect that all primitives write much fewer than 4kb (the
-- default short buffer size). Hence, it is safe to ignore the additional
-- memory spilled due to the more aggressive buffer wrapping introduced by this
-- optimization.
--
primBounded :: BoundedPrim a -> (a -> Builder)
primBounded w x =
    -- It is important to avoid recursive 'BuildStep's where possible, as
    -- their closure allocation is expensive. Using 'ensureFree' allows the
    -- 'step' to assume that at least 'sizeBound w' free space is available.
    ensureFree (I.sizeBound w) `mappend` builder step
  where
    step k (BufferRange op ope) = do
        op' <- runB w x op
        let !br' = BufferRange op' ope
        k br'

{-# INLINE primMapListBounded #-}
-- | Create a 'Builder' that encodes a list of values consecutively using a
-- 'BoundedPrim' for each element. This function is more efficient than
--
-- > mconcat . map (primBounded w)
--
-- or
--
-- > foldMap (primBounded w)
--
-- because it moves several variables out of the inner loop.
primMapListBounded :: BoundedPrim a -> [a] -> Builder
primMapListBounded w xs0 =
    builder $ step xs0
  where
    step xs1 k (BufferRange op0 ope0) =
        go xs1 op0
      where
        go []          !op             = k (BufferRange op ope0)
        go xs@(x':xs') !op
          | op `plusPtr` bound <= ope0 = runB w x' op >>= go xs'
          | otherwise                  =
             return $ bufferFull bound op (step xs k)

    bound = I.sizeBound w

{-# INLINE char8 #-}
-- | Char8 encode a 'Char'.
char8 :: FixedPrim Char
char8 = (fromIntegral . ord) >$< word8

