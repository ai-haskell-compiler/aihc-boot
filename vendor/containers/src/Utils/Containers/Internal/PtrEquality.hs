{-# LANGUAGE MagicHash #-}

{-# OPTIONS_HADDOCK hide #-}

-- | Really unsafe pointer equality
module Utils.Containers.Internal.PtrEquality (ptrEq) where

import GHC.Exts ( reallyUnsafePtrEquality#, isTrue# )

-- | Checks if two pointers are equal. Yes means yes;
-- no means maybe. The values should be forced to at least
-- WHNF before comparison to get moderately reliable results.
ptrEq :: a -> a -> Bool
ptrEq x y = isTrue# (reallyUnsafePtrEquality# x y)
{-# INLINE ptrEq #-}

infix 4 `ptrEq`
