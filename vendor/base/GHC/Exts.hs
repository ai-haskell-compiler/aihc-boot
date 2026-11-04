{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

-- | The internals of GHC that the vendored packages use: the boxed types
-- with their constructors, the primitive types and their primops, and
-- some functions for the optimizer of GHC.
module GHC.Exts
  ( Int (..),
    Word (..),
    Char (..),
    Ptr (..),
    module GHC.Prim,
    SPEC (..),
    Levity (..),
    RuntimeRep (..),
    VecCount (..),
    VecElem (..),
    IsList (..),
    Down (..),
    isTrue#,
    realWorld#,
    lazy,
    inline,
    oneShot,
    build,
    augment,
    atomicModifyMutVar#,
    sameMutVar#,
    sameMVar#,
    tryTakeMVar#,
    tryPutMVar#,
    tryReadMVar#,
  )
where

import Data.Ord (Down (..))
import GHC.Base
import GHC.IsList
import GHC.Prim
import GHC.Ptr (Ptr (..))
import GHC.Types

-- | A comparison result of a primop as a Bool.
isTrue# :: Int# -> Bool
isTrue# 1# = True
isTrue# _ = False

-- | No-ops for the optimizer of GHC.
lazy :: a -> a
lazy x = x

inline :: a -> a
inline x = x

oneShot :: (a -> b) -> a -> b
oneShot f = f

-- | List fusion of GHC: a list from its fold.
build :: (forall b. (a -> b -> b) -> b -> b) -> [a]
build g = g (:) []

augment :: (forall b. (a -> b -> b) -> b -> b) -> [a] -> [a]
augment g xs = g (:) xs

-- | Apply a function to the value of a MutVar#, and give back the new
-- value and the result. The boot evaluator has one thread, so this is
-- atomic.
atomicModifyMutVar# :: MutVar# s a -> (a -> (a, b)) -> State# s -> (# State# s, b #)
atomicModifyMutVar# mv f s = case readMutVar# mv s of
  (# s1, x #) -> case f x of
    (x', b) -> case writeMutVar# mv x' s1 of
      s2 -> (# s2, b #)

sameMutVar# :: MutVar# s a -> MutVar# s a -> Int#
sameMutVar# = reallyUnsafePtrEquality#

sameMVar# :: MVar# s a -> MVar# s a -> Int#
sameMVar# = reallyUnsafePtrEquality#

tryTakeMVar# :: MVar# s a -> State# s -> (# State# s, Int#, a #)
tryTakeMVar# m s = case isEmptyMVar# m s of
  (# s1, 1# #) -> (# s1, 0#, unsafeCoerce# () #)
  (# s1, _ #) -> case takeMVar# m s1 of
    (# s2, a #) -> (# s2, 1#, a #)

tryPutMVar# :: MVar# s a -> a -> State# s -> (# State# s, Int# #)
tryPutMVar# m a s = case isEmptyMVar# m s of
  (# s1, 1# #) -> case putMVar# m a s1 of
    s2 -> (# s2, 1# #)
  (# s1, _ #) -> (# s1, 0# #)

tryReadMVar# :: MVar# s a -> State# s -> (# State# s, Int#, a #)
tryReadMVar# m s = case isEmptyMVar# m s of
  (# s1, 1# #) -> (# s1, 0#, unsafeCoerce# () #)
  (# s1, _ #) -> case readMVar# m s1 of
    (# s2, a #) -> (# s2, 1#, a #)
