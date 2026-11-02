{-# LANGUAGE MagicHash #-}
module Utils.Containers.Internal.PtrEquality (ptrEq) where {
import GHC.Exts (reallyUnsafePtrEquality#, isTrue#);
ptrEq :: a -> a -> Bool;
ptrEq x y = isTrue# (reallyUnsafePtrEquality# x y);
infix 4 `ptrEq`
}
