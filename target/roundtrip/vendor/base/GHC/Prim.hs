{-# LANGUAGE BangPatterns #-}
module GHC.Prim (seq) where {
infixr 0 `seq`;
seq :: a -> b -> b;
seq !_ b = b
}
