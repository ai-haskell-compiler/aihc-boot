{-# LANGUAGE CPP #-}
-- The CPP step: GHC's version macros, MIN_VERSION_base and the line
-- numbers of the module after the directives.
module Cpp where

#if __GLASGOW_HASKELL__ >= 912
ghc :: Int
ghc = 912
#else
ghc :: Int
ghc = 0
#endif

#if MIN_VERSION_base(4,21,0) && !MIN_VERSION_base(5,0,0)
base :: Bool
base = True
#endif

#define TWICE(x) (x + x)
four :: Int
four = TWICE(2)
