{-# LANGUAGE CPP #-}
module Data.Foo.Bar (a, B, C(..), D(E, f), (+), module M, pattern P, type (:+), S(.., P)) where
import A
import qualified B as C
import D qualified as E
import F (x, Y(..), Z(a, B), (<|>), pattern P, type (+))
import G hiding (null)
import {-# SOURCE #-} H
import "base" Prelude
import A (as, hiding, qualified, pattern)
