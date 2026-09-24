{-# OPTIONS_HADDOCK not-home #-}

-- | Implements 'isAscii', using efficient C routines by default.
--
-- Similarly implements asciiPrefixLength, used internally in Data.Text.Encoding.
module Data.Text.Internal.IsAscii (
  ) where

import Prelude hiding (all)
import Data.Text.Unsafe ()
import Data.Text.Internal ()

