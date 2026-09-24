{-# OPTIONS_HADDOCK not-home #-}

-- |
-- Module      : Data.Text.Internal.Unsafe
-- Copyright   : (c) 2009, 2010, 2011 Bryan O'Sullivan
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
-- /Warning/: this is an internal module, and does not have a stable
-- API or name. Functions in this module may not check or enforce
-- preconditions expected by public modules. Use at your own risk!
--
-- A module containing /unsafe/ operations, for /very very careful/ use
-- in /heavily tested/ code.
module Data.Text.Internal.Unsafe (
    unsafeWithForeignPtr,
  ) where

import Foreign.Ptr (Ptr)
import Foreign.ForeignPtr (ForeignPtr)
import qualified GHC.ForeignPtr (unsafeWithForeignPtr)

unsafeWithForeignPtr :: ForeignPtr a -> (Ptr a -> IO b) -> IO b
unsafeWithForeignPtr = GHC.ForeignPtr.unsafeWithForeignPtr

