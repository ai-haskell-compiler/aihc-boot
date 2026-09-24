

-- |
-- Module      : Data.ByteString.Utils.UnalignedAccess
-- Copyright   : (c) Matthew Craven 2023-2024
-- License     : BSD-style
-- Maintainer  : clyring@gmail.com
-- Stability   : internal
-- Portability : non-portable
--
-- Primitives for reading and writing at potentially-unaligned memory locations

module Data.ByteString.Utils.UnalignedAccess (
    unalignedReadU64,
  ) where

import Foreign.Ptr
import Data.Word
import GHC.IO (IO(..))
import GHC.Word (Word64(..))
import GHC.Exts

unalignedReadU64 :: Ptr Word8 -> IO Word64
unalignedReadU64 = coerce $ \(Ptr p#) s
  -> case readWord8OffAddrAsWord64# p# 0# s of
       (# s', w64# #) -> (# s', W64# w64# #)

