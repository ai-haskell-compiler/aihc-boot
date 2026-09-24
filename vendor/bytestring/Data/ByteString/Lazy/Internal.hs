{-# OPTIONS_HADDOCK not-home #-}

-- |
-- Module      : Data.ByteString.Lazy.Internal
-- Copyright   : (c) Don Stewart 2006-2008
--               (c) Duncan Coutts 2006-2011
-- License     : BSD-style
-- Maintainer  : dons00@gmail.com, duncan@community.haskell.org
-- Stability   : unstable
-- Portability : non-portable
--
-- A module containing semi-public 'ByteString' internals. This exposes
-- the 'ByteString' representation and low level construction functions.
-- Modules which extend the 'ByteString' system will need to use this module
-- while ideally most users will be able to make do with the public interface
-- modules.
--
module Data.ByteString.Lazy.Internal (
    ByteString(Empty, Chunk),
    LazyByteString,
    defaultChunkSize,
    smallChunkSize,
    chunkOverhead,
    toStrict,
  ) where

import Prelude hiding (concat)
import qualified Data.ByteString.Internal.Type as S
import Foreign.Storable (Storable(sizeOf))

data ByteString = Empty | Chunk  {-# UNPACK #-} !S.StrictByteString ByteString
  -- INVARIANT: The S.StrictByteString field of any Chunk is not empty.
  -- (See also the 'invariant' and 'checkInvariant' functions.)

  -- To make testing of this invariant convenient, we add an
  -- assertion to that effect when the HS_BYTESTRING_ASSERTIONS
  -- preprocessor macro is defined, by renaming the actual constructor
  -- and providing a pattern synonym that does the checking:

-- | Type synonym for the lazy flavour of 'ByteString'.
--
-- @since 0.11.2.0
type LazyByteString = ByteString

-- | The chunk size used for I\/O. Currently set to 32k, less the memory management overhead
defaultChunkSize :: Int
defaultChunkSize = 32 * k - chunkOverhead
   where k = 1024

-- | The recommended chunk size. Currently set to 4k, less the memory management overhead
smallChunkSize :: Int
smallChunkSize = 4 * k - chunkOverhead
   where k = 1024

-- | The memory management overhead. Currently this is tuned for GHC only.
chunkOverhead :: Int
chunkOverhead = 2 * sizeOf (undefined :: Int)

-- |/O(n)/ Convert a 'LazyByteString' into a 'S.StrictByteString'.
--
-- Note that this is an /expensive/ operation that forces the whole
-- 'LazyByteString' into memory and then copies all the data. If possible, try to
-- avoid converting back and forth between strict and lazy bytestrings.
--
toStrict :: LazyByteString -> S.StrictByteString
toStrict = \cs -> goLen0 cs cs
    -- We pass the original [ByteString] (bss0) through as an argument through
    -- goLen0, goLen1, and goLen since we will need it again in goCopy. Passing
    -- it as an explicit argument avoids capturing it in these functions'
    -- closures which would result in unnecessary closure allocation.
  where
    -- It's still possible that the result is empty
    goLen0 _   Empty                 = S.BS S.nullForeignPtr 0
    goLen0 cs0 (Chunk c cs)          = goLen1 cs0 c cs

    -- It's still possible that the result is a single chunk
    goLen1 _   bs Empty = bs
    goLen1 cs0 (S.BS _ bl) (Chunk (S.BS _ cl) cs) =
        goLen cs0 (S.checkedAdd "Lazy.toStrict" bl cl) cs

    -- General case, just find the total length we'll need
    goLen cs0 !total (Chunk (S.BS _ cl) cs) =
      goLen cs0 (S.checkedAdd "Lazy.toStrict" total cl) cs
    goLen cs0 total Empty =
      S.unsafeCreateFp total $ \ptr -> goCopy cs0 ptr

    -- Copy the data
    goCopy Empty                    !_   = return ()
    goCopy (Chunk (S.BS fp len) cs) !ptr = do
      S.memcpyFp ptr fp len
      goCopy cs (ptr `S.plusForeignPtr` len)

