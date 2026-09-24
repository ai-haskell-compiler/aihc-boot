-- |
-- Module      : Data.Text.Encoding
-- Copyright   : (c) 2009, 2010, 2011 Bryan O'Sullivan,
--               (c) 2009 Duncan Coutts,
--               (c) 2008, 2009 Tom Harper
--               (c) 2021 Andrew Lelechenko
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Portability : portable
--
-- Functions for converting 'Text' values to and from 'ByteString',
-- using several standard encodings.
--
-- To gain access to a much larger family of encodings, use the
-- <http://hackage.haskell.org/package/text-icu text-icu package>.

module Data.Text.Encoding (
    decodeUtf8With,
  ) where

import Data.ByteString (ByteString)
import Data.Text.Internal ()
import Data.Text.Encoding.Error (OnDecodeError)
import Data.Text.Internal (Text(..))
import Data.Text.Internal.Encoding
import Data.Text.Internal.IsAscii ()
import Data.Text.Unsafe ()
import Data.Text.Show ()

-- | Decode a 'ByteString' containing UTF-8 encoded text.
--
-- Surrogate code points in replacement character returned by 'OnDecodeError'
-- will be automatically remapped to the replacement char @U+FFFD@.
decodeUtf8With ::

  OnDecodeError -> ByteString -> Text
decodeUtf8With onErr = decodeUtf8With1 onErr invalidUtf8Msg

invalidUtf8Msg :: String
invalidUtf8Msg = "Data.Text.Encoding: Invalid UTF-8 stream"

