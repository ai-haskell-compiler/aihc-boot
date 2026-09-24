-- |
-- Module      : Data.Text.Encoding.Error
-- Copyright   : (c) Bryan O'Sullivan 2009
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Portability : GHC
--
-- Types and functions for dealing with encoding and decoding errors
-- in Unicode text.
--
-- The standard functions for encoding and decoding text are strict,
-- which is to say that they throw exceptions on invalid input.  This
-- is often unhelpful on real world input, so alternative functions
-- exist that accept custom handlers for dealing with invalid inputs.
-- These 'OnError' handlers are normal Haskell functions.  You can use
-- one of the presupplied functions in this module, or you can write a
-- custom handler of your own.

module Data.Text.Encoding.Error (
    OnError,
    OnDecodeError,
    lenientDecode,
  ) where

import Data.Word (Word8)

-- | Function type for handling a coding error.  It is supplied with
-- two inputs:
--
-- * A 'String' that describes the error.
--
-- * The input value that caused the error.  If the error arose
--   because the end of input was reached or could not be identified
--   precisely, this value will be 'Nothing'.
--
-- If the handler returns a value wrapped with 'Just', that value will
-- be used in the output as the replacement for the invalid input.  If
-- it returns 'Nothing', no value will be used in the output.
--
-- Should the handler need to abort processing, it should use 'error'
-- or 'throw' an exception (preferably a 'UnicodeException').  It may
-- use the description provided to construct a more helpful error
-- report.
type OnError a b = String -> Maybe a -> Maybe b

-- | A handler for a decoding error.
type OnDecodeError = OnError Word8 Char

-- | Replace an invalid input byte with the Unicode replacement
-- character U+FFFD.
lenientDecode :: OnDecodeError
lenientDecode _ _ = Just '\xfffd'

