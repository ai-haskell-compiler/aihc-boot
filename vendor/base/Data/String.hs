{-# LANGUAGE FlexibleInstances #-}

-- | The 'IsString' class: a string literal with OverloadedStrings
-- desugars to 'fromString'.
module Data.String
  ( String,
    IsString (..),
    lines,
    words,
    unlines,
    unwords,
  )
where

import Data.List (lines, unlines, unwords, words)
import GHC.Base
import GHC.Types

class IsString a where
  fromString :: String -> a

instance IsString [Char] where
  fromString xs = xs
