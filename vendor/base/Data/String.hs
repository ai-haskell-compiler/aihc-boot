{-# LANGUAGE TypeFamilies #-}

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

class IsString a where
  fromString :: String -> a

instance (a ~ Char) => IsString [a] where
  fromString xs = xs
