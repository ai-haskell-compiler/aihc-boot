-- | Conversion of values to strings.
module Text.Show
  ( ShowS,
    Show (..),
    shows,
    showChar,
    showString,
    showParen,
    showListWith,
  )
where

import GHC.Base ((++))
import GHC.Show

-- | Show a list with the given function for its elements.
showListWith :: (a -> ShowS) -> [a] -> ShowS
showListWith _ [] s = "[]" ++ s
showListWith showx (x : xs) s = '[' : showx x (showl xs)
  where
    showl [] = ']' : s
    showl (y : ys) = ',' : showx y (showl ys)
