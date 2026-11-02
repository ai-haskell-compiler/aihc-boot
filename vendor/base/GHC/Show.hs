-- | Showing values. Only Int for now: the boot base has no classes yet.
module GHC.Show
  ( ShowS,
    show,
    shows,
    showString,
    showChar,
    showParen,
  )
where

import GHC.Base
import GHC.Prim
import GHC.Types

type ShowS = String -> String

show :: Int -> String
show = primIntShow

shows :: Int -> ShowS
shows n s = primIntShow n ++ s

showString :: String -> ShowS
showString = (++)

showChar :: Char -> ShowS
showChar = (:)

showParen :: Bool -> ShowS -> ShowS
showParen b p = if b then showChar '(' . p . showChar ')' else p
