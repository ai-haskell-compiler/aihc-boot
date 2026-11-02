-- | Character functions, for ASCII.
module Data.Char
  ( ord,
    chr,
    isDigit,
    isSpace,
    isUpper,
    isLower,
    isAlpha,
    isAlphaNum,
    toUpper,
    toLower,
    digitToInt,
    intToDigit,
  )
where

import GHC.Classes
import GHC.Num
import GHC.Prim
import GHC.Types

ord :: Char -> Int
ord = primOrd

chr :: Int -> Char
chr = primChr

isDigit :: Char -> Bool
isDigit c = geChar c '0' && leChar c '9'

isSpace :: Char -> Bool
isSpace c = eqChar c ' ' || (geChar c '\t' && leChar c '\r')

isUpper :: Char -> Bool
isUpper c = geChar c 'A' && leChar c 'Z'

isLower :: Char -> Bool
isLower c = geChar c 'a' && leChar c 'z'

isAlpha :: Char -> Bool
isAlpha c = isUpper c || isLower c

isAlphaNum :: Char -> Bool
isAlphaNum c = isAlpha c || isDigit c

toUpper :: Char -> Char
toUpper c = if isLower c then primChr (primOrd c - 32) else c

toLower :: Char -> Char
toLower c = if isUpper c then primChr (primOrd c + 32) else c

digitToInt :: Char -> Int
digitToInt c
  | isDigit c = primOrd c - primOrd '0'
  | geChar c 'a' && leChar c 'f' = primOrd c - primOrd 'a' + 10
  | geChar c 'A' && leChar c 'F' = primOrd c - primOrd 'A' + 10
  | True = error "Char.digitToInt: not a digit"

intToDigit :: Int -> Char
intToDigit i
  | i >= 0 && i <= 9 = primChr (primOrd '0' + i)
  | i >= 10 && i <= 15 = primChr (primOrd 'a' + i - 10)
  | True = error "Char.intToDigit: not a digit"
