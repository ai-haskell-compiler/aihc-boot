-- | Character functions. The classes outside ASCII come from the Unicode
-- general category of each character, as in GHC's base.
module Data.Char
  ( Char,
    String,
    ord,
    chr,
    GeneralCategory (..),
    generalCategory,
    isAscii,
    isLatin1,
    isControl,
    isPrint,
    isSpace,
    isUpper,
    isLower,
    isAlpha,
    isLetter,
    isDigit,
    isOctDigit,
    isHexDigit,
    isAlphaNum,
    isNumber,
    isMark,
    isPunctuation,
    isSymbol,
    isSeparator,
    isAsciiUpper,
    isAsciiLower,
    toUpper,
    toLower,
    digitToInt,
    intToDigit,
  )
where

import GHC.Base
import GHC.Classes
import GHC.Enum
import GHC.Err
import GHC.Num
import GHC.Prim
import GHC.Show
import GHC.Types

-- | The Unicode general categories, in the order of the Unicode standard.
data GeneralCategory
  = UppercaseLetter
  | LowercaseLetter
  | TitlecaseLetter
  | ModifierLetter
  | OtherLetter
  | NonSpacingMark
  | SpacingCombiningMark
  | EnclosingMark
  | DecimalNumber
  | LetterNumber
  | OtherNumber
  | ConnectorPunctuation
  | DashPunctuation
  | OpenPunctuation
  | ClosePunctuation
  | InitialQuote
  | FinalQuote
  | OtherPunctuation
  | MathSymbol
  | CurrencySymbol
  | ModifierSymbol
  | OtherSymbol
  | Space
  | LineSeparator
  | ParagraphSeparator
  | Control
  | Format
  | Surrogate
  | PrivateUse
  | NotAssigned
  deriving (Show, Eq, Ord, Enum, Bounded)

ord :: Char -> Int
ord = primOrd

chr :: Int -> Char
chr = primChr

-- | The general category of a character, from the Unicode tables of the
-- evaluator.
generalCategory :: Char -> GeneralCategory
generalCategory c = toEnum (primGeneralCategory c)

isAscii :: Char -> Bool
isAscii c = c < '\x80'

isLatin1 :: Char -> Bool
isLatin1 c = c <= '\xff'

isAsciiUpper :: Char -> Bool
isAsciiUpper c = c >= 'A' && c <= 'Z'

isAsciiLower :: Char -> Bool
isAsciiLower c = c >= 'a' && c <= 'z'

isControl :: Char -> Bool
isControl c = generalCategory c == Control

isPrint :: Char -> Bool
isPrint c = case generalCategory c of
  LineSeparator -> False
  ParagraphSeparator -> False
  Control -> False
  Format -> False
  Surrogate -> False
  PrivateUse -> False
  NotAssigned -> False
  _ -> True

isSpace :: Char -> Bool
isSpace c
  | c == ' ' || (c >= '\t' && c <= '\r') || c == '\xa0' = True
  | isAscii c = False
  | True = generalCategory c == Space

isUpper :: Char -> Bool
isUpper c
  | isAscii c = isAsciiUpper c
  | True = case generalCategory c of
      UppercaseLetter -> True
      TitlecaseLetter -> True
      _ -> False

isLower :: Char -> Bool
isLower c
  | isAscii c = isAsciiLower c
  | True = generalCategory c == LowercaseLetter

isAlpha :: Char -> Bool
isAlpha c
  | isAscii c = isAsciiUpper c || isAsciiLower c
  | True = case generalCategory c of
      UppercaseLetter -> True
      LowercaseLetter -> True
      TitlecaseLetter -> True
      ModifierLetter -> True
      OtherLetter -> True
      _ -> False

isLetter :: Char -> Bool
isLetter = isAlpha

isDigit :: Char -> Bool
isDigit c = c >= '0' && c <= '9'

isOctDigit :: Char -> Bool
isOctDigit c = c >= '0' && c <= '7'

isHexDigit :: Char -> Bool
isHexDigit c = isDigit c || (c >= 'A' && c <= 'F') || (c >= 'a' && c <= 'f')

isAlphaNum :: Char -> Bool
isAlphaNum c
  | isAscii c = isAlpha c || isDigit c
  | True = isAlpha c || isNumber c

isNumber :: Char -> Bool
isNumber c = case generalCategory c of
  DecimalNumber -> True
  LetterNumber -> True
  OtherNumber -> True
  _ -> False

isMark :: Char -> Bool
isMark c = case generalCategory c of
  NonSpacingMark -> True
  SpacingCombiningMark -> True
  EnclosingMark -> True
  _ -> False

isPunctuation :: Char -> Bool
isPunctuation c = case generalCategory c of
  ConnectorPunctuation -> True
  DashPunctuation -> True
  OpenPunctuation -> True
  ClosePunctuation -> True
  InitialQuote -> True
  FinalQuote -> True
  OtherPunctuation -> True
  _ -> False

isSymbol :: Char -> Bool
isSymbol c = case generalCategory c of
  MathSymbol -> True
  CurrencySymbol -> True
  ModifierSymbol -> True
  OtherSymbol -> True
  _ -> False

isSeparator :: Char -> Bool
isSeparator c = case generalCategory c of
  Space -> True
  LineSeparator -> True
  ParagraphSeparator -> True
  _ -> False

toUpper :: Char -> Char
toUpper c = if isAsciiLower c then primChr (primOrd c - 32) else c

toLower :: Char -> Char
toLower c = if isAsciiUpper c then primChr (primOrd c + 32) else c

digitToInt :: Char -> Int
digitToInt c
  | isDigit c = primOrd c - primOrd '0'
  | c >= 'a' && c <= 'f' = primOrd c - primOrd 'a' + 10
  | c >= 'A' && c <= 'F' = primOrd c - primOrd 'A' + 10
  | True = error "Char.digitToInt: not a digit"

intToDigit :: Int -> Char
intToDigit i
  | i >= 0 && i <= 9 = primChr (primOrd '0' + i)
  | i >= 10 && i <= 15 = primChr (primOrd 'a' + i - 10)
  | True = error "Char.intToDigit: not a digit"
