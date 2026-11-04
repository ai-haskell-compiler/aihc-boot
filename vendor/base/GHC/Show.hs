-- | The 'Show' class and its instances for the basic types.
--
-- The module comes before GHC.Num and GHC.Base, so it has no integer
-- literals and its own small list functions. The few Int constants
-- come from character codes: @primOrd '\\v'@ is 11, the precedence of
-- function application.
module GHC.Show
  ( Show (..),
    ShowS,
    shows,
    showChar,
    showString,
    showParen,
    showSpace,
    showList__,
    showLitChar,
    showLitString,
    showCommaSpace,
  )
where

import GHC.Classes
import GHC.Maybe
import GHC.Prim
import GHC.Types

type ShowS = [Char] -> [Char]

class Show a where
  showsPrec :: Int -> a -> ShowS
  show :: a -> [Char]
  showList :: [a] -> ShowS
  showsPrec _ x s = show x `append` s
  show x = shows x ""
  showList ls s = showList__ shows ls s

append :: [a] -> [a] -> [a]
append [] ys = ys
append (x : xs) ys = x : append xs ys

zero :: Int
zero = primOrd '\NUL'

-- | The precedence of function application.
appPrec :: Int
appPrec = primOrd '\v'

-- | The precedence of negation.
negPrec :: Int
negPrec = primOrd '\ACK'

shows :: (Show a) => a -> ShowS
shows = showsPrec zero

showChar :: Char -> ShowS
showChar = (:)

showString :: [Char] -> ShowS
showString = append

showParen :: Bool -> ShowS -> ShowS
showParen b p = if b then \s -> '(' : p (')' : s) else p

showSpace :: ShowS
showSpace = \xs -> ' ' : xs

showCommaSpace :: ShowS
showCommaSpace = showString ", "

showList__ :: (a -> ShowS) -> [a] -> ShowS
showList__ _ [] s = "[]" `append` s
showList__ showx (x : xs) s = '[' : showx x (showl xs)
  where
    showl [] = ']' : s
    showl (y : ys) = ',' : showx y (showl ys)

instance Show Int where
  showsPrec d n s =
    if primIntGt d negPrec && primIntLt n zero
      then '(' : primIntShow n `append` (')' : s)
      else primIntShow n `append` s

-- | Unsigned machine words, in decimal.
instance Show Word where
  showsPrec _ a = showString (primWordShow (primUnsafeCoerce a))

instance Show Char where
  showsPrec _ '\'' = showString "'\\''"
  showsPrec _ c = \s -> '\'' : showLitChar c ('\'' : s)
  showList cs = \s -> '"' : showLitString cs ('"' : s)

instance Show Bool where
  showsPrec _ True = showString "True"
  showsPrec _ False = showString "False"

instance Show Ordering where
  showsPrec _ LT = showString "LT"
  showsPrec _ EQ = showString "EQ"
  showsPrec _ GT = showString "GT"

instance Show () where
  showsPrec _ () = showString "()"

instance (Show a) => Show [a] where
  showsPrec _ = showList

instance (Show a, Show b) => Show (a, b) where
  showsPrec _ (a, b) s = '(' : shows a (',' : shows b (')' : s))

instance (Show a, Show b, Show c) => Show (a, b, c) where
  showsPrec _ (a, b, c) s = '(' : shows a (',' : shows b (',' : shows c (')' : s)))

instance (Show a) => Show (Maybe a) where
  showsPrec _ Nothing = showString "Nothing"
  showsPrec d (Just x) = showParen (d >= appPrec) (\s -> "Just " `append` showsPrec appPrec x s)

showLitChar :: Char -> ShowS
showLitChar c s
  | c > '\DEL' = '\\' : protectEsc isDec (primIntShow (primOrd c)) s
showLitChar '\DEL' s = showString "\\DEL" s
showLitChar '\\' s = showString "\\\\" s
showLitChar c s
  | c >= ' ' = c : s
showLitChar '\a' s = showString "\\a" s
showLitChar '\b' s = showString "\\b" s
showLitChar '\f' s = showString "\\f" s
showLitChar '\n' s = showString "\\n" s
showLitChar '\r' s = showString "\\r" s
showLitChar '\t' s = showString "\\t" s
showLitChar '\v' s = showString "\\v" s
showLitChar '\SO' s = protectEsc (== 'H') "\\SO" s
showLitChar c s = '\\' : (asciiTab `index` primOrd c) `append` s

showLitString :: [Char] -> ShowS
showLitString [] s = s
showLitString ('"' : cs) s = showString "\\\"" (showLitString cs s)
showLitString (c : cs) s = showLitChar c (showLitString cs s)

isDec :: Char -> Bool
isDec c = c >= '0' && c <= '9'

-- | Show the text, and separate it from a following character that
-- would continue the escape.
protectEsc :: (Char -> Bool) -> [Char] -> ShowS
protectEsc p text s = text `append` cont s
  where
    cont t@(c : _) | p c = "\\&" `append` t
    cont t = t

index :: [a] -> Int -> a
index (x : xs) n = if primIntEq n zero then x else index xs (primIntSub n (primOrd '\SOH'))
index [] _ = index [] zero

asciiTab :: [[Char]]
asciiTab =
  [ "NUL", "SOH", "STX", "ETX", "EOT", "ENQ", "ACK", "BEL",
    "BS", "HT", "LF", "VT", "FF", "CR", "SO", "SI",
    "DLE", "DC1", "DC2", "DC3", "DC4", "NAK", "SYN", "ETB",
    "CAN", "EM", "SUB", "ESC", "FS", "GS", "RS", "US",
    "SP"
  ]

instance (Show a, Show b, Show c, Show d) => Show (a, b, c, d) where
  showsPrec _ (a, b, c, d) s =
    '(' : shows a (',' : shows b (',' : shows c (',' : shows d (')' : s))))

instance (Show a, Show b, Show c, Show d, Show e) => Show (a, b, c, d, e) where
  showsPrec _ (a, b, c, d, e) s =
    '(' : shows a (',' : shows b (',' : shows c (',' : shows d (',' : shows e (')' : s)))))

instance (Show a, Show b, Show c, Show d, Show e, Show f) => Show (a, b, c, d, e, f) where
  showsPrec _ (a, b, c, d, e, f) s =
    '(' : shows a (',' : shows b (',' : shows c (',' : shows d (',' : shows e (',' : shows f (')' : s))))))

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g) => Show (a, b, c, d, e, f, g) where
  showsPrec _ (a, b, c, d, e, f, g) s =
    '(' : shows a (',' : shows b (',' : shows c (',' : shows d (',' : shows e (',' : shows f (',' : shows g (')' : s)))))))
