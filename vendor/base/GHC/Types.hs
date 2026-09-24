-- | The types that the language itself uses.
--
-- aihc-resolve finds the list constructor @:@ in this module: the
-- syntax uses it without an import.
module GHC.Types
  ( List (..),
    Int,
    Char,
    Type,
  )
where

infixr 5 :

-- | The list type. @[a]@ is the syntax for @List a@.
data List a = [] | a : List a

-- | Fixed-precision integers. The representation is primitive.
data Int

-- | Unicode characters. The representation is primitive.
data Char

-- | The kind of ordinary types.
data Type
