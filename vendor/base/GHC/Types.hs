-- | The types that the language itself uses.
--
-- aihc-resolve finds the list constructor @:@ in this module: the
-- syntax uses it without an import.
module GHC.Types
  ( List (..),
    Int,
    Word,
    Char,
    Bool (..),
    Ordering (..),
    IO (..),
    IORes (..),
    World,
    Type,
  )
where

infixr 5 :

-- | The list type. @[a]@ is the syntax for @List a@.
data List a = [] | a : List a

-- | Fixed-precision integers. The representation is primitive.
data Int

-- | Unsigned machine words. The representation is primitive.
data Word

-- | Unicode characters. The representation is primitive.
data Char

data Bool = False | True

data Ordering = LT | EQ | GT

-- | An IO action: a function from the world to its result. The
-- evaluator runs @main@ by applying it to the world and forcing the
-- result.
newtype IO a = IO (World -> IORes a)

-- | The result of an IO action.
data IORes a = IORes a

-- | The state of the world. The evaluator passes one token.
data World

-- | The kind of ordinary types.
data Type
