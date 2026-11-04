{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

-- | The types that the language itself uses, and the primitive types.
--
-- As in GHC, a boxed type holds a primitive value: @data Int = I# Int#@.
-- The desugarer erases such a box, so 'Int' and 'Int#' have the same
-- representation at run time, and a primitive works on both.
--
-- aihc-resolve finds the list constructor @:@ in this module: the
-- syntax uses it without an import.
module GHC.Types
  ( List (..),
    Int (..),
    Word (..),
    Char (..),
    Bool (..),
    Ordering (..),
    IO (..),
    Type,
    Constraint,
    SPEC (..),
    Levity (..),
    RuntimeRep (..),
    VecCount (..),
    VecElem (..),
    -- * Primitive types
    Int#,
    Word#,
    Char#,
    Double#,
    Float#,
    Addr#,
    State#,
    RealWorld,
    Array#,
    MutableArray#,
    SmallArray#,
    SmallMutableArray#,
    ByteArray#,
    MutableByteArray#,
    MutVar#,
    MVar#,
    TVar#,
    ThreadId#,
    StablePtr#,
    StableName#,
    Weak#,
  )
where

infixr 5 :

-- | The list type. @[a]@ is the syntax for @List a@.
data List a = [] | a : List a

-- | Fixed-precision integers.
data Int = I# Int#

-- | Unsigned machine words.
data Word = W# Word#

-- | Unicode characters.
data Char = C# Char#

data Bool = False | True

data Ordering = LT | EQ | GT

-- | An IO action: a function on the state token of the real world. The
-- evaluator runs @main@ on a token and forces the result.
newtype IO a = IO (State# RealWorld -> (# State# RealWorld, a #))

-- | The kind of ordinary types.
data Type

-- | The kind of class constraints.
data Constraint

-- | An argument that tells the optimizer of GHC to specialise a loop.
data SPEC = SPEC | SPEC2

data Levity = Lifted | Unlifted

-- | The representation of a type, as GHC's kinds record it.
data RuntimeRep
  = BoxedRep Levity
  | IntRep
  | Int8Rep
  | Int16Rep
  | Int32Rep
  | Int64Rep
  | WordRep
  | Word8Rep
  | Word16Rep
  | Word32Rep
  | Word64Rep
  | AddrRep
  | FloatRep
  | DoubleRep

data VecCount = Vec2 | Vec4 | Vec8 | Vec16 | Vec32 | Vec64

data VecElem
  = Int8ElemRep
  | Int16ElemRep
  | Int32ElemRep
  | Int64ElemRep
  | Word8ElemRep
  | Word16ElemRep
  | Word32ElemRep
  | Word64ElemRep
  | FloatElemRep
  | DoubleElemRep

-- Primitive types. The evaluator implements them; they have no
-- constructors.

data Int#

data Word#

data Char#

data Double#

data Float#

-- | An address, as a machine integer.
data Addr#

-- | The state token of a state thread.
data State# s

-- | The state thread of IO.
data RealWorld

data Array# a

data MutableArray# s a

data SmallArray# a

data SmallMutableArray# s a

data ByteArray#

data MutableByteArray# s

data MutVar# s a

data MVar# s a

data TVar# s a

data ThreadId#

data StablePtr# a

data StableName# a

data Weak# b
