{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | The representation types and the 'Generic' class for datatype-generic
-- programming.
module GHC.Generics
  ( U1 (..),
    K1 (..),
    M1 (..),
    (:+:) (..),
    (:*:) (..),
    Generic (..),
  )
where

import GHC.Types (Type)

infixr 5 :+:

infixr 6 :*:

-- | A constructor without arguments.
data U1 p = U1

-- | Constants, additional parameters and recursion of kind 'Type'.
newtype K1 i c p = K1 {unK1 :: c}

-- | Meta-information: constructor names and similar.
newtype M1 i c f p = M1 {unM1 :: f p}

-- | Sums: a choice between two representations.
data (:+:) f g p = L1 (f p) | R1 (g p)

-- | Products: two representations together.
data (:*:) f g p = f p :*: g p

-- | Types that have a generic representation.
class Generic a where
  -- | The representation of the type.
  type Rep a :: Type -> Type

  -- | Convert a value to its representation.
  from :: a -> Rep a x

  -- | Convert a representation back to a value.
  to :: Rep a x -> a
