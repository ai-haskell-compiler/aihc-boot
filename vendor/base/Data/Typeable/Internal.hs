-- | Run-time type representations.
--
-- The typechecker solves every 'Typeable' constraint on a known type by
-- itself: the dictionary holds the type as text. No module writes an
-- instance.
module Data.Typeable.Internal
  ( Typeable (..),
    TypeRep (..),
    typeOf,
    cast,
  )
where

import GHC.Classes
import GHC.Maybe
import GHC.Prim
import GHC.Types

-- | A type, as its text.
data TypeRep = TypeRep [Char]

instance Eq TypeRep where
  TypeRep a == TypeRep b = a == b

instance Ord TypeRep where
  compare (TypeRep a) (TypeRep b) = compare a b

class Typeable a where
  -- | The representation of the type of the proxy's argument.
  typeRepProxy :: proxy a -> TypeRep

typeOf :: (Typeable a) => a -> TypeRep
typeOf x = typeRepProxy (Just x)

-- | The value at another type, when the two types are the same.
cast :: (Typeable a, Typeable b) => a -> Maybe b
cast x = r
  where
    r = if typeRepProxy (Just x) == typeRepProxy r then Just (primUnsafeCoerce x) else Nothing
