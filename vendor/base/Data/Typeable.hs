{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Run-time type representations. See Data.Typeable.Internal.
module Data.Typeable
  ( Typeable,
    TypeRep,
    typeOf,
    typeRep,
    cast,
    gcast,
    gcast1,
    Proxy (..),
  )
where

import Data.Proxy
import Data.Typeable.Internal
import GHC.Classes
import GHC.Maybe
import GHC.Prim
import GHC.Types

-- | The representation of the type of the proxy's argument.
typeRep :: (Typeable a) => proxy a -> TypeRep
typeRep = typeRepProxy

-- | The value at another type, inside a type constructor, when the two
-- types are the same.
gcast :: forall a b c. (Typeable a, Typeable b) => c a -> Maybe (c b)
gcast x = if typeRepProxy (proxyA x) == typeRepProxy r then Just (primUnsafeCoerce x) else Nothing
  where
    r = Nothing :: Maybe b
    proxyA :: c a -> Maybe a
    proxyA _ = Nothing

-- | The value at another type, inside two type constructors, when the
-- type arguments are the same.
gcast1 :: forall c t t' a. (Typeable t, Typeable t') => c (t a) -> Maybe (c (t' a))
gcast1 x = if typeRepProxy (proxyT x) == typeRepProxy r then Just (primUnsafeCoerce x) else Nothing
  where
    r = Nothing :: Maybe (T1 t')
    proxyT :: c (t a) -> Maybe (T1 t)
    proxyT _ = Nothing

-- | A type of kind @Type@ for a type constructor of one argument, so that
-- Typeable sees the constructor.
data T1 t
