-- | Values of any Typeable type, with their type.
module Data.Dynamic
  ( Typeable,
    Dynamic,
    toDyn,
    fromDynamic,
    fromDyn,
    dynTypeRep,
  )
where

import Data.Typeable
import GHC.Base
import GHC.Maybe
import GHC.Show
import GHC.Types

data Dynamic = forall a. (Typeable a) => Dynamic a

instance Show Dynamic where
  showsPrec _ _ = showString "<<Dynamic>>"

toDyn :: (Typeable a) => a -> Dynamic
toDyn = Dynamic

fromDynamic :: (Typeable a) => Dynamic -> Maybe a
fromDynamic (Dynamic v) = cast v

fromDyn :: (Typeable a) => Dynamic -> a -> a
fromDyn d def = case fromDynamic d of
  Just v -> v
  Nothing -> def

dynTypeRep :: Dynamic -> TypeRep
dynTypeRep (Dynamic v) = typeOf v
