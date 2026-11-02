-- | The 'Traversable' class.
module Data.Traversable
  ( Traversable (..),
    for,
    forM,
  )
where

import Data.Foldable
import GHC.Base
import GHC.Maybe

class (Functor t, Foldable t) => Traversable t where
  traverse :: (Applicative f) => (a -> f b) -> t a -> f (t b)
  sequenceA :: (Applicative f) => t (f a) -> f (t a)
  mapM :: (Monad m) => (a -> m b) -> t a -> m (t b)
  sequence :: (Monad m) => t (m a) -> m (t a)
  traverse f = sequenceA . fmap f
  sequenceA = traverse id
  mapM = traverse
  sequence = sequenceA

instance Traversable [] where
  traverse f = GHC.Base.foldr (\x ys -> liftA2 (:) (f x) ys) (pure [])

instance Traversable Maybe where
  traverse _ Nothing = pure Nothing
  traverse f (Just x) = fmap Just (f x)

instance Traversable NonEmpty where
  traverse f (a :| as) = liftA2 (:|) (f a) (traverse f as)

for :: (Traversable t, Applicative f) => t a -> (a -> f b) -> f (t b)
for = flip traverse

forM :: (Traversable t, Monad m) => t a -> (a -> m b) -> m (t b)
forM = flip mapM
