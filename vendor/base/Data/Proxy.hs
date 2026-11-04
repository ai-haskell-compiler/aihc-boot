-- | A value that only carries a type.
module Data.Proxy (Proxy (..), asProxyTypeOf) where

import Data.Foldable
import Data.Traversable
import GHC.Base
import GHC.Classes
import GHC.Enum
import GHC.Show

data Proxy t = Proxy
  deriving (Eq, Ord, Show, Bounded)

asProxyTypeOf :: a -> proxy a -> a
asProxyTypeOf = const

instance Semigroup (Proxy s) where
  _ <> _ = Proxy

instance Monoid (Proxy s) where
  mempty = Proxy

instance Functor Proxy where
  fmap _ _ = Proxy

instance Applicative Proxy where
  pure _ = Proxy
  _ <*> _ = Proxy

instance Monad Proxy where
  _ >>= _ = Proxy

instance Foldable Proxy where
  foldMap _ _ = mempty
  foldr _ z _ = z

instance Traversable Proxy where
  traverse _ _ = pure Proxy
