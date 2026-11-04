-- | Applicative functors and the Alternative class.
module Control.Applicative
  ( Applicative (..),
    Alternative (..),
    Const (..),
    (<$>),
    (<$),
    (<**>),
    liftA,
    liftA3,
    optional,
    asum,
  )
where

import Data.Foldable (asum)
import Data.Functor.Const
import GHC.Base
import GHC.Maybe

infixl 4 <**>

-- | A variant of '<*>' with the arguments reversed.
(<**>) :: (Applicative f) => f a -> f (a -> b) -> f b
(<**>) = liftA2 (\a f -> f a)

-- | One or none.
optional :: (Alternative f) => f a -> f (Maybe a)
optional v = Just <$> v <|> pure Nothing
