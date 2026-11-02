-- | Functors.
module Data.Functor
  ( Functor (..),
    (<$>),
    ($>),
    (<&>),
    void,
  )
where

import GHC.Base

infixl 4 $>

infixl 1 <&>

($>) :: (Functor f) => f a -> b -> f b
($>) = flip (<$)

(<&>) :: (Functor f) => f a -> (a -> b) -> f b
as <&> f = f <$> as

void :: (Functor f) => f a -> f ()
void x = () <$ x
