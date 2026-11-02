-- | Monads and functions on them.
module Control.Monad
  ( Functor (..),
    Applicative (..),
    Monad (..),
    mapM,
    mapM_,
    forM,
    forM_,
    sequence,
    sequence_,
    (=<<),
    (>=>),
    (<=<),
    forever,
    void,
    join,
    foldM,
    foldM_,
    replicateM,
    replicateM_,
    zipWithM,
    zipWithM_,
    filterM,
    when,
    unless,
    liftM,
    liftM2,
    ap,
    (<$!>),
  )
where

import Data.Foldable
import Data.Traversable
import GHC.Base
import GHC.Classes
import GHC.List (zipWith)
import GHC.Num
import GHC.Prim (seq)
import GHC.Types

infixr 1 >=>, <=<

infixl 4 <$!>

(>=>) :: (Monad m) => (a -> m b) -> (b -> m c) -> a -> m c
f >=> g = \x -> f x >>= g

(<=<) :: (Monad m) => (b -> m c) -> (a -> m b) -> a -> m c
g <=< f = \x -> f x >>= g

forever :: (Applicative f) => f a -> f b
forever a = let a' = a *> a' in a'

void :: (Functor f) => f a -> f ()
void x = () <$ x

foldM :: (Foldable t, Monad m) => (b -> a -> m b) -> b -> t a -> m b
foldM = foldlM

foldM_ :: (Foldable t, Monad m) => (b -> a -> m b) -> b -> t a -> m ()
foldM_ f a xs = foldlM f a xs >> return ()

replicateM :: (Applicative m) => Int -> m a -> m [a]
replicateM n f = go n
  where
    go k = if k <= 0 then pure [] else liftA2 (:) f (go (k - 1))

replicateM_ :: (Applicative m) => Int -> m a -> m ()
replicateM_ n f = go n
  where
    go k = if k <= 0 then pure () else f *> go (k - 1)

zipWithM :: (Applicative m) => (a -> b -> m c) -> [a] -> [b] -> m [c]
zipWithM f xs ys = sequenceA (zipWith f xs ys)

zipWithM_ :: (Applicative m) => (a -> b -> m c) -> [a] -> [b] -> m ()
zipWithM_ f xs ys = sequenceA_ (zipWith f xs ys)

filterM :: (Applicative m) => (a -> m Bool) -> [a] -> m [a]
filterM p = GHC.Base.foldr (\x -> liftA2 (\flg -> if flg then (x :) else id) (p x)) (pure [])

(<$!>) :: (Monad m) => (a -> b) -> m a -> m b
f <$!> m = m >>= \x -> let z = f x in z `seq` return z
