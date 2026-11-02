-- | The 'Foldable' class: data structures that fold to a summary
-- value.
module Data.Foldable
  ( Foldable (..),
    foldrM,
    foldlM,
    traverse_,
    for_,
    mapM_,
    forM_,
    sequenceA_,
    sequence_,
    concat,
    concatMap,
    and,
    or,
    any,
    all,
    maximumBy,
    minimumBy,
    notElem,
    find,
  )
where

import GHC.Base hiding (foldr)
import qualified GHC.Base
import GHC.Classes
import GHC.Err
import qualified GHC.List as List
import GHC.Maybe
import GHC.Num
import GHC.Types

infix 4 `elem`, `notElem`

-- | The monoid of endomorphisms under composition.
newtype Endo a = Endo (a -> a)

appEndo :: Endo a -> a -> a
appEndo (Endo f) = f

instance Semigroup (Endo a) where
  Endo f <> Endo g = Endo (f . g)

instance Monoid (Endo a) where
  mempty = Endo id

class Foldable t where
  foldMap :: (Monoid m) => (a -> m) -> t a -> m
  foldr :: (a -> b -> b) -> b -> t a -> b
  foldl :: (b -> a -> b) -> b -> t a -> b
  foldr' :: (a -> b -> b) -> b -> t a -> b
  foldl' :: (b -> a -> b) -> b -> t a -> b
  foldr1 :: (a -> a -> a) -> t a -> a
  foldl1 :: (a -> a -> a) -> t a -> a
  toList :: t a -> [a]
  null :: t a -> Bool
  length :: t a -> Int
  elem :: (Eq a) => a -> t a -> Bool
  maximum :: (Ord a) => t a -> a
  minimum :: (Ord a) => t a -> a
  sum :: (Num a) => t a -> a
  product :: (Num a) => t a -> a
  foldMap f = foldr (\x acc -> f x <> acc) mempty
  foldr f z t = appEndo (foldMap (\x -> Endo (f x)) t) z
  foldl f z t = List.foldl f z (toList t)
  foldr' f z t = foldr f z t
  foldl' f z t = List.foldl' f z (toList t)
  foldr1 f t = List.foldr1 f (toList t)
  foldl1 f t = List.foldl1 f (toList t)
  toList t = foldr (:) [] t
  null t = List.null (toList t)
  length t = List.length (toList t)
  elem x t = List.elem x (toList t)
  maximum t = List.maximum (toList t)
  minimum t = List.minimum (toList t)
  sum t = List.sum (toList t)
  product t = List.product (toList t)

instance Foldable [] where
  foldMap f = GHC.Base.foldr (\x acc -> f x <> acc) mempty
  foldr = GHC.Base.foldr
  foldl = List.foldl
  foldl' = List.foldl'
  foldr1 = List.foldr1
  foldl1 = List.foldl1
  toList xs = xs
  null = List.null
  length = List.length
  elem = List.elem
  maximum = List.maximum
  minimum = List.minimum
  sum = List.sum
  product = List.product

instance Foldable Maybe where
  foldMap _ Nothing = mempty
  foldMap f (Just x) = f x
  foldr _ z Nothing = z
  foldr f z (Just x) = f x z
  toList Nothing = []
  toList (Just x) = [x]

instance Foldable NonEmpty where
  foldr f z (a :| as) = f a (GHC.Base.foldr f z as)
  toList (a :| as) = a : as

instance Foldable ((,) a) where
  foldr f z (_, y) = f y z

foldrM :: (Foldable t, Monad m) => (a -> b -> m b) -> b -> t a -> m b
foldrM f z0 xs = List.foldl c return (toList xs) z0
  where
    c k x z = f x z >>= k

foldlM :: (Foldable t, Monad m) => (b -> a -> m b) -> b -> t a -> m b
foldlM f z0 xs = go z0 (toList xs)
  where
    go z [] = return z
    go z (y : ys) = f z y >>= \z' -> go z' ys

traverse_ :: (Foldable t, Applicative f) => (a -> f b) -> t a -> f ()
traverse_ f = foldr (\x k -> f x *> k) (pure ())

for_ :: (Foldable t, Applicative f) => t a -> (a -> f b) -> f ()
for_ = flip traverse_

mapM_ :: (Foldable t, Monad m) => (a -> m b) -> t a -> m ()
mapM_ f = foldr (\x k -> f x >> k) (return ())

forM_ :: (Foldable t, Monad m) => t a -> (a -> m b) -> m ()
forM_ = flip mapM_

sequenceA_ :: (Foldable t, Applicative f) => t (f a) -> f ()
sequenceA_ = foldr (\m k -> m *> k) (pure ())

sequence_ :: (Foldable t, Monad m) => t (m a) -> m ()
sequence_ = foldr (\m k -> m >> k) (return ())

concat :: (Foldable t) => t [a] -> [a]
concat xs = foldr (++) [] xs

concatMap :: (Foldable t) => (a -> [b]) -> t a -> [b]
concatMap f xs = foldr (\x b -> f x ++ b) [] xs

and :: (Foldable t) => t Bool -> Bool
and = foldr (&&) True

or :: (Foldable t) => t Bool -> Bool
or = foldr (||) False

any :: (Foldable t) => (a -> Bool) -> t a -> Bool
any p = foldr (\x b -> p x || b) False

all :: (Foldable t) => (a -> Bool) -> t a -> Bool
all p = foldr (\x b -> p x && b) True

maximumBy :: (Foldable t) => (a -> a -> Ordering) -> t a -> a
maximumBy cmp = foldl1 (\x y -> case cmp x y of GT -> x; _ -> y)

minimumBy :: (Foldable t) => (a -> a -> Ordering) -> t a -> a
minimumBy cmp = foldl1 (\x y -> case cmp x y of GT -> y; _ -> x)

notElem :: (Foldable t, Eq a) => a -> t a -> Bool
notElem x = not . elem x

find :: (Foldable t) => (a -> Bool) -> t a -> Maybe a
find p t = case List.filter p (toList t) of
  [] -> Nothing
  (x : _) -> Just x
