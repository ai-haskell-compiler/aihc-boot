-- | The Either type.
module Data.Either
  ( Either (..),
    either,
    lefts,
    rights,
    partitionEithers,
    isLeft,
    isRight,
    fromLeft,
    fromRight,
  )
where

import GHC.Base
import GHC.Classes
import GHC.Show
import GHC.Types

data Either a b = Left a | Right b

instance (Eq a, Eq b) => Eq (Either a b) where
  Left x == Left y = x == y
  Right x == Right y = x == y
  _ == _ = False

instance (Ord a, Ord b) => Ord (Either a b) where
  compare (Left x) (Left y) = compare x y
  compare (Left _) (Right _) = LT
  compare (Right _) (Left _) = GT
  compare (Right x) (Right y) = compare x y

instance (Show a, Show b) => Show (Either a b) where
  showsPrec d (Left x) = showParen (d >= 11) (showString "Left " . showsPrec 11 x)
  showsPrec d (Right x) = showParen (d >= 11) (showString "Right " . showsPrec 11 x)

instance Functor (Either e) where
  fmap _ (Left e) = Left e
  fmap f (Right x) = Right (f x)

instance Applicative (Either e) where
  pure = Right
  Left e <*> _ = Left e
  Right f <*> r = fmap f r

instance Monad (Either e) where
  Left e >>= _ = Left e
  Right x >>= k = k x

either :: (a -> c) -> (b -> c) -> Either a b -> c
either f _ (Left x) = f x
either _ g (Right y) = g y

lefts :: [Either a b] -> [a]
lefts xs = foldr (\e r -> either (\x -> x : r) (\_ -> r) e) [] xs

rights :: [Either a b] -> [b]
rights xs = foldr (\e r -> either (\_ -> r) (\y -> y : r) e) [] xs

partitionEithers :: [Either a b] -> ([a], [b])
partitionEithers xs = (lefts xs, rights xs)

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False

isRight :: Either a b -> Bool
isRight (Left _) = False
isRight (Right _) = True

fromLeft :: a -> Either a b -> a
fromLeft _ (Left a) = a
fromLeft a _ = a

fromRight :: b -> Either a b -> b
fromRight _ (Right b) = b
fromRight b _ = b
