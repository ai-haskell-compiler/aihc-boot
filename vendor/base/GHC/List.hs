-- | List functions. The functions that compare elements work on Int
-- only: the boot base has no classes yet.
module GHC.List
  ( filter,
    foldl,
    foldl',
    foldr1,
    foldl1,
    head,
    tail,
    last,
    init,
    null,
    length,
    (!!),
    reverse,
    and,
    or,
    any,
    all,
    concat,
    concatMap,
    iterate,
    repeat,
    replicate,
    cycle,
    take,
    drop,
    splitAt,
    takeWhile,
    dropWhile,
    span,
    break,
    sum,
    product,
    maximum,
    minimum,
    elem,
    notElem,
    lookup,
    zip,
    zip3,
    zipWith,
    zipWith3,
    unzip,
    unzip3,
    scanl,
    scanl1,
    scanr,
    scanr1,
  )
where

import GHC.Base
import GHC.Classes
import GHC.Maybe
import GHC.Num
import GHC.Prim
import GHC.Types

infixl 9 !!

infix 4 `elem`, `notElem`

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter p (x : xs)
  | p x = x : filter p xs
  | otherwise = filter p xs

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f z0 xs0 = go z0 xs0
  where
    go z [] = z
    go z (x : xs) = go (f z x) xs

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f z0 xs0 = go z0 xs0
  where
    go !z [] = z
    go !z (x : xs) = go (f z x) xs

foldr1 :: (a -> a -> a) -> [a] -> a
foldr1 _ [x] = x
foldr1 f (x : xs) = f x (foldr1 f xs)
foldr1 _ [] = error "Prelude.foldr1: empty list"

foldl1 :: (a -> a -> a) -> [a] -> a
foldl1 f (x : xs) = foldl f x xs
foldl1 _ [] = error "Prelude.foldl1: empty list"

head :: [a] -> a
head (x : _) = x
head [] = error "Prelude.head: empty list"

tail :: [a] -> [a]
tail (_ : xs) = xs
tail [] = error "Prelude.tail: empty list"

last :: [a] -> a
last [x] = x
last (_ : xs) = last xs
last [] = error "Prelude.last: empty list"

init :: [a] -> [a]
init [_] = []
init (x : xs) = x : init xs
init [] = error "Prelude.init: empty list"

null :: [a] -> Bool
null [] = True
null (_ : _) = False

length :: [a] -> Int
length xs = go 0 xs
  where
    go !n [] = n
    go !n (_ : ys) = go (n + 1) ys

(!!) :: [a] -> Int -> a
xs !! n
  | n < 0 = error "Prelude.!!: negative index"
  | otherwise = go xs n
  where
    go [] _ = error "Prelude.!!: index too large"
    go (y : ys) k = if k == 0 then y else go ys (k - 1)

reverse :: [a] -> [a]
reverse xs = go [] xs
  where
    go acc [] = acc
    go acc (y : ys) = go (y : acc) ys

and :: [Bool] -> Bool
and = foldr (&&) True

or :: [Bool] -> Bool
or = foldr (||) False

any :: (a -> Bool) -> [a] -> Bool
any p = or . map p

all :: (a -> Bool) -> [a] -> Bool
all p = and . map p

concat :: [[a]] -> [a]
concat = foldr (++) []

concatMap :: (a -> [b]) -> [a] -> [b]
concatMap f = foldr (\x r -> f x ++ r) []

iterate :: (a -> a) -> a -> [a]
iterate f x = x : iterate f (f x)

repeat :: a -> [a]
repeat x = xs
  where
    xs = x : xs

replicate :: Int -> a -> [a]
replicate n x = take n (repeat x)

cycle :: [a] -> [a]
cycle [] = error "Prelude.cycle: empty list"
cycle xs = ys
  where
    ys = xs ++ ys

take :: Int -> [a] -> [a]
take n xs
  | n <= 0 = []
  | otherwise = case xs of
      [] -> []
      (y : ys) -> y : take (n - 1) ys

drop :: Int -> [a] -> [a]
drop n xs
  | n <= 0 = xs
  | otherwise = case xs of
      [] -> []
      (_ : ys) -> drop (n - 1) ys

splitAt :: Int -> [a] -> ([a], [a])
splitAt n xs = (take n xs, drop n xs)

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile p (x : xs)
  | p x = x : takeWhile p xs
  | otherwise = []

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ [] = []
dropWhile p xs@(x : xs')
  | p x = dropWhile p xs'
  | otherwise = xs

span :: (a -> Bool) -> [a] -> ([a], [a])
span _ [] = ([], [])
span p xs@(x : xs')
  | p x = case span p xs' of
      (ys, zs) -> (x : ys, zs)
  | otherwise = ([], xs)

break :: (a -> Bool) -> [a] -> ([a], [a])
break p = span (not . p)

sum :: [Int] -> Int
sum = foldl' (+) 0

product :: [Int] -> Int
product = foldl' (*) 1

maximum :: [Int] -> Int
maximum [] = error "Prelude.maximum: empty list"
maximum (x : xs) = foldl' max x xs

minimum :: [Int] -> Int
minimum [] = error "Prelude.minimum: empty list"
minimum (x : xs) = foldl' min x xs

elem :: Int -> [Int] -> Bool
elem x = any (== x)

notElem :: Int -> [Int] -> Bool
notElem x = all (/= x)

lookup :: Int -> [(Int, b)] -> Maybe b
lookup _ [] = Nothing
lookup k ((k', v) : rest)
  | k == k' = Just v
  | otherwise = lookup k rest

zip :: [a] -> [b] -> [(a, b)]
zip (x : xs) (y : ys) = (x, y) : zip xs ys
zip _ _ = []

zip3 :: [a] -> [b] -> [c] -> [(a, b, c)]
zip3 (x : xs) (y : ys) (z : zs) = (x, y, z) : zip3 xs ys zs
zip3 _ _ _ = []

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f (x : xs) (y : ys) = f x y : zipWith f xs ys
zipWith _ _ _ = []

zipWith3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
zipWith3 f (x : xs) (y : ys) (z : zs) = f x y z : zipWith3 f xs ys zs
zipWith3 _ _ _ _ = []

unzip :: [(a, b)] -> ([a], [b])
unzip = foldr (\(a, b) ~(as, bs) -> (a : as, b : bs)) ([], [])

unzip3 :: [(a, b, c)] -> ([a], [b], [c])
unzip3 = foldr (\(a, b, c) ~(as, bs, cs) -> (a : as, b : bs, c : cs)) ([], [], [])

scanl :: (b -> a -> b) -> b -> [a] -> [b]
scanl f q ls =
  q : case ls of
    [] -> []
    (x : xs) -> scanl f (f q x) xs

scanl1 :: (a -> a -> a) -> [a] -> [a]
scanl1 f (x : xs) = scanl f x xs
scanl1 _ [] = []

scanr :: (a -> b -> b) -> b -> [a] -> [b]
scanr _ q0 [] = [q0]
scanr f q0 (x : xs) = case scanr f q0 xs of
  qs@(q : _) -> f x q : qs
  [] -> error "Prelude.scanr: impossible"

scanr1 :: (a -> a -> a) -> [a] -> [a]
scanr1 _ [] = []
scanr1 _ [x] = [x]
scanr1 f (x : xs) = case scanr1 f xs of
  qs@(q : _) -> f x q : qs
  [] -> error "Prelude.scanr1: impossible"
