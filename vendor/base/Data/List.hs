-- | More list functions. The functions that compare elements take the
-- comparison as an argument, or work on Int: the boot base has no
-- classes yet.
module Data.List
  ( foldl',
    lines,
    unlines,
    words,
    unwords,
    intercalate,
    intersperse,
    partition,
    sortBy,
    insertBy,
    sort,
    nub,
    transpose,
    unfoldr,
    isPrefixOf,
    tails,
    inits,
    group,
    groupBy,
  )
where

import Data.Char
import GHC.Base
import GHC.Classes
import GHC.List
import GHC.Maybe
import GHC.Prim
import GHC.Types

lines :: String -> [String]
lines "" = []
lines s = case break (eqChar '\n') s of
  (l, s') -> l : case s' of
    [] -> []
    (_ : s'') -> lines s''

unlines :: [String] -> String
unlines = concatMap (++ "\n")

words :: String -> [String]
words s = case dropWhile isSpace s of
  "" -> []
  s' -> case break isSpace s' of
    (w, s'') -> w : words s''

unwords :: [String] -> String
unwords [] = ""
unwords [w] = w
unwords (w : ws) = w ++ (' ' : unwords ws)

intercalate :: [a] -> [[a]] -> [a]
intercalate xs xss = concat (intersperse xs xss)

intersperse :: a -> [a] -> [a]
intersperse _ [] = []
intersperse sep (x : xs) = x : go xs
  where
    go [] = []
    go (y : ys) = sep : y : go ys

partition :: (a -> Bool) -> [a] -> ([a], [a])
partition p xs = foldr select ([], []) xs
  where
    select x ~(ts, fs)
      | p x = (x : ts, fs)
      | otherwise = (ts, x : fs)

-- | A merge sort.
sortBy :: (a -> a -> Ordering) -> [a] -> [a]
sortBy cmp = mergeAll . map (\x -> [x])
  where
    mergeAll [] = []
    mergeAll [xs] = xs
    mergeAll xss = mergeAll (mergePairs xss)
    mergePairs (a : b : xss) = merge a b : mergePairs xss
    mergePairs xss = xss
    merge as@(a : as') bs@(b : bs') = case cmp a b of
      GT -> b : merge as bs'
      _ -> a : merge as' bs
    merge [] bs = bs
    merge as [] = as

insertBy :: (a -> a -> Ordering) -> a -> [a] -> [a]
insertBy _ x [] = [x]
insertBy cmp x ys@(y : ys') = case cmp x y of
  GT -> y : insertBy cmp x ys'
  _ -> x : ys

sort :: [Int] -> [Int]
sort = sortBy compare

nub :: [Int] -> [Int]
nub xs = go [] xs
  where
    go _ [] = []
    go seen (y : ys)
      | elem y seen = go seen ys
      | otherwise = y : go (y : seen) ys

transpose :: [[a]] -> [[a]]
transpose [] = []
transpose ([] : xss) = transpose xss
transpose ((x : xs) : xss) = (x : heads) : transpose (xs : tails')
  where
    heads = concatMap (\ys -> take 1 ys) xss
    tails' = map (drop 1) xss

unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr f b = case f b of
  Just (a, b') -> a : unfoldr f b'
  Nothing -> []

isPrefixOf :: [Int] -> [Int] -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x : xs) (y : ys) = x == y && isPrefixOf xs ys

tails :: [a] -> [[a]]
tails [] = [[]]
tails xs@(_ : xs') = xs : tails xs'

inits :: [a] -> [[a]]
inits [] = [[]]
inits (x : xs) = [] : map (x :) (inits xs)

group :: [Int] -> [[Int]]
group = groupBy (==)

groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy _ [] = []
groupBy eq (x : xs) = case span (eq x) xs of
  (ys, zs) -> (x : ys) : groupBy eq zs
