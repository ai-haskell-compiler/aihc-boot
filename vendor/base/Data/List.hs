-- | List functions. As in GHC, the folds are the ones of Foldable.
module Data.List
  ( (++),
    head,
    last,
    tail,
    init,
    uncons,
    singleton,
    null,
    length,
    map,
    reverse,
    intersperse,
    intercalate,
    transpose,
    foldl,
    foldl',
    foldl1,
    foldr,
    foldr1,
    concat,
    concatMap,
    and,
    or,
    any,
    all,
    sum,
    product,
    maximum,
    minimum,
    maximumBy,
    minimumBy,
    scanl,
    scanl1,
    scanr,
    scanr1,
    iterate,
    repeat,
    replicate,
    cycle,
    unfoldr,
    take,
    drop,
    splitAt,
    takeWhile,
    dropWhile,
    span,
    break,
    stripPrefix,
    group,
    groupBy,
    inits,
    tails,
    isPrefixOf,
    isSuffixOf,
    isInfixOf,
    elem,
    notElem,
    lookup,
    find,
    filter,
    partition,
    (!!),
    elemIndex,
    findIndex,
    zip,
    zip3,
    zip4,
    zipWith,
    zipWith3,
    unzip,
    unzip3,
    lines,
    words,
    unlines,
    unwords,
    nub,
    nubBy,
    delete,
    deleteBy,
    (\\),
    union,
    intersect,
    sort,
    sortOn,
    sortBy,
    insert,
    insertBy,
    genericLength,
    mapAccumL,
    mapAccumR,
    dropWhileEnd,
    subsequences,
    permutations,
  )
where

import Data.Char
import Data.Foldable
import GHC.Base hiding (foldr)
import GHC.Classes
import GHC.List hiding (all, and, any, concat, concatMap, elem, foldl, foldl', foldl1, foldr1, length, maximum, minimum, notElem, null, or, product, sum)
import GHC.Maybe
import GHC.Num
import GHC.Types

infix 5 \\

lines :: String -> [String]
lines "" = []
lines s = case break (== '\n') s of
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

sort :: (Ord a) => [a] -> [a]
sort = sortBy compare

sortOn :: (Ord b) => (a -> b) -> [a] -> [a]
sortOn f = sortBy (\x y -> compare (f x) (f y))

insert :: (Ord a) => a -> [a] -> [a]
insert = insertBy compare

nub :: (Eq a) => [a] -> [a]
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

isPrefixOf :: (Eq a) => [a] -> [a] -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x : xs) (y : ys) = x == y && isPrefixOf xs ys

tails :: [a] -> [[a]]
tails [] = [[]]
tails xs@(_ : xs') = xs : tails xs'

inits :: [a] -> [[a]]
inits [] = [[]]
inits (x : xs) = [] : map (x :) (inits xs)

group :: (Eq a) => [a] -> [[a]]
group = groupBy (==)

groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy _ [] = []
groupBy eq (x : xs) = case span (eq x) xs of
  (ys, zs) -> (x : ys) : groupBy eq zs

nubBy :: (a -> a -> Bool) -> [a] -> [a]
nubBy eq l = go l []
  where
    go [] _ = []
    go (y : ys) xs
      | any (eq y) xs = go ys xs
      | otherwise = y : go ys (y : xs)

isSuffixOf :: (Eq a) => [a] -> [a] -> Bool
isSuffixOf a b = reverse a `isPrefixOf` reverse b

isInfixOf :: (Eq a) => [a] -> [a] -> Bool
isInfixOf needle haystack = any (isPrefixOf needle) (tails haystack)

stripPrefix :: (Eq a) => [a] -> [a] -> Maybe [a]
stripPrefix [] ys = Just ys
stripPrefix (x : xs) (y : ys)
  | x == y = stripPrefix xs ys
stripPrefix _ _ = Nothing

delete :: (Eq a) => a -> [a] -> [a]
delete = deleteBy (==)

deleteBy :: (a -> a -> Bool) -> a -> [a] -> [a]
deleteBy _ _ [] = []
deleteBy eq x (y : ys) = if x `eq` y then ys else y : deleteBy eq x ys

(\\) :: (Eq a) => [a] -> [a] -> [a]
(\\) = foldl (flip delete)

union :: (Eq a) => [a] -> [a] -> [a]
union xs ys = xs ++ foldl (flip delete) (nub ys) xs

intersect :: (Eq a) => [a] -> [a] -> [a]
intersect xs ys = [x | x <- xs, any (== x) ys]

elemIndex :: (Eq a) => a -> [a] -> Maybe Int
elemIndex x = findIndex (== x)

findIndex :: (a -> Bool) -> [a] -> Maybe Int
findIndex p = go 0
  where
    go _ [] = Nothing
    go i (y : ys) = if p y then Just i else go (i + 1) ys

uncons :: [a] -> Maybe (a, [a])
uncons [] = Nothing
uncons (x : xs) = Just (x, xs)

singleton :: a -> [a]
singleton x = [x]

zip4 :: [a] -> [b] -> [c] -> [d] -> [(a, b, c, d)]
zip4 (a : as) (b : bs) (c : cs) (d : ds) = (a, b, c, d) : zip4 as bs cs ds
zip4 _ _ _ _ = []

genericLength :: (Num i) => [a] -> i
genericLength [] = 0
genericLength (_ : l) = 1 + genericLength l

-- | A map that threads an accumulator from the left.
mapAccumL :: (acc -> x -> (acc, y)) -> acc -> [x] -> (acc, [y])
mapAccumL _ s [] = (s, [])
mapAccumL f s (x : xs) = (s'', y : ys)
  where
    (s', y) = f s x
    (s'', ys) = mapAccumL f s' xs

-- | A map that threads an accumulator from the right.
mapAccumR :: (acc -> x -> (acc, y)) -> acc -> [x] -> (acc, [y])
mapAccumR _ s [] = (s, [])
mapAccumR f s (x : xs) = (s'', y : ys)
  where
    (s'', y) = f s' x
    (s', ys) = mapAccumR f s xs

-- | Drop the longest suffix whose elements satisfy the predicate.
dropWhileEnd :: (a -> Bool) -> [a] -> [a]
dropWhileEnd p = foldr (\x xs -> if p x && null xs then [] else x : xs) []

-- | All subsequences, in the order of GHC's base.
subsequences :: [a] -> [[a]]
subsequences xs = [] : nonEmptySubsequences xs

nonEmptySubsequences :: [a] -> [[a]]
nonEmptySubsequences [] = []
nonEmptySubsequences (x : xs) = [x] : foldr f [] (nonEmptySubsequences xs)
  where
    f ys r = ys : (x : ys) : r

-- | All permutations, in the order of GHC's base.
permutations :: [a] -> [[a]]
permutations xs0 = xs0 : perms xs0 []
  where
    perms [] _ = []
    perms (t : ts) is' = foldr interleave (perms ts (t : is')) (permutations is')
      where
        interleave xs r = let (_, zs) = interleave' id xs r in zs
        interleave' _ [] r = (ts, r)
        interleave' f (y : ys) r =
          let (us, zs) = interleave' (f . (y :)) ys r
           in (y : us, f (t : y : us) : zs)
