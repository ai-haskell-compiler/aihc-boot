-- Pattern matching: clauses, guards, where, case, nested and special
-- patterns.
{-# LANGUAGE BangPatterns #-}
module Main where

data Tree = Leaf | Node Tree Int Tree

insert :: Int -> Tree -> Tree
insert x Leaf = Node Leaf x Leaf
insert x t@(Node l y r)
  | x < y = Node (insert x l) y r
  | x > y = Node l y (insert x r)
  | otherwise = t

toList :: Tree -> [Int]
toList Leaf = []
toList (Node l x r) = toList l ++ [x] ++ toList r

depth :: Tree -> Int
depth Leaf = 0
depth (Node l _ r) = 1 + max (depth l) (depth r)

classify :: Int -> String
classify 0 = "zero"
classify 1 = "one"
classify (-1) = "minus one"
classify n
  | even n, n > 100 = "big even"
  | even n = "even"
  | Just m <- half n = "odd with half " ++ show m
  | otherwise = "odd"
  where
    half k = if k > 10 then Just (k `div` 2) else Nothing

greet :: String -> String
greet "world" = "hello, world"
greet ('x' : rest) = "x then " ++ rest
greet name = "hi " ++ name

swapPair :: (a, b) -> (b, a)
swapPair (x, y) = (y, x)

sumPairs :: [(Int, Int)] -> Int
sumPairs [] = 0
sumPairs ((a, b) : rest) = a + b + sumPairs rest

lazyPat :: (Int, Int) -> Int
lazyPat ~(a, _) = 1

strictLen :: [a] -> Int
strictLen = go 0
  where
    go !acc [] = acc
    go !acc (_ : xs) = go (acc + 1) xs

infixr 5 +++
(+++) :: [a] -> [a] -> [a]
[] +++ ys = ys
(x : xs) +++ ys = x : (xs +++ ys)

describe :: [Int] -> String
describe xs = case xs of
  [] -> "empty"
  [x] -> "one: " ++ show x
  [x, y] | x == y -> "pair of equal"
         | otherwise -> "pair"
  (x : _ : _ : _) -> "many starting with " ++ show x

main :: IO ()
main = do
  let t = foldr insert Leaf [5, 3, 8, 1, 4, 7, 9, 3]
  putStrLn (unwords (map show (toList t)))
  print (depth t)
  mapM_ (putStrLn . classify) [0, 1, -1, 200, 4, 15, 7]
  putStrLn (greet "world")
  putStrLn (greet "xyz")
  putStrLn (greet "bob")
  print (fst (swapPair (1, 2)))
  print (sumPairs [(1, 2), (3, 4)])
  print (lazyPat (error "never"))
  print (strictLen "abcd")
  print (length ([1, 2] +++ [3] +++ []))
  mapM_ (putStrLn . describe) [[], [1], [2, 2], [2, 3], [1, 2, 3, 4]]
  print (let (a, b) = (3, 4) in a * b)
  print (let f 0 = 1; f n = n * f (n - 1) in f 5)
  print (let (q, r) = 17 `divMod` 5; s = q + r in s)
  print (case (1, (2, 3)) of (a, (b, c)) -> a + b + c)
  print ((\(x, y) -> x - y) (10, 4))
  print ((\x -> case x of { Just y -> y; Nothing -> 0 }) (Just 9))
  print ((subtract 1 . (* 2)) 5)
  print ((`div` 2) 9 + (2 `div`) 9)
  print ((3 -) 1)
