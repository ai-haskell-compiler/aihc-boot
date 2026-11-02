-- Scoping: shadowing, where over guards, mutual recursion, local
-- operators, sections and polymorphic local functions.
module Main (main) where

import qualified Data.List as L
import Data.Char (toUpper)

x :: Int
x = 1

shadow :: Int -> Int
shadow x = let x' = x + 1 in x' * 2

isEven, isOdd :: Int -> Bool
isEven 0 = True
isEven n = isOdd (n - 1)
isOdd 0 = False
isOdd n = isEven (n - 1)

collatz :: Int -> Int
collatz n = go n 0
  where
    go 1 steps = steps
    go m steps
      | even m = go (m `div` 2) (steps + 1)
      | otherwise = go (3 * m + 1) (steps + 1)

applyTwice :: (a -> a) -> a -> a
applyTwice f = f . f

compose3 :: [Int -> Int] -> Int -> Int
compose3 fs = foldr (.) id fs

main :: IO ()
main = do
  print x
  print (shadow x)
  print (let x = 10 in x + 1)
  print (if isEven 10 && isOdd 7 then 1 else 0)
  print (collatz 27)
  print (applyTwice (* 3) 2)
  print (compose3 [(+ 1), (* 2), subtract 3] 10)
  let pair = (,) 1
  print (snd (pair 2))
  let (a, b) = (applyTwice (+ 1) 0, applyTwice ('x' :) "")
  print a
  putStrLn b
  print (L.foldl' (+) 0 [1, 2, 3])
  putStrLn (map toUpper (L.intercalate "-" ["a", "b"]))
  let f = id
  print (f 1)
  putStrLn (f "poly")
  print (let infixr 5 +++; (+++) = (++) in length ([1] +++ [2, 3] +++ []))
  print ((\x -> \y -> x - y) 10 3)
  print (let g x y z = x * 100 + y * 10 + z; h = g 1 in h 2 3)
  print (case 5 of n | n > 3 -> n * 2; _ -> 0)
  print (let xs = [1, 2, 3]; ys = map (* x) xs where x = 10 in sum ys)
