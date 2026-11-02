-- Pattern matching compiles to nested cases with fall-through.
module Main where

f :: Int -> [Int] -> Int
f 0 [] = 1
f n (x : xs)
  | x > n = x
  | otherwise = f (n - 1) xs
  where
    unused = n
f _ _ = 0

g :: Maybe (Int, Int) -> Int
g m = case m of
  Just (a, b) | a == b -> a
  Just (a, _) -> a + 1
  Nothing -> 0

main :: IO ()
main = do
  print (f 3 [1, 2, 3])
  x <- getLine
  let n = length x
  print (g (Just (n, n)))
