-- List functions from the Prelude and Data.List.
import Data.List (intercalate, intersperse, partition, sortBy, sort, nub, transpose, isPrefixOf, tails, inits, group, unfoldr)

showInts :: [Int] -> String
showInts xs = "[" ++ intercalate "," (map show xs) ++ "]"

showIntss :: [[Int]] -> String
showIntss xss = "[" ++ intercalate "," (map showInts xss) ++ "]"

main :: IO ()
main = do
  putStrLn (showInts (map (* 2) [1, 2, 3]))
  putStrLn (showInts (filter odd [1 .. 10]))
  print (foldr (-) 0 [1, 2, 3])
  print (foldl (-) 0 [1, 2, 3])
  print (length "hello")
  putStrLn (showInts (reverse [1, 2, 3]))
  putStrLn (showInts (take 3 [10 ..]))
  putStrLn (showInts (drop 2 [1, 2, 3, 4]))
  putStrLn (showInts (takeWhile (< 4) [1 ..]))
  putStrLn (showInts (dropWhile (< 4) [1 .. 6]))
  putStrLn (showInts [10, 8 .. 1])
  putStrLn (showInts [1, 3 .. 10])
  putStrLn (showInts (concat [[1], [2, 3], []]))
  putStrLn (showInts (concatMap (\x -> [x, x]) [1, 2]))
  putStrLn (showInts (zipWith (+) [1, 2, 3] [10, 20]))
  print ([1, 2, 3] !! 1)
  print (head [5, 6] + last [5, 6])
  putStrLn (showInts (init [1, 2, 3] ++ tail [1, 2, 3]))
  print (if null [] then 1 else 0)
  print (if elem 3 [1, 2, 3] && notElem 4 [1, 2, 3] then 1 else 0)
  putStrLn (showInts (replicate 3 7))
  putStrLn (showInts (take 7 (cycle [1, 2, 3])))
  putStrLn (showInts (scanl (+) 0 [1, 2, 3]))
  putStrLn (showInts (scanr (+) 0 [1, 2, 3]))
  putStrLn (showInts (scanl1 (+) [1, 2, 3]))
  putStrLn (showInts (scanr1 (+) [1, 2, 3]))
  putStrLn (showInts (sort [3, 1, 2, 5, 4, 1]))
  putStrLn (showInts (sortBy (\a b -> compare b a) [3, 1, 2]))
  putStrLn (showInts (nub [1, 2, 1, 3, 2]))
  putStrLn (showIntss (transpose [[1, 2, 3], [4, 5], [6]]))
  print (if isPrefixOf [1, 2] [1, 2, 3] then 1 else 0)
  putStrLn (showIntss (tails [1, 2]))
  putStrLn (showIntss (inits [1, 2]))
  putStrLn (showIntss (group [1, 1, 2, 3, 3, 3]))
  putStrLn (showInts (unfoldr (\n -> if n > 3 then Nothing else Just (n * n, n + 1)) 1))
  case partition even [1 .. 6] of
    (es, os) -> putStrLn (showInts es ++ " " ++ showInts os)
  case unzip [(1, 'a'), (2, 'b')] of
    (ns, cs) -> putStrLn (showInts ns ++ " " ++ cs)
  case span (< 3) [1 .. 5] of
    (a, b) -> putStrLn (showInts a ++ " " ++ showInts b)
  case break (> 3) [1 .. 5] of
    (a, b) -> putStrLn (showInts a ++ " " ++ showInts b)
  case splitAt 2 [1 .. 5] of
    (a, b) -> putStrLn (showInts a ++ " " ++ showInts b)
  putStrLn (intersperse '-' "abc")
  print (if and [True, False] || or [False, True] then 1 else 0)
  print (if any even [1, 3] || all odd [1, 3] then 1 else 0)
  print (foldl1 max [3, 8, 2] + foldr1 min [3, 8, 2])
  putStrLn (showInts [x + y | (x, y) <- zip [1, 2, 3] [10, 20, 30], x /= 2])
  putStrLn (showInts [x | Just x <- [Just 1, Nothing, Just 3]])
  putStrLn (showInts [y | x <- [1, 2], let y = x * 10])
  case lookup 2 [(1, "one"), (2, "two")] of
    Just s -> putStrLn s
    Nothing -> putStrLn "none"
