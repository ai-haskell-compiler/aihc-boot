-- Allocation and deep recursion: the collector and the stack must cope.
import Data.List (foldl')

count :: Int -> Int
count n = go 0 1
  where
    go !acc i
      | i > n = acc
      | otherwise = go (acc + i) (i + 1)

mkList :: Int -> [Int]
mkList n = [1 .. n]

main :: IO ()
main = do
  print (count 1000000)
  print (foldl' (+) 0 (mkList 1000000))
  print (length (mkList 1000000))
  print (foldr (+) 0 (mkList 200000))
  print (sum (map (`mod` 7) (mkList 300000)))
  print (length (show (product (mkList 15))))
  let big = concatMap (\i -> replicate 10 i) (mkList 50000)
  print (length big)
  print (last big)
  let pairs = zip (mkList 100000) (reverse (mkList 100000))
  print (foldl' (\acc (a, b) -> acc + a * b `mod` 13) 0 pairs)
