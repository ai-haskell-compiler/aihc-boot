-- Lazy evaluation: infinite structures, unused bottoms, sharing.
import Data.List (intercalate)

showInts :: [Int] -> String
showInts xs = "[" ++ intercalate "," (map show xs) ++ "]"

fibs :: [Int]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

primes :: [Int]
primes = sieve [2 ..]
  where
    sieve (p : xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

ones :: [Int]
ones = 1 : ones

main :: IO ()
main = do
  putStrLn (showInts (take 10 fibs))
  putStrLn (showInts (take 10 primes))
  putStrLn (showInts (take 3 ones))
  print (const 1 (error "not forced"))
  print (fst (2, error "not forced either"))
  print (length [error "a", error "b"])
  let loop = loop + 1 :: Int
  print (if True then 7 else loop)
  print (head (filter (> 100) (map (* 3) [1 ..])))
  let xs = map (* 2) [1 .. 5]
  print (sum xs + product xs)
  print (snd (let p = (p, 3) in p))
  print (take 1 [x | x <- [1 ..], x > 1000] !! 0)
  print (case undefinedTail of (x : _) -> x)
  print (seq (3 :: Int) 4)
  print (fibs !! 50)
  where
    undefinedTail = 1 : error "tail is not needed"
