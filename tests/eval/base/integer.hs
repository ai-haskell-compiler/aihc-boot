-- Arbitrary-precision Integer arithmetic. The boot base has no Num or
-- Show class yet, so the test uses the functions of GHC.Num.Integer.
import Data.Char (chr, ord)
import GHC.Num.Integer

int :: Int -> Integer
int = integerFromInt

-- The decimal digits of an Integer.
showInteger :: Integer -> String
showInteger n
  | integerIsNegative n = '-' : digits (integerNegate n) ""
  | otherwise = digits n ""
  where
    digits m acc =
      let (q, r) = integerQuotRem m (int 10)
          acc' = chr (ord '0' + integerToInt r) : acc
       in if integerIsZero q then acc' else digits q acc'

say :: Integer -> IO ()
say n = putStrLn (showInteger n)

power :: Integer -> Int -> Integer
power _ 0 = integerOne
power b e = integerMul b (power b (e - 1))

factorial :: Int -> Integer
factorial n = go n integerOne
  where
    go 0 acc = acc
    go k acc = go (k - 1) (integerMul (int k) acc)

fib :: Int -> Integer
fib n = go n integerZero integerOne
  where
    go 0 a _ = a
    go k a b = go (k - 1) b (integerAdd a b)

-- Every combination of signs for the four divisions.
divisions :: Integer -> Integer -> IO ()
divisions a b = do
  let (q, r) = integerQuotRem a b
      (d, m) = integerDivMod a b
  say q
  say r
  say d
  say m
  print (bool (integerEq a (integerAdd (integerMul q b) r)))
  print (bool (integerEq a (integerAdd (integerMul d b) m)))

bool :: Bool -> Int
bool b = if b then 1 else 0

ordering :: Ordering -> Int
ordering LT = -1
ordering EQ = 0
ordering GT = 1

main :: IO ()
main = do
  let big = power (int 2) 64
      maxInt = integerSub (power (int 2) 63) integerOne
      minInt = integerNegate (power (int 2) 63)
  say (factorial 30)
  say big
  say (integerSub (power (int 2) 100) integerOne)
  say (integerNegate (power (int 3) 50))
  say (fib 200)
  -- Around the bounds of Int.
  say (integerAdd maxInt integerOne)
  say (integerSub minInt integerOne)
  print (integerToInt maxInt)
  print (integerToInt minInt)
  print (integerToInt (integerSub (integerAdd maxInt integerOne) integerOne))
  print (integerToInt (integerAdd big (int 5)))
  print (integerToInt (integerNegate (integerAdd big (int 5))))
  print (integerToInt (integerAdd maxInt integerOne))
  say (integerMul minInt (int (-1)))
  say (integerQuot minInt (int (-1)))
  say (integerMul maxInt maxInt)
  -- Division.
  let x = integerAdd (power (int 10) 30) (int 7)
      y = integerSub (power (int 7) 20) (int 3)
  divisions x y
  divisions (integerNegate x) y
  divisions x (integerNegate y)
  divisions (integerNegate x) (integerNegate y)
  divisions x (int 13)
  divisions (integerNegate x) (int 13)
  divisions (int (-7)) (int 2)
  say (integerQuot (factorial 100) (factorial 98))
  say (integerGcd (factorial 20) (power (int 6) 30))
  say (integerLcm (power (int 4) 20) (integerNegate (power (int 6) 20)))
  say (integerGcd (int 0) (int 0))
  -- Signs and comparisons.
  say (integerAbs (integerNegate big))
  say (integerSignum (integerNegate big))
  say (integerSignum integerZero)
  say (integerSignum big)
  mapM_ (print . bool) [integerLt minInt big, integerLt big minInt, integerLe big big, integerGt big maxInt, integerGe minInt big, integerNe big maxInt]
  mapM_ (print . ordering) [integerCompare big maxInt, integerCompare (integerNegate big) minInt, integerCompare (integerAdd maxInt integerOne) (power (int 2) 63)]
