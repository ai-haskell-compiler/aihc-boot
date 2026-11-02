-- Arithmetic and comparisons on Int.
main :: IO ()
main = do
  print (1 + 2 * 3 - 4)
  print (2 ^ 10)
  print (negate 5 `div` 2)
  print (negate 5 `quot` 2)
  print ((-7) `mod` 3)
  print ((-7) `rem` 3)
  print (7 `div` (-2))
  print (7 `mod` (-2))
  print (abs (-42) + signum (-3) + signum 0 + signum 9)
  print (max 3 7 * min 3 7)
  print (gcd 12 18 + lcm 4 6)
  print (if 3 < 4 && not (4 <= 3) then 1 else 0)
  print (if 3 == 3 || 1 /= 1 then 1 else 0)
  print (subtract 3 10)
  print (10 - 3 - 2)
  print (2 ^ 3 ^ 2)
  print (- 2 + 3)
  print (- 2 * 3)
  print (sum [1 .. 100])
  print (product [1 .. 10])
  print (maximum [3, 9, 2] - minimum [3, 9, 2])
  print (fst (divMod 17 5) + snd (quotRem 17 5))
  print 0x1F
  print 0o17
  print 0b101
  print (if even 4 && odd 7 then 1 else 0)
