-- Let groups, sections, comprehensions and lambdas.
main :: IO ()
main = print (sum [y | x <- [1 .. 10], let y = x * x, odd y] + h 3)
  where
    h = let (a, b) = (1, 2); k z = a + b + z in k
