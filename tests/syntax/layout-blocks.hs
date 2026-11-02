f x = do
  a
  b
  where
    a = 1
    b = 2
g = 3
x = let a = 1 in a
y = let a = 1
        b = 2
    in a
z = (do a) + [case y of A -> 1]
xs = [y | x <- zs, let y = x, y > 0]
f = g where { h = Rec { a = 1 } }
