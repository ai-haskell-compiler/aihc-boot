f x
  | a, b = 1
  | otherwise = 2
f = (case x of A | a, b -> 1)
f = case x of
  A -> 1
  where y = 2
f = x where
g = 1
f = \case
  A -> 1
  B -> 2
f = do
  if a
  then b
  else c
class C a where
