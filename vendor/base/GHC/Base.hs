-- | Basic functions.
module GHC.Base
  ( (.),
    foldr,
  )
where

infixr 9 .

-- | Function composition.
(.) :: (b -> c) -> (a -> b) -> a -> c
(.) f g = \x -> f (g x)

-- | Right-associative fold of a list.
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr k z = go
  where
    go [] = z
    go (y : ys) = k y (go ys)
