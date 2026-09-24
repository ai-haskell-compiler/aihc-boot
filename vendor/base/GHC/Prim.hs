{-# LANGUAGE BangPatterns #-}

-- | The primitive operations.
module GHC.Prim
  ( seq,
  )
where

infixr 0 `seq`

-- | Evaluate the first argument to weak head normal form, then return
-- the second argument.
seq :: a -> b -> b
seq !_ b = b
