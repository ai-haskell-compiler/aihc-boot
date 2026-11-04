-- | Simple combinators on functions.
module Data.Function
  ( id,
    const,
    (.),
    flip,
    ($),
    (&),
    on,
    fix,
    applyWhen,
  )
where

import GHC.Base
import GHC.Types

infixl 0 `on`

infixl 1 &

-- | The least fixed point of a function.
fix :: (a -> a) -> a
fix f = let x = f x in x

-- | Apply a binary function to the results of a unary function.
on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
on op f x y = f x `op` f y

-- | Reverse application.
(&) :: a -> (a -> b) -> b
x & f = f x

applyWhen :: Bool -> (a -> a) -> a -> a
applyWhen True f x = f x
applyWhen False _ x = x
