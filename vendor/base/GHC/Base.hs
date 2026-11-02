-- | Basic functions, lists and the IO monad.
module GHC.Base
  ( String,
    (.),
    ($),
    ($!),
    id,
    const,
    flip,
    otherwise,
    until,
    asTypeOf,
    foldr,
    map,
    (++),
    (>>=),
    (>>),
    return,
    mapM_,
    sequence_,
    when,
    unless,
  )
where

import GHC.Prim
import GHC.Types

infixr 9 .

infixr 0 $, $!

infixr 5 ++

infixl 1 >>=, >>

type String = [Char]

-- | Function composition.
(.) :: (b -> c) -> (a -> b) -> a -> c
(.) f g = \x -> f (g x)

($) :: (a -> b) -> a -> b
f $ x = f x

-- | Strict application.
($!) :: (a -> b) -> a -> b
f $! x = x `seq` f x

id :: a -> a
id x = x

const :: a -> b -> a
const x _ = x

flip :: (a -> b -> c) -> b -> a -> c
flip f x y = f y x

otherwise :: Bool
otherwise = True

until :: (a -> Bool) -> (a -> a) -> a -> a
until p f = go
  where
    go x = if p x then x else go (f x)

asTypeOf :: a -> a -> a
asTypeOf = const

-- | Right-associative fold of a list.
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr k z = go
  where
    go [] = z
    go (y : ys) = k y (go ys)

map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x : xs) = f x : map f xs

(++) :: [a] -> [a] -> [a]
(++) [] ys = ys
(++) (x : xs) ys = x : (xs ++ ys)

-- | The IO monad, on IO only: the boot base has no classes yet.
(>>=) :: IO a -> (a -> IO b) -> IO b
(>>=) (IO m) k = IO (\w -> case m w of IORes a -> unIO (k a) w)

(>>) :: IO a -> IO b -> IO b
(>>) m k = m >>= \_ -> k

return :: a -> IO a
return x = IO (\_ -> IORes x)

unIO :: IO a -> World -> IORes a
unIO (IO m) = m

mapM_ :: (a -> IO b) -> [a] -> IO ()
mapM_ f = foldr (\x r -> f x >> r) (return ())

sequence_ :: [IO a] -> IO ()
sequence_ = foldr (>>) (return ())

when :: Bool -> IO () -> IO ()
when p s = if p then s else return ()

unless :: Bool -> IO () -> IO ()
unless p s = if p then return () else s
