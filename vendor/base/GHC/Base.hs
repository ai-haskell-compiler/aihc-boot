{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

-- | Basic functions and the classes 'Semigroup', 'Monoid', 'Functor',
-- 'Applicative' and 'Monad'. A @do@ block desugars to '>>=' and '>>'.
module GHC.Base
  ( String,
    Semigroup (..),
    Monoid (..),
    Functor (..),
    Applicative (..),
    Monad (..),
    Alternative (..),
    MonadPlus (..),
    NonEmpty (..),
    (<$>),
    (=<<),
    liftA,
    liftA3,
    liftM,
    liftM2,
    ap,
    join,
    when,
    unless,
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
    unIO,
    realWorld#,
    ord,
    unsafeChr,
  )
where

import GHC.Classes
import GHC.Err
import GHC.Maybe
import GHC.Num
import GHC.Prim
import GHC.Real
import GHC.Types

infixr 9 .

infixr 0 $, $!

infixr 5 ++

infixl 1 >>=, >>

infixr 1 =<<

infixl 4 <$>, <$, <*>, *>, <*

infixl 3 <|>

infixr 6 <>

infixr 5 :|

type String = [Char]

-- | A non-empty list.
data NonEmpty a = a :| [a]

class Semigroup a where
  (<>) :: a -> a -> a
  sconcat :: NonEmpty a -> a
  stimes :: (Integral b) => b -> a -> a
  sconcat (a :| as) = go a as
    where
      go b (c : cs) = b <> go c cs
      go b [] = b
  stimes n x
    | n <= 0 = errorWithoutStackTrace "stimes: positive multiplier expected"
    | n == 1 = x
    | True = x <> stimes (n - 1) x

class (Semigroup a) => Monoid a where
  mempty :: a
  mappend :: a -> a -> a
  mconcat :: [a] -> a
  mappend = (<>)
  mconcat = foldr mappend mempty

class Functor f where
  fmap :: (a -> b) -> f a -> f b
  (<$) :: a -> f b -> f a
  (<$) = fmap . const

class (Functor f) => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
  liftA2 :: (a -> b -> c) -> f a -> f b -> f c
  (*>) :: f a -> f b -> f b
  (<*) :: f a -> f b -> f a
  (<*>) = liftA2 id
  liftA2 f x y = fmap f x <*> y
  a1 *> a2 = (id <$ a1) <*> a2
  a1 <* a2 = liftA2 const a1 a2

class (Applicative m) => Monad m where
  (>>=) :: m a -> (a -> m b) -> m b
  (>>) :: m a -> m b -> m b
  return :: a -> m a
  m >> k = m >>= \_ -> k
  return = pure

-- | A monoid on applicative functors.
class (Applicative f) => Alternative f where
  empty :: f a
  (<|>) :: f a -> f a -> f a
  some :: f a -> f [a]
  many :: f a -> f [a]
  some v = liftA2 (:) v (many v)
  many v = some v <|> pure []

-- | Monads that also support choice and failure.
class (Alternative m, Monad m) => MonadPlus m where
  mzero :: m a
  mplus :: m a -> m a -> m a
  mzero = empty
  mplus = (<|>)

(<$>) :: (Functor f) => (a -> b) -> f a -> f b
(<$>) = fmap

(=<<) :: (Monad m) => (a -> m b) -> m a -> m b
f =<< x = x >>= f

liftA :: (Applicative f) => (a -> b) -> f a -> f b
liftA f a = pure f <*> a

liftA3 :: (Applicative f) => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
liftA3 f a b c = liftA2 f a b <*> c

liftM :: (Monad m) => (a1 -> r) -> m a1 -> m r
liftM f m1 = m1 >>= \x1 -> return (f x1)

liftM2 :: (Monad m) => (a1 -> a2 -> r) -> m a1 -> m a2 -> m r
liftM2 f m1 m2 = m1 >>= \x1 -> m2 >>= \x2 -> return (f x1 x2)

ap :: (Monad m) => m (a -> b) -> m a -> m b
ap m1 m2 = m1 >>= \x1 -> m2 >>= \x2 -> return (x1 x2)

join :: (Monad m) => m (m a) -> m a
join x = x >>= id

when :: (Applicative f) => Bool -> f () -> f ()
when p s = if p then s else pure ()

unless :: (Applicative f) => Bool -> f () -> f ()
unless p s = if p then pure () else s

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

unIO :: IO a -> State# RealWorld -> (# State# RealWorld, a #)
unIO (IO m) = m

-- | The state token of the real world. At run time it is the unit value.
realWorld# :: State# RealWorld
realWorld# = primUnsafeCoerce ()

ord :: Char -> Int
ord = primOrd

unsafeChr :: Int -> Char
unsafeChr = primChr

-- Instances for lists.

instance Semigroup [a] where
  (<>) = (++)

instance Monoid [a] where
  mempty = []
  mconcat xss = [x | xs <- xss, x <- xs]

instance Functor [] where
  fmap = map

instance Applicative [] where
  pure x = [x]
  fs <*> xs = [f x | f <- fs, x <- xs]
  liftA2 f xs ys = [f x y | x <- xs, y <- ys]
  xs *> ys = [y | _ <- xs, y <- ys]

instance Monad [] where
  xs >>= f = [y | x <- xs, y <- f x]

instance Alternative [] where
  empty = []
  (<|>) = (++)

instance MonadPlus []

-- Instances for Maybe.

instance (Semigroup a) => Semigroup (Maybe a) where
  Nothing <> b = b
  a <> Nothing = a
  Just a <> Just b = Just (a <> b)

instance (Semigroup a) => Monoid (Maybe a) where
  mempty = Nothing

instance Functor Maybe where
  fmap _ Nothing = Nothing
  fmap f (Just a) = Just (f a)

instance Applicative Maybe where
  pure = Just
  Just f <*> m = fmap f m
  Nothing <*> _ = Nothing
  liftA2 f (Just x) (Just y) = Just (f x y)
  liftA2 _ _ _ = Nothing
  Just _ *> m = m
  Nothing *> _ = Nothing

instance Monad Maybe where
  (Just x) >>= k = k x
  Nothing >>= _ = Nothing

instance Alternative Maybe where
  empty = Nothing
  Nothing <|> r = r
  l <|> _ = l

instance MonadPlus Maybe

-- Instances for IO.

instance Functor IO where
  fmap f (IO m) = IO (\s -> case m s of (# s', a #) -> (# s', f a #))

instance Applicative IO where
  pure x = IO (\s -> (# s, x #))
  mf <*> mx = mf >>= \f -> mx >>= \x -> pure (f x)
  liftA2 f ma mb = ma >>= \a -> mb >>= \b -> pure (f a b)
  ma *> mb = ma >>= \_ -> mb

instance Monad IO where
  (IO m) >>= k = IO (\s -> case m s of (# s', a #) -> unIO (k a) s')

instance (Semigroup a) => Semigroup (IO a) where
  (<>) = liftA2 (<>)

instance (Monoid a) => Monoid (IO a) where
  mempty = pure mempty

-- Other instances.

instance Semigroup Ordering where
  LT <> _ = LT
  EQ <> y = y
  GT <> _ = GT

instance Monoid Ordering where
  mempty = EQ

instance Semigroup () where
  _ <> _ = ()

instance Monoid () where
  mempty = ()

instance (Semigroup a, Semigroup b) => Semigroup (a, b) where
  (a, b) <> (a', b') = (a <> a', b <> b')

instance (Monoid a, Monoid b) => Monoid (a, b) where
  mempty = (mempty, mempty)

instance Functor ((,) a) where
  fmap f (x, y) = (x, f y)

instance Functor NonEmpty where
  fmap f (a :| as) = f a :| map f as

instance Semigroup (NonEmpty a) where
  (a :| as) <> (b :| bs) = a :| (as ++ b : bs)
