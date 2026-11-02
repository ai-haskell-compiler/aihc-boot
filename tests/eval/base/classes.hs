{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Type classes: classes with superclasses and defaults, instances with
-- contexts, polymorphic and local bindings, deriving, records, Functor
-- and Monad instances, Foldable and Traversable, return-type
-- polymorphism and defaulting.
import Control.Exception
import Data.List (sortBy)

class Shape a where
  area :: a -> Int
  name :: a -> String
  name _ = "shape"
  describe :: a -> String
  describe x = name x ++ " with area " ++ show (area x)

data Square = Square Int

data Rect = Rect {width :: Int, height :: Int}
  deriving (Eq, Show)

instance Shape Square where
  area (Square s) = s * s
  name _ = "square"

instance Shape Rect where
  area r = width r * height r

class (Shape a) => Solid a where
  volume :: a -> Int -> Int
  volume x depth = area x * depth

instance Solid Square

data Color = Red | Green | Blue
  deriving (Eq, Ord, Show, Enum, Bounded)

data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving (Show, Eq)

instance Functor Tree where
  fmap _ Leaf = Leaf
  fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)

instance Foldable Tree where
  foldr _ z Leaf = z
  foldr f z (Node l x r) = foldr f (f x (foldr f z r)) l

insert :: (Ord a) => a -> Tree a -> Tree a
insert x Leaf = Node Leaf x Leaf
insert x t@(Node l y r)
  | x < y = Node (insert x l) y r
  | x > y = Node l y (insert x r)
  | otherwise = t

newtype State s a = State {runState :: s -> (a, s)}

instance Functor (State s) where
  fmap f (State g) = State (\s -> let (a, s') = g s in (f a, s'))

instance Applicative (State s) where
  pure a = State (\s -> (a, s))
  State f <*> State g = State (\s -> let (h, s1) = f s; (a, s2) = g s1 in (h a, s2))

instance Monad (State s) where
  State g >>= k = State (\s -> let (a, s') = g s in runState (k a) s')

get :: State s s
get = State (\s -> (s, s))

put :: s -> State s ()
put s = State (\_ -> ((), s))

counter :: State Int [Int]
counter = do
  a <- tick
  b <- tick
  c <- tick
  return [a, b, c]
  where
    tick = do
      n <- get
      put (n + 1)
      return n

class Container f where
  empty :: f a
  insertC :: a -> f a -> f a
  toL :: f a -> [a]

newtype Stack a = Stack [a]

instance Container Stack where
  empty = Stack []
  insertC x (Stack xs) = Stack (x : xs)
  toL (Stack xs) = xs

fill :: (Container f) => [a] -> f a
fill = foldr insertC empty

data MyErr = MyErr String
  deriving (Show)

instance Exception MyErr

pairUp :: forall a b. (Show a, Show b) => a -> b -> String
pairUp x y = go x
  where
    go :: a -> String
    go v = show v ++ "/" ++ show y

classify :: Int -> String
classify n = if | n < 0 -> "negative" | n == 0 -> "zero" | otherwise -> "positive"

main :: IO ()
main = do
  putStrLn (describe (Square 3))
  putStrLn (describe (Rect 2 5))
  print (volume (Square 2) 10)
  print (Rect 1 2, Rect {height = 4, width = 3} == Rect 3 4)
  print ((Rect 1 2) {height = 9})
  print [minBound .. maxBound :: Color]
  print (succ Red, fromEnum Blue, compare Red Blue, maximum [Green, Red, Blue])
  let t = foldr insert Leaf [5, 2, 8, 1, 9, 3 :: Int]
  print (sum t, product t, length t, maximum t, elem 8 t, foldr (:) [] (fmap (* 10) t))
  print (fmap show (Node Leaf True Leaf))
  print (fst (runState counter 10))
  print (toL (fill "abc" :: Stack Char))
  print (sortBy (\a b -> compare b a) [3, 1, 2 :: Int])
  print (traverse (\x -> if x > 0 then Just x else Nothing) [1, 2, 3 :: Int])
  print (traverse (\x -> if x > 0 then Just x else Nothing) [1, -2, 3 :: Int])
  print (mapM (\x -> [x, x * 10]) [1, 2 :: Int])
  print (2 ^ 70, fromIntegral (3 :: Int) + (1 :: Integer))
  print (show 3 ++ show (negate 4 :: Integer) ++ show (2 ^ 64 `div` 3))
  r <- try (throwIO (MyErr "oops"))
  case r of
    Left (MyErr m) -> putStrLn ("caught " ++ m)
    Right () -> putStrLn "nothing"
  handle (\(e :: SomeException) -> putStrLn ("some: " ++ show e)) (throwIO (MyErr "boom"))
  handle (\(e :: ArithException) -> print e) (print (1 `mod` (0 :: Int)))
  putStrLn (pairUp 'x' [True])
  mapM_ (putStrLn . classify) [-5, 0, 5]
  let poly :: (Show x) => x -> String
      poly v = "<" ++ show v ++ ">"
  putStrLn (poly (1 :: Int) ++ poly "s" ++ poly (Just Green))
  print (mconcat ["ab", "cd"], mempty :: String, [1, 2] <> [3 :: Int])
  print (words "a b  c", unwords ["x", "y"], lines "l1\nl2")
