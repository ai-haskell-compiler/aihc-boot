-- | The classes 'Eq' and 'Ord', and the Boolean operations.
module GHC.Classes
  ( Eq (..),
    Ord (..),
    (&&),
    (||),
    not,
  )
where

import GHC.Prim
import GHC.Types

infix 4 ==, /=, <, <=, >, >=

infixr 3 &&

infixr 2 ||

class Eq a where
  (==), (/=) :: a -> a -> Bool
  x /= y = not (x == y)
  x == y = not (x /= y)

class (Eq a) => Ord a where
  compare :: a -> a -> Ordering
  (<), (<=), (>), (>=) :: a -> a -> Bool
  max, min :: a -> a -> a
  compare x y = if x == y then EQ else if x <= y then LT else GT
  x < y = case compare x y of
    LT -> True
    _ -> False
  x <= y = case compare x y of
    GT -> False
    _ -> True
  x > y = case compare x y of
    GT -> True
    _ -> False
  x >= y = case compare x y of
    LT -> False
    _ -> True
  max x y = if x <= y then y else x
  min x y = if x <= y then x else y

(&&) :: Bool -> Bool -> Bool
True && x = x
False && _ = False

(||) :: Bool -> Bool -> Bool
True || _ = True
False || x = x

not :: Bool -> Bool
not True = False
not False = True

instance Eq Int where
  (==) = primIntEq
  (/=) = primIntNe

instance Ord Int where
  compare x y = if primIntLt x y then LT else if primIntEq x y then EQ else GT
  (<) = primIntLt
  (<=) = primIntLe
  (>) = primIntGt
  (>=) = primIntGe

instance Eq Char where
  (==) = primCharEq
  (/=) = primCharNe

instance Ord Char where
  compare x y = if primCharLt x y then LT else if primCharEq x y then EQ else GT
  (<) = primCharLt
  (<=) = primCharLe
  (>) = primCharGt
  (>=) = primCharGe

instance Eq Bool where
  True == True = True
  False == False = True
  _ == _ = False

instance Ord Bool where
  compare False True = LT
  compare True False = GT
  compare _ _ = EQ

instance Eq Ordering where
  LT == LT = True
  EQ == EQ = True
  GT == GT = True
  _ == _ = False

instance Ord Ordering where
  compare x y = compare (tag x) (tag y)
    where
      tag LT = primOrd 'a'
      tag EQ = primOrd 'b'
      tag GT = primOrd 'c'

instance Eq () where
  _ == _ = True

instance Ord () where
  compare _ _ = EQ

instance (Eq a) => Eq [a] where
  [] == [] = True
  (x : xs) == (y : ys) = x == y && xs == ys
  _ == _ = False

instance (Ord a) => Ord [a] where
  compare [] [] = EQ
  compare [] (_ : _) = LT
  compare (_ : _) [] = GT
  compare (x : xs) (y : ys) = case compare x y of
    EQ -> compare xs ys
    other -> other

instance (Eq a, Eq b) => Eq (a, b) where
  (a, b) == (c, d) = a == c && b == d

instance (Ord a, Ord b) => Ord (a, b) where
  compare (a, b) (c, d) = case compare a c of
    EQ -> compare b d
    other -> other

instance (Eq a, Eq b, Eq c) => Eq (a, b, c) where
  (a, b, c) == (d, e, f) = a == d && b == e && c == f

instance (Ord a, Ord b, Ord c) => Ord (a, b, c) where
  compare (a, b, c) (d, e, f) = case compare a d of
    EQ -> case compare b e of
      EQ -> compare c f
      other -> other
    other -> other

instance (Eq a, Eq b, Eq c, Eq d) => Eq (a, b, c, d) where
  (a, b, c, d) == (e, f, g, h) = a == e && b == f && c == g && d == h

instance (Ord a, Ord b, Ord c, Ord d) => Ord (a, b, c, d) where
  compare (a, b, c, d) (e, f, g, h) = case compare a e of
    EQ -> compare (b, c, d) (f, g, h)
    other -> other

instance (Eq a, Eq b, Eq c, Eq d, Eq e) => Eq (a, b, c, d, e) where
  (a, b, c, d, e) == (f, g, h, i, j) = a == f && b == g && c == h && d == i && e == j

instance (Ord a, Ord b, Ord c, Ord d, Ord e) => Ord (a, b, c, d, e) where
  compare (a, b, c, d, e) (f, g, h, i, j) = case compare a f of
    EQ -> compare (b, c, d, e) (g, h, i, j)
    other -> other

instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f) => Eq (a, b, c, d, e, f) where
  (a, b, c, d, e, f) == (g, h, i, j, k, l) = a == g && b == h && c == i && d == j && e == k && f == l

instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f) => Ord (a, b, c, d, e, f) where
  compare (a, b, c, d, e, f) (g, h, i, j, k, l) = case compare a g of
    EQ -> compare (b, c, d, e, f) (h, i, j, k, l)
    other -> other

instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g) => Eq (a, b, c, d, e, f, g) where
  (a, b, c, d, e, f, g) == (h, i, j, k, l, m, n) = a == h && b == i && c == j && d == k && e == l && f == m && g == n

instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g) => Ord (a, b, c, d, e, f, g) where
  compare (a, b, c, d, e, f, g) (h, i, j, k, l, m, n) = case compare a h of
    EQ -> compare (b, c, d, e, f, g) (i, j, k, l, m, n)
    other -> other
