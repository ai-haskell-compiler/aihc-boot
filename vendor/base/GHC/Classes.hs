-- | Comparisons and Boolean operations. The boot base has no classes yet:
-- the comparisons work on Int only, and the Char comparisons have their
-- own names.
module GHC.Classes
  ( (==),
    (/=),
    (<),
    (<=),
    (>),
    (>=),
    compare,
    max,
    min,
    eqChar,
    neChar,
    ltChar,
    leChar,
    gtChar,
    geChar,
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

(==) :: Int -> Int -> Bool
(==) = primIntEq

(/=) :: Int -> Int -> Bool
(/=) = primIntNe

(<) :: Int -> Int -> Bool
(<) = primIntLt

(<=) :: Int -> Int -> Bool
(<=) = primIntLe

(>) :: Int -> Int -> Bool
(>) = primIntGt

(>=) :: Int -> Int -> Bool
(>=) = primIntGe

compare :: Int -> Int -> Ordering
compare x y
  | primIntLt x y = LT
  | primIntEq x y = EQ
  | True = GT

max :: Int -> Int -> Int
max x y = if primIntLe x y then y else x

min :: Int -> Int -> Int
min x y = if primIntLe x y then x else y

eqChar :: Char -> Char -> Bool
eqChar = primCharEq

neChar :: Char -> Char -> Bool
neChar = primCharNe

ltChar :: Char -> Char -> Bool
ltChar = primCharLt

leChar :: Char -> Char -> Bool
leChar = primCharLe

gtChar :: Char -> Char -> Bool
gtChar = primCharGt

geChar :: Char -> Char -> Bool
geChar = primCharGe

(&&) :: Bool -> Bool -> Bool
True && x = x
False && _ = False

(||) :: Bool -> Bool -> Bool
True || _ = True
False || x = x

not :: Bool -> Bool
not True = False
not False = True
