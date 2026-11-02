-- | The classes 'Enum' and 'Bounded'. Arithmetic sequences desugar to
-- the methods of 'Enum'.
module GHC.Enum
  ( Enum (..),
    Bounded (..),
  )
where

import GHC.Classes
import GHC.Err
import GHC.Num
import GHC.Num.Integer
import GHC.Prim
import GHC.Types

class Bounded a where
  minBound, maxBound :: a

class Enum a where
  succ, pred :: a -> a
  toEnum :: Int -> a
  fromEnum :: a -> Int
  enumFrom :: a -> [a]
  enumFromThen :: a -> a -> [a]
  enumFromTo :: a -> a -> [a]
  enumFromThenTo :: a -> a -> a -> [a]
  succ x = toEnum (fromEnum x + 1)
  pred x = toEnum (fromEnum x - 1)
  enumFrom x = mapEnum toEnum (enumFrom (fromEnum x))
  enumFromThen x y = mapEnum toEnum (enumFromThen (fromEnum x) (fromEnum y))
  enumFromTo x y = mapEnum toEnum (enumFromTo (fromEnum x) (fromEnum y))
  enumFromThenTo x y z = mapEnum toEnum (enumFromThenTo (fromEnum x) (fromEnum y) (fromEnum z))

mapEnum :: (a -> b) -> [a] -> [b]
mapEnum _ [] = []
mapEnum f (x : xs) = f x : mapEnum f xs

instance Enum Int where
  succ x = x + 1
  pred x = x - 1
  toEnum x = x
  fromEnum x = x
  enumFrom x = x : enumFrom (x + 1)
  enumFromThen x y = go x
    where
      step = y - x
      go n = n : go (n + step)
  enumFromTo x y = if x > y then [] else x : enumFromTo (x + 1) y
  enumFromThenTo x y z
    | step >= 0 = up x
    | True = down x
    where
      step = y - x
      up n = if n > z then [] else n : up (n + step)
      down n = if n < z then [] else n : down (n + step)

instance Bounded Int where
  minBound = negate 9223372036854775807 - 1
  maxBound = 9223372036854775807

instance Enum Integer where
  succ x = x + 1
  pred x = x - 1
  toEnum = integerFromInt
  fromEnum = integerToInt
  enumFrom x = x : enumFrom (x + 1)
  enumFromThen x y = go x
    where
      step = y - x
      go n = n : go (n + step)
  enumFromTo x y = if x > y then [] else x : enumFromTo (x + 1) y
  enumFromThenTo x y z
    | step >= 0 = up x
    | True = down x
    where
      step = y - x
      up n = if n > z then [] else n : up (n + step)
      down n = if n < z then [] else n : down (n + step)

instance Enum Char where
  toEnum = primChr
  fromEnum = primOrd
  enumFrom c = enumFromTo c '\1114111'
  enumFromThen x y = enumFromThenTo x y (if y >= x then '\1114111' else '\0')

instance Bounded Char where
  minBound = '\0'
  maxBound = '\1114111'

instance Enum Bool where
  toEnum 0 = False
  toEnum 1 = True
  toEnum _ = error "Prelude.Enum.Bool.toEnum: bad argument"
  fromEnum False = 0
  fromEnum True = 1
  enumFrom x = enumFromTo x True
  enumFromThen x y = enumFromThenTo x y (if fromEnum y >= fromEnum x then True else False)

instance Bounded Bool where
  minBound = False
  maxBound = True

instance Enum Ordering where
  toEnum 0 = LT
  toEnum 1 = EQ
  toEnum 2 = GT
  toEnum _ = error "Prelude.Enum.Ordering.toEnum: bad argument"
  fromEnum LT = 0
  fromEnum EQ = 1
  fromEnum GT = 2
  enumFrom x = enumFromTo x GT
  enumFromThen x y = enumFromThenTo x y (if fromEnum y >= fromEnum x then GT else LT)

instance Bounded Ordering where
  minBound = LT
  maxBound = GT

instance Enum () where
  toEnum _ = ()
  fromEnum () = 0
  enumFrom () = [()]
  enumFromThen () () = repeatUnit
    where
      repeatUnit = () : repeatUnit

instance Bounded () where
  minBound = ()
  maxBound = ()
