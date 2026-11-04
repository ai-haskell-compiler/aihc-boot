{-# LANGUAGE MagicHash #-}

-- | Machine addresses. As in GHC, a 'Ptr' holds an 'Addr#', and an
-- 'Addr#' is an Int at run time.
module GHC.Ptr
  ( Ptr (..),
    nullPtr,
    castPtr,
    plusPtr,
    minusPtr,
    ptrToInt,
    intToPtr,
  )
where

import GHC.Base
import GHC.Classes
import GHC.Num
import GHC.Prim
import GHC.Show
import GHC.Types

-- | An address of a value of type a.
data Ptr a = Ptr Addr#

ptrToInt :: Ptr a -> Int
ptrToInt = primUnsafeCoerce

intToPtr :: Int -> Ptr a
intToPtr = primUnsafeCoerce

nullPtr :: Ptr a
nullPtr = intToPtr 0

castPtr :: Ptr a -> Ptr b
castPtr = primUnsafeCoerce

plusPtr :: Ptr a -> Int -> Ptr b
plusPtr p n = intToPtr (ptrToInt p + n)

minusPtr :: Ptr a -> Ptr b -> Int
minusPtr p q = ptrToInt p - ptrToInt q

instance Eq (Ptr a) where
  p == q = ptrToInt p == ptrToInt q

instance Ord (Ptr a) where
  compare p q = compare (ptrToInt p) (ptrToInt q)

instance Show (Ptr a) where
  showsPrec _ p = showString "0x" . showString (primWordShow (ptrToInt p))
