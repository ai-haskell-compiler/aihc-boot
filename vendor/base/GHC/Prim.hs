{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GHCForeignImportPrim #-}

-- | The primitive operations. The evaluator (src/eval.c) implements
-- every @foreign import prim@; the string names the primitive.
module GHC.Prim
  ( seq,
    error,
    primIntAdd,
    primIntSub,
    primIntMul,
    primIntQuot,
    primIntRem,
    primIntDiv,
    primIntMod,
    primIntNegate,
    primIntEq,
    primIntNe,
    primIntLt,
    primIntLe,
    primIntGt,
    primIntGe,
    primCharEq,
    primCharNe,
    primCharLt,
    primCharLe,
    primCharGt,
    primCharGe,
    primOrd,
    primChr,
    primIntShow,
    primPutChar,
    primGetChar,
    primFlush,
  )
where

import GHC.Types

infixr 0 `seq`

-- | Evaluate the first argument to weak head normal form, then return
-- the second argument.
seq :: a -> b -> b
seq !_ b = b

-- | Stop the program with a message.
foreign import prim "error" error :: [Char] -> a

foreign import prim "intAdd" primIntAdd :: Int -> Int -> Int

foreign import prim "intSub" primIntSub :: Int -> Int -> Int

foreign import prim "intMul" primIntMul :: Int -> Int -> Int

foreign import prim "intQuot" primIntQuot :: Int -> Int -> Int

foreign import prim "intRem" primIntRem :: Int -> Int -> Int

foreign import prim "intDiv" primIntDiv :: Int -> Int -> Int

foreign import prim "intMod" primIntMod :: Int -> Int -> Int

foreign import prim "intNegate" primIntNegate :: Int -> Int

foreign import prim "intEq" primIntEq :: Int -> Int -> Bool

foreign import prim "intNe" primIntNe :: Int -> Int -> Bool

foreign import prim "intLt" primIntLt :: Int -> Int -> Bool

foreign import prim "intLe" primIntLe :: Int -> Int -> Bool

foreign import prim "intGt" primIntGt :: Int -> Int -> Bool

foreign import prim "intGe" primIntGe :: Int -> Int -> Bool

foreign import prim "charEq" primCharEq :: Char -> Char -> Bool

foreign import prim "charNe" primCharNe :: Char -> Char -> Bool

foreign import prim "charLt" primCharLt :: Char -> Char -> Bool

foreign import prim "charLe" primCharLe :: Char -> Char -> Bool

foreign import prim "charGt" primCharGt :: Char -> Char -> Bool

foreign import prim "charGe" primCharGe :: Char -> Char -> Bool

foreign import prim "ord" primOrd :: Char -> Int

foreign import prim "chr" primChr :: Int -> Char

-- | The decimal digits of an integer, with a minus sign when negative.
foreign import prim "intShow" primIntShow :: Int -> [Char]

foreign import prim "putChar" primPutChar :: Char -> IO ()

-- | One character of the standard input as a code point, or a negative
-- number at the end of the input.
foreign import prim "getChar" primGetChar :: IO Int

foreign import prim "flush" primFlush :: IO ()
