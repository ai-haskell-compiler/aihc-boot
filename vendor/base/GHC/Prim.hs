{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GHCForeignImportPrim #-}

-- | The primitive operations. The evaluator (src/eval.c) implements
-- every @foreign import prim@; the string names the primitive.
module GHC.Prim
  ( seq,
    primRaise,
    primUnsafeCoerce,
    primCatch,
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
    primIntAnd,
    primIntOr,
    primIntXor,
    primIntShl,
    primIntShrl,
    primIntShra,
    primIntPopCount,
    primIntClz,
    primIntCtz,
    primWordLt,
    primWordLe,
    primWordGt,
    primWordGe,
    primWordQuot,
    primWordRem,
    primWordShow,
    primMalloc,
    primPeek8,
    primPeek16,
    primPeek32,
    primPeek64,
    primPoke8,
    primPoke16,
    primPoke32,
    primPoke64,
    primMemcpy,
    primMemmove,
    primMemset,
    primMemcmp,
    primFileSize,
    primReadFileInto,
    primWriteFileFrom,
  )
where

import GHC.Types

infixr 0 `seq`

-- | Evaluate the first argument to weak head normal form, then return
-- the second argument.
seq :: a -> b -> b
seq !_ b = b

-- | Raise an exception. The evaluator unwinds the stack to the
-- innermost 'primCatch'. GHC.Exception.Type wraps every exception in
-- a @SomeException@ first.
foreign import prim "raise" primRaise :: e -> a

-- | Run an action on the world. When it raises an exception, apply the
-- handler to the exception and the world.
foreign import prim "catch" primCatch :: (World -> IORes a) -> (e -> World -> IORes a) -> World -> IORes a

-- | The same value at another type.
foreign import prim "unsafeCoerce" primUnsafeCoerce :: a -> b

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

-- | Bit operations on Int. The shifts give 0 (or -1) for counts of the
-- word size or more.

foreign import prim "intAnd" primIntAnd :: Int -> Int -> Int

foreign import prim "intOr" primIntOr :: Int -> Int -> Int

foreign import prim "intXor" primIntXor :: Int -> Int -> Int

foreign import prim "intShl" primIntShl :: Int -> Int -> Int

foreign import prim "intShrl" primIntShrl :: Int -> Int -> Int

foreign import prim "intShra" primIntShra :: Int -> Int -> Int

foreign import prim "intPopCount" primIntPopCount :: Int -> Int

foreign import prim "intClz" primIntClz :: Int -> Int

foreign import prim "intCtz" primIntCtz :: Int -> Int

-- | The unsigned operations on the bits of an Int.

foreign import prim "wordLt" primWordLt :: Int -> Int -> Bool

foreign import prim "wordLe" primWordLe :: Int -> Int -> Bool

foreign import prim "wordGt" primWordGt :: Int -> Int -> Bool

foreign import prim "wordGe" primWordGe :: Int -> Int -> Bool

foreign import prim "wordQuot" primWordQuot :: Int -> Int -> Int

foreign import prim "wordRem" primWordRem :: Int -> Int -> Int

foreign import prim "wordShow" primWordShow :: Int -> [Char]

-- | Memory. An address is an Int; the memory is never freed. The
-- reads and writes take an address and an offset in bytes.

foreign import prim "malloc" primMalloc :: Int -> IO Int

foreign import prim "peek8" primPeek8 :: Int -> Int -> IO Int

foreign import prim "peek16" primPeek16 :: Int -> Int -> IO Int

foreign import prim "peek32" primPeek32 :: Int -> Int -> IO Int

foreign import prim "peek64" primPeek64 :: Int -> Int -> IO Int

foreign import prim "poke8" primPoke8 :: Int -> Int -> Int -> IO ()

foreign import prim "poke16" primPoke16 :: Int -> Int -> Int -> IO ()

foreign import prim "poke32" primPoke32 :: Int -> Int -> Int -> IO ()

foreign import prim "poke64" primPoke64 :: Int -> Int -> Int -> IO ()

foreign import prim "memcpy" primMemcpy :: Int -> Int -> Int -> IO ()

foreign import prim "memmove" primMemmove :: Int -> Int -> Int -> IO ()

foreign import prim "memset" primMemset :: Int -> Int -> Int -> IO ()

foreign import prim "memcmp" primMemcmp :: Int -> Int -> Int -> IO Int

-- | Files. The path is a String; the data is in memory at an address.
foreign import prim "fileSize" primFileSize :: [Char] -> IO Int

foreign import prim "readFileInto" primReadFileInto :: [Char] -> Int -> Int -> IO Int

foreign import prim "writeFileFrom" primWriteFileFrom :: [Char] -> Int -> Int -> Int -> IO Int
