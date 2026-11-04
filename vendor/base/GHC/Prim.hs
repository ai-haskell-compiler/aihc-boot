{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GHCForeignImportPrim #-}

-- | The primitive operations. The evaluator (src/eval.c) implements
-- every @foreign import prim@; the string names the primitive.
module GHC.Prim
  ( seq,
    -- * The primitive types, as in GHC
    Int#,
    Word#,
    Char#,
    Double#,
    Float#,
    Addr#,
    State#,
    RealWorld,
    Array#,
    MutableArray#,
    SmallArray#,
    SmallMutableArray#,
    ByteArray#,
    MutableByteArray#,
    MutVar#,
    MVar#,
    TVar#,
    ThreadId#,
    StablePtr#,
    StableName#,
    Weak#,
    -- * The primops of GHC
    (+#),
    (-#),
    (*#),
    quotInt#,
    remInt#,
    negateInt#,
    (==#),
    (/=#),
    (<#),
    (<=#),
    (>#),
    (>=#),
    andI#,
    orI#,
    xorI#,
    notI#,
    uncheckedIShiftL#,
    uncheckedIShiftRA#,
    uncheckedIShiftRL#,
    plusWord#,
    minusWord#,
    timesWord#,
    quotWord#,
    remWord#,
    and#,
    or#,
    xor#,
    not#,
    uncheckedShiftL#,
    uncheckedShiftRL#,
    int2Word#,
    word2Int#,
    eqWord#,
    neWord#,
    ltWord#,
    leWord#,
    gtWord#,
    geWord#,
    popCnt#,
    clz#,
    ctz#,
    chr#,
    ord#,
    eqChar#,
    neChar#,
    ltChar#,
    leChar#,
    gtChar#,
    geChar#,
    unsafeCoerce#,
    reallyUnsafePtrEquality#,
    newMutVar#,
    readMutVar#,
    writeMutVar#,
    newMVar#,
    takeMVar#,
    readMVar#,
    putMVar#,
    isEmptyMVar#,
    touch#,
    noDuplicate#,
    seq#,
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
    primGeneralCategory,
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
foreign import prim "catch" primCatch :: (State# RealWorld -> (# State# RealWorld, a #)) -> (e -> State# RealWorld -> (# State# RealWorld, a #)) -> State# RealWorld -> (# State# RealWorld, a #)

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

-- | The index of the general category of a character in
-- Data.Char.GeneralCategory.
foreign import prim "generalCategory" primGeneralCategory :: Char -> Int

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

-- The primops of GHC. The boxed and the primitive types have the same
-- representation at run time, so the primitives of the boot base serve
-- for both. A comparison gives 1# or 0#.

foreign import prim "intAdd" (+#) :: Int# -> Int# -> Int#

foreign import prim "intSub" (-#) :: Int# -> Int# -> Int#

foreign import prim "intMul" (*#) :: Int# -> Int# -> Int#

foreign import prim "intQuot" quotInt# :: Int# -> Int# -> Int#

foreign import prim "intRem" remInt# :: Int# -> Int# -> Int#

foreign import prim "intNegate" negateInt# :: Int# -> Int#

foreign import prim "intEq" primIntEq# :: Int# -> Int# -> Bool

foreign import prim "intNe" primIntNe# :: Int# -> Int# -> Bool

foreign import prim "intLt" primIntLt# :: Int# -> Int# -> Bool

foreign import prim "intLe" primIntLe# :: Int# -> Int# -> Bool

foreign import prim "intGt" primIntGt# :: Int# -> Int# -> Bool

foreign import prim "intGe" primIntGe# :: Int# -> Int# -> Bool

boolInt# :: Bool -> Int#
boolInt# True = 1#
boolInt# False = 0#

(==#), (/=#), (<#), (<=#), (>#), (>=#) :: Int# -> Int# -> Int#
a ==# b = boolInt# (primIntEq# a b)
a /=# b = boolInt# (primIntNe# a b)
a <# b = boolInt# (primIntLt# a b)
a <=# b = boolInt# (primIntLe# a b)
a ># b = boolInt# (primIntGt# a b)
a >=# b = boolInt# (primIntGe# a b)

infix 4 ==#, /=#, <#, <=#, >#, >=#

infixl 6 +#, -#

infixl 7 *#

foreign import prim "intAnd" andI# :: Int# -> Int# -> Int#

foreign import prim "intOr" orI# :: Int# -> Int# -> Int#

foreign import prim "intXor" xorI# :: Int# -> Int# -> Int#

notI# :: Int# -> Int#
notI# a = xorI# a (negateInt# 1#)

foreign import prim "intShl" uncheckedIShiftL# :: Int# -> Int# -> Int#

foreign import prim "intShra" uncheckedIShiftRA# :: Int# -> Int# -> Int#

foreign import prim "intShrl" uncheckedIShiftRL# :: Int# -> Int# -> Int#

foreign import prim "intAdd" plusWord# :: Word# -> Word# -> Word#

foreign import prim "intSub" minusWord# :: Word# -> Word# -> Word#

foreign import prim "intMul" timesWord# :: Word# -> Word# -> Word#

foreign import prim "wordQuot" quotWord# :: Word# -> Word# -> Word#

foreign import prim "wordRem" remWord# :: Word# -> Word# -> Word#

foreign import prim "intAnd" and# :: Word# -> Word# -> Word#

foreign import prim "intOr" or# :: Word# -> Word# -> Word#

foreign import prim "intXor" xor# :: Word# -> Word# -> Word#

not# :: Word# -> Word#
not# a = xor# a (int2Word# (negateInt# 1#))

foreign import prim "intShl" uncheckedShiftL# :: Word# -> Int# -> Word#

foreign import prim "intShrl" uncheckedShiftRL# :: Word# -> Int# -> Word#

foreign import prim "unsafeCoerce" int2Word# :: Int# -> Word#

foreign import prim "unsafeCoerce" word2Int# :: Word# -> Int#

foreign import prim "intEq" primWordEq# :: Word# -> Word# -> Bool

foreign import prim "intNe" primWordNe# :: Word# -> Word# -> Bool

foreign import prim "wordLt" primWordLt# :: Word# -> Word# -> Bool

foreign import prim "wordLe" primWordLe# :: Word# -> Word# -> Bool

foreign import prim "wordGt" primWordGt# :: Word# -> Word# -> Bool

foreign import prim "wordGe" primWordGe# :: Word# -> Word# -> Bool

eqWord#, neWord#, ltWord#, leWord#, gtWord#, geWord# :: Word# -> Word# -> Int#
eqWord# a b = boolInt# (primWordEq# a b)
neWord# a b = boolInt# (primWordNe# a b)
ltWord# a b = boolInt# (primWordLt# a b)
leWord# a b = boolInt# (primWordLe# a b)
gtWord# a b = boolInt# (primWordGt# a b)
geWord# a b = boolInt# (primWordGe# a b)

foreign import prim "intPopCount" popCnt# :: Word# -> Word#

foreign import prim "intClz" clz# :: Word# -> Word#

foreign import prim "intCtz" ctz# :: Word# -> Word#

foreign import prim "chr" chr# :: Int# -> Char#

foreign import prim "ord" ord# :: Char# -> Int#

foreign import prim "charEq" primCharEq# :: Char# -> Char# -> Bool

foreign import prim "charNe" primCharNe# :: Char# -> Char# -> Bool

foreign import prim "charLt" primCharLt# :: Char# -> Char# -> Bool

foreign import prim "charLe" primCharLe# :: Char# -> Char# -> Bool

foreign import prim "charGt" primCharGt# :: Char# -> Char# -> Bool

foreign import prim "charGe" primCharGe# :: Char# -> Char# -> Bool

eqChar#, neChar#, ltChar#, leChar#, gtChar#, geChar# :: Char# -> Char# -> Int#
eqChar# a b = boolInt# (primCharEq# a b)
neChar# a b = boolInt# (primCharNe# a b)
ltChar# a b = boolInt# (primCharLt# a b)
leChar# a b = boolInt# (primCharLe# a b)
gtChar# a b = boolInt# (primCharGt# a b)
geChar# a b = boolInt# (primCharGe# a b)

foreign import prim "unsafeCoerce" unsafeCoerce# :: a -> b

-- | Whether two values are the same object, without evaluating them.
foreign import prim "ptrEq" reallyUnsafePtrEquality# :: a -> a -> Int#

foreign import prim "mutVarNew" newMutVar# :: a -> State# s -> (# State# s, MutVar# s a #)

foreign import prim "mutVarRead" readMutVar# :: MutVar# s a -> State# s -> (# State# s, a #)

foreign import prim "mutVarWrite" writeMutVar# :: MutVar# s a -> a -> State# s -> State# s

foreign import prim "mvarNew" newMVar# :: State# s -> (# State# s, MVar# s a #)

-- | Take the value of a full MVar. The boot evaluator has one thread, so
-- an empty MVar is an error.
foreign import prim "mvarTake" takeMVar# :: MVar# s a -> State# s -> (# State# s, a #)

foreign import prim "mvarRead" readMVar# :: MVar# s a -> State# s -> (# State# s, a #)

foreign import prim "mvarPut" putMVar# :: MVar# s a -> a -> State# s -> State# s

foreign import prim "mvarIsEmpty" isEmptyMVar# :: MVar# s a -> State# s -> (# State# s, Int# #)

-- | Keep a value alive until this point. The collector of the boot
-- evaluator moves no memory that a Ptr points to, so this does nothing.
touch# :: a -> State# s -> State# s
touch# _ s = s

noDuplicate# :: State# s -> State# s
noDuplicate# s = s

-- | Evaluate the value in the state thread.
seq# :: a -> State# s -> (# State# s, a #)
seq# a s = a `seq` (# s, a #)
