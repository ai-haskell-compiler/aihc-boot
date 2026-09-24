{-# OPTIONS_HADDOCK not-home #-}

-- |
-- Module      : Data.ByteString.Internal.Type
-- Copyright   : (c) Don Stewart 2006-2008
--               (c) Duncan Coutts 2006-2012
-- License     : BSD-style
-- Maintainer  : dons00@gmail.com, duncan@community.haskell.org
-- Stability   : unstable
-- Portability : non-portable
--
-- The 'ByteString' type, its instances, and whatever related
-- utilities the bytestring developers see fit to use internally.
--
module Data.ByteString.Internal.Type (
    ByteString(BS),
    StrictByteString,
    findIndexOrLength,
    packChars,
    unsafePackLenChars,
    unpackChars,
    unpackAppendCharsLazy,
    unpackAppendCharsStrict,
    unsafePackLenLiteral,
    empty,
    createFp,
    unsafeCreateFp,
    unsafeCreate,
    mallocByteString,
    mkDeferredByteString,
    nullForeignPtr,
    peekFp,
    pokeFp,
    peekFpByteOff,
    memcpyFp,
    deferForeignPtrAvailability,
    unsafeDupablePerformIO,
    SizeOverflowException,
    overflowError,
    checkedAdd,
    checkedMultiply,
    memchr,
    memcmp,
    cIsValidUtf8,
    cIsValidUtf8Safe,
    w2c,
    c2w,
    accursedUnutterablePerformIO,
    plusForeignPtr,
    unsafeWithForeignPtr,
  ) where

import Prelude hiding (concat, null)
import qualified Data.List as List
import Foreign.ForeignPtr       (ForeignPtr, withForeignPtr)
import Foreign.Ptr
import Foreign.Storable         (Storable(..))
import Foreign.C.Types
import Foreign.Marshal.Utils
import qualified Data.ByteString.Internal.Pure as Pure
import Data.Bits                (toIntegralSized, Bits)
import Data.Maybe               (fromMaybe)
import Control.Monad            ((<$!>))
import Data.Semigroup           (Semigroup (..))
import Data.List.NonEmpty       (NonEmpty ((:|)))
import Control.DeepSeq          (NFData(rnf))
import Data.String              (IsString(..))
import Control.Exception        (assert, throw, Exception)
import Data.Char                (ord)
import Data.Word
import GHC.Base (nullAddr#, realWorld#, unsafeChr)
import GHC.Exts (Addr#, runRW#, lazy)
import GHC.Exts                (timesInt2#)
import GHC.IO                   (IO(IO))
import GHC.ForeignPtr           (ForeignPtr(ForeignPtr)
                                , mallocPlainForeignPtrBytes)
import GHC.ForeignPtr           (plusForeignPtr)
import GHC.ForeignPtr           (ForeignPtrContents(FinalPtr))
import GHC.Int                  (Int (..))
import GHC.ForeignPtr           (unsafeWithForeignPtr)

peekFp :: Storable a => ForeignPtr a -> IO a
peekFp fp = unsafeWithForeignPtr fp peek

pokeFp :: Storable a => ForeignPtr a -> a -> IO ()
pokeFp fp val = unsafeWithForeignPtr fp $ \p -> poke p val

peekFpByteOff :: Storable a => ForeignPtr a -> Int -> IO a
peekFpByteOff fp off = unsafeWithForeignPtr fp $ \p ->
  peekByteOff p off

-- | Most operations on a 'ByteString' need to read from the buffer
-- given by its @ForeignPtr Word8@ field.  But since most operations
-- on @ByteString@ are (nominally) pure, their implementations cannot
-- see the IO state thread that was used to initialize the contents of
-- that buffer.  This means that under some circumstances, these
-- buffer-reads may be executed before the writes used to initialize
-- the buffer are executed, with unpredictable results.
--
-- 'deferForeignPtrAvailability' exists to help solve this problem.
-- At runtime, a call @'deferForeignPtrAvailability' x@ is equivalent
-- to @pure $! x@, but the former is more opaque to the simplifier, so
-- that reads from the pointer in its result cannot be executed until
-- the @'deferForeignPtrAvailability' x@ call is complete.
--
-- The opaque bits evaporate during CorePrep, so using
-- 'deferForeignPtrAvailability' incurs no direct overhead.
--
-- @since 0.11.5.0
deferForeignPtrAvailability :: ForeignPtr a -> IO (ForeignPtr a)
deferForeignPtrAvailability (ForeignPtr addr0# guts) = IO $ \s0 ->
  case lazy runRW# (\_ -> (# s0, addr0# #)) of
    (# s1, addr1# #) -> (# s1, ForeignPtr addr1# guts #)

-- | Variant of 'fromForeignPtr0' that calls 'deferForeignPtrAvailability'
--
-- @since 0.11.5.0
mkDeferredByteString :: ForeignPtr Word8 -> Int -> IO ByteString
mkDeferredByteString fp len = do
  deferredFp <- deferForeignPtrAvailability fp
  pure $! BS deferredFp len

unsafeDupablePerformIO :: IO a -> a
-- Why does this exist? In base-4.15.1.0 until at least base-4.18.0.0,
-- the version of unsafeDupablePerformIO in base prevents unboxing of
-- its results with an opaque call to GHC.Exts.lazy, for reasons described
-- in Note [unsafePerformIO and strictness] in GHC.IO.Unsafe. (See
-- https://hackage.haskell.org/package/base-4.18.0.0/docs/src/GHC.IO.Unsafe.html#line-30 .)
-- Even if we accept the (very questionable) premise that the sort of
-- function described in that note should work, we expect no such
-- calls to be made in the context of bytestring.  (And we really want
-- unboxing!)
unsafeDupablePerformIO (IO act) = case runRW# act of (# _, res #) -> res

-- | A space-efficient representation of a 'Word8' vector, supporting many
-- efficient operations.
--
-- A 'ByteString' contains 8-bit bytes, or by using the operations from
-- "Data.ByteString.Char8" it can be interpreted as containing 8-bit
-- characters.
--
data ByteString = BS {-# UNPACK #-} !(ForeignPtr Word8) -- payload
                     {-# UNPACK #-} !Int                -- length
                     -- ^ @since 0.11.0.0

-- | Type synonym for the strict flavour of 'ByteString'.
--
-- @since 0.11.2.0
type StrictByteString = ByteString

-- | 'findIndexOrLength' is a variant of findIndex, that returns the length
-- of the string if no element is found, rather than Nothing.
findIndexOrLength :: (Word8 -> Bool) -> ByteString -> Int
findIndexOrLength k (BS x l) =
    accursedUnutterablePerformIO $ g x
  where
    g ptr = go 0
      where
        go !n | n >= l    = return l
              | otherwise = do w <- peekFp $ ptr `plusForeignPtr` n
                               if k w
                                 then return n
                                 else go (n+1)
{-# INLINE findIndexOrLength #-}

packChars :: [Char] -> ByteString
packChars cs = unsafePackLenChars (List.length cs) cs
{-# INLINE [0] packChars #-}

unsafePackLenChars :: Int -> [Char] -> ByteString
unsafePackLenChars len cs0 =
    unsafeCreateFp len $ \p -> go p cs0
  where
    go !_ []     = return ()
    go !p (c:cs) = pokeFp p (c2w c) >> go (p `plusForeignPtr` 1) cs

-- | See 'unsafePackLiteral'. This function is similar,
-- but takes an additional length argument rather then computing
-- it with @strlen@.
-- Therefore embedding @\'\\0\'@ characters is possible.
--
-- @since 0.11.2.0
unsafePackLenLiteral :: Int -> Addr# -> ByteString
unsafePackLenLiteral len addr# =

  BS (ForeignPtr addr# FinalPtr) len
{-# INLINE unsafePackLenLiteral #-}

unpackChars :: ByteString -> [Char]
unpackChars bs = unpackAppendCharsLazy bs []

unpackAppendCharsLazy :: ByteString -> [Char] -> [Char]
unpackAppendCharsLazy (BS fp len) cs
  | len <= 100 = unpackAppendCharsStrict (BS fp len) cs
  | otherwise  = unpackAppendCharsStrict (BS fp 100) remainder
  where
    remainder  = unpackAppendCharsLazy (BS (plusForeignPtr fp 100) (len-100)) cs

unpackAppendCharsStrict :: ByteString -> [Char] -> [Char]
unpackAppendCharsStrict (BS fp len) xs =
    accursedUnutterablePerformIO $ unsafeWithForeignPtr fp $ \base ->
      loop (base `plusPtr` (-1)) (base `plusPtr` (-1+len)) xs
  where
    loop !sentinal !p acc
      | p == sentinal = return acc
      | otherwise     = do x <- peek p
                           loop sentinal (p `plusPtr` (-1)) (w2c x:acc)

-- | The 0 pointer. Used to indicate the empty Bytestring.
nullForeignPtr :: ForeignPtr Word8
nullForeignPtr = ForeignPtr nullAddr# FinalPtr

-- | A way of creating ByteStrings outside the IO monad. The @Int@
-- argument gives the final size of the ByteString.
unsafeCreateFp :: Int -> (ForeignPtr Word8 -> IO ()) -> ByteString
unsafeCreateFp l f = unsafeDupablePerformIO (createFp l f)
{-# INLINE unsafeCreateFp #-}

-- | Create ByteString of size @l@ and use action @f@ to fill its contents.
createFp :: Int -> (ForeignPtr Word8 -> IO ()) -> IO ByteString
createFp len action = assert (len >= 0) $ do
    fp <- mallocByteString len
    action fp
    mkDeferredByteString fp len
{-# INLINE createFp #-}

wrapAction :: (Ptr Word8 -> IO res) -> ForeignPtr Word8 -> IO res
wrapAction = flip withForeignPtr
  -- Cannot use unsafeWithForeignPtr, because action can diverge

-- | A way of creating ByteStrings outside the IO monad. The @Int@
-- argument gives the final size of the ByteString.
unsafeCreate :: Int -> (Ptr Word8 -> IO ()) -> ByteString
unsafeCreate l f = unsafeCreateFp l (wrapAction f)
{-# INLINE unsafeCreate #-}

-- | Wrapper of 'Foreign.ForeignPtr.mallocForeignPtrBytes' with faster implementation for GHC
--
mallocByteString :: Int -> IO (ForeignPtr a)
mallocByteString = mallocPlainForeignPtrBytes
{-# INLINE mallocByteString #-}

eq :: ByteString -> ByteString -> Bool
eq a@(BS fp len) b@(BS fp' len')
  | len /= len' = False    -- short cut on length
  | fp == fp'   = True     -- short cut for the same string
  | otherwise   = compareBytes a b == EQ
{-# INLINE eq #-}

compareBytes :: ByteString -> ByteString -> Ordering
compareBytes (BS _   0)    (BS _   0)    = EQ  -- short cut for empty strings
compareBytes (BS fp1 len1) (BS fp2 len2) =
    accursedUnutterablePerformIO $
      unsafeWithForeignPtr fp1 $ \p1 ->
      unsafeWithForeignPtr fp2 $ \p2 -> do
        i <- memcmp p1 p2 (min len1 len2)
        return $! case i `compare` 0 of
                    EQ  -> len1 `compare` len2
                    x   -> x

-- | /O(1)/ The empty 'ByteString'
empty :: ByteString
-- This enables bypassing #457 by not using (polymorphic) mempty in
-- any definitions used by the (Monoid ByteString) instance
empty = BS nullForeignPtr 0

append :: ByteString -> ByteString -> ByteString
append (BS _   0)    b                  = b
append a             (BS _   0)    = a
append (BS fp1 len1) (BS fp2 len2) =
    unsafeCreateFp (checkedAdd "append" len1 len2) $ \destptr1 -> do
      let destptr2 = destptr1 `plusForeignPtr` len1
      memcpyFp destptr1 fp1 len1
      memcpyFp destptr2 fp2 len2

concat :: [ByteString] -> ByteString
concat = \bss0 -> goLen0 bss0 bss0
    -- The idea here is we first do a pass over the input list to determine:
    --
    --  1. is a copy necessary? e.g. @concat []@, @concat [mempty, "hello"]@,
    --     and @concat ["hello", mempty, mempty]@ can all be handled without
    --     copying.
    --  2. if a copy is necessary, how large is the result going to be?
    --
    -- If a copy is necessary then we create a buffer of the appropriate size
    -- and do another pass over the input list, copying the chunks into the
    -- buffer. Also, since foreign calls aren't entirely free we skip over
    -- empty chunks while copying.
    --
    -- We pass the original [ByteString] (bss0) through as an argument through
    -- goLen0, goLen1, and goLen since we will need it again in goCopy. Passing
    -- it as an explicit argument avoids capturing it in these functions'
    -- closures which would result in unnecessary closure allocation.
  where
    -- It's still possible that the result is empty
    goLen0 _    []                     = empty
    goLen0 bss0 (BS _ 0     :bss)    = goLen0 bss0 bss
    goLen0 bss0 (bs           :bss)    = goLen1 bss0 bs bss

    -- It's still possible that the result is a single chunk
    goLen1 _    bs []                  = bs
    goLen1 bss0 bs (BS _ 0  :bss)    = goLen1 bss0 bs bss
    goLen1 bss0 bs (BS _ len:bss)    = goLen bss0 (checkedAdd "concat" len' len) bss
      where BS _ len' = bs

    -- General case, just find the total length we'll need
    goLen bss0 !total (BS _ len:bss) = goLen bss0 total' bss
      where total' = checkedAdd "concat" total len
    goLen bss0 total [] =
      unsafeCreateFp total $ \ptr -> goCopy bss0 ptr

    -- Copy the data
    goCopy []                  !_   = return ()
    goCopy (BS _  0  :bss) !ptr = goCopy bss ptr
    goCopy (BS fp len:bss) !ptr = do
      memcpyFp ptr fp len
      goCopy bss (ptr `plusForeignPtr` len)
{-# NOINLINE concat #-}

-- | Repeats the given ByteString n times.
-- Polymorphic wrapper to make sure any generated
-- specializations are reasonably small.
stimesPolymorphic :: Integral a => a -> ByteString -> ByteString
{-# INLINABLE stimesPolymorphic #-}
stimesPolymorphic nRaw !bs = case checkedIntegerToInt n of
  Just nInt
    | nInt >= 0  -> stimesNonNegativeInt nInt bs
    | otherwise  -> stimesNegativeErr
  Nothing
    | n < 0  -> stimesNegativeErr
    | BS _ 0 <- bs  -> empty
    | otherwise     -> stimesOverflowErr
  where  n = toInteger nRaw
  -- By exclusively using n instead of nRaw, the semantics are kept simple
  -- and the likelihood of potentially dangerous mistakes minimized.

stimesNegativeErr :: ByteString
-- See Note [Float error calls out of INLINABLE things]
stimesNegativeErr
  = errorWithoutStackTrace "stimes @ByteString: non-negative multiplier expected"

stimesOverflowErr :: ByteString
-- See Note [Float error calls out of INLINABLE things]
stimesOverflowErr = overflowError "stimes"

-- | Repeats the given ByteString n times.
stimesNonNegativeInt :: Int -> ByteString -> ByteString
stimesNonNegativeInt n (BS fp len)
  | n == 0 = empty
  | n == 1 = BS fp len
  | len == 0 = empty
  | len == 1 = unsafeCreateFp n $ \destfptr -> do
      byte <- peekFp fp
      unsafeWithForeignPtr destfptr $ \destptr ->
        fillBytes destptr byte n
  | otherwise = unsafeCreateFp size $ \destptr -> do
      memcpyFp destptr fp len
      fillFrom destptr len
  where
    size = checkedMultiply "stimes" n len
    halfSize = (size - 1) `div` 2 -- subtraction and division won't overflow

    fillFrom :: ForeignPtr Word8 -> Int -> IO ()
    fillFrom destptr copied
      | copied <= halfSize = do
        memcpyFp (destptr `plusForeignPtr` copied) destptr copied
        fillFrom destptr (copied * 2)
      | otherwise = memcpyFp (destptr `plusForeignPtr` copied) destptr (size - copied)

-- | Conversion between 'Word8' and 'Char'. Should compile to a no-op.
w2c :: Word8 -> Char
w2c = unsafeChr . fromIntegral
{-# INLINE w2c #-}

-- | Unsafe conversion between 'Char' and 'Word8'. This is a no-op and
-- silently truncates to 8 bits Chars > '\255'. It is provided as
-- convenience for ByteString construction.
c2w :: Char -> Word8
c2w = fromIntegral . ord
{-# INLINE c2w #-}

-- | The type of exception raised by 'overflowError'
-- and on failure by overflow-checked arithmetic operations.
newtype SizeOverflowException
  = SizeOverflowException String

-- | Raises a 'SizeOverflowException',
-- with a message using the given function name.
overflowError :: String -> a
overflowError fun = throw $ SizeOverflowException msg
  where msg = "Data.ByteString." ++ fun ++ ": size overflow"

-- | Add two non-negative numbers.
-- Calls 'overflowError' on overflow.
checkedAdd :: String -> Int -> Int -> Int
{-# INLINE checkedAdd #-}
checkedAdd fun x y
  -- checking "r < 0" here matches the condition in mallocPlainForeignPtrBytes,
  -- helping the compiler see the latter is redundant in some places
  | r < 0     = overflowError fun
  | otherwise = r
  where r = assert (min x y >= 0) $ x + y

-- | Multiplies two non-negative numbers.
-- Calls 'overflowError' on overflow.
checkedMultiply :: String -> Int -> Int -> Int
{-# INLINE checkedMultiply #-}
checkedMultiply fun !x@(I# x#) !y@(I# y#) = assert (min x y >= 0) $

  case timesInt2# x# y# of
    (# 0#, _, result #) -> I# result
    _ -> overflowError fun

-- | Attempts to convert an 'Integer' value to an 'Int', returning
-- 'Nothing' if doing so would result in an overflow.
checkedIntegerToInt :: Integer -> Maybe Int
{-# INLINE checkedIntegerToInt #-}
-- We could use Data.Bits.toIntegralSized, but this hand-rolled
-- version is currently a bit faster as of GHC 9.2.
-- It's even faster to just match on the Integer constructors, but
-- we'd still need a fallback implementation for integer-simple.
checkedIntegerToInt x
  | x == toInteger res = Just res
  | otherwise = Nothing
  where  res = fromInteger x :: Int

{-# INLINE accursedUnutterablePerformIO #-}
-- | This \"function\" has a superficial similarity to 'System.IO.Unsafe.unsafePerformIO' but
-- it is in fact a malevolent agent of chaos. It unpicks the seams of reality
-- (and the 'IO' monad) so that the normal rules no longer apply. It lulls you
-- into thinking it is reasonable, but when you are not looking it stabs you
-- in the back and aliases all of your mutable buffers. The carcass of many a
-- seasoned Haskell programmer lie strewn at its feet.
--
-- Witness the trail of destruction:
--
-- * <https://github.com/haskell/bytestring/commit/71c4b438c675aa360c79d79acc9a491e7bbc26e7>
--
-- * <https://github.com/haskell/bytestring/commit/210c656390ae617d9ee3b8bcff5c88dd17cef8da>
--
-- * <https://github.com/haskell/aeson/commit/720b857e2e0acf2edc4f5512f2b217a89449a89d>
--
-- * <https://ghc.haskell.org/trac/ghc/ticket/3486>
--
-- * <https://ghc.haskell.org/trac/ghc/ticket/3487>
--
-- * <https://ghc.haskell.org/trac/ghc/ticket/7270>
--
-- * <https://gitlab.haskell.org/ghc/ghc/-/issues/22204>
--
-- Do not talk about \"safe\"! You do not know what is safe!
--
-- Yield not to its blasphemous call! Flee traveller! Flee or you will be
-- corrupted and devoured!
--
accursedUnutterablePerformIO :: IO a -> a
accursedUnutterablePerformIO (IO m) = case m realWorld# of (# _, r #) -> r

memchr :: Ptr Word8 -> Word8 -> CSize -> IO (Ptr Word8)

memcmp :: Ptr Word8 -> Ptr Word8 -> Int -> IO CInt

memchr p w len = Pure.memchr p w (checkedCast len)

memcmp p q s = checkedCast <$!> Pure.memcmp p q s

memcpyFp :: ForeignPtr Word8 -> ForeignPtr Word8 -> Int -> IO ()
memcpyFp fp fq s = unsafeWithForeignPtr fp $ \p ->
                     unsafeWithForeignPtr fq $ \q -> copyBytes p q s

cIsValidUtf8 :: Ptr Word8 -> CSize -> IO CInt
cIsValidUtf8 ptr sz = bool_to_cint <$> Pure.isValidUtf8 ptr (checkedCast sz)

cIsValidUtf8Safe :: Ptr Word8 -> CSize -> IO CInt
cIsValidUtf8Safe = cIsValidUtf8

bool_to_cint :: Bool -> CInt
bool_to_cint True = 1
bool_to_cint False = 0

checkedCast :: (Bits a, Bits b, Integral a, Integral b) => a -> b
checkedCast x =
  fromMaybe (errorWithoutStackTrace "checkedCast: overflow")
            (toIntegralSized x)

instance Exception SizeOverflowException

instance Show SizeOverflowException where
  show (SizeOverflowException err) = err

instance Monoid ByteString where
    mempty  = empty
    mappend = (<>)
    mconcat = concat

instance Eq  ByteString where
    (==)    = eq

instance Semigroup ByteString where
    (<>)    = append
    sconcat (b:|bs) = concat (b:bs)
    {-# INLINE stimes #-}
    stimes  = stimesPolymorphic

instance Ord ByteString where
    compare = compareBytes

instance NFData ByteString where
    rnf BS{} = ()

instance Show ByteString where
    showsPrec p ps r = showsPrec p (unpackChars ps) r

-- | Beware: 'fromString' truncates multi-byte characters to octets.
-- e.g. "枯朶に烏のとまりけり秋の暮" becomes �6k�nh~�Q��n�
instance IsString ByteString where
    {-# INLINE fromString #-}
    fromString = packChars

