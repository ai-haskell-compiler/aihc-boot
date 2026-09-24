{-# OPTIONS_HADDOCK prune #-}

-- |
-- Module      : Data.ByteString
-- Copyright   : (c) The University of Glasgow 2001,
--               (c) David Roundy 2003-2005,
--               (c) Simon Marlow 2005,
--               (c) Bjorn Bringert 2006,
--               (c) Don Stewart 2005-2008,
--               (c) Duncan Coutts 2006-2013
-- License     : BSD-style
--
-- Maintainer  : dons00@gmail.com, duncan@community.haskell.org
-- Stability   : stable
-- Portability : portable
--
-- A time- and space-efficient implementation of byte vectors using
-- packed Word8 arrays, suitable for high performance use, both in terms
-- of large data quantities and high speed requirements. Byte vectors
-- are encoded as strict 'Word8' arrays of bytes, held in a 'ForeignPtr',
-- and can be passed between C and Haskell with little effort.
--
-- The recomended way to assemble ByteStrings from smaller parts
-- is to use the builder monoid from "Data.ByteString.Builder".
--
-- This module is intended to be imported @qualified@, to avoid name
-- clashes with "Prelude" functions.  eg.
--
-- > import qualified Data.ByteString as B
--
-- Original GHC implementation by Bryan O\'Sullivan.
-- Rewritten to use 'Data.Array.Unboxed.UArray' by Simon Marlow.
-- Rewritten to support slices and use 'ForeignPtr' by David Roundy.
-- Rewritten again and extended by Don Stewart and Duncan Coutts.
--

module Data.ByteString (
        -- * Strict @ByteString@
        ByteString,
    empty,
    singleton,
    cons,
    snoc,
    head,
    uncons,
    unsnoc,
    last,
    null,
    length,
    intercalate,
    foldl',
    foldr,
    concat,
    concatMap,
    all,
    replicate,
    take,
    drop,
    takeWhile,
    takeWhileEnd,
    dropWhile,
    dropWhileEnd,
    span,
    break,
    split,
    splitWith,
    isPrefixOf,
    isSuffixOf,
    isValidUtf8,
    breakSubstring,
    index,
    elemIndex,
    elemIndexEnd,
    findIndexEnd,
    copy,
  ) where

import Prelude hiding           (reverse,head,tail,last,init,Foldable(..)
                                ,map,lines,unlines
                                ,concat,any,take,drop,splitAt,takeWhile
                                ,dropWhile,span,break,filter
                                ,all,concatMap
                                ,scanl,scanl1,scanr,scanr1
                                ,readFile,writeFile,appendFile,replicate
                                ,getContents,getLine,putStr,putStrLn,interact
                                ,zip,zipWith,unzip,notElem
                                )
import Data.Bits                (finiteBitSize, shiftL, (.|.), (.&.))
import Data.ByteString.Internal.Type
import Data.ByteString.Unsafe
import qualified Data.List as List
import Data.Word                (Word8)
import Control.Exception (assert)
import Foreign.ForeignPtr       (ForeignPtr, touchForeignPtr)
import Foreign.ForeignPtr.Unsafe(unsafeForeignPtrToPtr)
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable         (Storable(..))
import GHC.Stack.Types          (HasCallStack)
import GHC.Word hiding (Word8)

-- | /O(1)/ Convert a 'Word8' into a 'ByteString'
singleton :: Word8 -> ByteString
-- Taking a slice of some static data rather than allocating a new
-- buffer for each call is nice for several reasons. Since it doesn't
-- involve any side effects hidden in a 'GHC.Magic.runRW#' call, it
-- can be simplified to a constructor application. This may enable GHC
-- to perform further optimizations after inlining, and also causes a
-- fresh singleton to take only 4 words of heap space instead of 9.
-- (The buffer object itself would take up 3 words: header, size, and
-- 1 word of content. The ForeignPtrContents object used to keep the
-- buffer alive would need two more.)
singleton c = unsafeTake 1 $ unsafeDrop (fromIntegral c) allBytes
{-# INLINE singleton #-}

-- | A static blob of all possible bytes (0x00 to 0xff) in order
allBytes :: ByteString
allBytes = unsafePackLenLiteral 0x100
  "\x00\x01\x02\x03\x04\x05\x06\x07\x08\x09\x0a\x0b\x0c\x0d\x0e\x0f\x10\x11\x12\x13\x14\x15\x16\x17\x18\x19\x1a\x1b\x1c\x1d\x1e\x1f\x20\x21\x22\x23\x24\x25\x26\x27\x28\x29\x2a\x2b\x2c\x2d\x2e\x2f\x30\x31\x32\x33\x34\x35\x36\x37\x38\x39\x3a\x3b\x3c\x3d\x3e\x3f\x40\x41\x42\x43\x44\x45\x46\x47\x48\x49\x4a\x4b\x4c\x4d\x4e\x4f\x50\x51\x52\x53\x54\x55\x56\x57\x58\x59\x5a\x5b\x5c\x5d\x5e\x5f\x60\x61\x62\x63\x64\x65\x66\x67\x68\x69\x6a\x6b\x6c\x6d\x6e\x6f\x70\x71\x72\x73\x74\x75\x76\x77\x78\x79\x7a\x7b\x7c\x7d\x7e\x7f\x80\x81\x82\x83\x84\x85\x86\x87\x88\x89\x8a\x8b\x8c\x8d\x8e\x8f\x90\x91\x92\x93\x94\x95\x96\x97\x98\x99\x9a\x9b\x9c\x9d\x9e\x9f\xa0\xa1\xa2\xa3\xa4\xa5\xa6\xa7\xa8\xa9\xaa\xab\xac\xad\xae\xaf\xb0\xb1\xb2\xb3\xb4\xb5\xb6\xb7\xb8\xb9\xba\xbb\xbc\xbd\xbe\xbf\xc0\xc1\xc2\xc3\xc4\xc5\xc6\xc7\xc8\xc9\xca\xcb\xcc\xcd\xce\xcf\xd0\xd1\xd2\xd3\xd4\xd5\xd6\xd7\xd8\xd9\xda\xdb\xdc\xdd\xde\xdf\xe0\xe1\xe2\xe3\xe4\xe5\xe6\xe7\xe8\xe9\xea\xeb\xec\xed\xee\xef\xf0\xf1\xf2\xf3\xf4\xf5\xf6\xf7\xf8\xf9\xfa\xfb\xfc\xfd\xfe\xff"#

-- | /O(1)/ Test whether a ByteString is empty.
null :: ByteString -> Bool
null (BS _ l) = assert (l >= 0) $ l <= 0
{-# INLINE null #-}

-- ---------------------------------------------------------------------
-- | /O(1)/ 'length' returns the length of a ByteString as an 'Int'.
length :: ByteString -> Int
length (BS _ l) = assert (l >= 0) l
{-# INLINE length #-}

-- | /O(n)/ 'cons' is analogous to (:) for lists, but of different
-- complexity, as it requires making a copy.
cons :: Word8 -> ByteString -> ByteString
cons c (BS x len) = unsafeCreateFp (checkedAdd "cons" len 1) $ \p -> do
        pokeFp p c
        memcpyFp (p `plusForeignPtr` 1) x len
{-# INLINE cons #-}

-- | /O(n)/ Append a byte to the end of a 'ByteString'
snoc :: ByteString -> Word8 -> ByteString
snoc (BS x len) c = unsafeCreateFp (checkedAdd "snoc" len 1) $ \p -> do
        memcpyFp p x len
        pokeFp (p `plusForeignPtr` len) c
{-# INLINE snoc #-}

-- | /O(1)/ Extract the first element of a ByteString, which must be non-empty.
-- An exception will be thrown in the case of an empty ByteString.
--
-- This is a partial function, consider using 'uncons' instead.
head :: HasCallStack => ByteString -> Word8
head (BS x l)
    | l <= 0    = errorEmptyList "head"
    | otherwise = accursedUnutterablePerformIO $ unsafeWithForeignPtr x $ \p -> peek p
{-# INLINE head #-}

-- | /O(1)/ Extract the 'head' and 'tail' of a ByteString, returning 'Nothing'
-- if it is empty.
uncons :: ByteString -> Maybe (Word8, ByteString)
uncons (BS x l)
    | l <= 0    = Nothing
    | otherwise = Just (accursedUnutterablePerformIO $ unsafeWithForeignPtr x
                                                     $ \p -> peek p,
                        BS (plusForeignPtr x 1) (l-1))
{-# INLINE uncons #-}

-- | /O(1)/ Extract the last element of a ByteString, which must be finite and non-empty.
-- An exception will be thrown in the case of an empty ByteString.
--
-- This is a partial function, consider using 'unsnoc' instead.
last :: HasCallStack => ByteString -> Word8
last ps@(BS x l)
    | null ps   = errorEmptyList "last"
    | otherwise = accursedUnutterablePerformIO $
                    unsafeWithForeignPtr x $ \p -> peekByteOff p (l-1)
{-# INLINE last #-}

-- | /O(1)/ Extract the 'init' and 'last' of a ByteString, returning 'Nothing'
-- if it is empty.
unsnoc :: ByteString -> Maybe (ByteString, Word8)
unsnoc (BS x l)
    | l <= 0    = Nothing
    | otherwise = Just (BS x (l-1),
                        accursedUnutterablePerformIO $
                          unsafeWithForeignPtr x $ \p -> peekByteOff p (l-1))
{-# INLINE unsnoc #-}

{-
Note [fold inlining]:

GHC will only inline a function marked INLINE
if it is fully saturated (meaning the number of
arguments provided at the call site is at least
equal to the number of lhs arguments).

-}
-- | 'foldl'' is like 'foldl', but strict in the accumulator.
--
foldl' :: (a -> Word8 -> a) -> a -> ByteString -> a
foldl' f v = \(BS fp len) ->
          -- see fold inlining
  let
    g ptr = go v ptr
      where
        end  = ptr `plusForeignPtr` len
        -- tail recursive; traverses array left to right
        go !z !p | p == end  = return z
                 | otherwise = do x <- peekFp p
                                  go (f z x) (p `plusForeignPtr` 1)
  in
    accursedUnutterablePerformIO $ g fp
{-# INLINE foldl' #-}

-- | 'foldr', applied to a binary operator, a starting value
-- (typically the right-identity of the operator), and a ByteString,
-- reduces the ByteString using the binary operator, from right to left.
foldr :: (Word8 -> a -> a) -> a -> ByteString -> a
foldr k z = \(BS fp len) ->
          -- see fold inlining
  let
    ptr = unsafeForeignPtrToPtr fp
    end = ptr `plusPtr` len
    -- not tail recursive; traverses array left to right
    go !p | p == end  = z
          | otherwise = let !x = accursedUnutterablePerformIO $ do
                                   x' <- peek p
                                   touchForeignPtr fp
                                   return x'
                         in k x (go (p `plusPtr` 1))
  in
    go ptr
{-# INLINE foldr #-}

-- | /O(n)/ Concatenate a list of ByteStrings.
concat :: [ByteString] -> ByteString
concat = mconcat

-- | Map a function over a 'ByteString' and concatenate the results
concatMap :: (Word8 -> ByteString) -> ByteString -> ByteString
concatMap f = concat . foldr ((:) . f) []

-- | /O(n)/ Applied to a predicate and a 'ByteString', 'all' determines
-- if all elements of the 'ByteString' satisfy the predicate.
all :: (Word8 -> Bool) -> ByteString -> Bool
all _ (BS _ 0)   = True
all f (BS x len) = accursedUnutterablePerformIO $ g x
  where
    g ptr = go ptr
      where
        end = ptr `plusForeignPtr` len
        go !p | p == end  = return True  -- end of list
              | otherwise = do c <- peekFp p
                               if f c
                                  then go (p `plusForeignPtr` 1)
                                  else return False
{-# INLINE [1] all #-}

-- | /O(n)/ 'replicate' @n x@ is a ByteString of length @n@ with @x@
-- the value of every element. The following holds:
--
-- > replicate w c = fst (unfoldrN w (\u -> Just (u,u)) c)
replicate :: Int -> Word8 -> ByteString
replicate w c
    | w <= 0    = empty
    | otherwise = unsafeCreateFp w $ \fptr ->
        unsafeWithForeignPtr fptr $ \ptr ->
                      fillBytes ptr c w
{-# INLINE replicate #-}

-- | /O(1)/ 'take' @n@, applied to a ByteString @xs@, returns the prefix
-- of @xs@ of length @n@, or @xs@ itself if @n > 'length' xs@.
take :: Int -> ByteString -> ByteString
take n ps@(BS x l)
    | n <= 0    = empty
    | n >= l    = ps
    | otherwise = BS x n
{-# INLINE take #-}

-- | /O(1)/ 'drop' @n xs@ returns the suffix of @xs@ after the first @n@
-- elements, or 'empty' if @n > 'length' xs@.
drop  :: Int -> ByteString -> ByteString
drop n ps@(BS x l)
    | n <= 0    = ps
    | n >= l    = empty
    | otherwise = BS (plusForeignPtr x n) (l-n)
{-# INLINE drop #-}

-- | Similar to 'Prelude.takeWhile',
-- returns the longest (possibly empty) prefix of elements
-- satisfying the predicate.
takeWhile :: (Word8 -> Bool) -> ByteString -> ByteString
takeWhile f ps = unsafeTake (findIndexOrLength (not . f) ps) ps
{-# INLINE [1] takeWhile #-}

-- | Returns the longest (possibly empty) suffix of elements
-- satisfying the predicate.
--
-- @'takeWhileEnd' p@ is equivalent to @'reverse' . 'takeWhile' p . 'reverse'@.
--
-- @since 0.10.12.0
takeWhileEnd :: (Word8 -> Bool) -> ByteString -> ByteString
takeWhileEnd f ps = unsafeDrop (findFromEndUntil (not . f) ps) ps
{-# INLINE takeWhileEnd #-}

-- | Similar to 'Prelude.dropWhile',
-- drops the longest (possibly empty) prefix of elements
-- satisfying the predicate and returns the remainder.
dropWhile :: (Word8 -> Bool) -> ByteString -> ByteString
dropWhile f ps = unsafeDrop (findIndexOrLength (not . f) ps) ps
{-# INLINE [1] dropWhile #-}

-- | Similar to 'Prelude.dropWhileEnd',
-- drops the longest (possibly empty) suffix of elements
-- satisfying the predicate and returns the remainder.
--
-- @'dropWhileEnd' p@ is equivalent to @'reverse' . 'dropWhile' p . 'reverse'@.
--
-- @since 0.10.12.0
dropWhileEnd :: (Word8 -> Bool) -> ByteString -> ByteString
dropWhileEnd f ps = unsafeTake (findFromEndUntil (not . f) ps) ps
{-# INLINE dropWhileEnd #-}

-- | Similar to 'Prelude.break',
-- returns the longest (possibly empty) prefix of elements which __do not__
-- satisfy the predicate and the remainder of the string.
--
-- 'break' @p@ is equivalent to @'span' (not . p)@ and to @('takeWhile' (not . p) &&& 'dropWhile' (not . p))@.
--
-- Under GHC, a rewrite rule will transform break (==) into a
-- call to the specialised breakByte:
--
-- > break ((==) x) = breakByte x
-- > break (==x) = breakByte x
--
break :: (Word8 -> Bool) -> ByteString -> (ByteString, ByteString)
break p ps = case findIndexOrLength p ps of n -> (unsafeTake n ps, unsafeDrop n ps)
{-# INLINE [1] break #-}

-- | 'breakByte' breaks its ByteString argument at the first occurrence
-- of the specified byte. It is more efficient than 'break' as it is
-- implemented with @memchr(3)@. I.e.
--
-- > break (==99) "abcd" == breakByte 99 "abcd" -- fromEnum 'c' == 99
--
breakByte :: Word8 -> ByteString -> (ByteString, ByteString)
breakByte c p = case elemIndex c p of
    Nothing -> (p,empty)
    Just n  -> (unsafeTake n p, unsafeDrop n p)
{-# INLINE breakByte #-}

-- | Similar to 'Prelude.span',
-- returns the longest (possibly empty) prefix of elements
-- satisfying the predicate and the remainder of the string.
--
-- 'span' @p@ is equivalent to @'break' (not . p)@ and to @('takeWhile' p &&& 'dropWhile' p)@.
--
span :: (Word8 -> Bool) -> ByteString -> (ByteString, ByteString)
span p = break (not . p)
{-# INLINE [1] span #-}

-- | /O(n)/ Splits a 'ByteString' into components delimited by
-- separators, where the predicate returns True for a separator element.
-- The resulting components do not contain the separators.  Two adjacent
-- separators result in an empty component in the output.  eg.
--
-- > splitWith (==97) "aabbaca" == ["","","bb","c",""] -- fromEnum 'a' == 97
-- > splitWith undefined ""     == []                  -- and not [""]
--
splitWith :: (Word8 -> Bool) -> ByteString -> [ByteString]
splitWith _ (BS _  0) = []
splitWith predicate (BS fp len) = splitWith0 0 len fp
  where splitWith0 !off' !len' !fp' =
          accursedUnutterablePerformIO $
              splitLoop fp 0 off' len' fp'

        splitLoop :: ForeignPtr Word8
                  -> Int -> Int -> Int
                  -> ForeignPtr Word8
                  -> IO [ByteString]
        splitLoop p idx2 off' len' fp' = go idx2
          where
            go idx'
                | idx' >= len'  = return [BS (plusForeignPtr fp' off') idx']
                | otherwise = do
                    w <- peekFpByteOff p (off'+idx')
                    if predicate w
                       then return (BS (plusForeignPtr fp' off') idx' :
                                  splitWith0 (off'+idx'+1) (len'-idx'-1) fp')
                       else go (idx'+1)
{-# INLINE splitWith #-}

-- | /O(n)/ Break a 'ByteString' into pieces separated by the byte
-- argument, consuming the delimiter. I.e.
--
-- > split 10  "a\nb\nd\ne" == ["a","b","d","e"]   -- fromEnum '\n' == 10
-- > split 97  "aXaXaXa"    == ["","X","X","X",""] -- fromEnum 'a' == 97
-- > split 120 "x"          == ["",""]             -- fromEnum 'x' == 120
-- > split undefined ""     == []                  -- and not [""]
--
-- and
--
-- > intercalate [c] . split c == id
-- > split == splitWith . (==)
--
-- As for all splitting functions in this library, this function does
-- not copy the substrings, it just constructs new 'ByteString's that
-- are slices of the original.
--
split :: Word8 -> ByteString -> [ByteString]
split _ (BS _ 0) = []
split w (BS x l) = loop 0
    where
        loop !n =
            let q = accursedUnutterablePerformIO $ unsafeWithForeignPtr x $ \p ->
                      memchr (p `plusPtr` n)
                             w (fromIntegral (l-n))
            in if q == nullPtr
                then [BS (plusForeignPtr x n) (l-n)]
                else let i = q `minusPtr` unsafeForeignPtrToPtr x
                      in BS (plusForeignPtr x n) (i-n) : loop (i+1)
{-# INLINE split #-}

-- | /O(n)/ The 'intercalate' function takes a 'ByteString' and a list of
-- 'ByteString's and concatenates the list after interspersing the first
-- argument between each element of the list.
intercalate :: ByteString -> [ByteString] -> ByteString
intercalate _ [] = mempty
intercalate _ [x] = x -- This branch exists for laziness, not speed
intercalate (BS sepPtr sepLen) (BS hPtr hLen : t) =
  unsafeCreateFp totalLen $ \dstPtr0 -> do
      memcpyFp dstPtr0 hPtr hLen
      let go _ [] = pure ()
          go dstPtr (BS chunkPtr chunkLen : chunks) = do
            memcpyFp dstPtr sepPtr sepLen
            let destPtr' = dstPtr `plusForeignPtr` sepLen
            memcpyFp destPtr' chunkPtr chunkLen
            go (destPtr' `plusForeignPtr` chunkLen) chunks
      go (dstPtr0 `plusForeignPtr` hLen) t
  where
  totalLen = List.foldl' (\acc chunk -> acc +! sepLen +! length chunk) hLen t
  (+!) = checkedAdd "intercalate"
{-# INLINABLE intercalate #-}

-- | /O(1)/ 'ByteString' index (subscript) operator, starting from 0.
--
-- This is a partial function, consider using 'indexMaybe' instead.
index :: HasCallStack => ByteString -> Int -> Word8
index ps n
    | n < 0          = moduleError "index" ("negative index: " ++ show n)
    | n >= length ps = moduleError "index" ("index too large: " ++ show n
                                         ++ ", length = " ++ show (length ps))
    | otherwise      = ps `unsafeIndex` n
{-# INLINE index #-}

-- | /O(n)/ The 'elemIndex' function returns the index of the first
-- element in the given 'ByteString' which is equal to the query
-- element, or 'Nothing' if there is no such element.
-- This implementation uses memchr(3).
elemIndex :: Word8 -> ByteString -> Maybe Int
elemIndex c (BS x l) = accursedUnutterablePerformIO $ unsafeWithForeignPtr x $ \p -> do
    q <- memchr p c (fromIntegral l)
    return $! if q == nullPtr then Nothing else Just $! q `minusPtr` p
{-# INLINE elemIndex #-}

-- | /O(n)/ The 'elemIndexEnd' function returns the last index of the
-- element in the given 'ByteString' which is equal to the query
-- element, or 'Nothing' if there is no such element. The following
-- holds:
--
-- > elemIndexEnd c xs = case elemIndex c (reverse xs) of
-- >   Nothing -> Nothing
-- >   Just i  -> Just (length xs - 1 - i)
--
elemIndexEnd :: Word8 -> ByteString -> Maybe Int
elemIndexEnd = findIndexEnd . (==)
{-# INLINE elemIndexEnd #-}

-- | /O(n)/ The 'findIndexEnd' function takes a predicate and a 'ByteString' and
-- returns the index of the last element in the ByteString
-- satisfying the predicate.
--
-- @since 0.10.12.0
findIndexEnd :: (Word8 -> Bool) -> ByteString -> Maybe Int
findIndexEnd k (BS x l) = accursedUnutterablePerformIO $ g x
  where
    g !ptr = go (l-1)
      where
        go !n | n < 0     = return Nothing
              | otherwise = do w <- peekFpByteOff ptr n
                               if k w
                                 then return (Just n)
                                 else go (n-1)
{-# INLINE findIndexEnd #-}

-- |/O(n)/ The 'isPrefixOf' function takes two ByteStrings and returns 'True'
-- if the first is a prefix of the second.
isPrefixOf :: ByteString -> ByteString -> Bool
isPrefixOf (BS x1 l1) (BS x2 l2)
    | l1 == 0   = True
    | l2 < l1   = False
    | otherwise = accursedUnutterablePerformIO $ unsafeWithForeignPtr x1 $ \p1 ->
        unsafeWithForeignPtr x2 $ \p2 -> do
            i <- memcmp p1 p2 (fromIntegral l1)
            return $! i == 0

-- | /O(n)/ The 'isSuffixOf' function takes two ByteStrings and returns 'True'
-- iff the first is a suffix of the second.
--
-- The following holds:
--
-- > isSuffixOf x y == reverse x `isPrefixOf` reverse y
--
-- However, the real implementation uses memcmp to compare the end of the
-- string only, with no reverse required..
isSuffixOf :: ByteString -> ByteString -> Bool
isSuffixOf (BS x1 l1) (BS x2 l2)
    | l1 == 0   = True
    | l2 < l1   = False
    | otherwise = accursedUnutterablePerformIO $ unsafeWithForeignPtr x1 $ \p1 ->
        unsafeWithForeignPtr x2 $ \p2 -> do
            i <- memcmp p1 (p2 `plusPtr` (l2 - l1)) (fromIntegral l1)
            return $! i == 0

-- | /O(n)/ Check whether a 'ByteString' represents valid UTF-8.
--
-- @since 0.11.2.0
isValidUtf8 :: ByteString -> Bool
isValidUtf8 (BS ptr len) = accursedUnutterablePerformIO $ unsafeWithForeignPtr ptr $ \p -> do
  -- Use a safe FFI call for large inputs to avoid GC synchronization pauses
  -- in multithreaded contexts.
  -- This specific limit was chosen based on results of a simple benchmark, see:
  -- https://github.com/haskell/bytestring/issues/451#issuecomment-991879338
  -- When changing this function, also consider changing the related function:
  -- Data.ByteString.Short.Internal.isValidUtf8
  i <- if len < 1000000
     then cIsValidUtf8 p (fromIntegral len)
     else cIsValidUtf8Safe p (fromIntegral len)
  pure $ i /= 0

-- | Break a string on a substring, returning a pair of the part of the
-- string prior to the match, and the rest of the string.
--
-- The following relationships hold:
--
-- > break (== c) l == breakSubstring (singleton c) l
--
-- For example, to tokenise a string, dropping delimiters:
--
-- > tokenise x y = h : if null t then [] else tokenise x (drop (length x) t)
-- >     where (h,t) = breakSubstring x y
--
-- To skip to the first occurrence of a string:
--
-- > snd (breakSubstring x y)
--
-- To take the parts of a string before a delimiter:
--
-- > fst (breakSubstring x y)
--
-- Note that calling `breakSubstring x` does some preprocessing work, so
-- you should avoid unnecessarily duplicating breakSubstring calls with the same
-- pattern.
--
breakSubstring :: ByteString -- ^ String to search for
               -> ByteString -- ^ String to search in
               -> (ByteString,ByteString) -- ^ Head and tail of string broken at substring
breakSubstring pat =
  case lp of
    0 -> (empty,)
    1 -> breakByte (unsafeHead pat)
    _ -> if lp * 8 <= finiteBitSize (0 :: Word)
             then shift
             else karpRabin
  where
    unsafeSplitAt i s = (unsafeTake i s, unsafeDrop i s)
    lp                = length pat
    karpRabin :: ByteString -> (ByteString, ByteString)
    karpRabin src
        | length src < lp = (src,empty)
        | otherwise = search (rollingHash $ unsafeTake lp src) lp
      where
        k           = 2891336453 :: Word32
        rollingHash = foldl' (\h b -> h * k + fromIntegral b) 0
        hp          = rollingHash pat
        m           = k ^ lp
        get = fromIntegral . unsafeIndex src
        search !hs !i
            | hp == hs && pat == unsafeTake lp b = u
            | length src <= i                    = (src,empty) -- not found
            | otherwise                          = search hs' (i + 1)
          where
            u@(_, b) = unsafeSplitAt (i - lp) src
            hs' = hs * k +
                  get i -
                  m * get (i - lp)
    {-# INLINE karpRabin #-}

    shift :: ByteString -> (ByteString, ByteString)
    shift !src
        | length src < lp = (src,empty)
        | otherwise       = search (intoWord $ unsafeTake lp src) lp
      where
        intoWord :: ByteString -> Word
        intoWord = foldl' (\w b -> (w `shiftL` 8) .|. fromIntegral b) 0
        wp   = intoWord pat
        mask = (1 `shiftL` (8 * lp)) - 1
        search !w !i
            | w == wp         = unsafeSplitAt (i - lp) src
            | length src <= i = (src, empty)
            | otherwise       = search w' (i + 1)
          where
            b  = fromIntegral (unsafeIndex src i)
            w' = mask .&. ((w `shiftL` 8) .|. b)
    {-# INLINE shift #-}

-- | /O(n)/ Make a copy of the 'ByteString' with its own storage.
-- This is mainly useful to allow the rest of the data pointed
-- to by the 'ByteString' to be garbage collected, for example
-- if a large string has been read in, and only a small part of it
-- is needed in the rest of the program.
--
copy :: ByteString -> ByteString
copy (BS x l) = unsafeCreateFp l $ \p -> memcpyFp p x l

-- Common up near identical calls to `error' to reduce the number
-- constant strings created when compiled:
errorEmptyList :: HasCallStack => String -> a
errorEmptyList fun = moduleError fun "empty ByteString"
{-# NOINLINE errorEmptyList #-}

moduleError :: HasCallStack => String -> String -> a
moduleError fun msg = error (moduleErrorMsg fun msg)
{-# NOINLINE moduleError #-}

moduleErrorMsg :: String -> String -> String
moduleErrorMsg fun msg = "Data.ByteString." ++ fun ++ ':':' ':msg

-- Find from the end of the string using predicate
findFromEndUntil :: (Word8 -> Bool) -> ByteString -> Int
findFromEndUntil f ps@(BS _ l) = case unsnoc ps of
  Nothing     -> 0
  Just (b, c) ->
    if f c
      then l
      else findFromEndUntil f b

