{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Data.Text.Internal.Builder
-- License     : BSD-style (see LICENSE)
-- Stability   : experimental
--
-- /Warning/: this is an internal module, and does not have a stable
-- API or name. Functions in this module may not check or enforce
-- preconditions expected by public modules. Use at your own risk!
--
-- Internals of "Data.Text.Encoding".
--
-- @since 2.0.2
module Data.Text.Internal.Encoding (
    decodeUtf8With1,
    decodeUtf8With2,
    Utf8State,
    startUtf8State,
    skipIncomplete,
  ) where

import Data.Bits (shiftL, shiftR)
import Data.ByteString (ByteString)
import Data.Word (Word32, Word8)
import Foreign.Storable (pokeElemOff)
import Data.Text.Encoding.Error (OnDecodeError)
import Data.Text.Internal (Text(..))
import Data.Text.Internal.Encoding.Utf8
  (DecoderState, utf8AcceptState, utf8RejectState, updateDecoderState)
import Data.Text.Internal.StrictBuilder (StrictTextBuilder)
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Short.Internal as SBS
import qualified Data.Text.Array as A
import qualified Data.Text.Internal.StrictBuilder as SB

-- Internal invariant:
-- the first component is the initial state if and only if
-- the second component is empty.
--
-- @
-- 'utf9CodePointState' s = 'utf8StartState'
-- <=>
-- 'partialUtf8CodePoint' s = 'PartialUtf8CodePoint' 0
-- @
data Utf8State = Utf8State
  { -- | State of the UTF-8 state machine.
    utf8CodePointState :: {-# UNPACK #-} !DecoderState
    -- | Bytes of the currently incomplete code point (if any).
  , partialUtf8CodePoint :: {-# UNPACK #-} !PartialUtf8CodePoint
  }
  deriving (Eq, Show)

-- | Initial 'Utf8State'.
--
-- @since 2.0.2
startUtf8State :: Utf8State
startUtf8State = Utf8State utf8AcceptState partUtf8Empty

-- | Prefix of a UTF-8 code point encoded in 4 bytes,
-- possibly empty.
--
-- - The most significant byte contains the number of bytes,
--   between 0 and 3.
-- - The remaining bytes hold the incomplete code point.
-- - Unused bytes must be 0.
--
-- All of operations available on it are the functions below.
-- The constructor should never be used outside of those.
--
-- @since 2.0.2
newtype PartialUtf8CodePoint = PartialUtf8CodePoint Word32
  deriving (Eq, Show)

-- | Empty prefix.
partUtf8Empty :: PartialUtf8CodePoint
partUtf8Empty = PartialUtf8CodePoint 0

-- | Length of the partial code point, stored in the most significant byte.
partUtf8Len :: PartialUtf8CodePoint -> Int
partUtf8Len (PartialUtf8CodePoint w) = fromIntegral $ w `shiftR` 24

-- | Get the @n@-th byte, assuming it is within bounds: @0 <= n < partUtf8Len c@.
--
-- Unsafe: no bounds checking.
partUtf8UnsafeIndex ::

  PartialUtf8CodePoint -> Int -> Word8
partUtf8UnsafeIndex _c@(PartialUtf8CodePoint w) n =

  fromIntegral $ w `shiftR` (16 - 8 * n)

-- | Append some bytes.
--
-- Unsafe: no bounds checking.
partUtf8UnsafeAppend ::

  PartialUtf8CodePoint -> ByteString -> PartialUtf8CodePoint
partUtf8UnsafeAppend c@(PartialUtf8CodePoint word) bs =

  PartialUtf8CodePoint $
    tryPush 0 $ tryPush 1 $ tryPush 2 $ word + (fromIntegral lenbs `shiftL` 24)
  where
    lenc = partUtf8Len c
    lenbs = B.length bs
    tryPush i w =
      if i < lenbs
      then w + (fromIntegral (B.index bs i) `shiftL` fromIntegral (16 - 8 * (lenc + i)))
      else w

{-# INLINE partUtf8Foldr #-}
-- | Fold a 'PartialUtf8CodePoint'. This avoids recursion so it can unfold to straightline code.
partUtf8Foldr :: (Word8 -> a -> a) -> a -> PartialUtf8CodePoint -> a
partUtf8Foldr f x0 c = case partUtf8Len c of
    0 -> x0
    1 -> build 0 x0
    2 -> build 0 (build 1 x0)
    _ -> build 0 (build 1 (build 2 x0))
  where
    build i x = f (partUtf8UnsafeIndex c i) x

-- | Convert 'PartialUtf8CodePoint' to 'ByteString'.
partUtf8ToByteString :: PartialUtf8CodePoint -> B.ByteString
partUtf8ToByteString c = BI.unsafeCreate (partUtf8Len c) $ \ptr ->
  partUtf8Foldr (\w k i -> pokeElemOff ptr i w >> k (i+1)) (\_ -> pure ()) c 0

{-# INLINE validateUtf8ChunkFrom #-}
-- Assume bytes up to offset @ofs@ have been validated already.
--
-- Using CPS lets us inline the continuation and avoid allocating a @Maybe@
-- in the @decode...@ functions.
validateUtf8ChunkFrom :: forall r. Int -> ByteString -> (Int -> Maybe Utf8State -> r) -> r
validateUtf8ChunkFrom ofs bs k
  -- B.isValidUtf8 is buggy before bytestring-0.11.5.3 / bytestring-0.12.1.0.
  -- MIN_VERSION_bytestring does not allow us to differentiate
  -- between 0.11.5.2 and 0.11.5.3 so no choice except demanding 0.12.1+.

  | guessUtf8Boundary > 0 &&
    -- the rest of the bytestring is valid utf-8 up to the boundary
    (

      B.isValidUtf8 $ B.take guessUtf8Boundary (B.drop ofs bs)

    ) = slowValidateUtf8ChunkFrom (ofs + guessUtf8Boundary)
    -- No
  | otherwise = slowValidateUtf8ChunkFrom ofs
  where
    len = B.length bs - ofs
    isBoundary n p = len >= n && p (B.index bs (ofs + len - n))
    guessUtf8Boundary
      | isBoundary 1 (<= 0x80) = len      -- last char is ASCII (common short-circuit)
      | isBoundary 1 (0xc2 <=) = len - 1  -- last char starts a two-(or more-)byte code point
      | isBoundary 2 (0xe0 <=) = len - 2  -- pre-last char starts a three-or-four-byte code point
      | isBoundary 3 (0xf0 <=) = len - 3  -- third to last char starts a four-byte code point
      | otherwise = len

    -- A pure Haskell implementation of validateUtf8More.
    -- Ideally the primitives 'B.isValidUtf8' or 'c_is_valid_utf8' should give us
    -- indices to let us avoid this function.
    slowValidateUtf8ChunkFrom :: Int -> r
    slowValidateUtf8ChunkFrom ofs1 = slowLoop ofs1 ofs1 utf8AcceptState

    slowLoop !utf8End i s
      | i < B.length bs =
          case updateDecoderState (B.index bs i) s of
            s' | s' == utf8RejectState -> k utf8End Nothing
               | s' == utf8AcceptState -> slowLoop (i + 1) (i + 1) s'
               | otherwise -> slowLoop utf8End (i + 1) s'
      | otherwise = k utf8End (Just (Utf8State s (partUtf8UnsafeAppend partUtf8Empty (B.drop utf8End bs))))

{-# INLINE validateUtf8MoreCont #-}
-- CPS: inlining the continuation lets us make more tail calls and avoid
-- allocating a @Maybe@ in @decodeWith1/2@.
validateUtf8MoreCont :: Utf8State -> ByteString -> (Int -> Maybe Utf8State -> r) -> r
validateUtf8MoreCont st@(Utf8State s0 part) bs k
  | len > 0 = loop 0 s0
  | otherwise = k (- partUtf8Len part) (Just st)
  where
    len = B.length bs
    -- Complete an incomplete code point (if there is one)
    -- and then jump to validateUtf8ChunkFrom
    loop !i s
      | s == utf8AcceptState = validateUtf8ChunkFrom i bs k
      | i < len =
        case updateDecoderState (B.index bs i) s of
          s' | s' == utf8RejectState -> k (- partUtf8Len part) Nothing
             | otherwise -> loop (i + 1) s'
      | otherwise = k (- partUtf8Len part) (Just (Utf8State s (partUtf8UnsafeAppend part bs)))

-- Eta-expanded to inline partUtf8Foldr
partUtf8ToStrictBuilder :: PartialUtf8CodePoint -> StrictTextBuilder
partUtf8ToStrictBuilder c =
  partUtf8Foldr ((<>) . SB.unsafeFromWord8) mempty c

utf8StateToStrictBuilder :: Utf8State -> StrictTextBuilder
utf8StateToStrictBuilder = partUtf8ToStrictBuilder . partialUtf8CodePoint

{-# INLINE skipIncomplete #-}
-- | Call the error handler on each byte of the partial code point stored in
-- 'Utf8State' and append the results.
--
-- Exported for use in lazy 'Data.Text.Lazy.Encoding.decodeUtf8With'.
--
-- @since 2.0.2
skipIncomplete :: OnDecodeError -> String -> Utf8State -> StrictTextBuilder
skipIncomplete onErr msg s =
  partUtf8Foldr
    ((<>) . handleUtf8Error onErr msg)
    mempty (partialUtf8CodePoint s)

{-# INLINE handleUtf8Error #-}
handleUtf8Error :: OnDecodeError -> String -> Word8 -> StrictTextBuilder
handleUtf8Error onErr msg w = case onErr msg (Just w) of
  Just c -> SB.fromChar c
  Nothing -> mempty

-- This could be shorter by calling 'decodeUtf8With2' directly, but we make the
-- first call validateUtf8Chunk directly to return even faster in successful
-- cases.
decodeUtf8With1 ::

  OnDecodeError -> String -> ByteString -> Text
decodeUtf8With1 onErr msg bs = validateUtf8ChunkFrom 0 bs $ \len ms -> case ms of
    Just s
      | len == B.length bs ->
        let !(SBS.SBS arr) = SBS.toShort bs in
        Text (A.ByteArray arr) 0 len
      | otherwise -> SB.toText $
          SB.unsafeFromByteString (B.take len bs) <> skipIncomplete onErr msg s
    Nothing ->
       let (builder, _, s) = decodeUtf8With2 onErr msg startUtf8State (B.drop (len + 1) bs) in
       SB.toText $
         SB.unsafeFromByteString (B.take len bs) <>
         handleUtf8Error onErr msg (B.index bs len) <>
         builder <>
         skipIncomplete onErr msg s

-- | Helper for 'Data.Text.Encoding.decodeUtf8With',
-- 'Data.Text.Encoding.streamDecodeUtf8With', and lazy
-- 'Data.Text.Lazy.Encoding.decodeUtf8With',
-- which use an 'OnDecodeError' to process bad bytes.
--
-- See 'decodeUtf8Chunk' for a more flexible alternative.
--
-- @since 2.0.2
decodeUtf8With2 ::

  OnDecodeError -> String -> Utf8State -> ByteString -> (StrictTextBuilder, ByteString, Utf8State)
decodeUtf8With2 onErr msg s0 bs = loop s0 0 mempty
  where
    loop s i !builder =
      let nonEmptyPrefix len = builder
            <> utf8StateToStrictBuilder s
            <> SB.unsafeFromByteString (B.take len (B.drop i bs))
      in validateUtf8MoreCont s (B.drop i bs) $ \len ms -> case ms of
        Nothing ->
          if len < 0
          then
            -- If the first byte cannot complete the partial code point in s,
            -- retry from startUtf8State.
            let builder' = builder <> skipIncomplete onErr msg s
            -- Note: loop is strict on builder, so if onErr raises an error it will
            -- be forced here, short-circuiting the loop as desired.
            in loop startUtf8State i builder'
          else
            let builder' = nonEmptyPrefix len
                  <> handleUtf8Error onErr msg (B.index bs (i + len))
            in loop startUtf8State (i + len + 1) builder'
        Just s' ->
          let builder' = if len <= 0 then builder else nonEmptyPrefix len
              undecoded = if B.length bs >= partUtf8Len (partialUtf8CodePoint s')
                then B.drop (i + len) bs  -- Reuse bs if possible
                else partUtf8ToByteString (partialUtf8CodePoint s')
          in (builder', undecoded, s')

