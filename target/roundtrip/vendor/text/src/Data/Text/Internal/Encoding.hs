{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Text.Internal.Encoding (decodeUtf8With1, decodeUtf8With2, Utf8State, startUtf8State, skipIncomplete) where {
import Data.Bits (shiftL, shiftR);
import Data.ByteString (ByteString);
import Data.Word (Word32, Word8);
import Foreign.Storable (pokeElemOff);
import Data.Text.Encoding.Error (OnDecodeError);
import Data.Text.Internal (Text(..));
import Data.Text.Internal.Encoding.Utf8 (DecoderState, utf8AcceptState, utf8RejectState, updateDecoderState);
import Data.Text.Internal.StrictBuilder (StrictTextBuilder);
import qualified Data.ByteString as B;
import qualified Data.ByteString.Internal as BI;
import qualified Data.Text.Internal.StrictBuilder as SB;
data Utf8State = Utf8State { utf8CodePointState :: !DecoderState, partialUtf8CodePoint :: !PartialUtf8CodePoint } deriving (Eq, Show);
startUtf8State :: Utf8State;
startUtf8State = Utf8State utf8AcceptState partUtf8Empty;
newtype PartialUtf8CodePoint = PartialUtf8CodePoint Word32 deriving (Eq, Show);
partUtf8Empty :: PartialUtf8CodePoint;
partUtf8Empty = PartialUtf8CodePoint 0;
partUtf8Len :: PartialUtf8CodePoint -> Int;
partUtf8Len (PartialUtf8CodePoint w) = fromIntegral $ w `shiftR` 24;
partUtf8UnsafeIndex :: PartialUtf8CodePoint -> Int -> Word8;
partUtf8UnsafeIndex _c@(PartialUtf8CodePoint w) n = fromIntegral $ w `shiftR` (16 - 8 * n);
partUtf8UnsafeAppend :: PartialUtf8CodePoint -> ByteString -> PartialUtf8CodePoint;
partUtf8UnsafeAppend c@(PartialUtf8CodePoint word) bs = PartialUtf8CodePoint $ tryPush 0 $ tryPush 1 $ tryPush 2 $ word + (fromIntegral lenbs `shiftL` 24) where {
  lenc = partUtf8Len c;
  lenbs = B.length bs;
  tryPush i w = if i < lenbs then w + (fromIntegral (B.index bs i) `shiftL` fromIntegral (16 - 8 * (lenc + i))) else w};
partUtf8Foldr :: (Word8 -> a -> a) -> a -> PartialUtf8CodePoint -> a;
partUtf8Foldr f x0 c = case partUtf8Len c of {
  0 -> x0;
  1 -> build 0 x0;
  2 -> build 0 (build 1 x0);
  _ -> build 0 (build 1 (build 2 x0))} where {
  build i x = f (partUtf8UnsafeIndex c i) x};
partUtf8ToByteString :: PartialUtf8CodePoint -> B.ByteString;
partUtf8ToByteString c = BI.unsafeCreate (partUtf8Len c) $ \ptr -> partUtf8Foldr (\w k i -> pokeElemOff ptr i w >> k (i + 1)) (\_ -> pure ()) c 0;
validateUtf8ChunkFrom :: forall r. Int -> ByteString -> (Int -> Maybe Utf8State -> r) -> r;
validateUtf8ChunkFrom ofs bs k | guessUtf8Boundary > 0 && (B.isValidUtf8 $ B.take guessUtf8Boundary (B.drop ofs bs)) = slowValidateUtf8ChunkFrom (ofs + guessUtf8Boundary) | otherwise = slowValidateUtf8ChunkFrom ofs where {
  len = B.length bs - ofs;
  isBoundary n p = len >= n && p (B.index bs (ofs + len - n));
  guessUtf8Boundary | isBoundary 1 (<= 0x80) = len | isBoundary 1 (0xc2 <=) = len - 1 | isBoundary 2 (0xe0 <=) = len - 2 | isBoundary 3 (0xf0 <=) = len - 3 | otherwise = len;
  slowValidateUtf8ChunkFrom :: Int -> r;
  slowValidateUtf8ChunkFrom ofs1 = slowLoop ofs1 ofs1 utf8AcceptState;
  slowLoop !utf8End i s | i < B.length bs = case updateDecoderState (B.index bs i) s of {
  s' | s' == utf8RejectState -> k utf8End Nothing | s' == utf8AcceptState -> slowLoop (i + 1) (i + 1) s' | otherwise -> slowLoop utf8End (i + 1) s'} | otherwise = k utf8End (Just (Utf8State s (partUtf8UnsafeAppend partUtf8Empty (B.drop utf8End bs))))};
validateUtf8MoreCont :: Utf8State -> ByteString -> (Int -> Maybe Utf8State -> r) -> r;
validateUtf8MoreCont st@(Utf8State s0 part) bs k | len > 0 = loop 0 s0 | otherwise = k (-partUtf8Len part) (Just st) where {
  len = B.length bs;
  loop !i s | s == utf8AcceptState = validateUtf8ChunkFrom i bs k | i < len = case updateDecoderState (B.index bs i) s of {
  s' | s' == utf8RejectState -> k (-partUtf8Len part) Nothing | otherwise -> loop (i + 1) s'} | otherwise = k (-partUtf8Len part) (Just (Utf8State s (partUtf8UnsafeAppend part bs)))};
partUtf8ToStrictBuilder :: PartialUtf8CodePoint -> StrictTextBuilder;
partUtf8ToStrictBuilder c = partUtf8Foldr ((<>) . SB.unsafeFromWord8) mempty c;
utf8StateToStrictBuilder :: Utf8State -> StrictTextBuilder;
utf8StateToStrictBuilder = partUtf8ToStrictBuilder . partialUtf8CodePoint;
skipIncomplete :: OnDecodeError -> String -> Utf8State -> StrictTextBuilder;
skipIncomplete onErr msg s = partUtf8Foldr ((<>) . handleUtf8Error onErr msg) mempty (partialUtf8CodePoint s);
handleUtf8Error :: OnDecodeError -> String -> Word8 -> StrictTextBuilder;
handleUtf8Error onErr msg w = case onErr msg (Just w) of {
  Just c -> SB.fromChar c;
  Nothing -> mempty};
decodeUtf8With1 :: OnDecodeError -> String -> ByteString -> Text;
decodeUtf8With1 onErr msg bs = validateUtf8ChunkFrom 0 bs $ \len ms -> case ms of {
  Just s | len == B.length bs -> SB.toText (SB.unsafeFromByteString bs) | otherwise -> SB.toText $ SB.unsafeFromByteString (B.take len bs) <> skipIncomplete onErr msg s;
  Nothing -> let {
  (builder, _, s) = decodeUtf8With2 onErr msg startUtf8State (B.drop (len + 1) bs)} in SB.toText $ SB.unsafeFromByteString (B.take len bs) <> handleUtf8Error onErr msg (B.index bs len) <> builder <> skipIncomplete onErr msg s};
decodeUtf8With2 :: OnDecodeError -> String -> Utf8State -> ByteString -> (StrictTextBuilder, ByteString, Utf8State);
decodeUtf8With2 onErr msg s0 bs = loop s0 0 mempty where {
  loop s i !builder = let {
  nonEmptyPrefix len = builder <> utf8StateToStrictBuilder s <> SB.unsafeFromByteString (B.take len (B.drop i bs))} in validateUtf8MoreCont s (B.drop i bs) $ \len ms -> case ms of {
  Nothing -> if len < 0 then let {
  builder' = builder <> skipIncomplete onErr msg s} in loop startUtf8State i builder' else let {
  builder' = nonEmptyPrefix len <> handleUtf8Error onErr msg (B.index bs (i + len))} in loop startUtf8State (i + len + 1) builder';
  Just s' -> let {
  builder' = if len <= 0 then builder else nonEmptyPrefix len;
  undecoded = if B.length bs >= partUtf8Len (partialUtf8CodePoint s') then B.drop (i + len) bs else partUtf8ToByteString (partialUtf8CodePoint s')} in (builder', undecoded, s')}}
}
