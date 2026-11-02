module Data.ByteString.Builder.Internal (Buffer(..), BufferRange(..), newBuffer, bufferSize, byteStringFromBuffer, ChunkIOStream(..), buildStepToCIOS, ciosUnitToLazyByteString, BuildSignal, BuildStep, finalBuildStep, bufferFull, insertChunk, fillWithBuildStep, Builder, builder, runBuilder, runBuilderWith, empty, append, ensureFree, byteStringThreshold, maximalCopySize, byteString, toLazyByteString, toLazyByteStringWith, AllocationStrategy, safeStrategy) where {
import Data.Semigroup (Semigroup(..));
import Data.List.NonEmpty (NonEmpty(..));
import qualified Data.ByteString as S;
import qualified Data.ByteString.Unsafe as S;
import qualified Data.ByteString.Internal.Type as S;
import qualified Data.ByteString.Lazy.Internal as L;
import Foreign;
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr);
import System.IO.Unsafe (unsafeDupablePerformIO);
data BufferRange = BufferRange !(Ptr Word8) !(Ptr Word8);
data Buffer = Buffer !(ForeignPtr Word8) !BufferRange;
bufferSize :: Buffer -> Int;
bufferSize (Buffer fpbuf (BufferRange _ ope)) = ope `minusPtr` unsafeForeignPtrToPtr fpbuf;
newBuffer :: Int -> IO Buffer;
newBuffer size = do {
  fpbuf <- S.mallocByteString size;
  let {
  pbuf = unsafeForeignPtrToPtr fpbuf};
  return $! Buffer fpbuf (BufferRange pbuf (pbuf `plusPtr` size))};
byteStringFromBuffer :: Buffer -> S.StrictByteString;
byteStringFromBuffer (Buffer fpbuf (BufferRange op _)) = S.BS fpbuf (op `minusPtr` unsafeForeignPtrToPtr fpbuf);
trimmedChunkFromBuffer :: AllocationStrategy -> Buffer -> L.LazyByteString -> L.LazyByteString;
trimmedChunkFromBuffer (AllocationStrategy _ _ trim) buf k | S.null bs = k | trim (S.length bs) (bufferSize buf) = L.Chunk (S.copy bs) k | otherwise = L.Chunk bs k where {
  bs = byteStringFromBuffer buf};
data ChunkIOStream a = Finished Buffer a | Yield1 S.StrictByteString (IO (ChunkIOStream a));
yield1 :: S.StrictByteString -> IO (ChunkIOStream a) -> IO (ChunkIOStream a);
yield1 bs cios | S.null bs = cios | otherwise = return $ Yield1 bs cios;
ciosUnitToLazyByteString :: AllocationStrategy -> L.LazyByteString -> ChunkIOStream () -> L.LazyByteString;
ciosUnitToLazyByteString strategy k = go where {
  go (Finished buf _) = trimmedChunkFromBuffer strategy buf k;
  go (Yield1 bs io) = L.Chunk bs $ unsafeDupablePerformIO (go <$> io)};
type BuildStep a = BufferRange -> IO (BuildSignal a);
data BuildSignal a = Done !(Ptr Word8) a | BufferFull !Int !(Ptr Word8) (BuildStep a) | InsertChunk !(Ptr Word8) S.StrictByteString (BuildStep a);
bufferFull :: Int -> Ptr Word8 -> BuildStep a -> BuildSignal a;
bufferFull = BufferFull;
insertChunk :: Ptr Word8 -> S.StrictByteString -> BuildStep a -> BuildSignal a;
insertChunk = InsertChunk;
fillWithBuildStep :: BuildStep a -> (Ptr Word8 -> a -> IO b) -> (Ptr Word8 -> Int -> BuildStep a -> IO b) -> (Ptr Word8 -> S.StrictByteString -> BuildStep a -> IO b) -> BufferRange -> IO b;
fillWithBuildStep step fDone fFull fChunk !br = do {
  signal <- step br;
  case signal of {
  Done op x -> fDone op x;
  BufferFull minSize op nextStep -> fFull op minSize nextStep;
  InsertChunk op bs nextStep -> fChunk op bs nextStep}};
newtype Builder = Builder (forall r. BuildStep r -> BuildStep r);
builder :: (forall r. BuildStep r -> BuildStep r) -> Builder;
builder = Builder;
finalBuildStep :: BuildStep ();
finalBuildStep (BufferRange op _) = return $ Done op ();
runBuilder :: Builder -> BuildStep ();
runBuilder b = runBuilderWith b finalBuildStep;
runBuilderWith :: Builder -> BuildStep a -> BuildStep a;
runBuilderWith (Builder b) = b;
empty :: Builder;
empty = Builder (\k br -> k br);
append :: Builder -> Builder -> Builder;
append (Builder b1) (Builder b2) = Builder $ b1 . b2;
stimesBuilder :: Integral t => t -> Builder -> Builder;
stimesBuilder n b | n >= 0 = go n | otherwise = stimesNegativeErr where {
  go 0 = empty;
  go k = b `append` go (k - 1)};
stimesNegativeErr :: Builder;
stimesNegativeErr = errorWithoutStackTrace "stimes @Builder: non-negative multiplier expected";
ensureFree :: Int -> Builder;
ensureFree minFree = builder step where {
  step k br@(BufferRange op ope) | ope `minusPtr` op < minFree = return $ bufferFull minFree op k | otherwise = k br};
wrappedBytesCopyStep :: S.StrictByteString -> BuildStep a -> BuildStep a;
wrappedBytesCopyStep bs0 k = go bs0 where {
  go !bs@(S.BS ifp inpRemaining) (BufferRange op ope) | inpRemaining <= outRemaining = do {
  S.unsafeWithForeignPtr ifp $ \ip -> copyBytes op ip inpRemaining;
  let {
  !br' = BufferRange (op `plusPtr` inpRemaining) ope};
  k br'} | otherwise = do {
  S.unsafeWithForeignPtr ifp $ \ip -> copyBytes op ip outRemaining;
  let {
  !bs' = S.unsafeDrop outRemaining bs};
  return $ bufferFull 1 ope (go bs')} where {
  outRemaining = ope `minusPtr` op}};
byteStringThreshold :: Int -> S.StrictByteString -> Builder;
byteStringThreshold maxCopySize = \bs -> builder $ step bs where {
  step bs@(S.BS _ len) k br@(BufferRange !op _) | len <= maxCopySize = byteStringCopyStep bs k br | otherwise = return $ insertChunk op bs k};
byteStringCopyStep :: S.StrictByteString -> BuildStep a -> BuildStep a;
byteStringCopyStep bs@(S.BS ifp isize) k br@(BufferRange op ope) | isize <= osize = do {
  S.unsafeWithForeignPtr ifp $ \ip -> copyBytes op ip isize;
  k (BufferRange op' ope)} | otherwise = wrappedBytesCopyStep bs k br where {
  osize = ope `minusPtr` op;
  op' = op `plusPtr` isize};
byteString :: S.StrictByteString -> Builder;
byteString = byteStringThreshold maximalCopySize;
maximalCopySize :: Int;
maximalCopySize = 2 * L.smallChunkSize;
data AllocationStrategy = AllocationStrategy (Maybe (Buffer, Int) -> IO Buffer) !Int (Int -> Int -> Bool);
sanitize :: Int -> Int;
sanitize = max (sizeOf (undefined :: Int));
safeStrategy :: Int -> Int -> AllocationStrategy;
safeStrategy firstSize bufSize = AllocationStrategy nextBuffer (sanitize bufSize) trim where {
  trim used size = 2 * used < size;
  nextBuffer Nothing = newBuffer $ sanitize firstSize;
  nextBuffer (Just (_, minSize)) = newBuffer minSize};
toLazyByteString :: Builder -> L.LazyByteString;
toLazyByteString = toLazyByteStringWith (safeStrategy L.smallChunkSize L.defaultChunkSize) L.Empty;
toLazyByteStringWith :: AllocationStrategy -> L.LazyByteString -> Builder -> L.LazyByteString;
toLazyByteStringWith strategy k b = ciosUnitToLazyByteString strategy k $ unsafeDupablePerformIO $ buildStepToCIOS strategy (runBuilder b);
buildStepToCIOS :: forall a. AllocationStrategy -> BuildStep a -> IO (ChunkIOStream a);
buildStepToCIOS (AllocationStrategy nextBuffer bufSize trim) = \step -> nextBuffer Nothing >>= fill step where {
  fill :: BuildStep a -> Buffer -> IO (ChunkIOStream a);
  fill !step buf@(Buffer fpbuf br@(BufferRange _ pe)) = do {
  res <- fillWithBuildStep step doneH fullH insertChunkH br;
  touchForeignPtr fpbuf;
  return res} where {
  pbuf :: Ptr Word8;
  pbuf = unsafeForeignPtrToPtr fpbuf;
  doneH :: Ptr Word8 -> a -> IO (ChunkIOStream a);
  doneH op' x = return $ Finished (Buffer fpbuf (BufferRange op' pe)) x;
  fullH :: Ptr Word8 -> Int -> BuildStep a -> IO (ChunkIOStream a);
  fullH op' minSize nextStep = wrapChunk op' $ const $ nextBuffer (Just (buf, max minSize bufSize)) >>= fill nextStep;
  insertChunkH :: Ptr Word8 -> S.StrictByteString -> BuildStep a -> IO (ChunkIOStream a);
  insertChunkH op' bs nextStep = wrapChunk op' $ \isEmpty -> yield1 bs $ if isEmpty then fill nextStep buf else do {
  buf' <- nextBuffer (Just (buf, bufSize));
  fill nextStep buf'};
  wrapChunk :: Ptr Word8 -> (Bool -> IO (ChunkIOStream a)) -> IO (ChunkIOStream a);
  wrapChunk !op' mkCIOS | chunkSize == 0 = mkCIOS True | trim chunkSize size = do {
  bs <- S.createFp chunkSize $ \fpbuf' -> S.memcpyFp fpbuf' fpbuf chunkSize;
  return $ Yield1 bs (mkCIOS False)} | otherwise = return $ Yield1 (S.BS fpbuf chunkSize) (mkCIOS False) where {
  chunkSize = op' `minusPtr` pbuf;
  size = pe `minusPtr` pbuf}}};
instance Monoid Builder where {
  mempty = empty;
  mappend = (<>);
  mconcat = foldr mappend mempty};
instance Semigroup Builder where {
  (<>) = append;
  sconcat (b :| bs) = b <> foldr mappend mempty bs;
  stimes = stimesBuilder}
}
