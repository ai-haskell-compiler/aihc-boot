{-# OPTIONS_HADDOCK not-home #-}

-- | Copyright : (c) 2010 - 2011 Simon Meier
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : unstable, private
-- Portability : GHC
--
-- *Warning:* this module is internal. If you find that you need it then please
-- contact the maintainers and explain what you are trying to do and discuss
-- what you would need in the public API. It is important that you do this as
-- the module may not be exposed at all in future releases.
--
-- Core types and functions for the 'Builder' monoid and its generalization,
-- the 'Put' monad.
--
-- The design of the 'Builder' monoid is optimized such that
--
--   1. buffers of arbitrary size can be filled as efficiently as possible and
--
--   2. sequencing of 'Builder's is as cheap as possible.
--
-- We achieve (1) by completely handing over control over writing to the buffer
-- to the 'BuildStep' implementing the 'Builder'. This 'BuildStep' is just told
-- the start and the end of the buffer (represented as a 'BufferRange'). Then,
-- the 'BuildStep' can write to as big a prefix of this 'BufferRange' in any
-- way it desires. If the 'BuildStep' is done, the 'BufferRange' is full, or a
-- long sequence of bytes should be inserted directly, then the 'BuildStep'
-- signals this to its caller using a 'BuildSignal'.
--
-- We achieve (2) by requiring that every 'Builder' is implemented by a
-- 'BuildStep' that takes a continuation 'BuildStep', which it calls with the
-- updated 'BufferRange' after it is done. Therefore, only two pointers have
-- to be passed in a function call to implement concatenation of 'Builder's.
-- Moreover, many 'Builder's are completely inlined, which enables the compiler
-- to sequence them without a function call and with no boxing at all.
--
-- This design gives the implementation of a 'Builder' full access to the 'IO'
-- monad. Therefore, utmost care has to be taken to not overwrite anything
-- outside the given 'BufferRange's. Moreover, further care has to be taken to
-- ensure that 'Builder's and 'Put's are referentially transparent. See the
-- comments of the 'builder' and 'put' functions for further information.
-- Note that there are /no safety belts/ at all, when implementing a 'Builder'
-- using an 'IO' action: you are writing code that might enable the next
-- buffer-overflow attack on a Haskell server!
--
module Data.ByteString.Builder.Internal (
    Buffer(..),
    BufferRange(..),
    newBuffer,
    bufferSize,
    byteStringFromBuffer,
    ChunkIOStream(..),
    buildStepToCIOS,
    ciosUnitToLazyByteString,
    BuildSignal,
    BuildStep,
    finalBuildStep,
    bufferFull,
    insertChunk,
    fillWithBuildStep,
    Builder,
    builder,
    runBuilder,
    runBuilderWith,
    empty,
    append,
    ensureFree,
    byteStringThreshold,
    maximalCopySize,
    byteString,
    toLazyByteString,
    toLazyByteStringWith,
    AllocationStrategy,
    safeStrategy,
  ) where

import           Data.Semigroup (Semigroup(..))
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.ByteString               as S
import qualified Data.ByteString.Unsafe        as S
import qualified Data.ByteString.Internal.Type as S
import qualified Data.ByteString.Lazy.Internal as L
import           Foreign
import           Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import           System.IO.Unsafe (unsafeDupablePerformIO)

------------------------------------------------------------------------------
-- Buffers
------------------------------------------------------------------------------
-- | A range of bytes in a buffer represented by the pointer to the first byte
-- of the range and the pointer to the first byte /after/ the range.
data BufferRange = BufferRange {-# UNPACK #-} !(Ptr Word8)  -- First byte of range
                               {-# UNPACK #-} !(Ptr Word8)  -- First byte /after/ range

-- | A 'Buffer' together with the 'BufferRange' of free bytes. The filled
-- space starts at offset 0 and ends at the first free byte.
data Buffer = Buffer {-# UNPACK #-} !(ForeignPtr Word8)
                     {-# UNPACK #-} !BufferRange

{-# INLINE bufferSize #-}
-- | Combined size of the filled and free space in the buffer.
bufferSize :: Buffer -> Int
bufferSize (Buffer fpbuf (BufferRange _ ope)) =
    ope `minusPtr` unsafeForeignPtrToPtr fpbuf

{-# INLINE newBuffer #-}
-- | Allocate a new buffer of the given size.
newBuffer :: Int -> IO Buffer
newBuffer size = do
    fpbuf <- S.mallocByteString size
    let pbuf = unsafeForeignPtrToPtr fpbuf
    return $! Buffer fpbuf (BufferRange pbuf (pbuf `plusPtr` size))

{-# INLINE byteStringFromBuffer #-}
-- | Convert the filled part of a 'Buffer' to a 'S.StrictByteString'.
byteStringFromBuffer :: Buffer -> S.StrictByteString
byteStringFromBuffer (Buffer fpbuf (BufferRange op _)) =
    S.BS fpbuf (op `minusPtr` unsafeForeignPtrToPtr fpbuf)

{-# INLINE trimmedChunkFromBuffer #-}
-- | Prepend the filled part of a 'Buffer' to a 'L.LazyByteString'
-- trimming it if necessary.
trimmedChunkFromBuffer :: AllocationStrategy -> Buffer
                       -> L.LazyByteString -> L.LazyByteString
trimmedChunkFromBuffer (AllocationStrategy _ _ trim) buf k
  | S.null bs                           = k
  | trim (S.length bs) (bufferSize buf) = L.Chunk (S.copy bs) k
  | otherwise                           = L.Chunk bs          k
  where
    bs = byteStringFromBuffer buf

-- | A stream of chunks that are constructed in the 'IO' monad.
--
-- This datatype serves as the common interface for the buffer-by-buffer
-- execution of a 'BuildStep' by 'buildStepToCIOS'. Typical users of this
-- interface are 'ciosToLazyByteString' or iteratee-style libraries like
-- @enumerator@.
data ChunkIOStream a =
       Finished Buffer a
       -- ^ The partially filled last buffer together with the result.
     | Yield1 S.StrictByteString (IO (ChunkIOStream a))
       -- ^ Yield a /non-empty/ 'S.StrictByteString'.

{-# INLINE yield1 #-}
-- | A smart constructor for yielding one chunk that ignores the chunk if
-- it is empty.
yield1 :: S.StrictByteString -> IO (ChunkIOStream a) -> IO (ChunkIOStream a)
yield1 bs cios | S.null bs = cios
               | otherwise = return $ Yield1 bs cios

{-# INLINE ciosUnitToLazyByteString #-}
-- | Convert a @'ChunkIOStream' ()@ to a 'L.LazyByteString' using
-- 'unsafeDupablePerformIO'.
ciosUnitToLazyByteString :: AllocationStrategy
                         -> L.LazyByteString -> ChunkIOStream () -> L.LazyByteString
ciosUnitToLazyByteString strategy k = go
  where
    go (Finished buf _) = trimmedChunkFromBuffer strategy buf k
    go (Yield1 bs io)   = L.Chunk bs $ unsafeDupablePerformIO (go <$> io)

-- | 'BuildStep's may be called *multiple times* and they must not rise an
-- async. exception.
type BuildStep a = BufferRange -> IO (BuildSignal a)

-- | 'BuildSignal's abstract signals to the caller of a 'BuildStep'. There are
-- three signals: 'done', 'bufferFull', or 'insertChunks signals
data BuildSignal a =
    Done {-# UNPACK #-} !(Ptr Word8) a
  | BufferFull
      {-# UNPACK #-} !Int
      {-# UNPACK #-} !(Ptr Word8)
                     (BuildStep a)
  | InsertChunk
      {-# UNPACK #-} !(Ptr Word8)
                     S.StrictByteString
                     (BuildStep a)

{-# INLINE bufferFull #-}
-- | Signal that the current buffer is full.
bufferFull :: Int
           -- ^ Minimal size of next 'BufferRange'.
           -> Ptr Word8
           -- ^ Next free byte in current 'BufferRange'.
           -> BuildStep a
           -- ^ 'BuildStep' to run on the next 'BufferRange'. This 'BuildStep'
           -- may assume that it is called with a 'BufferRange' of at least the
           -- required minimal size; i.e., the caller of this 'BuildStep' must
           -- guarantee this.
           -> BuildSignal a
bufferFull = BufferFull

{-# INLINE insertChunk #-}
-- | Signal that a 'S.StrictByteString' chunk should be inserted directly.
insertChunk :: Ptr Word8
            -- ^ Next free byte in current 'BufferRange'
            -> S.StrictByteString
            -- ^ Chunk to insert.
            -> BuildStep a
            -- ^ 'BuildStep' to run on next 'BufferRange'
            -> BuildSignal a
insertChunk = InsertChunk

{-# INLINE fillWithBuildStep #-}
-- | Fill a 'BufferRange' using a 'BuildStep'.
fillWithBuildStep
    :: BuildStep a
    -- ^ Build step to use for filling the 'BufferRange'.
    -> (Ptr Word8 -> a -> IO b)
    -- ^ Handling the 'done' signal
    -> (Ptr Word8 -> Int -> BuildStep a -> IO b)
    -- ^ Handling the 'bufferFull' signal
    -> (Ptr Word8 -> S.StrictByteString -> BuildStep a -> IO b)
    -- ^ Handling the 'insertChunk' signal
    -> BufferRange
    -- ^ Buffer range to fill.
    -> IO b
    -- ^ Value computed while filling this 'BufferRange'.
fillWithBuildStep step fDone fFull fChunk !br = do
    signal <- step br
    case signal of
        Done op x                      -> fDone op x
        BufferFull minSize op nextStep -> fFull op minSize nextStep
        InsertChunk op bs nextStep     -> fChunk op bs nextStep

-- | 'Builder's denote sequences of bytes.
-- They are 'Monoid's where
--   'mempty' is the zero-length sequence and
--   'mappend' is concatenation, which runs in /O(1)/.
newtype Builder = Builder (forall r. BuildStep r -> BuildStep r)

{-# INLINE builder #-}
-- | Construct a 'Builder'. In contrast to 'BuildStep's, 'Builder's are
-- referentially transparent.
builder :: (forall r. BuildStep r -> BuildStep r)
        -- ^ A function that fills a 'BufferRange', calls the continuation with
        -- the updated 'BufferRange' once its done, and signals its caller how
        -- to proceed using 'done', 'bufferFull', or 'insertChunk'.
        --
        -- This function must be referentially transparent; i.e., calling it
        -- multiple times with equally sized 'BufferRange's must result in the
        -- same sequence of bytes being written. If you need mutable state,
        -- then you must allocate it anew upon each call of this function.
        -- Moreover, this function must call the continuation once its done.
        -- Otherwise, concatenation of 'Builder's does not work. Finally, this
        -- function must write to all bytes that it claims it has written.
        -- Otherwise, the resulting 'Builder' is not guaranteed to be
        -- referentially transparent and sensitive data might leak.
        -> Builder
builder = Builder

-- | The final build step that returns the 'done' signal.
finalBuildStep :: BuildStep ()
finalBuildStep (BufferRange op _) = return $ Done op ()

{-# INLINE runBuilder #-}
-- | Run a 'Builder' with the 'finalBuildStep'.
runBuilder :: Builder      -- ^ 'Builder' to run
           -> BuildStep () -- ^ 'BuildStep' that writes the byte stream of this
                           -- 'Builder' and signals 'done' upon completion.
runBuilder b = runBuilderWith b finalBuildStep

{-# INLINE runBuilderWith #-}
-- | Run a 'Builder'.
runBuilderWith :: Builder      -- ^ 'Builder' to run
               -> BuildStep a -- ^ Continuation 'BuildStep'
               -> BuildStep a
runBuilderWith (Builder b) = b

-- | The 'Builder' denoting a zero-length sequence of bytes. This function is
-- only exported for use in rewriting rules. Use 'mempty' otherwise.
empty :: Builder
empty = Builder (\k br -> k br)

-- | Concatenate two 'Builder's. This function is only exported for use in rewriting
-- rules. Use 'mappend' otherwise.
append :: Builder -> Builder -> Builder
append (Builder b1) (Builder b2) = Builder $ b1 . b2

stimesBuilder :: Integral t => t -> Builder -> Builder
{-# INLINABLE stimesBuilder #-}
stimesBuilder n b
  | n >= 0 = go n
  | otherwise = stimesNegativeErr
  where go 0 = empty
        go k = b `append` go (k - 1)

stimesNegativeErr :: Builder
-- See Note [Float error calls out of INLINABLE things]
-- in Data.ByteString.Internal.Type
stimesNegativeErr
  = errorWithoutStackTrace "stimes @Builder: non-negative multiplier expected"

{-# INLINE ensureFree #-}
-- | @'ensureFree' n@ ensures that there are at least @n@ free bytes
-- for the following 'Builder'.
ensureFree :: Int -> Builder
ensureFree minFree =
    builder step
  where
    step k br@(BufferRange op ope)
      | ope `minusPtr` op < minFree = return $ bufferFull minFree op k
      | otherwise                   = k br

-- | Copy the bytes from a 'S.StrictByteString' into the output stream.
wrappedBytesCopyStep :: S.StrictByteString  -- ^ Input 'S.StrictByteString'.
                     -> BuildStep a -> BuildStep a
-- See Note [byteStringCopyStep and wrappedBytesCopyStep]
wrappedBytesCopyStep bs0 k =
    go bs0
  where
    go !bs@(S.BS ifp inpRemaining) (BufferRange op ope)
      | inpRemaining <= outRemaining = do
          S.unsafeWithForeignPtr ifp $ \ip -> copyBytes op ip inpRemaining
          let !br' = BufferRange (op `plusPtr` inpRemaining) ope
          k br'
      | otherwise = do
          S.unsafeWithForeignPtr ifp $ \ip -> copyBytes op ip outRemaining
          let !bs' = S.unsafeDrop outRemaining bs
          return $ bufferFull 1 ope (go bs')
      where
        outRemaining = ope `minusPtr` op

{-# INLINE byteStringThreshold #-}
-- | Construct a 'Builder' that copies the 'S.StrictByteString's, if it is
-- smaller than the treshold, and inserts it directly otherwise.
--
-- For example, @byteStringThreshold 1024@ copies 'S.StrictByteString's whose size
-- is less or equal to 1kb, and inserts them directly otherwise. This implies
-- that the average chunk-size of the generated 'L.LazyByteString' may be as
-- low as 513 bytes, as there could always be just a single byte between the
-- directly inserted 1025 byte, 'S.StrictByteString's.
--
byteStringThreshold :: Int -> S.StrictByteString -> Builder
byteStringThreshold maxCopySize =
    \bs -> builder $ step bs
  where
    step bs@(S.BS _ len) k br@(BufferRange !op _)
      | len <= maxCopySize = byteStringCopyStep bs k br
      | otherwise          = return $ insertChunk op bs k

{-# INLINE byteStringCopyStep #-}
byteStringCopyStep :: S.StrictByteString -> BuildStep a -> BuildStep a
-- See Note [byteStringCopyStep and wrappedBytesCopyStep]
byteStringCopyStep bs@(S.BS ifp isize) k br@(BufferRange op ope)
    | isize <= osize = do
        S.unsafeWithForeignPtr ifp $ \ip -> copyBytes op ip isize
        k (BufferRange op' ope)
    | otherwise  = wrappedBytesCopyStep bs k br
  where
    osize = ope `minusPtr` op
    op'  = op `plusPtr` isize

{-# INLINE byteString #-}
-- | Create a 'Builder' denoting the same sequence of bytes as a
-- 'S.StrictByteString'.
-- The 'Builder' inserts large 'S.StrictByteString's directly, but copies small ones
-- to ensure that the generated chunks are large on average.
--
byteString :: S.StrictByteString -> Builder
byteString = byteStringThreshold maximalCopySize

-- | The maximal size of a 'S.StrictByteString' that is copied.
-- @2 * 'L.smallChunkSize'@ to guarantee that on average a chunk is of
-- 'L.smallChunkSize'.
maximalCopySize :: Int
maximalCopySize = 2 * L.smallChunkSize

-- | A buffer allocation strategy for executing 'Builder's.
data AllocationStrategy = AllocationStrategy
         (Maybe (Buffer, Int) -> IO Buffer)
         {-# UNPACK #-} !Int
         (Int -> Int -> Bool)

{-# INLINE sanitize #-}
-- | Sanitize a buffer size; i.e., make it at least the size of an 'Int'.
sanitize :: Int -> Int
sanitize = max (sizeOf (undefined :: Int))

{-# INLINE safeStrategy #-}
-- | Use this strategy for generating 'L.LazyByteString's whose chunks are
-- likely to survive one garbage collection. This strategy trims buffers
-- that are filled less than half in order to avoid spilling too much memory.
safeStrategy :: Int  -- ^ Size of first buffer
             -> Int  -- ^ Size of successive buffers
             -> AllocationStrategy
             -- ^ An allocation strategy that guarantees that at least half
             -- of the allocated memory is used for live data
safeStrategy firstSize bufSize =
    AllocationStrategy nextBuffer (sanitize bufSize) trim
  where
    trim used size                 = 2 * used < size
    {-# INLINE nextBuffer #-}
    nextBuffer Nothing             = newBuffer $ sanitize firstSize
    nextBuffer (Just (_, minSize)) = newBuffer minSize

{-# NOINLINE toLazyByteString #-} -- ensure code is shared
-- | Execute a 'Builder' and return the generated chunks as a 'L.LazyByteString'.
-- The work is performed lazy, i.e., only when a chunk of the 'L.LazyByteString'
-- is forced.
toLazyByteString :: Builder -> L.LazyByteString
toLazyByteString = toLazyByteStringWith
    (safeStrategy L.smallChunkSize L.defaultChunkSize) L.Empty

{-# INLINE toLazyByteStringWith #-}
-- | /Heavy inlining./ Execute a 'Builder' with custom execution parameters.
--
-- This function is inlined despite its heavy code-size to allow fusing with
-- the allocation strategy. For example, the default 'Builder' execution
-- function 'Data.ByteString.Builder.Internal.toLazyByteString' is defined as follows.
--
-- @
-- {-\# NOINLINE toLazyByteString \#-}
-- toLazyByteString =
--   toLazyByteStringWith ('safeStrategy' 'L.smallChunkSize' 'L.defaultChunkSize') L.Empty
-- @
--
-- where @L.Empty@ is the zero-length 'L.LazyByteString'.
--
-- In most cases, the parameters used by 'Data.ByteString.Builder.toLazyByteString' give good
-- performance. A sub-performing case of 'Data.ByteString.Builder.toLazyByteString' is executing short
-- (<128 bytes) 'Builder's. In this case, the allocation overhead for the first
-- 4kb buffer and the trimming cost dominate the cost of executing the
-- 'Builder'. You can avoid this problem using
--
-- >toLazyByteStringWith (safeStrategy 128 smallChunkSize) L.Empty
--
-- This reduces the allocation and trimming overhead, as all generated
-- 'L.LazyByteString's fit into the first buffer and there is no trimming
-- required, if more than 64 bytes and less than 128 bytes are written.
--
toLazyByteStringWith
    :: AllocationStrategy
       -- ^ Buffer allocation strategy to use
    -> L.LazyByteString
       -- ^ 'L.LazyByteString' to use as the tail of the generated lazy
       -- 'L.LazyByteString'
    -> Builder
       -- ^ 'Builder' to execute
    -> L.LazyByteString
       -- ^ Resulting 'L.LazyByteString'
toLazyByteStringWith strategy k b =
    ciosUnitToLazyByteString strategy k $ unsafeDupablePerformIO $
        buildStepToCIOS strategy (runBuilder b)

{-# INLINE buildStepToCIOS #-}
-- | Convert a 'BuildStep' to a 'ChunkIOStream' stream by executing it on
-- 'Buffer's allocated according to the given 'AllocationStrategy'.
buildStepToCIOS
    :: forall a.
       AllocationStrategy          -- ^ Buffer allocation strategy to use
    -> BuildStep a                 -- ^ 'BuildStep' to execute
    -> IO (ChunkIOStream a)
buildStepToCIOS (AllocationStrategy nextBuffer bufSize trim) =
    \step -> nextBuffer Nothing >>= fill step
  where
    fill :: BuildStep a -> Buffer -> IO (ChunkIOStream a)
    fill !step buf@(Buffer fpbuf br@(BufferRange _ pe)) = do
        res <- fillWithBuildStep step doneH fullH insertChunkH br
        touchForeignPtr fpbuf
        return res
      where
        pbuf :: Ptr Word8
        pbuf = unsafeForeignPtrToPtr fpbuf

        doneH :: Ptr Word8 -> a -> IO (ChunkIOStream a)
        doneH op' x = return $
            Finished (Buffer fpbuf (BufferRange op' pe)) x

        fullH :: Ptr Word8 -> Int -> BuildStep a -> IO (ChunkIOStream a)
        fullH op' minSize nextStep =
            wrapChunk op' $ const $
                nextBuffer (Just (buf, max minSize bufSize)) >>= fill nextStep

        insertChunkH :: Ptr Word8 -> S.StrictByteString -> BuildStep a -> IO (ChunkIOStream a)
        insertChunkH op' bs nextStep =
            wrapChunk op' $ \isEmpty -> yield1 bs $
                -- Checking for empty case avoids allocating 'n-1' empty
                -- buffers for 'n' insertChunkH right after each other.
                if isEmpty
                  then fill nextStep buf
                  else do buf' <- nextBuffer (Just (buf, bufSize))
                          fill nextStep buf'

        -- Wrap and yield a chunk, trimming it if necesary
        {-# INLINE wrapChunk #-}
        wrapChunk :: Ptr Word8 -> (Bool -> IO (ChunkIOStream a)) -> IO (ChunkIOStream a)
        wrapChunk !op' mkCIOS
          | chunkSize == 0      = mkCIOS True
          | trim chunkSize size = do
              bs <- S.createFp chunkSize $ \fpbuf' ->
                        S.memcpyFp fpbuf' fpbuf chunkSize
              -- It is not safe to re-use the old buffer (see #690),
              -- so we allocate a new buffer after trimming.
              return $ Yield1 bs (mkCIOS False)
          | otherwise            =
              return $ Yield1 (S.BS fpbuf chunkSize) (mkCIOS False)
          where
            chunkSize = op' `minusPtr` pbuf
            size      = pe  `minusPtr` pbuf

instance Monoid Builder where
  {-# INLINE mempty #-}
  mempty = empty
  {-# INLINE mappend #-}
  mappend = (<>)
  {-# INLINE mconcat #-}
  mconcat = foldr mappend mempty

instance Semigroup Builder where
  {-# INLINE (<>) #-}
  (<>) = append
  sconcat (b:|bs) = b <> foldr mappend mempty bs
  {-# INLINE stimes #-}
  stimes = stimesBuilder

