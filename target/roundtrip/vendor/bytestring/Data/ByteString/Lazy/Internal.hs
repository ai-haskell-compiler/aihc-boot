module Data.ByteString.Lazy.Internal (ByteString(Empty, Chunk), LazyByteString, defaultChunkSize, smallChunkSize, chunkOverhead, toStrict) where {
import Prelude hiding (concat);
import qualified Data.ByteString.Internal.Type as S;
import Foreign.Storable (Storable(sizeOf));
data ByteString = Empty | Chunk !S.StrictByteString ByteString;
type LazyByteString = ByteString;
defaultChunkSize :: Int;
defaultChunkSize = 32 * k - chunkOverhead where {
  k = 1024};
smallChunkSize :: Int;
smallChunkSize = 4 * k - chunkOverhead where {
  k = 1024};
chunkOverhead :: Int;
chunkOverhead = 2 * sizeOf (undefined :: Int);
toStrict :: LazyByteString -> S.StrictByteString;
toStrict = \cs -> goLen0 cs cs where {
  goLen0 _ Empty = S.BS S.nullForeignPtr 0;
  goLen0 cs0 (Chunk c cs) = goLen1 cs0 c cs;
  goLen1 _ bs Empty = bs;
  goLen1 cs0 (S.BS _ bl) (Chunk (S.BS _ cl) cs) = goLen cs0 (S.checkedAdd "Lazy.toStrict" bl cl) cs;
  goLen cs0 !total (Chunk (S.BS _ cl) cs) = goLen cs0 (S.checkedAdd "Lazy.toStrict" total cl) cs;
  goLen cs0 total Empty = S.unsafeCreateFp total $ \ptr -> goCopy cs0 ptr;
  goCopy Empty !_ = return ();
  goCopy (Chunk (S.BS fp len) cs) !ptr = do {
  S.memcpyFp ptr fp len;
  goCopy cs (ptr `S.plusForeignPtr` len)}}
}
