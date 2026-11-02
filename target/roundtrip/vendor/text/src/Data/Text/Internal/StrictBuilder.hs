{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
module Data.Text.Internal.StrictBuilder (StrictTextBuilder(..), toText, fromChar, unsafeFromByteString, unsafeFromWord8) where {
import Control.Monad.ST (ST, runST);
import Control.Monad.ST.Unsafe (unsafeIOToST, unsafeSTToIO);
import Data.Functor (void);
import Data.Word (Word8);
import Data.ByteString (ByteString);
import Data.Text.Internal (Text(..), empty, safe);
import Data.Text.Internal.ByteStringCompat (withBS);
import Data.Text.Internal.Encoding.Utf8 (utf8Length);
import Data.Text.Internal.Unsafe (unsafeWithForeignPtr);
import qualified Data.ByteString as B;
import qualified Data.Text.Array as A;
import qualified Data.Text.Internal.Unsafe.Char as Char;
data StrictTextBuilder = StrictTextBuilder { sbLength :: !Int, sbWrite :: forall s. A.MArray s -> Int -> ST s () };
toText :: StrictTextBuilder -> Text;
toText (StrictTextBuilder 0 _) = empty;
toText (StrictTextBuilder n write) = runST (do {
  dst <- A.new n;
  write dst 0;
  arr <- A.unsafeFreeze dst;
  pure (Text arr 0 n)});
emptyStrictBuilder :: StrictTextBuilder;
emptyStrictBuilder = StrictTextBuilder 0 (\_ _ -> pure ());
appendRStrictBuilder :: StrictTextBuilder -> StrictTextBuilder -> StrictTextBuilder;
appendRStrictBuilder (StrictTextBuilder 0 _) b2 = b2;
appendRStrictBuilder b1 (StrictTextBuilder 0 _) = b1;
appendRStrictBuilder (StrictTextBuilder n1 write1) (StrictTextBuilder n2 write2) = StrictTextBuilder (n1 + n2) (\dst ofs -> do {
  write2 dst (ofs + n1);
  write1 dst ofs});
copyFromByteString :: A.MArray s -> Int -> ByteString -> ST s ();
copyFromByteString dst ofs src = withBS src $ \srcFPtr len -> unsafeIOToST $ unsafeWithForeignPtr srcFPtr $ \srcPtr -> do {
  unsafeSTToIO $ A.copyFromPointer dst ofs srcPtr len};
unsafeFromByteString :: ByteString -> StrictTextBuilder;
unsafeFromByteString bs = StrictTextBuilder (B.length bs) (\dst ofs -> copyFromByteString dst ofs bs);
fromChar :: Char -> StrictTextBuilder;
fromChar c = StrictTextBuilder (utf8Length c) (\dst ofs -> void (Char.unsafeWrite dst ofs (safe c)));
unsafeFromWord8 :: Word8 -> StrictTextBuilder;
unsafeFromWord8 !w = StrictTextBuilder 1 (\dst ofs -> A.unsafeWrite dst ofs w);
instance Semigroup StrictTextBuilder where {
  (<>) = appendRStrictBuilder};
instance Monoid StrictTextBuilder where {
  mempty = emptyStrictBuilder;
  mappend = (<>)}
}
