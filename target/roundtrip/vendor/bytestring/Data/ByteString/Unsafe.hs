module Data.ByteString.Unsafe (unsafeHead, unsafeIndex, unsafeTake, unsafeDrop) where {
import Data.ByteString.Internal;
import Foreign.Storable (Storable(..));
import Control.Exception (assert);
import Data.Word (Word8);
unsafeHead :: ByteString -> Word8;
unsafeHead (BS x l) = assert (l > 0) $ accursedUnutterablePerformIO $ unsafeWithForeignPtr x $ \p -> peek p;
unsafeIndex :: ByteString -> Int -> Word8;
unsafeIndex (BS x l) i = assert (i >= 0 && i < l) $ accursedUnutterablePerformIO $ unsafeWithForeignPtr x $ \p -> peekByteOff p i;
unsafeTake :: Int -> ByteString -> ByteString;
unsafeTake n (BS x l) = assert (0 <= n && n <= l) $ BS x n;
unsafeDrop :: Int -> ByteString -> ByteString;
unsafeDrop n (BS x l) = assert (0 <= n && n <= l) $ BS (plusForeignPtr x n) (l - n)
}
