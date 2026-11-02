{-# LANGUAGE RankNTypes #-}
module Data.Text.Array (Array, MArray, resizeM, shrinkM, copyI, copyFromPointer, empty, equal, run, unsafeFreeze, unsafeIndex, new, unsafeWrite) where {
import Control.Monad.ST (ST, runST);
import Control.Monad.ST.Unsafe (unsafeIOToST);
import Data.Word (Word8);
import Foreign.ForeignPtr (ForeignPtr, mallocForeignPtrBytes, withForeignPtr);
import Foreign.Marshal.Utils (copyBytes);
import Foreign.Ptr (Ptr, plusPtr);
import Foreign.Storable (peekByteOff, pokeByteOff);
import System.IO.Unsafe (unsafeDupablePerformIO);
newtype Array = Array (ForeignPtr Word8);
data MArray s = MArray !(ForeignPtr Word8) !Int;
new :: forall s. Int -> ST s (MArray s);
new len = unsafeIOToST $ do {
  fp <- mallocForeignPtrBytes len;
  pure (MArray fp len)};
unsafeFreeze :: MArray s -> ST s Array;
unsafeFreeze (MArray fp _) = pure (Array fp);
unsafeIndex :: Array -> Int -> Word8;
unsafeIndex (Array fp) i = unsafeDupablePerformIO $ withForeignPtr fp $ \p -> peekByteOff p i;
unsafeWrite :: MArray s -> Int -> Word8 -> ST s ();
unsafeWrite (MArray fp _) i e = unsafeIOToST $ withForeignPtr fp $ \p -> pokeByteOff p i e;
empty :: Array;
empty = runST (new 0 >>= unsafeFreeze);
run :: (forall s. ST s (MArray s)) -> Array;
run k = runST (k >>= unsafeFreeze);
resizeM :: MArray s -> Int -> ST s (MArray s);
resizeM (MArray src srcLen) len = unsafeIOToST $ do {
  dst <- mallocForeignPtrBytes len;
  withForeignPtr src $ \sp -> withForeignPtr dst $ \dp -> copyBytes dp sp (min srcLen len);
  pure (MArray dst len)};
shrinkM :: MArray s -> Int -> ST s ();
shrinkM _ _ = pure ();
copyI :: Int -> MArray s -> Int -> Array -> Int -> ST s ();
copyI count (MArray dst _) dstOff (Array src) srcOff = unsafeIOToST $ withForeignPtr src $ \sp -> withForeignPtr dst $ \dp -> copyBytes (dp `plusPtr` dstOff) (sp `plusPtr` srcOff) count;
copyFromPointer :: MArray s -> Int -> Ptr Word8 -> Int -> ST s ();
copyFromPointer (MArray dst _) dstOff src count = unsafeIOToST $ withForeignPtr dst $ \dp -> copyBytes (dp `plusPtr` dstOff) src count;
equal :: Array -> Int -> Array -> Int -> Int -> Bool;
equal (Array fp1) off1 (Array fp2) off2 count = unsafeDupablePerformIO $ withForeignPtr fp1 $ \p1 -> withForeignPtr fp2 $ \p2 -> let {
  go i | i >= count = pure True | otherwise = do {
  a <- peekByteOff p1 (off1 + i) :: IO Word8;
  b <- peekByteOff p2 (off2 + i);
  if a == b then go (i + 1) else pure False}} in go 0
}
