module Data.Text.Internal.Unsafe (unsafeWithForeignPtr) where {
import Foreign.Ptr (Ptr);
import Foreign.ForeignPtr (ForeignPtr);
import qualified GHC.ForeignPtr (unsafeWithForeignPtr);
unsafeWithForeignPtr :: ForeignPtr a -> (Ptr a -> IO b) -> IO b;
unsafeWithForeignPtr = GHC.ForeignPtr.unsafeWithForeignPtr
}
