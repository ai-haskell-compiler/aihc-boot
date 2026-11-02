-- | Pointers to memory that the program owns. The boot base never
-- frees the memory, so a 'ForeignPtr' is a 'Ptr'.
module GHC.ForeignPtr
  ( ForeignPtr,
    mallocForeignPtrBytes,
    mallocPlainForeignPtrBytes,
    newForeignPtr_,
    withForeignPtr,
    unsafeWithForeignPtr,
    unsafeForeignPtrToPtr,
    touchForeignPtr,
    plusForeignPtr,
    castForeignPtr,
  )
where

import GHC.Base
import GHC.Classes
import GHC.Prim
import GHC.Ptr
import GHC.Show
import GHC.Types

newtype ForeignPtr a = MkForeignPtr (Ptr a)

mallocForeignPtrBytes :: Int -> IO (ForeignPtr a)
mallocForeignPtrBytes n = fmap (\a -> MkForeignPtr (intToPtr a)) (primMalloc n)

mallocPlainForeignPtrBytes :: Int -> IO (ForeignPtr a)
mallocPlainForeignPtrBytes = mallocForeignPtrBytes

newForeignPtr_ :: Ptr a -> IO (ForeignPtr a)
newForeignPtr_ p = pure (MkForeignPtr p)

withForeignPtr :: ForeignPtr a -> (Ptr a -> IO b) -> IO b
withForeignPtr (MkForeignPtr p) f = f p

unsafeWithForeignPtr :: ForeignPtr a -> (Ptr a -> IO b) -> IO b
unsafeWithForeignPtr = withForeignPtr

unsafeForeignPtrToPtr :: ForeignPtr a -> Ptr a
unsafeForeignPtrToPtr (MkForeignPtr p) = p

touchForeignPtr :: ForeignPtr a -> IO ()
touchForeignPtr _ = pure ()

plusForeignPtr :: ForeignPtr a -> Int -> ForeignPtr b
plusForeignPtr (MkForeignPtr p) n = MkForeignPtr (plusPtr p n)

castForeignPtr :: ForeignPtr a -> ForeignPtr b
castForeignPtr (MkForeignPtr p) = MkForeignPtr (castPtr p)

instance Eq (ForeignPtr a) where
  MkForeignPtr p == MkForeignPtr q = p == q

instance Ord (ForeignPtr a) where
  compare (MkForeignPtr p) (MkForeignPtr q) = compare p q

instance Show (ForeignPtr a) where
  showsPrec d (MkForeignPtr p) = showsPrec d p
