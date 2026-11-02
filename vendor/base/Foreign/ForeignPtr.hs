-- | Pointers to memory that the program owns.
module Foreign.ForeignPtr
  ( ForeignPtr,
    mallocForeignPtrBytes,
    newForeignPtr_,
    withForeignPtr,
    touchForeignPtr,
    plusForeignPtr,
    castForeignPtr,
  )
where

import GHC.ForeignPtr
