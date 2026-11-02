-- | The foreign function interface: pointers, storable values and
-- integer types.
module Foreign
  ( module Data.Bits,
    module Data.Int,
    module Data.Word,
    module Foreign.Ptr,
    module Foreign.ForeignPtr,
    module Foreign.Storable,
    module Foreign.Marshal.Alloc,
    module Foreign.Marshal.Utils,
  )
where

import Data.Bits
import Data.Int
import Data.Word
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
