-- | Memory allocation. The boot base never frees memory.
module Foreign.Marshal.Alloc
  ( mallocBytes,
    free,
  )
where

import GHC.Base
import GHC.Prim
import GHC.Ptr
import GHC.Types

mallocBytes :: Int -> IO (Ptr a)
mallocBytes n = fmap intToPtr (primMalloc n)

free :: Ptr a -> IO ()
free _ = pure ()
