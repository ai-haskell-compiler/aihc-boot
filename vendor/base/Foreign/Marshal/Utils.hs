-- | Copying and filling memory.
module Foreign.Marshal.Utils
  ( copyBytes,
    moveBytes,
    fillBytes,
  )
where

import GHC.Prim
import GHC.Ptr
import GHC.Types
import GHC.Word

-- | Copy bytes from the second area to the first. The areas must not
-- overlap.
copyBytes :: Ptr a -> Ptr a -> Int -> IO ()
copyBytes dst src n = primMemcpy (ptrToInt dst) (ptrToInt src) n

-- | Copy bytes from the second area to the first. The areas may
-- overlap.
moveBytes :: Ptr a -> Ptr a -> Int -> IO ()
moveBytes dst src n = primMemmove (ptrToInt dst) (ptrToInt src) n

fillBytes :: Ptr a -> Word8 -> Int -> IO ()
fillBytes dst w n = primMemset (ptrToInt dst) (toWord8 w) n
