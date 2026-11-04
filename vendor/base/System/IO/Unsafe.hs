{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

-- | Running IO actions outside of IO.
module System.IO.Unsafe
  ( unsafePerformIO,
    unsafeDupablePerformIO,
    unsafeInterleaveIO,
  )
where

import GHC.Base
import GHC.Prim
import GHC.Types

-- | Run an action on a state token. The token is the unit value at run
-- time.
unsafePerformIO :: IO a -> a
unsafePerformIO (IO m) = case m (primUnsafeCoerce ()) of
  (# _, a #) -> a

unsafeDupablePerformIO :: IO a -> a
unsafeDupablePerformIO = unsafePerformIO

unsafeInterleaveIO :: IO a -> IO a
unsafeInterleaveIO m = IO (\s -> (# s, unsafePerformIO m #))
