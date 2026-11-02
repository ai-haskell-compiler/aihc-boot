-- | Unsafe operations on the ST monad.
module Control.Monad.ST.Unsafe
  ( unsafeIOToST,
    unsafeSTToIO,
    unsafeInterleaveST,
  )
where

import GHC.ST
