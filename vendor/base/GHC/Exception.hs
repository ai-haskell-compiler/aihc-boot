-- | 'ErrorCall', the exception of 'error', and 'throw'.
module GHC.Exception
  ( ErrorCall (..),
    throw,
    errorCallException,
  )
where

import GHC.Exception.Type
import GHC.Prim
import GHC.Show
import GHC.Types

-- | The exception that 'error' raises.
data ErrorCall = ErrorCall [Char]

instance Show ErrorCall where
  showsPrec _ (ErrorCall err) = showString err

instance Exception ErrorCall

-- | Raise an exception in pure code.
throw :: (Exception e) => e -> a
throw e = primRaise (toException e)

errorCallException :: [Char] -> SomeException
errorCallException s = toException (ErrorCall s)
