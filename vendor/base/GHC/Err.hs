-- | 'error' and 'undefined'.
module GHC.Err
  ( error,
    errorWithoutStackTrace,
    undefined,
  )
where

import GHC.Exception
import GHC.Prim
import GHC.Types

-- | Raise an 'ErrorCall' with a message.
error :: [Char] -> a
error s = primRaise (errorCallException s)

errorWithoutStackTrace :: [Char] -> a
errorWithoutStackTrace s = primRaise (errorCallException s)

undefined :: a
undefined = error "Prelude.undefined"
