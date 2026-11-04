-- | The Bool type and its functions.
module Data.Bool (Bool (..), (&&), (||), not, otherwise, bool) where

import GHC.Base
import GHC.Classes
import GHC.Types

-- | Case analysis: the first argument for False, the second for True.
bool :: a -> a -> Bool -> a
bool f _ False = f
bool _ t True = t
