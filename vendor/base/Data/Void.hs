-- | The uninhabited type.
module Data.Void (Void, absurd, vacuous) where

import GHC.Base
import GHC.Classes
import GHC.Err
import GHC.Prim
import GHC.Show
import GHC.Types

data Void

-- | Since Void has no values, the argument is bottom.
absurd :: Void -> a
absurd v = v `seq` errorWithoutStackTrace "Data.Void.absurd"

vacuous :: (Functor f) => f Void -> f a
vacuous = fmap absurd

instance Eq Void where
  _ == _ = True

instance Ord Void where
  compare _ _ = EQ

instance Show Void where
  showsPrec _ = absurd

instance Semigroup Void where
  a <> _ = a
