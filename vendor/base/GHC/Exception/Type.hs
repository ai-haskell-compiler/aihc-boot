{-# LANGUAGE ExistentialQuantification #-}

-- | The 'Exception' class, 'SomeException' and the errors of
-- arithmetic.
--
-- Every raised exception is a 'SomeException'. A handler selects the
-- exceptions of its type with 'fromException', which compares the
-- run-time types (see Data.Typeable.Internal).
module GHC.Exception.Type
  ( Exception (..),
    SomeException (..),
    ArithException (..),
    divZeroError,
  )
where

import Data.Typeable.Internal
import GHC.Maybe
import GHC.Prim
import GHC.Show
import GHC.Types

data SomeException = forall e. (Exception e) => SomeException e

class (Typeable e, Show e) => Exception e where
  toException :: e -> SomeException
  fromException :: SomeException -> Maybe e
  displayException :: e -> [Char]
  toException = SomeException
  fromException (SomeException e) = cast e
  displayException e = show e

instance Show SomeException where
  showsPrec p (SomeException e) = showsPrec p e

instance Exception SomeException where
  toException se = se
  fromException = Just
  displayException (SomeException e) = displayException e

-- | The errors of arithmetic.
data ArithException
  = Overflow
  | Underflow
  | LossOfPrecision
  | DivideByZero
  | Denormal
  | RatioZeroDenominator

instance Show ArithException where
  showsPrec _ Overflow = showString "arithmetic overflow"
  showsPrec _ Underflow = showString "arithmetic underflow"
  showsPrec _ LossOfPrecision = showString "loss of precision"
  showsPrec _ DivideByZero = showString "divide by zero"
  showsPrec _ Denormal = showString "denormal"
  showsPrec _ RatioZeroDenominator = showString "Ratio has zero denominator"

instance Exception ArithException

divZeroError :: a
divZeroError = primRaise (toException DivideByZero)
