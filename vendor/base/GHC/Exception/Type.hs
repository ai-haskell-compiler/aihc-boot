-- | The exception types and 'throw'.
--
-- The boot base has no Exception class yet. An exception is the thrown
-- value itself, and a handler gets every exception. 'SomeException' is
-- a newtype, so the evaluator erases its constructor: the pattern
-- @SomeException e@ matches every exception.
module GHC.Exception.Type
  ( SomeException (..),
    ArithException (..),
    ErrorCall (..),
    throw,
    divZeroError,
  )
where

import GHC.Prim
import GHC.Types

newtype SomeException = SomeException e

-- | The errors of arithmetic.
data ArithException
  = Overflow
  | Underflow
  | LossOfPrecision
  | DivideByZero
  | Denormal
  | RatioZeroDenominator

-- | Raise an exception in pure code.
throw :: e -> a
throw e = primRaise e

divZeroError :: a
divZeroError = primRaise DivideByZero
