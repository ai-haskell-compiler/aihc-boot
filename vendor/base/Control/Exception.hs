{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

-- | Exceptions: raise, catch and clean up.
--
-- A handler gets the exceptions of its type: 'catch' selects them with
-- 'fromException'. There are no asynchronous exceptions, so 'mask_'
-- does nothing.
module Control.Exception
  ( Exception (..),
    SomeException (..),
    ArithException (..),
    ErrorCall (..),
    throw,
    throwIO,
    catch,
    handle,
    try,
    evaluate,
    bracket,
    bracket_,
    finally,
    onException,
    mask_,
    assert,
  )
where

import Data.Either
import GHC.Base
import GHC.Exception
import GHC.Exception.Type
import GHC.Maybe
import GHC.Prim
import GHC.Types

-- | Raise an exception when the action runs.
throwIO :: (Exception e) => e -> IO a
throwIO e = IO (\_ -> primRaise (toException e))

-- | Run an action. When it raises an exception of the handler's type,
-- run the handler on it.
catch :: (Exception e) => IO a -> (e -> IO a) -> IO a
catch (IO m) h = IO (\w -> primCatch m handler w)
  where
    handler se = case fromException se of
      Just e -> unIO (h e)
      Nothing -> primRaise se

handle :: (Exception e) => (e -> IO a) -> IO a -> IO a
handle h a = catch a h

-- | Run an action. Return the exception that it raises, or its result.
try :: (Exception e) => IO a -> IO (Either e a)
try a = catch (a >>= \v -> return (Right v)) (\e -> return (Left e))

-- | Evaluate a value to weak head normal form when the action runs.
evaluate :: a -> IO a
evaluate x = IO (\s -> x `seq` (# s, x #))

-- | Run the second action when the first one raises an exception, then
-- raise the exception again.
onException :: IO a -> IO b -> IO a
onException io what = catch io (\e -> what >> throwIO (e :: SomeException))

-- | Acquire a resource, use it and release it. The release also runs
-- when the use raises an exception.
bracket :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c
bracket before after thing =
  before >>= \a ->
    onException (thing a) (after a) >>= \r ->
      after a >> return r

bracket_ :: IO a -> IO b -> IO c -> IO c
bracket_ before after thing = bracket before (const after) (const thing)

-- | Run the second action after the first one, also when the first one
-- raises an exception.
finally :: IO a -> IO b -> IO a
finally a sequel = onException a sequel >>= \r -> sequel >> return r

mask_ :: IO a -> IO a
mask_ a = a

-- | The boot base does not check assertions.
assert :: Bool -> a -> a
assert _ x = x
