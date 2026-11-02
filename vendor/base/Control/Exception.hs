-- | Exceptions: raise, catch and clean up.
--
-- The boot base has no Exception class yet, so the types do not select
-- the exceptions: 'catch', 'handle' and 'try' get every exception. A
-- program that catches only the exceptions of the handler's type gets
-- the same result as with GHC. There are no asynchronous exceptions, so
-- 'mask_' does nothing.
module Control.Exception
  ( SomeException (..),
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
  )
where

import Data.Either
import GHC.Base
import GHC.Exception.Type
import GHC.Prim
import GHC.Types

-- | Raise an exception when the action runs.
throwIO :: e -> IO a
throwIO e = IO (\_ -> primRaise e)

-- | Run an action. When it raises an exception, run the handler on it.
catch :: IO a -> (e -> IO a) -> IO a
catch (IO m) h = IO (\w -> primCatch m (\e -> unIO (h e)) w)

handle :: (e -> IO a) -> IO a -> IO a
handle h a = catch a h

-- | Run an action. Return the exception that it raises, or its result.
try :: IO a -> IO (Either e a)
try a = catch (a >>= \v -> return (Right v)) (\e -> return (Left e))

-- | Evaluate a value to weak head normal form when the action runs.
evaluate :: a -> IO a
evaluate x = IO (\_ -> x `seq` IORes x)

-- | Run the second action when the first one raises an exception, then
-- raise the exception again.
onException :: IO a -> IO b -> IO a
onException io what = catch io (\e -> what >> throwIO e)

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
