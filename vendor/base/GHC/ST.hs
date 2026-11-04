{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UnboxedTuples #-}

-- | The representation of the ST monad, as in GHC: a function on the
-- state token of the thread.
module GHC.ST
  ( ST (..),
    STRep,
    runST,
    stToIO,
    unsafeIOToST,
    unsafeSTToIO,
    unsafeInterleaveST,
    RealWorld,
  )
where

import GHC.Base
import GHC.Prim
import GHC.Types

type STRep s a = State# s -> (# State# s, a #)

newtype ST s a = ST (STRep s a)

unST :: ST s a -> STRep s a
unST (ST m) = m

instance Functor (ST s) where
  fmap f (ST m) = ST (\s -> case m s of (# s', a #) -> (# s', f a #))

instance Applicative (ST s) where
  pure x = ST (\s -> (# s, x #))
  mf <*> mx = mf >>= \f -> mx >>= \x -> pure (f x)
  liftA2 f ma mb = ma >>= \a -> mb >>= \b -> pure (f a b)
  ma *> mb = ma >>= \_ -> mb

instance Monad (ST s) where
  ST m >>= k = ST (\s -> case m s of (# s', a #) -> unST (k a) s')

instance (Semigroup a) => Semigroup (ST s a) where
  (<>) = liftA2 (<>)

instance (Monoid a) => Monoid (ST s a) where
  mempty = pure mempty

runST :: (forall s. ST s a) -> a
runST st = case unST st realWorld# of (# _, a #) -> a

stToIO :: ST RealWorld a -> IO a
stToIO (ST m) = IO m

unsafeIOToST :: IO a -> ST s a
unsafeIOToST (IO m) = ST (primUnsafeCoerce m)

unsafeSTToIO :: ST s a -> IO a
unsafeSTToIO (ST m) = IO (primUnsafeCoerce m)

-- | Run the action when its result is needed.
unsafeInterleaveST :: ST s a -> ST s a
unsafeInterleaveST (ST m) = ST (\s -> (# s, case m s of (# _, r #) -> r #))
