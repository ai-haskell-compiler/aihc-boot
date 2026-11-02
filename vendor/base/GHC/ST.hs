{-# LANGUAGE RankNTypes #-}

-- | The representation of the ST monad.
module GHC.ST
  ( ST (..),
    runST,
    stToIO,
    unsafeIOToST,
    unsafeSTToIO,
    unsafeInterleaveST,
    RealWorld,
  )
where

import GHC.Base
import GHC.Types
import System.IO.Unsafe

-- | The state of the real world, for 'stToIO'.
data RealWorld

newtype ST s a = ST (IO a)

unST :: ST s a -> IO a
unST (ST m) = m

instance Functor (ST s) where
  fmap f (ST m) = ST (fmap f m)

instance Applicative (ST s) where
  pure x = ST (pure x)
  ST f <*> ST x = ST (f <*> x)
  liftA2 f (ST a) (ST b) = ST (liftA2 f a b)
  ST a *> ST b = ST (a *> b)

instance Monad (ST s) where
  ST m >>= k = ST (m >>= \x -> unST (k x))

instance (Semigroup a) => Semigroup (ST s a) where
  (<>) = liftA2 (<>)

instance (Monoid a) => Monoid (ST s a) where
  mempty = pure mempty

runST :: (forall s. ST s a) -> a
runST st = unsafePerformIO (unST st)

stToIO :: ST RealWorld a -> IO a
stToIO = unST

unsafeIOToST :: IO a -> ST s a
unsafeIOToST = ST

unsafeSTToIO :: ST s a -> IO a
unsafeSTToIO = unST

unsafeInterleaveST :: ST s a -> ST s a
unsafeInterleaveST (ST m) = ST (unsafeInterleaveIO m)
