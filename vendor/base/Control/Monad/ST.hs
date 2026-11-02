{-# LANGUAGE RankNTypes #-}

-- | The strict state thread monad. An ST action is an IO action that
-- 'runST' runs on its own.
module Control.Monad.ST
  ( ST,
    runST,
    stToIO,
    RealWorld,
  )
where

import GHC.ST
