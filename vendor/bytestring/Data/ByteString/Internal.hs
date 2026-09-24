{-# OPTIONS_HADDOCK not-home #-}

-- |
-- Module      : Data.ByteString.Internal
-- Copyright   : (c) Don Stewart 2006-2008
--               (c) Duncan Coutts 2006-2012
-- License     : BSD-style
-- Maintainer  : dons00@gmail.com, duncan@community.haskell.org
-- Stability   : unstable
-- Portability : non-portable
--
-- A module containing semi-public 'ByteString' internals. This exposes the
-- 'ByteString' representation and low level construction functions. As such
-- all the functions in this module are unsafe. The API is also not stable.
--
-- Where possible application should instead use the functions from the normal
-- public interface modules, such as "Data.ByteString.Unsafe". Packages that
-- extend the ByteString system at a low level will need to use this module.
--
module Data.ByteString.Internal (
        -- | aihc-boot re-exports the whole internal module. Its export
        -- list holds only the names that the vendored tree uses.
        module Data.ByteString.Internal.Type
  ) where

import Data.ByteString.Internal.Type
