{- | Copyright   : (c) 2010 Jasper Van der Jeugt
                   (c) 2010 - 2011 Simon Meier
License     : BSD3-style (see LICENSE)
Maintainer  : Simon Meier <iridcode@gmail.com>
Portability : GHC

'Builder's are used to efficiently construct sequences of bytes from
  smaller parts.

aihc-boot keeps only the parts that the vendored tree uses.
-}

module Data.ByteString.Builder
    (
      -- * The Builder type
      Builder

      -- * Executing Builders
    , toLazyByteString

      -- * Creating Builders

      -- ** Binary encodings
    , byteString

      -- ** Character encodings
      -- *** ASCII (Char8)
    , char8
    , string8
    ) where

import           Data.ByteString.Builder.Internal
import qualified Data.ByteString.Builder.Prim  as P

{-# INLINE char8 #-}
-- | Char8 encode a 'Char'.
char8 :: Char -> Builder
char8 = P.primFixed P.char8

{-# INLINE [1] string8 #-} -- phased to allow P.cstring rewrite
-- | Char8 encode a 'String'.
string8 :: String -> Builder
string8 = P.primMapListFixed P.char8

