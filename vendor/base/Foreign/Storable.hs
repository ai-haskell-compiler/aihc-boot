-- | Values that the program reads from and writes to memory.
module Foreign.Storable (Storable (..)) where

import GHC.Base
import GHC.Classes
import GHC.Int
import GHC.Num
import GHC.Prim
import GHC.Ptr
import GHC.Real
import GHC.Types
import GHC.Word

class Storable a where
  sizeOf :: a -> Int
  alignment :: a -> Int
  peekElemOff :: Ptr a -> Int -> IO a
  pokeElemOff :: Ptr a -> Int -> a -> IO ()
  peekByteOff :: Ptr b -> Int -> IO a
  pokeByteOff :: Ptr b -> Int -> a -> IO ()
  peek :: Ptr a -> IO a
  poke :: Ptr a -> a -> IO ()
  peekElemOff p i = r
    where
      r = peekByteOff p (i * sizeOf (unIO' r))
  pokeElemOff p i x = pokeByteOff p (i * sizeOf x) x
  peekByteOff p off = peek (plusPtr p off)
  pokeByteOff p off = poke (plusPtr p off)
  peek p = peekElemOff p 0
  poke p = pokeElemOff p 0

-- | The type of the result of an action, for 'sizeOf'. It is never
-- evaluated.
unIO' :: IO a -> a
unIO' _ = primUnsafeCoerce ()

instance Storable Word8 where
  sizeOf _ = 1
  alignment _ = 1
  peekByteOff p off = fmap fromWord8 (primPeek8 (ptrToInt p) off)
  pokeByteOff p off x = primPoke8 (ptrToInt p) off (toWord8 x)

instance Storable Word16 where
  sizeOf _ = 2
  alignment _ = 2
  peekByteOff p off = fmap fromWord16 (primPeek16 (ptrToInt p) off)
  pokeByteOff p off x = primPoke16 (ptrToInt p) off (toWord16 x)

instance Storable Word32 where
  sizeOf _ = 4
  alignment _ = 4
  peekByteOff p off = fmap fromWord32 (primPeek32 (ptrToInt p) off)
  pokeByteOff p off x = primPoke32 (ptrToInt p) off (toWord32 x)

instance Storable Word64 where
  sizeOf _ = 8
  alignment _ = 8
  peekByteOff p off = fmap fromWord64 (primPeek64 (ptrToInt p) off)
  pokeByteOff p off x = primPoke64 (ptrToInt p) off (toWord64 x)

instance Storable Word where
  sizeOf _ = 8
  alignment _ = 8
  peekByteOff p off = fmap fromWord (primPeek64 (ptrToInt p) off)
  pokeByteOff p off x = primPoke64 (ptrToInt p) off (toWord x)

instance Storable Int8 where
  sizeOf _ = 1
  alignment _ = 1
  peekByteOff p off = fmap fromInt8 (primPeek8 (ptrToInt p) off)
  pokeByteOff p off x = primPoke8 (ptrToInt p) off (toInt8 x)

instance Storable Int16 where
  sizeOf _ = 2
  alignment _ = 2
  peekByteOff p off = fmap fromInt16 (primPeek16 (ptrToInt p) off)
  pokeByteOff p off x = primPoke16 (ptrToInt p) off (toInt16 x)

instance Storable Int32 where
  sizeOf _ = 4
  alignment _ = 4
  peekByteOff p off = fmap fromInt32 (primPeek32 (ptrToInt p) off)
  pokeByteOff p off x = primPoke32 (ptrToInt p) off (toInt32 x)

instance Storable Int64 where
  sizeOf _ = 8
  alignment _ = 8
  peekByteOff p off = fmap fromInt64 (primPeek64 (ptrToInt p) off)
  pokeByteOff p off x = primPoke64 (ptrToInt p) off (toInt64 x)

instance Storable Int where
  sizeOf _ = 8
  alignment _ = 8
  peekByteOff p off = primPeek64 (ptrToInt p) off
  pokeByteOff p off x = primPoke64 (ptrToInt p) off x

instance Storable Char where
  sizeOf _ = 4
  alignment _ = 4
  peekByteOff p off = fmap primChr (primPeek32 (ptrToInt p) off)
  pokeByteOff p off x = primPoke32 (ptrToInt p) off (primOrd x)

instance Storable (Ptr a) where
  sizeOf _ = 8
  alignment _ = 8
  peekByteOff p off = fmap intToPtr (primPeek64 (ptrToInt p) off)
  pokeByteOff p off x = primPoke64 (ptrToInt p) off (ptrToInt x)
