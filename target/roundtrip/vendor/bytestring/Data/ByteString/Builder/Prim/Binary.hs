module Data.ByteString.Builder.Prim.Binary (word8) where {
import Data.ByteString.Builder.Prim.Internal;
import Foreign;
word8 :: FixedPrim Word8;
word8 = fixedPrim 1 (flip poke)
}
