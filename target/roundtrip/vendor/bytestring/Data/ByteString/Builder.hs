module Data.ByteString.Builder (Builder, toLazyByteString, byteString, char8, string8) where {
import Data.ByteString.Builder.Internal;
import qualified Data.ByteString.Builder.Prim as P;
char8 :: Char -> Builder;
char8 = P.primFixed P.char8;
string8 :: String -> Builder;
string8 = P.primMapListFixed P.char8
}
