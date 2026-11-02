module Data.Text.Encoding.Error (OnError, OnDecodeError, lenientDecode) where {
import Data.Word (Word8);
type OnError a b = String -> Maybe a -> Maybe b;
type OnDecodeError = OnError Word8 Char;
lenientDecode :: OnDecodeError;
lenientDecode _ _ = Just '\xfffd'
}
