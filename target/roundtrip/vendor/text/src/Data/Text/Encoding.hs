module Data.Text.Encoding (decodeUtf8With) where {
import Data.ByteString (ByteString);
import Data.Text.Internal ();
import Data.Text.Encoding.Error (OnDecodeError);
import Data.Text.Internal (Text(..));
import Data.Text.Internal.Encoding;
import Data.Text.Internal.IsAscii ();
import Data.Text.Unsafe ();
import Data.Text.Show ();
decodeUtf8With :: OnDecodeError -> ByteString -> Text;
decodeUtf8With onErr = decodeUtf8With1 onErr invalidUtf8Msg;
invalidUtf8Msg :: String;
invalidUtf8Msg = "Data.Text.Encoding: Invalid UTF-8 stream"
}
