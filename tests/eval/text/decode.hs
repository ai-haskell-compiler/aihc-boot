-- Text: decodeUtf8With on valid and invalid UTF-8.
import qualified Data.ByteString.Internal as BI
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)

main :: IO ()
main = do
  print (decodeUtf8With lenientDecode (BI.packChars "plain ascii"))
  print (decodeUtf8With lenientDecode (BI.packChars "h\xc3\xa9llo \xce\xbb \xe2\x82\xac"))
  print (decodeUtf8With lenientDecode (BI.packChars "bad \xff byte"))
  print (decodeUtf8With lenientDecode (BI.packChars "cut \xe2\x82"))
