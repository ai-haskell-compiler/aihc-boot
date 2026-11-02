-- Text: pack, append, show and equality.
import Data.Text (Text)
import qualified Data.Text.Internal as T

hello :: Text
hello = T.pack "hello"

main :: IO ()
main = do
  print hello
  print (hello <> T.pack ", world")
  print (hello == T.pack "hello", hello == T.pack "help")
  print (T.pack "caf\233 \955 \8364")
  print (mempty :: Text)
