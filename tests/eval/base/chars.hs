import Data.Char
main :: IO ()
main = do
  print (map generalCategory "aZ1 \x3bb\x2028!$")
  print (map isPrint "a\n\x3bb", isUpper '\x393', isAlpha '\x3bb', isSpace '\x2003')
