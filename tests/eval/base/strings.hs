-- Strings and characters.
import Data.Char (ord, chr, toUpper, toLower, isDigit, isSpace, isAlpha, digitToInt, intToDigit)
import Data.List (intercalate)

capitalize :: String -> String
capitalize [] = []
capitalize (c : cs) = toUpper c : map toLower cs

digits :: String -> [Int]
digits s = map digitToInt (filter isDigit s)

rot13 :: Char -> Char
rot13 c
  | geChar c 'a' && leChar c 'z' = rotate 'a' c
  | geChar c 'A' && leChar c 'Z' = rotate 'A' c
  | otherwise = c
  where
    rotate base ch = chr (ord base + (ord ch - ord base + 13) `mod` 26)

count :: Char -> String -> Int
count c = length . filter (eqChar c)

main :: IO ()
main = do
  putStrLn (capitalize "hELLO")
  print (sum (digits "a1b2c3"))
  putStrLn (map rot13 "Hello, World!")
  putStrLn (map rot13 (map rot13 "Hello, World!"))
  print (count 'l' "hello world")
  putStrLn (unwords (words "  the quick   brown fox "))
  print (length (lines "one\ntwo\n\nthree"))
  putStr (unlines ["a", "b"])
  putStrLn (intercalate ", " ["x", "y", "z"])
  print (ord 'A')
  putStrLn [chr 72, chr 105]
  putStrLn (show 12 ++ show (-3) ++ show 0)
  putStrLn (shows 1 (showString " and " (shows 2 "")))
  putStrLn (showParen True (showString "inner") "")
  print (if isSpace ' ' && isAlpha 'q' && not (isAlpha '1') then 1 else 0)
  putStrLn [intToDigit 7, intToDigit 11]
  putStrLn "tab\there \"quoted\" \\ backslash"
  putStrLn "unicode: \955 \8364 caf\233"
  print (length "\955\8364")
  print (ord (head "\955"))
  putStrLn (reverse "stressed")
  putStrLn (takeWhile (neChar ' ') "first second")
  print (length (replicate 3 'x' ++ "yz"))
  putStrLn (concatMap (\c -> [c, c]) "ab")
  print (if ltChar 'a' 'b' && gtChar 'b' 'a' then 1 else 0)
  putStrLn (if "abc" `eqStr` "abc" then "equal" else "different")
  where
    eqStr [] [] = True
    eqStr (x : xs) (y : ys) = eqChar x y && eqStr xs ys
    eqStr _ _ = False
