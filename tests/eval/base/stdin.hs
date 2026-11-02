-- Reading the standard input: a line, then the rest.
main :: IO ()
main = do
  name <- getLine
  putStrLn ("Hello, " ++ name ++ "!")
  rest <- getContents
  let ls = lines rest
  print (length ls)
  print (sum (map read' ls))
  mapM_ (putStrLn . reverse) ls
  where
    read' :: String -> Int
    read' = foldl (\acc c -> acc * 10 + (fromEnum' c - 48)) 0
    fromEnum' c = case c of
      '0' -> 48
      '1' -> 49
      '2' -> 50
      '3' -> 51
      '4' -> 52
      '5' -> 53
      '6' -> 54
      '7' -> 55
      '8' -> 56
      '9' -> 57
      _ -> 0
