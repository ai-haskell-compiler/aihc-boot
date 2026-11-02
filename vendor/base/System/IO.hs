-- | Input and output on the standard streams.
module System.IO
  ( putChar,
    putStr,
    putStrLn,
    print,
    getChar,
    getLine,
    getContents,
    interact,
    hFlush,
    FilePath,
    readFile,
    writeFile,
    appendFile,
  )
where

import Data.Foldable
import GHC.Base
import GHC.Classes
import GHC.Err
import GHC.Num
import GHC.Prim
import GHC.Real
import GHC.Show
import GHC.Types

putChar :: Char -> IO ()
putChar = primPutChar

putStr :: String -> IO ()
putStr = mapM_ primPutChar

putStrLn :: String -> IO ()
putStrLn s = putStr s >> primPutChar '\n'

print :: (Show a) => a -> IO ()
print x = putStrLn (show x)

getChar :: IO Char
getChar = primGetChar >>= \c -> if c < 0 then error "Prelude.getChar: end of file" else return (primChr c)

-- | A line without its newline. At the end of the input, the line so far.
getLine :: IO String
getLine = primGetChar >>= \c ->
  if c < 0 || c == 10
    then return []
    else getLine >>= \cs -> return (primChr c : cs)

-- | The whole standard input. It is read at once: the boot base has no
-- lazy IO.
getContents :: IO String
getContents = primGetChar >>= \c ->
  if c < 0
    then return []
    else getContents >>= \cs -> return (primChr c : cs)

interact :: (String -> String) -> IO ()
interact f = getContents >>= \s -> putStr (f s)

-- | Flush the standard output. The handle argument is ignored: the boot
-- base has no handles.
hFlush :: a -> IO ()
hFlush _ = primFlush

type FilePath = String

-- | The contents of a file, decoded as UTF-8. The file is read at once.
readFile :: FilePath -> IO String
readFile path =
  primFileSize path >>= \n ->
    if n < 0
      then error ("openFile: does not exist (No such file or directory): " ++ path)
      else
        primMalloc n >>= \buf ->
          primReadFileInto path buf n >>= \got ->
            bytesFrom buf (got - 1) [] >>= \bytes -> return (decodeUtf8 bytes)

bytesFrom :: Int -> Int -> [Int] -> IO [Int]
bytesFrom buf i acc =
  if i < 0 then return acc else primPeek8 buf i >>= \b -> bytesFrom buf (i - 1) (b : acc)

writeFile :: FilePath -> String -> IO ()
writeFile path s = writeBytes path (encodeUtf8 s) 0

appendFile :: FilePath -> String -> IO ()
appendFile path s = writeBytes path (encodeUtf8 s) 1

writeBytes :: FilePath -> [Int] -> Int -> IO ()
writeBytes path bytes append =
  let n = length bytes
   in primMalloc n >>= \buf ->
        pokeAll buf 0 bytes >> primWriteFileFrom path buf n append >>= \r ->
          if r < 0 then error ("openFile: cannot write: " ++ path) else return ()

pokeAll :: Int -> Int -> [Int] -> IO ()
pokeAll _ _ [] = return ()
pokeAll buf i (b : bs) = primPoke8 buf i b >> pokeAll buf (i + 1) bs

decodeUtf8 :: [Int] -> String
decodeUtf8 [] = []
decodeUtf8 (b : bs)
  | b < 128 = primChr b : decodeUtf8 bs
  | b >= 192 && b < 224 = multi (b - 192) 1 bs
  | b >= 224 && b < 240 = multi (b - 224) 2 bs
  | b >= 240 && b < 248 = multi (b - 240) 3 bs
  | otherwise = '\xfffd' : decodeUtf8 bs
  where
    multi acc 0 rest = primChr acc : decodeUtf8 rest
    multi acc k (c : rest)
      | c >= 128 && c < 192 = multi (acc * 64 + (c - 128)) (k - 1) rest
    multi _ _ rest = '\xfffd' : decodeUtf8 rest

encodeUtf8 :: String -> [Int]
encodeUtf8 [] = []
encodeUtf8 (c : cs)
  | n < 128 = n : encodeUtf8 cs
  | n < 2048 = 192 + n `quot` 64 : 128 + n `rem` 64 : encodeUtf8 cs
  | n < 65536 = 224 + n `quot` 4096 : 128 + (n `quot` 64) `rem` 64 : 128 + n `rem` 64 : encodeUtf8 cs
  | otherwise =
      240 + n `quot` 262144 : 128 + (n `quot` 4096) `rem` 64 : 128 + (n `quot` 64) `rem` 64 : 128 + n `rem` 64 : encodeUtf8 cs
  where
    n = primOrd c
