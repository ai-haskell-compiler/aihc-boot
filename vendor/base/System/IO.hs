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
  )
where

import Data.Foldable
import GHC.Base
import GHC.Classes
import GHC.Err
import GHC.Num
import GHC.Prim
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
