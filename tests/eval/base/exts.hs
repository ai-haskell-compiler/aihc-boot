{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
-- GHC.Exts: boxes, primops on Int#, Word# and Char#, MutVar# and MVar#
-- through the IO constructor.
import GHC.Exts
import GHC.Types (IO (..))

plus :: Int -> Int -> Int
plus (I# a) (I# b) = I# (a +# b)

classify :: Int -> String
classify (I# n)
  | isTrue# (n <# 0#) = "negative"
  | isTrue# (n ==# 0#) = "zero"
  | True = "positive"

counter :: IO Int
counter = IO (\s -> case newMutVar# (0 :: Int) s of
  (# s1, mv #) -> case readMutVar# mv s1 of
    (# s2, v #) -> case writeMutVar# mv (v + 41) s2 of
      s3 -> case atomicModifyMutVar# mv (\x -> (x + 1, x)) s3 of
        (# s4, old #) -> case readMutVar# mv s4 of
          (# s5, new #) -> (# s5, old * 1000 + new #))

box :: IO (Int, Int)
box = IO (\s -> case newMVar# s of
  (# s1, m #) -> case isEmptyMVar# m s1 of
    (# s2, e1 #) -> case putMVar# m "full" s2 of
      s3 -> case isEmptyMVar# m s3 of
        (# s4, e2 #) -> case takeMVar# m s4 of
          (# s5, v #) -> (# s5, (I# e1 + I# e2 * 10, length v) #))

main :: IO ()
main = do
  print (plus 2 3)
  print (map classify [-5, 0, 7])
  print (C# (chr# (ord# 'a'# +# 1#)), I# (quotInt# 17# 5#), I# (remInt# 17# 5#))
  print (W# (plusWord# 3## 4##), isTrue# (gtWord# 3## 4##))
  counter >>= print
  box >>= print
  print (build (\c n -> c 'x' (c 'y' n)))
  print (toList (fromList [1, 2, 3] :: [Int]))
