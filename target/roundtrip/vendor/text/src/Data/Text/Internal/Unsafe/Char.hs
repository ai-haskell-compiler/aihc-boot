{-# LANGUAGE MagicHash #-}
module Data.Text.Internal.Unsafe.Char (ord, unsafeChr8, unsafeWrite) where {
import Control.Monad.ST (ST);
import Data.Text.Internal.Encoding.Utf8;
import GHC.Exts (Char(..), Int(..), chr#, ord#, word2Int#, word8ToWord#);
import GHC.Word (Word8(..));
import qualified Data.Text.Array as A;
ord :: Char -> Int;
ord (C# c#) = I# (ord# c#);
unsafeChr8 :: Word8 -> Char;
unsafeChr8 (W8# w#) = C# (chr# (word2Int# (word8ToWord# w#)));
unsafeWrite :: A.MArray s -> Int -> Char -> ST s Int;
unsafeWrite marr i c = case utf8Length c of {
  1 -> do {
  let {
  n0 = intToWord8 (ord c)};
  A.unsafeWrite marr i n0;
  return 1};
  2 -> do {
  let {
  (n0, n1) = ord2 c};
  A.unsafeWrite marr i n0;
  A.unsafeWrite marr (i + 1) n1;
  return 2};
  3 -> do {
  let {
  (n0, n1, n2) = ord3 c};
  A.unsafeWrite marr i n0;
  A.unsafeWrite marr (i + 1) n1;
  A.unsafeWrite marr (i + 2) n2;
  return 3};
  _ -> do {
  let {
  (n0, n1, n2, n3) = ord4 c};
  A.unsafeWrite marr i n0;
  A.unsafeWrite marr (i + 1) n1;
  A.unsafeWrite marr (i + 2) n2;
  A.unsafeWrite marr (i + 3) n3;
  return 4}};
intToWord8 :: Int -> Word8;
intToWord8 = fromIntegral
}
