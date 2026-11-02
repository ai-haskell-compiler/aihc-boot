{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
module Data.Text.Internal (Text(..), safe, empty, append, pack) where {
import Control.Monad.ST (ST, runST);
import Data.Bits;
import Data.Text.Internal.Unsafe.Char (unsafeWrite, ord);
import qualified Data.Text.Array as A;
data Text = Text !A.Array !Int !Int;
empty :: Text;
empty = Text A.empty 0 0;
append :: Text -> Text -> Text;
append a@(Text arr1 off1 len1) b@(Text arr2 off2 len2) | len1 == 0 = b | len2 == 0 = a | len > 0 = Text (A.run x) 0 len | otherwise = error $ "Data.Text.append: size overflow" where {
  len = len1 + len2;
  x :: ST s (A.MArray s);
  x = do {
  arr <- A.new len;
  A.copyI len1 arr 0 arr1 off1;
  A.copyI len2 arr len1 arr2 off2;
  return arr}};
safe :: Char -> Char;
safe c | ord c .&. 0x1ff800 /= 0xd800 = c | otherwise = '\xfffd';
pack :: String -> Text;
pack [] = empty;
pack xs = runST $ do {
  let {
  dstLen = 64};
  dst <- A.new dstLen;
  outer dst dstLen 0 xs} where {
  outer :: forall s. A.MArray s -> Int -> Int -> String -> ST s Text;
  outer !dst !dstLen = inner where {
  inner !dstOff [] = do {
  A.shrinkM dst dstOff;
  arr <- A.unsafeFreeze dst;
  return (Text arr 0 dstOff)};
  inner !dstOff ccs@(c : cs) | dstOff + 4 > dstLen = do {
  let {
  !dstLen' = dstLen * 2};
  dst' <- A.resizeM dst dstLen';
  outer dst' dstLen' dstOff ccs} | otherwise = do {
  d <- unsafeWrite dst dstOff (safe c);
  inner (dstOff + d) cs}}}
}
