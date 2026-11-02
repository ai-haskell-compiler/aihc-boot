module Data.Text.Unsafe (Iter(..), iterArray) where {
import Data.Text.Internal.Encoding.Utf8 (utf8LengthByLeader, chr2, chr3, chr4);
import Data.Text.Internal ();
import Data.Text.Internal.Unsafe ();
import Data.Text.Internal.Unsafe.Char (unsafeChr8);
import qualified Data.Text.Array as A;
data Iter = Iter !Char !Int deriving (Show);
iterArray :: A.Array -> Int -> Iter;
iterArray arr j = Iter chr l where {
  m0 = A.unsafeIndex arr j;
  m1 = A.unsafeIndex arr (j + 1);
  m2 = A.unsafeIndex arr (j + 2);
  m3 = A.unsafeIndex arr (j + 3);
  l = utf8LengthByLeader m0;
  chr = case l of {
  1 -> unsafeChr8 m0;
  2 -> chr2 m0 m1;
  3 -> chr3 m0 m1 m2;
  _ -> chr4 m0 m1 m2 m3}}
}
