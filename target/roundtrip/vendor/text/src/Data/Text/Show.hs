{-# LANGUAGE BangPatterns #-}
module Data.Text.Show (unpack) where {
import Data.Text.Internal (Text(..));
import Data.Text.Internal.Encoding.Utf8 ();
import Data.Text.Internal.Unsafe.Char ();
import Data.Text.Unsafe (Iter(..), iterArray);
unpack :: Text -> String;
unpack t = foldrText (:) [] t;
foldrText :: (Char -> b -> b) -> b -> Text -> b;
foldrText f z (Text arr off len) = go off where {
  go !i | i >= off + len = z | otherwise = let {
  !(Iter c l) = iterArray arr i} in f c (go (i + l))};
instance Show Text where {
  showsPrec p ps r = showsPrec p (unpack ps) r}
}
