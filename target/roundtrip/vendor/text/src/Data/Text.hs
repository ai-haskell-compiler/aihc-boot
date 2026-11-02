{-# LANGUAGE BangPatterns #-}
module Data.Text (Text) where {
import Control.DeepSeq (NFData(rnf));
import Data.String (IsString(..));
import qualified Data.Text.Array as A;
import Data.Text.Internal (Text(..), append, empty, pack);
import Data.Text.Show ();
instance Eq Text where {
  Text arrA offA lenA == Text arrB offB lenB | lenA == lenB = A.equal arrA offA arrB offB lenA | otherwise = False};
instance Semigroup Text where {
  (<>) = append};
instance Monoid Text where {
  mempty = empty};
instance IsString Text where {
  fromString = pack};
instance NFData Text where {
  rnf !_ = ()}
}
