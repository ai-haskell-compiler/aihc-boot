module Data.ByteString.Char8 (ByteString, singleton, pack, unpack, cons, snoc, uncons, concatMap, all, replicate, takeWhile, takeWhileEnd, dropWhile, dropWhileEnd, span, break, split, splitWith, elemIndexEnd, null, length, concat, intercalate, isPrefixOf, isSuffixOf, take, drop, readInt, readInteger) where {
import Prelude hiding (reverse, head, tail, last, init, Foldable(..), map, lines, unlines, concat, any, take, drop, splitAt, takeWhile, dropWhile, span, break, filter, unwords, words, all, concatMap, scanl, scanl1, scanr, scanr1, appendFile, readFile, writeFile, replicate, getContents, getLine, putStr, putStrLn, interact, zip, zipWith, unzip, notElem);
import qualified Data.ByteString as B;
import qualified Data.ByteString.Internal as B;
import Data.ByteString (null, length, concat, take, drop, intercalate, isPrefixOf, isSuffixOf);
import Data.ByteString.Internal.Type;
import Data.ByteString.ReadInt;
import Data.ByteString.ReadNat;
singleton :: Char -> ByteString;
singleton = B.singleton . c2w;
pack :: String -> ByteString;
pack = packChars;
unpack :: ByteString -> [Char];
unpack = B.unpackChars;
cons :: Char -> ByteString -> ByteString;
cons = B.cons . c2w;
snoc :: ByteString -> Char -> ByteString;
snoc p = B.snoc p . c2w;
uncons :: ByteString -> Maybe (Char, ByteString);
uncons bs = case B.uncons bs of {
  Nothing -> Nothing;
  Just (w, bs') -> Just (w2c w, bs')};
concatMap :: (Char -> ByteString) -> ByteString -> ByteString;
concatMap f = B.concatMap (f . w2c);
all :: (Char -> Bool) -> ByteString -> Bool;
all f = B.all (f . w2c);
replicate :: Int -> Char -> ByteString;
replicate n = B.replicate n . c2w;
takeWhile :: (Char -> Bool) -> ByteString -> ByteString;
takeWhile f = B.takeWhile (f . w2c);
takeWhileEnd :: (Char -> Bool) -> ByteString -> ByteString;
takeWhileEnd f = B.takeWhileEnd (f . w2c);
dropWhile :: (Char -> Bool) -> ByteString -> ByteString;
dropWhile f = B.dropWhile (f . w2c);
dropWhileEnd :: (Char -> Bool) -> ByteString -> ByteString;
dropWhileEnd f = B.dropWhileEnd (f . w2c);
break :: (Char -> Bool) -> ByteString -> (ByteString, ByteString);
break f = B.break (f . w2c);
span :: (Char -> Bool) -> ByteString -> (ByteString, ByteString);
span f = B.span (f . w2c);
split :: Char -> ByteString -> [ByteString];
split = B.split . c2w;
splitWith :: (Char -> Bool) -> ByteString -> [ByteString];
splitWith f = B.splitWith (f . w2c);
elemIndexEnd :: Char -> ByteString -> Maybe Int;
elemIndexEnd = B.elemIndexEnd . c2w
}
