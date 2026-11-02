{-# LANGUAGE BangPatterns #-}
module Aihc.Cpp.Cursor (Cursor(..), fromByteString, toBytes, null, peekByte, peekByte2, advance, advance2, sliceBytes, sliceSince, skipWhile, skipToInteresting, bufLength, findNewline, skipNewline, lineSlice, startsWithByte, peekByteAt, atEnd) where {
import Data.ByteString (ByteString);
import qualified Data.ByteString as BS;
import Data.Word (Word8);
import Prelude hiding (null);
data Cursor = Cursor { curBuf :: !ByteString, curPos :: !Int } deriving (Show);
fromByteString :: ByteString -> Cursor;
fromByteString bs = Cursor bs 0;
toBytes :: Cursor -> ByteString;
toBytes (Cursor buf pos) = BS.drop pos buf;
null :: Cursor -> Bool;
null (Cursor buf pos) = pos >= BS.length buf;
atEnd :: Cursor -> Bool;
atEnd = null;
peekByte :: Cursor -> Maybe Word8;
peekByte (Cursor buf pos) | pos >= BS.length buf = Nothing | otherwise = Just (BS.index buf pos);
peekByte2 :: Cursor -> Maybe (Word8, Word8);
peekByte2 (Cursor buf pos) | pos + 1 >= BS.length buf = Nothing | otherwise = Just (BS.index buf pos, BS.index buf (pos + 1));
peekByteAt :: Int -> Cursor -> Maybe Word8;
peekByteAt absPos (Cursor buf _) | absPos < 0 || absPos >= BS.length buf = Nothing | otherwise = Just (BS.index buf absPos);
advance :: Cursor -> Cursor;
advance (Cursor buf pos) = Cursor buf (pos + 1);
advance2 :: Cursor -> Cursor;
advance2 (Cursor buf pos) = Cursor buf (pos + 2);
sliceBytes :: Int -> Int -> Cursor -> ByteString;
sliceBytes start end (Cursor buf _) = BS.take (end - start) (BS.drop start buf);
sliceSince :: Int -> Cursor -> ByteString;
sliceSince start cur = sliceBytes start (curPos cur) cur;
skipWhile :: (Word8 -> Bool) -> Cursor -> Cursor;
skipWhile p = go where {
  go !cur = case peekByte cur of {
  Just b | p b -> go (advance cur);
  _ -> cur}};
bufLength :: Cursor -> Int;
bufLength (Cursor buf _) = BS.length buf;
skipToInteresting :: Cursor -> Cursor;
skipToInteresting = go where {
  go !cur = case peekByte cur of {
  Nothing -> cur;
  Just b | isInteresting b -> cur | otherwise -> go (advance cur)};
  isInteresting :: Word8 -> Bool;
  isInteresting b = b == 0x22 || b == 0x27 || b == 0x2A || b == 0x2D || b == 0x2F || b == 0x5C || b == 0x7B || b == 0x7D};
findNewline :: Cursor -> Cursor;
findNewline (Cursor buf pos) = case BS.elemIndex 0x0A (BS.drop pos buf) of {
  Just offset -> Cursor buf (pos + offset);
  Nothing -> Cursor buf (BS.length buf)};
skipNewline :: Cursor -> Maybe Cursor;
skipNewline cur = case peekByte cur of {
  Just 0x0A -> Just (advance cur);
  _ -> Nothing};
lineSlice :: Int -> Cursor -> Cursor;
lineSlice end (Cursor buf start) = Cursor (BS.take (end - start) (BS.drop start buf)) 0;
startsWithByte :: Word8 -> Cursor -> Bool;
startsWithByte b cur = peekByte cur == Just b
}
