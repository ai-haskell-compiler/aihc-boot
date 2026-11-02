{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Aihc.Cpp.Scanner (LineSpan(..), LineScan(..), scanLine, scanLineDepthOnly, expandLineBySpanMultiline) where {
import Aihc.Cpp.Cursor (Cursor(..), findNewline, null, skipNewline, sliceBytes);
import Aihc.Cpp.Evaluator (expandMacros, expandMacrosMultiline);
import Aihc.Cpp.Types (EngineState);
import Data.Bits (bit, testBit, (.|.));
import Data.ByteString (ByteString);
import qualified Data.ByteString as BS;
import qualified Data.ByteString.Char8 as C;
import qualified Data.ByteString.Unsafe as BSU;
import Data.Word (Word64, Word8);
import Prelude hiding (null);
data LineSpan = LineSpan { lineSpanInBlockComment :: !Bool, lineSpanText :: !ByteString };
data LineScan = LineScan { lineScanSpans :: ![LineSpan], lineScanFinalHsDepth :: !Int, lineScanFinalCDepth :: !Int };
expandLineBySpan :: EngineState -> [LineSpan] -> ByteString;
expandLineBySpan st = C.concat . map expandSpan where {
  expandSpan lineChunk | lineSpanInBlockComment lineChunk = lineSpanText lineChunk | otherwise = expandMacros st (lineSpanText lineChunk)};
expandLineBySpanMultiline :: EngineState -> [LineSpan] -> Cursor -> (ByteString, Int);
expandLineBySpanMultiline st spans futureCursor = let {
  commentSpans = filter lineSpanInBlockComment spans;
  hasLineComment = any (\s -> "--" `C.isPrefixOf` lineSpanText s) commentSpans;
  hasCBlockComment = any (C.all (== ' ') . lineSpanText) commentSpans;
  hasHsComment = case commentSpans of {
  [] -> False;
  _ -> not hasCBlockComment}} in if hasLineComment || hasHsComment then let {
  fullText = concatSpans spans} in (expandMacros st fullText, 0) else if hasCBlockComment then (expandLineBySpan st spans, 0) else let {
  codeText = concatSpans spans;
  futureCodeLines = cursorToLines futureCursor} in expandMacrosMultiline st codeText futureCodeLines;
concatSpans :: [LineSpan] -> ByteString;
concatSpans [one] = lineSpanText one;
concatSpans spans = C.concat (map lineSpanText spans);
cursorToLines :: Cursor -> [ByteString];
cursorToLines !cur | null cur = [] | otherwise = let {
  eol = findNewline cur;
  lineText = sliceBytes (curPos cur) (curPos eol) cur} in lineText : maybe [] cursorToLines (skipNewline eol);
scanLineDepthOnly :: Int -> Int -> ByteString -> (Int, Int);
scanLineDepthOnly hsDepth0 cDepth0 line = goDepth hsDepth0 cDepth0 0 where {
  len = BS.length line;
  goDepth :: Int -> Int -> Int -> (Int, Int);
  goDepth !hsDepth !cDepth !i | i + 1 >= len = (hsDepth, cDepth) | cDepth > 0 = if b1 == 0x2A && b2 == 0x2F then goDepth hsDepth 0 (i + 2) else goDepth hsDepth cDepth (i + 1) | hsDepth > 0 && b1 == 0x2D && b2 == 0x7D = goDepth (hsDepth - 1) cDepth (i + 2) | b1 == 0x7B && b2 == 0x2D = if hsDepth == 0 && i + 2 < len && BS.index line (i + 2) == 0x23 then goDepth hsDepth cDepth (i + 1) else goDepth (hsDepth + 1) cDepth (i + 2) | hsDepth == 0 && b1 == 0x2F && b2 == 0x2A = goDepth hsDepth 1 (i + 2) | hsDepth == 0 && b1 == 0x2D && b2 == 0x2D = (hsDepth, cDepth) | otherwise = goDepth hsDepth cDepth (i + 1) where {
  b1 = BS.index line i;
  b2 = BS.index line (i + 1)}};
scanLine :: Int -> Int -> ByteString -> LineScan;
scanLine 0 0 line | not (BS.null line) && isPlainCodeLine line = LineScan [LineSpan False line] 0 0;
scanLine hsDepth0 cDepth0 line = let {
  (spans, finalHsDepth, finalCDepth) = go hsDepth0 cDepth0 False False False [] 0 (hsDepth0 > 0 || cDepth0 > 0) 0} in LineScan {lineScanSpans = reverse spans, lineScanFinalHsDepth = finalHsDepth, lineScanFinalCDepth = finalCDepth} where {
  len = BS.length line;
  emit :: [LineSpan] -> Int -> Int -> Bool -> [LineSpan];
  emit acc start end inComment | start >= end = acc | otherwise = LineSpan inComment (BS.take (end - start) (BS.drop start line)) : acc;
  go :: Int -> Int -> Bool -> Bool -> Bool -> [LineSpan] -> Int -> Bool -> Int -> ([LineSpan], Int, Int);
  go !hsDepth !cDepth !inString !inChar !escaped !acc !spanStart !spanInComment !i | i >= len = (emit acc spanStart i spanInComment, hsDepth, cDepth) | i + 1 >= len = if cDepth > 0 then (LineSpan True " " : emit acc spanStart i spanInComment, hsDepth, cDepth) else (emit acc spanStart (i + 1) (hsDepth > 0), hsDepth, cDepth) | cDepth > 0 = if b1 == 0x2A && b2 == 0x2F then go hsDepth 0 False False False (LineSpan True "  " : emit acc spanStart i spanInComment) (i + 2) False (i + 2) else go hsDepth cDepth False False False (LineSpan True " " : emit acc spanStart i spanInComment) (i + 1) True (i + 1) | not inString && not inChar && hsDepth == 0 && b1 == 0x2D && b2 == 0x2D = (LineSpan True (BS.drop i line) : emit acc spanStart i spanInComment, hsDepth, cDepth) | inString = let {
  escaped' = not escaped && b1 == 0x5C;
  inString' = escaped || b1 /= 0x22} in go hsDepth cDepth inString' False escaped' acc spanStart spanInComment (i + 1) | inChar = let {
  escaped' = not escaped && b1 == 0x5C;
  inChar' = escaped || b1 /= 0x27} in go hsDepth cDepth False inChar' escaped' acc spanStart spanInComment (i + 1) | hsDepth == 0 && b1 == 0x22 = go hsDepth cDepth True False False acc spanStart spanInComment (i + 1) | hsDepth == 0 && b1 == 0x27 = go hsDepth cDepth False True False acc spanStart spanInComment (i + 1) | hsDepth > 0 && b1 == 0x2D && b2 == 0x7D = let {
  hsDepth' = hsDepth - 1;
  acc' = emit acc spanStart (i + 2) True} in go hsDepth' cDepth False False False acc' (i + 2) (hsDepth' > 0) (i + 2) | b1 == 0x7B && b2 == 0x2D = if hsDepth == 0 && i + 2 < len && BS.index line (i + 2) == 0x23 then go hsDepth cDepth False False False acc spanStart spanInComment (i + 1) else go (hsDepth + 1) cDepth False False False (LineSpan True "{-" : emit acc spanStart i spanInComment) (i + 2) True (i + 2) | hsDepth == 0 && b1 == 0x2F && b2 == 0x2A = go hsDepth 1 False False False (LineSpan True "  " : emit acc spanStart i spanInComment) (i + 2) True (i + 2) | otherwise = go hsDepth cDepth False False False acc spanStart spanInComment (skipDull (i + 1)) where {
  b1 = BS.index line i;
  b2 = BS.index line (i + 1)};
  skipDull :: Int -> Int;
  skipDull !i | i < len && not (isInteresting (BSU.unsafeIndex line i)) = skipDull (i + 1) | otherwise = i};
isPlainCodeLine :: ByteString -> Bool;
isPlainCodeLine line = go 0 False False False where {
  len = BS.length line;
  go :: Int -> Bool -> Bool -> Bool -> Bool;
  go !i !inString !inChar !escaped | i + 1 >= len = True | inString = go (i + 1) (escaped || b1 /= 0x22) False (not escaped && b1 == 0x5C) | inChar = go (i + 1) False (escaped || b1 /= 0x27) (not escaped && b1 == 0x5C) | b1 == 0x2D && b2 == 0x2D = False | b1 == 0x22 = go (i + 1) True False False | b1 == 0x27 = go (i + 1) False True False | b1 == 0x7B && b2 == 0x2D = i + 2 < len && BS.index line (i + 2) == 0x23 && go (i + 1) False False False | b1 == 0x2F && b2 == 0x2A = False | otherwise = go (skipPlainDull (i + 1)) False False False where {
  b1 = BS.index line i;
  b2 = BS.index line (i + 1)};
  skipPlainDull :: Int -> Int;
  skipPlainDull !i | i < len && not (isInteresting (BSU.unsafeIndex line i)) = skipPlainDull (i + 1) | otherwise = i};
isInteresting :: Word8 -> Bool;
isInteresting b | b < 64 = testBit interestingLow (fromIntegral b) | b < 128 = testBit interestingHigh (fromIntegral b - 64) | otherwise = False;
interestingLow :: Word64;
interestingLow = bit 0x22 .|. bit 0x27 .|. bit 0x2A .|. bit 0x2D .|. bit 0x2F;
interestingHigh :: Word64;
interestingHigh = bit (0x5C - 64) .|. bit (0x7B - 64) .|. bit (0x7D - 64)
}
