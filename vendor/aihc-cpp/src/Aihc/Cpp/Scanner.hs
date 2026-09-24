{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Aihc.Cpp.Scanner
  ( LineSpan (..),
    LineScan (..),
    scanLine,
    scanLineDepthOnly,
    expandLineBySpanMultiline,
  )
where

import Aihc.Cpp.Cursor
  ( Cursor (..),
    findNewline,
    null,
    skipNewline,
    sliceBytes,
  )
import Aihc.Cpp.Evaluator (expandMacros, expandMacrosMultiline)
import Aihc.Cpp.Types (EngineState)
import Data.Bits (bit, testBit, (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Unsafe as BSU
import Data.Word (Word64, Word8)
import Prelude hiding (null)

data LineSpan = LineSpan
  { lineSpanInBlockComment :: !Bool,
    lineSpanText :: !ByteString
  }

data LineScan = LineScan
  { lineScanSpans :: ![LineSpan],
    lineScanFinalHsDepth :: !Int,
    lineScanFinalCDepth :: !Int
  }

-- | Expand macros in a list of line spans (single-line, no lookahead).
expandLineBySpan :: EngineState -> [LineSpan] -> ByteString
expandLineBySpan st =
  C.concat . map expandSpan
  where
    expandSpan lineChunk
      | lineSpanInBlockComment lineChunk = lineSpanText lineChunk
      | otherwise = expandMacros st (lineSpanText lineChunk)

-- | Expand macros in a list of line spans with multi-line lookahead.
-- When a function macro call spans multiple lines, continuation lines
-- are consumed from the @futureCursor@ (positioned after the current line).
-- Returns (expanded text, number of extra lines consumed).
--
-- Multi-line expansion is only attempted for lines that consist entirely
-- of code spans (no inline comments). Mixed code/comment lines use
-- single-line expansion to preserve comment span positions.
expandLineBySpanMultiline :: EngineState -> [LineSpan] -> Cursor -> (ByteString, Int)
expandLineBySpanMultiline st spans futureCursor =
  let commentSpans = filter lineSpanInBlockComment spans
      hasLineComment = any (\s -> "--" `C.isPrefixOf` lineSpanText s) commentSpans
      hasCBlockComment = any (C.all (== ' ') . lineSpanText) commentSpans
      hasHsComment = case commentSpans of
        [] -> False
        _ -> not hasCBlockComment
   in if hasLineComment || hasHsComment
        then -- Haskell comments stay in the token stream, so expand the full line.
          let fullText = concatSpans spans
           in (expandMacros st fullText, 0)
        else
          if hasCBlockComment
            then -- C comments are stripped to spaces, so preserve per-span handling.
              (expandLineBySpan st spans, 0)
            else -- Pure code line: try multi-line expansion
              let codeText = concatSpans spans
                  futureCodeLines = cursorToLines futureCursor
               in expandMacrosMultiline st codeText futureCodeLines

-- | Join the text of a line's spans. A line with no comment on it is a
-- single span, and returning that slice unchanged keeps the common case
-- zero-copy; 'C.concat' would copy it.
concatSpans :: [LineSpan] -> ByteString
concatSpans [one] = lineSpanText one
concatSpans spans = C.concat (map lineSpanText spans)

-- | Extract lines from a cursor as a lazy list of byte slices.
-- Each line is the content up to the next newline (or EOF).
cursorToLines :: Cursor -> [ByteString]
cursorToLines !cur
  | null cur = []
  | otherwise =
      let eol = findNewline cur
          lineText = sliceBytes (curPos cur) (curPos eol) cur
       in lineText : maybe [] cursorToLines (skipNewline eol)

-- | Lightweight scan that only tracks block comment depth changes.
-- Does not build 'LineSpan' segments or track string/char literals.
-- Used for inactive conditional branches where only comment depth
-- tracking is needed (no macro expansion or span splitting).
--
-- Takes the text of one logical line.
scanLineDepthOnly :: Int -> Int -> ByteString -> (Int, Int)
scanLineDepthOnly hsDepth0 cDepth0 line = goDepth hsDepth0 cDepth0 0
  where
    len = BS.length line

    goDepth :: Int -> Int -> Int -> (Int, Int)
    goDepth !hsDepth !cDepth !i
      -- Fewer than two bytes left: no two-character sequence can start here.
      | i + 1 >= len = (hsDepth, cDepth)
      | cDepth > 0 =
          if b1 == 0x2A && b2 == 0x2F -- '*/'
            then goDepth hsDepth 0 (i + 2)
            else goDepth hsDepth cDepth (i + 1)
      | hsDepth > 0 && b1 == 0x2D && b2 == 0x7D -- '-}'
        =
          goDepth (hsDepth - 1) cDepth (i + 2)
      | b1 == 0x7B && b2 == 0x2D -- '{-'
        =
          if hsDepth == 0 && i + 2 < len && BS.index line (i + 2) == 0x23 -- '#'
            then -- {-# is a pragma, not a comment (only at depth 0; inside a
            -- comment it is an ordinary nested opener, balancing the -} of
            -- its closing #-})
              goDepth hsDepth cDepth (i + 1)
            else goDepth (hsDepth + 1) cDepth (i + 2)
      | hsDepth == 0 && b1 == 0x2F && b2 == 0x2A -- '/*'
        =
          goDepth hsDepth 1 (i + 2)
      | hsDepth == 0 && b1 == 0x2D && b2 == 0x2D -- '--' line comment
        =
          (hsDepth, cDepth)
      | otherwise = goDepth hsDepth cDepth (i + 1)
      where
        b1 = BS.index line i
        b2 = BS.index line (i + 1)

-- | Scan a line, tracking comment depths and splitting into spans that are
-- either inside or outside block comments.
--
-- Takes the text of one logical line, and walks it by byte offset: a
-- cursor-per-byte walk allocated a 'Cursor' for every byte of the input.
--
-- The scanner splits the line into 'LineSpan' segments. Each segment is
-- tagged with whether it is inside a block comment. Code spans (outside
-- comments) are zero-copy slices of the raw input. C89 comment
-- content is replaced with spaces to preserve column alignment.
scanLine :: Int -> Int -> ByteString -> LineScan
scanLine 0 0 line
  -- Overwhelmingly the common case: a line outside any block comment that
  -- contains no comment at all is one code span, and 'isPlainCodeLine'
  -- settles that with a loop over unboxed arguments. The general scanner
  -- below threads a span accumulator, which costs an allocation per byte.
  | not (BS.null line) && isPlainCodeLine line = LineScan [LineSpan False line] 0 0
scanLine hsDepth0 cDepth0 line =
  let (spans, finalHsDepth, finalCDepth) =
        go hsDepth0 cDepth0 False False False [] 0 (hsDepth0 > 0 || cDepth0 > 0) 0
   in LineScan
        { lineScanSpans = reverse spans,
          lineScanFinalHsDepth = finalHsDepth,
          lineScanFinalCDepth = finalCDepth
        }
  where
    len = BS.length line

    -- \| Emit a span from @start@ to @end@ if non-empty, prepending to @acc@.
    emit :: [LineSpan] -> Int -> Int -> Bool -> [LineSpan]
    emit acc start end inComment
      | start >= end = acc
      | otherwise = LineSpan inComment (BS.take (end - start) (BS.drop start line)) : acc
    {-# INLINE emit #-}

    go ::
      Int ->
      Int ->
      Bool ->
      Bool ->
      Bool ->
      [LineSpan] ->
      Int ->
      Bool ->
      Int ->
      ([LineSpan], Int, Int)
    go !hsDepth !cDepth !inString !inChar !escaped !acc !spanStart !spanInComment !i
      -- End of input: flush the accumulated span.
      | i >= len =
          (emit acc spanStart i spanInComment, hsDepth, cDepth)
      -- Only one byte left: no two-character sequence is possible.
      | i + 1 >= len =
          if cDepth > 0
            then -- In a C comment: flush what came before, emit a space.
              (LineSpan True " " : emit acc spanStart i spanInComment, hsDepth, cDepth)
            else -- Include this last byte in the accumulated span.
              (emit acc spanStart (i + 1) (hsDepth > 0), hsDepth, cDepth)
      -- === C block comment mode ===
      | cDepth > 0 =
          if b1 == 0x2A && b2 == 0x2F -- '*/'
            then
              go hsDepth 0 False False False (LineSpan True "  " : emit acc spanStart i spanInComment) (i + 2) False (i + 2)
            else
              go hsDepth cDepth False False False (LineSpan True " " : emit acc spanStart i spanInComment) (i + 1) True (i + 1)
      -- === Line comment: -- (outside strings and hs comments) ===
      | not inString && not inChar && hsDepth == 0 && b1 == 0x2D && b2 == 0x2D -- '--'
        =
          (LineSpan True (BS.drop i line) : emit acc spanStart i spanInComment, hsDepth, cDepth)
      -- === Inside string literal ===
      | inString =
          let escaped' = not escaped && b1 == 0x5C -- '\\'
              inString' = escaped || b1 /= 0x22 -- '"'
           in go hsDepth cDepth inString' False escaped' acc spanStart spanInComment (i + 1)
      -- === Inside char literal ===
      | inChar =
          let escaped' = not escaped && b1 == 0x5C -- '\\'
              inChar' = escaped || b1 /= 0x27 -- '\''
           in go hsDepth cDepth False inChar' escaped' acc spanStart spanInComment (i + 1)
      -- === Start of string literal ===
      | hsDepth == 0 && b1 == 0x22 -- '"'
        =
          go hsDepth cDepth True False False acc spanStart spanInComment (i + 1)
      -- === Start of char literal ===
      | hsDepth == 0 && b1 == 0x27 -- '\''
        =
          go hsDepth cDepth False True False acc spanStart spanInComment (i + 1)
      -- === End of Haskell block comment: -} ===
      | hsDepth > 0 && b1 == 0x2D && b2 == 0x7D -- '-}'
        =
          let hsDepth' = hsDepth - 1
              -- Flush everything up to and including -} as a comment span.
              acc' = emit acc spanStart (i + 2) True
           in go hsDepth' cDepth False False False acc' (i + 2) (hsDepth' > 0) (i + 2)
      -- === Start of Haskell block comment: {- (but not a top-level {-# pragma) ===
      | b1 == 0x7B && b2 == 0x2D -- '{-'
        =
          if hsDepth == 0 && i + 2 < len && BS.index line (i + 2) == 0x23 -- '#'
            then -- A pragma, not a block comment (only outside comments;
            -- nested, {-# opens one). Advance past '{' only.
              go hsDepth cDepth False False False acc spanStart spanInComment (i + 1)
            else -- Flush any text before {-, emit {- as a comment.
              go (hsDepth + 1) cDepth False False False (LineSpan True "{-" : emit acc spanStart i spanInComment) (i + 2) True (i + 2)
      -- === Start of C block comment: /* ===
      | hsDepth == 0 && b1 == 0x2F && b2 == 0x2A -- '/*'
        =
          go hsDepth 1 False False False (LineSpan True "  " : emit acc spanStart i spanInComment) (i + 2) True (i + 2)
      -- === Normal byte: bulk-skip bytes that can start nothing ===
      | otherwise =
          go hsDepth cDepth False False False acc spanStart spanInComment (skipDull (i + 1))
      where
        b1 = BS.index line i
        b2 = BS.index line (i + 1)

    -- \| Advance past bytes that cannot start any CPP-significant
    -- two-character sequence, so runs of plain text (identifiers,
    -- whitespace, operators, non-ASCII) are stepped over without
    -- per-byte dispatch.
    skipDull :: Int -> Int
    -- The @i < len@ test guards the read on the same line: this is the
    -- innermost loop of the scan and the bounds check doubled its cost.
    skipDull !i
      | i < len && not (isInteresting (BSU.unsafeIndex line i)) = skipDull (i + 1)
      | otherwise = i

-- | Would the full scan of this line produce exactly one code span and
-- leave both comment depths at zero? That is, does the line open no
-- comment of either kind and contain no @--@ outside a literal?
--
-- This runs the same string- and char-literal state machine as 'scanLine',
-- so the two always agree about whether a @--@ or a @{-@ is a comment. It
-- carries no accumulator, so it compiles to a loop over unboxed arguments
-- that allocates nothing.
isPlainCodeLine :: ByteString -> Bool
isPlainCodeLine line = go 0 False False False
  where
    len = BS.length line

    go :: Int -> Bool -> Bool -> Bool -> Bool
    go !i !inString !inChar !escaped
      -- Fewer than two bytes left: no two-character sequence can start.
      | i + 1 >= len = True
      | inString = go (i + 1) (escaped || b1 /= 0x22) False (not escaped && b1 == 0x5C)
      | inChar = go (i + 1) False (escaped || b1 /= 0x27) (not escaped && b1 == 0x5C)
      | b1 == 0x2D && b2 == 0x2D = False -- '--'
      | b1 == 0x22 = go (i + 1) True False False -- '"'
      | b1 == 0x27 = go (i + 1) False True False -- '\''
      | b1 == 0x7B && b2 == 0x2D -- '{-'
      -- A {-# pragma is not a comment; the scan resumes after the '{'.
        =
          i + 2 < len && BS.index line (i + 2) == 0x23 && go (i + 1) False False False
      | b1 == 0x2F && b2 == 0x2A = False -- '/*'
      | otherwise = go (skipPlainDull (i + 1)) False False False
      where
        b1 = BS.index line i
        b2 = BS.index line (i + 1)

    skipPlainDull :: Int -> Int
    -- The @i < len@ test guards the read on the same line: this is the
    -- innermost loop of the scan and the bounds check doubled its cost.
    skipPlainDull !i
      | i < len && not (isInteresting (BSU.unsafeIndex line i)) = skipPlainDull (i + 1)
      | otherwise = i

-- | Can this byte start a CPP-significant two-character sequence?
--
-- A bit test against a pair of masks rather than a chain of comparisons:
-- this runs on every byte of the input, and the eight-way chain it
-- replaces cost more than the rest of the scan.
--
-- The bytes are @"@ (0x22), @\'@ (0x27), @*@ (0x2A), @-@ (0x2D), @/@
-- (0x2F), @\\@ (0x5C), @{@ (0x7B) and @}@ (0x7D). All are ASCII, so any
-- byte >= 0x80 — a continuation byte of whatever the source encoding is —
-- is uninteresting by construction.
isInteresting :: Word8 -> Bool
isInteresting b
  | b < 64 = testBit interestingLow (fromIntegral b)
  | b < 128 = testBit interestingHigh (fromIntegral b - 64)
  | otherwise = False
{-# INLINE isInteresting #-}

interestingLow :: Word64
interestingLow = bit 0x22 .|. bit 0x27 .|. bit 0x2A .|. bit 0x2D .|. bit 0x2F

interestingHigh :: Word64
interestingHigh = bit (0x5C - 64) .|. bit (0x7B - 64) .|. bit (0x7D - 64)
