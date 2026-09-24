{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Aihc.Cpp.Evaluator
  ( expandMacros,
    expandMacrosMultiline,
    substituteParams,
    evalCondition,
    evalNumeric,
    Token (..),
    tokenize,
    parseExpr,
    parseOr,
    parseAnd,
    parseEq,
    parseRel,
    parseAdd,
    parseMul,
    parseUnary,
    parseAtom,
    replaceDefined,
    replaceRemainingWithZero,
  )
where

import Aihc.Cpp.Parser (isIdentChar, isIdentStart, isOpChar, isSpaceChar)
import Aihc.Cpp.Types (EngineState (..), MacroDef (..), bloomMember)
import Data.Bits ((.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Unsafe as BSU
import Data.Char (isDigit)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Word (Word8)

-- | Expand macros in a single piece of text using the blue-paint algorithm.
-- A single pass with a suppression set replaces the previous iterate-up-to-32
-- fixpoint approach.
expandMacros :: EngineState -> ByteString -> ByteString
expandMacros st = expandWith st S.empty

-- | Expand macros with multi-line support. When a function-like macro call
-- spans multiple lines, continuation lines are consumed from @moreLines@.
-- Returns the expanded text and the number of extra lines consumed.
expandMacrosMultiline :: EngineState -> ByteString -> [ByteString] -> (ByteString, Int)
expandMacrosMultiline st txt moreLines =
  let extraNeeded = countExtraLinesConsumed st txt moreLines
   in if extraNeeded == 0
        then (expandMacros st txt, 0)
        else
          let combinedLines = txt : take extraNeeded moreLines
              combined = C.intercalate "\n" combinedLines
              expanded = expandMacros st combined
           in (expanded, extraNeeded)

-- | Count how many extra lines a function macro call consumes.
-- Scans the first line for an identifier that matches a function macro,
-- then checks if parseCallArgs needs to span into continuation lines.
countExtraLinesConsumed :: EngineState -> ByteString -> [ByteString] -> Int
countExtraLinesConsumed st txt moreLines
  -- A module that defines no function-like macro cannot have a call
  -- spanning lines, and most do not; skipping the scan saves a second
  -- walk over every line of the file.
  | funBloom == 0 = 0
  | otherwise = scanForFunctionMacro False False False txt
  where
    macros = stMacros st
    funBloom = stFunMacroBloom st

    scanForFunctionMacro :: Bool -> Bool -> Bool -> ByteString -> Int
    scanForFunctionMacro _ _ _ t | C.null t = 0
    scanForFunctionMacro inString inChar escaped t =
      case C.uncons t of
        Nothing -> 0
        Just (c, rest)
          | inString ->
              let escaped' = c == '\\' && not escaped
                  inString' = not (c == '"' && not escaped)
               in scanForFunctionMacro inString' False escaped' rest
          | inChar ->
              let escaped' = c == '\\' && not escaped
                  inChar' = not (c == '\'' && not escaped)
               in scanForFunctionMacro False inChar' escaped' rest
          | startsHsBlockComment t ->
              let (_, remaining) = consumeHsBlockComment t
               in scanForFunctionMacro False False False remaining
          | c == '"' -> scanForFunctionMacro True False False rest
          | c == '\'' -> scanForFunctionMacro False True False rest
          | isIdentStart c ->
              let (ident, rest') = C.span isIdentChar t
               in if not (bloomMember funBloom (BS.head ident))
                    then scanForFunctionMacro False False False rest'
                    else case M.lookup ident macros of
                      Just (FunctionMacro _ _) ->
                        case tryMultilineCallArgs rest' of
                          Just n -> n
                          Nothing -> scanForFunctionMacro False False False rest'
                      _ -> scanForFunctionMacro False False False rest'
          | otherwise -> scanForFunctionMacro False False False rest

    -- Try to parse function call args, potentially spanning multiple lines.
    -- Returns Just n if the call spans n extra lines, Nothing if no call.
    tryMultilineCallArgs :: ByteString -> Maybe Int
    tryMultilineCallArgs rest = seekOpenParen (C.dropWhile isSpaceChar rest) 0

    seekOpenParen :: ByteString -> Int -> Maybe Int
    seekOpenParen remaining extraLines =
      case C.uncons remaining of
        Just ('(', afterOpen) ->
          findClosingParen 0 afterOpen extraLines
        Just _ ->
          Nothing
        Nothing ->
          case drop extraLines moreLines of
            [] -> Nothing
            (nextLine : _) ->
              seekOpenParen (C.dropWhile isSpaceChar nextLine) (extraLines + 1)

    findClosingParen :: Int -> ByteString -> Int -> Maybe Int
    findClosingParen = goClosing False False False
      where
        goClosing :: Bool -> Bool -> Bool -> Int -> ByteString -> Int -> Maybe Int
        goClosing inString inChar escaped depth remaining extraLines =
          case C.uncons remaining of
            Nothing ->
              -- Need more lines
              case drop extraLines moreLines of
                [] -> Nothing -- No more lines, unclosed call
                (nextLine : _) ->
                  goClosing inString inChar escaped depth (C.cons '\n' nextLine) (extraLines + 1)
            Just (ch, rest)
              | inString ->
                  let escaped' = ch == '\\' && not escaped
                      inString' = not (ch == '"' && not escaped)
                   in goClosing inString' False escaped' depth rest extraLines
              | inChar ->
                  let escaped' = ch == '\\' && not escaped
                      inChar' = not (ch == '\'' && not escaped)
                   in goClosing False inChar' escaped' depth rest extraLines
              | startsHsBlockComment remaining ->
                  let (_, afterComment) = consumeHsBlockComment remaining
                   in goClosing False False False depth afterComment extraLines
              | ch == '"' -> goClosing True False False depth rest extraLines
              | ch == '\'' -> goClosing False True False depth rest extraLines
              | ch == '(' -> goClosing False False False (depth + 1) rest extraLines
              | ch == ')' && depth > 0 -> goClosing False False False (depth - 1) rest extraLines
              | ch == ')' -> Just extraLines
              | otherwise -> goClosing False False False depth rest extraLines

-- | Blue-paint macro expansion: expand @txt@, leaving any name in
-- @painted@ alone so that a macro cannot re-enter itself. A single pass
-- with a suppression set replaces the previous iterate-up-to-32 fixpoint
-- approach.
--
-- The scan walks byte offsets and copies nothing until a macro actually
-- expands, so text that names no macro comes back as the very
-- 'ByteString' that went in. That is the case that matters: this runs on
-- every line of every module, and almost no line expands anything.
expandWith :: EngineState -> Set ByteString -> ByteString -> ByteString
expandWith st painted txt0 =
  case scan txt0 0 0 False False False mempty False of
    (_, False) -> txt0
    (acc, True) -> builderToBytes acc
  where
    macros = stMacros st
    bloom = stMacroBloom st

    -- \| @scan buf i flushed inString inChar escaped acc changed@ walks
    -- @buf@ from offset @i@; everything before @flushed@ is already in
    -- @acc@. An expansion may continue in a different buffer (a
    -- function-like call whose argument list held a line comment is
    -- rewritten), so the buffer travels with the loop.
    scan :: ByteString -> Int -> Int -> Bool -> Bool -> Bool -> BSB.Builder -> Bool -> (BSB.Builder, Bool)
    scan !buf !i !flushed !inString !inChar !escaped acc !changed
      | i >= len = (acc <> slice buf flushed len, changed)
      -- Matched against a 'case' rather than bound in a @where@: a
      -- @where@ binding the end-of-input guard does not use is a thunk,
      -- and this loop runs once per byte of the corpus.
      | otherwise = case BS.index buf i of
          c
            | inString ->
                let escaped' = c == 0x5C && not escaped -- '\\'
                    inString' = escaped || c /= 0x22 -- '"'
                 in scan buf (i + 1) flushed inString' False escaped' acc changed
            | inChar ->
                let escaped' = c == 0x5C && not escaped -- '\\'
                    inChar' = escaped || c /= 0x27 -- '\''
                 in scan buf (i + 1) flushed False inChar' escaped' acc changed
            | c == 0x22 -> scan buf (i + 1) flushed True False False acc changed -- '"'
            | c == 0x27 -> scan buf (i + 1) flushed False True False acc changed -- '\''
            | isIdentStartByte c -> expandIdent buf i flushed acc changed
            -- A block comment can only open on '{', so the three-byte test
            -- is gated on that byte rather than run against every byte.
            | c == 0x7B && startsHsComment buf len i ->
                -- Copied through verbatim, so there is nothing to flush.
                scan buf (hsCommentEnd buf i) flushed False False False acc changed
            | c == 0x2D && i + 1 < len && BS.index buf (i + 1) == 0x2D ->
                -- Haskell line comment: the rest is not expanded.
                (acc <> slice buf flushed len, changed)
            | otherwise ->
                scan buf (skipDull buf len (i + 1)) flushed False False False acc changed
      where
        len = BS.length buf

    -- \| Handle the identifier starting at @i@.
    --
    -- Split in two so that the common case — an identifier that can name
    -- no macro — allocates nothing. Everything the rare path needs
    -- (@name@, the painted set, the continuations) would otherwise be a
    -- thunk built once per identifier in the corpus.
    expandIdent :: ByteString -> Int -> Int -> BSB.Builder -> Bool -> (BSB.Builder, Bool)
    expandIdent !buf !i !flushed acc !changed
      -- No macro name starts with this byte: much the commonest outcome,
      -- and it costs one bit test rather than a walk of the macro map.
      | not (bloomMember bloom (BS.index buf i)) =
          scan buf end flushed False False False acc changed
      | otherwise = expandNamed buf i end (substr buf i end) flushed acc changed
      where
        !end = identEnd buf i

    -- \| Handle an identifier whose first byte a macro name could share.
    expandNamed :: ByteString -> Int -> Int -> ByteString -> Int -> BSB.Builder -> Bool -> (BSB.Builder, Bool)
    expandNamed !buf !i !end !name !flushed acc !changed
      | S.member name painted = verbatim
      | name == "__LINE__" = replaceWith (BSB.string8 (show (stCurrentLine st)))
      | name == "__FILE__" = replaceWith (BSB.string8 (show (stCurrentFile st)))
      | otherwise =
          case M.lookup name macros of
            Just (ObjectMacro replacement) ->
              replaceWith
                ( BSB.byteString
                    (expandWith st painted' (normalizeObjectReplacement replacement))
                )
            Just (FunctionMacro params body) ->
              case parseCallArgs (BS.drop end buf) of
                Just (args, restAfter)
                  | length args == length params ->
                      -- Arguments are expanded in the caller's paint context,
                      -- before @name@ is painted, so a nested call to the
                      -- same macro inside an argument still expands.
                      let macroArgs = map (macroArg st painted) args
                          body' = substituteMacroArgs (M.fromList (zip params macroArgs)) body
                          expanded = expandWith st painted' body'
                       in scan
                            restAfter
                            0
                            0
                            False
                            False
                            False
                            (acc <> slice buf flushed i <> BSB.byteString expanded)
                            True
                _ -> verbatim
            Nothing -> verbatim
      where
        painted' = S.insert name painted
        verbatim = scan buf end flushed False False False acc changed
        replaceWith b =
          scan buf end end False False False (acc <> slice buf flushed i <> b) True

-- | The offset just past the identifier starting at @i@.
identEnd :: ByteString -> Int -> Int
identEnd buf = go
  where
    len = BS.length buf
    -- The @i < len@ test guards the read on the same line: this is the
    -- innermost loop of the scan and the bounds check doubled its cost.
    go !i
      | i < len && isIdentByte (BSU.unsafeIndex buf i) = go (i + 1)
      | otherwise = i

-- | Advance past bytes that can neither start an identifier nor open a
-- literal or a comment, so runs of whitespace, digits and punctuation are
-- stepped over without re-entering the guard chain per byte.
skipDull :: ByteString -> Int -> Int -> Int
skipDull buf len = go
  where
    -- The @i < len@ test guards the read on the same line: this is the
    -- innermost loop of the scan and the bounds check doubled its cost.
    go !i
      | i < len && isDullByte (BSU.unsafeIndex buf i) = go (i + 1)
      | otherwise = i

isDullByte :: Word8 -> Bool
isDullByte b =
  not (isIdentStartByte b)
    && b /= 0x22 -- '"'
    && b /= 0x27 -- '\''
    && b /= 0x7B -- '{'
    && b /= 0x2D -- '-'
{-# INLINE isDullByte #-}

-- | Given that @buf@ has @{@ at @i@, does a Haskell block comment open
-- there? @{-#@ is a pragma, not a comment.
startsHsComment :: ByteString -> Int -> Int -> Bool
startsHsComment buf len i =
  i + 1 < len
    && BS.index buf (i + 1) == 0x2D -- '-'
    && (i + 2 >= len || BS.index buf (i + 2) /= 0x23) -- '#'

-- | The offset just past the Haskell block comment opening at @i@, or the
-- end of the buffer if it is never closed.
hsCommentEnd :: ByteString -> Int -> Int
hsCommentEnd buf = go (0 :: Int)
  where
    len = BS.length buf
    go :: Int -> Int -> Int
    go !depth !i
      | i + 1 >= len = len
      | b2 == 0x2D && b1 == 0x7B = go (depth + 1) (i + 2) -- '{-'
      | b2 == 0x7D && b1 == 0x2D = if depth <= 1 then i + 2 else go (depth - 1) (i + 2) -- '-}'
      | otherwise = go depth (i + 1)
      where
        b1 = BS.index buf i
        b2 = BS.index buf (i + 1)

-- | Byte-level 'isIdentStart'. See 'Aihc.Cpp.Parser.isIdentStart' for why
-- every byte >= 0x80 qualifies.
isIdentStartByte :: Word8 -> Bool
isIdentStartByte b =
  b == 0x5F -- '_'
    || (b >= 0x41 && b <= 0x5A) -- 'A'-'Z'
    || (b >= 0x61 && b <= 0x7A) -- 'a'-'z'
    || b >= 0x80
{-# INLINE isIdentStartByte #-}

-- | Byte-level 'Aihc.Cpp.Parser.isIdentChar'.
isIdentByte :: Word8 -> Bool
isIdentByte b = isIdentStartByte b || (b >= 0x30 && b <= 0x39)
{-# INLINE isIdentByte #-}

-- | The bytes of @buf@ in @[from, to)@, as a zero-copy slice.
substr :: ByteString -> Int -> Int -> ByteString
substr buf from to = BS.take (to - from) (BS.drop from buf)
{-# INLINE substr #-}

slice :: ByteString -> Int -> Int -> BSB.Builder
slice buf from to
  | to <= from = mempty
  | otherwise = BSB.byteString (substr buf from to)
{-# INLINE slice #-}

-- | A function-like macro argument in both the forms the replacement list
-- can need: the raw spelling (used by @#@ and @##@, which see arguments
-- unexpanded) and the macro-expanded spelling (used everywhere else).
data MacroArg = MacroArg
  { macroArgRaw :: !ByteString,
    macroArgExpanded :: !ByteString
  }

-- | Build a 'MacroArg' by expanding the argument text in the paint context of
-- the call site.
macroArg :: EngineState -> Set ByteString -> ByteString -> MacroArg
macroArg st painted raw = MacroArg raw (expandWith st painted raw)

-- | Normalize comments inside object-like macro replacement text while
-- preserving string and char literals. cpphs replaces @/* ... */@ with spaces
-- matching the width of the comment body, but treats empty @/**/@ as a token
-- pasting hack with zero width.
normalizeObjectReplacement :: ByteString -> ByteString
normalizeObjectReplacement = C.dropWhileEnd isSpaceChar . go False False False mempty
  where
    go :: Bool -> Bool -> Bool -> BSB.Builder -> ByteString -> ByteString
    go _ _ _ acc txt | C.null txt = builderToBytes acc
    go inString inChar escaped acc txt =
      case C.uncons txt of
        Nothing -> builderToBytes acc
        Just (c, rest)
          | inString ->
              let escaped' = c == '\\' && not escaped
                  inString' = not (c == '"' && not escaped)
               in go inString' False escaped' (acc <> BSB.char8 c) rest
          | inChar ->
              let escaped' = c == '\\' && not escaped
                  inChar' = not (c == '\'' && not escaped)
               in go False inChar' escaped' (acc <> BSB.char8 c) rest
          | c == '"' -> go True False False (acc <> BSB.char8 c) rest
          | c == '\'' -> go False True False (acc <> BSB.char8 c) rest
          | "/*" `C.isPrefixOf` txt ->
              let (commentText, remaining) = consumeCBlockComment txt
                  replacement = commentReplacement commentText
               in go False False False (acc <> BSB.byteString replacement) remaining
          | otherwise ->
              go False False False (acc <> BSB.char8 c) rest

consumeCBlockComment :: ByteString -> (ByteString, ByteString)
consumeCBlockComment txt =
  let afterOpen = C.drop 2 txt
      (inside, suffix) = BS.breakSubstring "*/" afterOpen
   in if C.null suffix
        then (txt, "")
        else ("/*" <> inside <> "*/", C.drop 2 suffix)

commentReplacement :: ByteString -> ByteString
commentReplacement commentText
  | commentText == "/**/" = ""
  | otherwise = C.replicate (charWidth (commentBody commentText)) ' '

-- | Number of characters in a UTF-8 buffer, for column alignment: count
-- every byte that is not a UTF-8 continuation byte. On valid UTF-8 this
-- is the character count; on anything else it degrades gracefully instead
-- of failing, and on ASCII it is just the length.
charWidth :: ByteString -> Int
charWidth = BS.foldl' step 0
  where
    step !n b = if b .&. 0xC0 == 0x80 then n else n + 1

commentBody :: ByteString -> ByteString
commentBody commentText =
  if "/*" `C.isPrefixOf` commentText && "*/" `C.isSuffixOf` commentText
    then C.take (C.length commentText - 4) (C.drop 2 commentText)
    else C.drop 2 commentText

-- | Parse function-like macro call arguments.
parseCallArgs :: ByteString -> Maybe ([ByteString], ByteString)
parseCallArgs input = do
  ('(', rest) <- C.uncons (C.dropWhile isSpaceChar input)
  parseArgs False False False 0 [] mempty rest

parseArgs :: Bool -> Bool -> Bool -> Int -> [ByteString] -> BSB.Builder -> ByteString -> Maybe ([ByteString], ByteString)
parseArgs inString inChar escaped depth argsRev current remaining =
  case C.uncons remaining of
    Nothing -> Nothing
    Just (ch, rest)
      | inString ->
          let escaped' = ch == '\\' && not escaped
              inString' = not (ch == '"' && not escaped)
           in parseArgs inString' False escaped' depth argsRev (current <> BSB.char8 ch) rest
      | inChar ->
          let escaped' = ch == '\\' && not escaped
              inChar' = not (ch == '\'' && not escaped)
           in parseArgs False inChar' escaped' depth argsRev (current <> BSB.char8 ch) rest
      | startsHsBlockComment remaining ->
          let (commentText, afterComment) = consumeHsBlockComment remaining
           in parseArgs False False False depth argsRev (current <> BSB.byteString commentText) afterComment
      | ch == '"' ->
          parseArgs True False False depth argsRev (current <> BSB.char8 ch) rest
      | ch == '\'' ->
          parseArgs False True False depth argsRev (current <> BSB.char8 ch) rest
      | ch == '(' ->
          parseArgs False False False (depth + 1) argsRev (current <> BSB.char8 ch) rest
      | ch == ')' && depth > 0 ->
          parseArgs False False False (depth - 1) argsRev (current <> BSB.char8 ch) rest
      | ch == ')' && depth == 0 ->
          let arg = trimSpacesBytes (builderToBytes current)
              argsRev' =
                if C.null arg && null argsRev
                  then [""]
                  else arg : argsRev
           in Just (reverse argsRev', rest)
      | ch == ',' && depth == 0 ->
          let arg = trimSpacesBytes (builderToBytes current)
           in parseArgs False False False depth (arg : argsRev) mempty rest
      | ch == '-' && depth == 0,
        Just ('-', afterDash) <- C.uncons rest ->
          -- Haskell line comment inside arg list: close the arg, find ')' in comment
          let commentText = "--" <> afterDash
           in case findLastCloseParen commentText of
                Nothing -> Nothing
                Just (commentPrefix, afterClose) ->
                  let currentText = builderToBytes current
                      arg = trimSpacesBytes currentText
                      trailingWS = C.takeWhileEnd isSpaceChar currentText
                      argsRev' = if C.null arg && null argsRev then [""] else arg : argsRev
                   in Just (reverse argsRev', trailingWS <> commentPrefix <> afterClose)
      | otherwise ->
          parseArgs False False False depth argsRev (current <> BSB.char8 ch) rest

-- | Find the last ')' in text and split before it.
findLastCloseParen :: ByteString -> Maybe (ByteString, ByteString)
findLastCloseParen txt =
  case C.elemIndexEnd ')' txt of
    Nothing -> Nothing
    Just idx -> Just (C.take idx txt, C.drop (idx + 1) txt)

startsHsBlockComment :: ByteString -> Bool
startsHsBlockComment txt =
  case C.uncons txt of
    Just ('{', rest) ->
      case C.uncons rest of
        Just ('-', rest') ->
          case C.uncons rest' of
            Just ('#', _) -> False
            _ -> True
        _ -> False
    _ -> False

consumeHsBlockComment :: ByteString -> (ByteString, ByteString)
consumeHsBlockComment = go 0 mempty
  where
    go :: Int -> BSB.Builder -> ByteString -> (ByteString, ByteString)
    go depth acc txt =
      case C.uncons txt of
        Nothing -> (builderToBytes acc, "")
        Just (c, rest) ->
          case C.uncons rest of
            Just ('-', rest')
              | c == '{' ->
                  go (depth + 1) (acc <> BSB.byteString "{-") rest'
            Just ('}', rest')
              | c == '-' && depth <= 1 ->
                  (builderToBytes (acc <> BSB.byteString "-}"), rest')
            Just ('}', rest')
              | c == '-' ->
                  go (depth - 1) (acc <> BSB.byteString "-}") rest'
            _ ->
              go depth (acc <> BSB.char8 c) rest

data Piece
  = PieceWhitespace !ByteString
  | PiecePaste
  | PieceRaw !ByteString
  | PieceParam !ByteString

substituteParams :: Map ByteString ByteString -> ByteString -> ByteString
substituteParams subs = substituteMacroArgs (M.map (\arg -> MacroArg arg arg) subs)

-- | Builder-based parameter substitution. Replaces identifiers found
-- in the substitution map, respecting string and char literals.
--
-- Parameters render as their macro-expanded argument, except as operands of
-- @#@ and @##@, which use the raw argument text.
substituteMacroArgs :: Map ByteString MacroArg -> ByteString -> ByteString
substituteMacroArgs subs = renderPieces . collapseTokenPastes . collapseStringizing . tokenizeReplacementList
  where
    tokenizeReplacementList :: ByteString -> [Piece]
    tokenizeReplacementList txt =
      case C.uncons txt of
        Nothing -> []
        Just (c, rest)
          | isSpaceChar c ->
              let (spaces, remaining) = C.span isSpaceChar txt
               in PieceWhitespace spaces : tokenizeReplacementList remaining
          | c == '"' ->
              let (literal, remaining) = scanQuoted '"' txt
               in PieceRaw literal : tokenizeReplacementList remaining
          | c == '\'' ->
              let (literal, remaining) = scanQuoted '\'' txt
               in PieceRaw literal : tokenizeReplacementList remaining
          | "/*" `C.isPrefixOf` txt ->
              let (commentText, remaining) = consumeCBlockComment txt
                  piece = if commentText == "/**/" then PiecePaste else PieceWhitespace (commentReplacement commentText)
               in piece : tokenizeReplacementList remaining
          | "##" `C.isPrefixOf` txt ->
              PiecePaste : tokenizeReplacementList (C.drop 2 txt)
          | isIdentStart c ->
              let (ident, remaining) = C.span isIdentChar txt
                  piece = if M.member ident subs then PieceParam ident else PieceRaw ident
               in piece : tokenizeReplacementList remaining
          | otherwise ->
              PieceRaw (C.singleton c) : tokenizeReplacementList rest

    scanQuoted :: Char -> ByteString -> (ByteString, ByteString)
    scanQuoted quote = go False mempty
      where
        go escaped acc remaining =
          case C.uncons remaining of
            Nothing -> (builderToBytes acc, "")
            Just (c, rest)
              | c == quote && not escaped ->
                  (builderToBytes (acc <> BSB.char8 c), rest)
              | c == '\\' ->
                  go (not escaped) (acc <> BSB.char8 c) rest
              | otherwise ->
                  go False (acc <> BSB.char8 c) rest

    collapseStringizing :: [Piece] -> [Piece]
    collapseStringizing [] = []
    collapseStringizing (PieceRaw "#" : PieceParam name : rest) =
      PieceRaw (stringizeArgument (lookupParamRaw name)) : collapseStringizing rest
    collapseStringizing (PieceRaw "#" : rest) =
      PieceRaw "#" : collapseStringizing rest
    collapseStringizing (piece : rest) = piece : collapseStringizing rest

    -- The accumulator is held reversed: appending to the end of a list once
    -- per piece is quadratic, and a macro body expanded on every line of a
    -- module makes that the single hottest allocation in the preprocessor.
    -- Reversed, the piece to the left of a @##@ is just the head.
    collapseTokenPastes :: [Piece] -> [Piece]
    collapseTokenPastes = go []
      where
        go acc [] = reverse acc
        go acc (piece : rest) =
          case piece of
            PiecePaste ->
              let accNoSpace = dropWhile isWhitespacePiece acc
                  (leadingSpace, restAfterSpace) = span isWhitespacePiece rest
               in case (accNoSpace, restAfterSpace) of
                    (leftPiece : accInit, rightPiece : remaining) ->
                      go (PieceRaw (renderPieceRaw leftPiece <> renderPieceRaw rightPiece) : accInit) remaining
                    _ -> go (reverse leadingSpace <> (PieceRaw "##" : acc)) restAfterSpace
            _ -> go (piece : acc) rest

    isWhitespacePiece :: Piece -> Bool
    isWhitespacePiece (PieceWhitespace _) = True
    isWhitespacePiece _ = False

    lookupParamWith :: (MacroArg -> ByteString) -> ByteString -> ByteString
    lookupParamWith field name = maybe name field (M.lookup name subs)

    lookupParamRaw :: ByteString -> ByteString
    lookupParamRaw = lookupParamWith macroArgRaw

    renderPieces :: [Piece] -> ByteString
    renderPieces = C.concat . map renderPiece

    renderPiece :: Piece -> ByteString
    renderPiece = renderPieceWith macroArgExpanded

    -- \| Render an operand of @##@, which sees the raw argument text.
    renderPieceRaw :: Piece -> ByteString
    renderPieceRaw = renderPieceWith macroArgRaw

    renderPieceWith :: (MacroArg -> ByteString) -> Piece -> ByteString
    renderPieceWith field piece =
      case piece of
        PieceWhitespace txt -> txt
        PiecePaste -> "##"
        PieceRaw txt -> txt
        PieceParam name -> lookupParamWith field name

    stringizeArgument :: ByteString -> ByteString
    stringizeArgument arg =
      let normalized = normalizeWhitespace arg
          escaped = C.concatMap escapeStringChar normalized
       in C.cons '"' (C.snoc escaped '"')

    -- Not 'C.words'/'C.unwords': those treat byte 0xA0 as whitespace and
    -- would split a multi-byte character down the middle.
    normalizeWhitespace :: ByteString -> ByteString
    normalizeWhitespace =
      C.intercalate " " . filter (not . C.null) . C.splitWith isSpaceChar

    escapeStringChar :: Char -> ByteString
    escapeStringChar '"' = "\\\""
    escapeStringChar '\\' = "\\\\"
    escapeStringChar c = C.singleton c

evalCondition :: EngineState -> ByteString -> Bool
evalCondition st expr = eval expr /= 0
  where
    macros = stMacros st
    eval = evalNumeric . replaceRemainingWithZero . expandMacros st . replaceDefined macros

evalNumeric :: ByteString -> Integer
evalNumeric input =
  let tokens = tokenize input
   in case parseExpr tokens of
        (val, _) -> val

data Token = TOp ByteString | TNum Integer | TIdent ByteString | TOpenParen | TCloseParen deriving (Show)

tokenize :: ByteString -> [Token]
tokenize input =
  case C.uncons input of
    Nothing -> []
    Just (c, rest)
      | isSpaceChar c ->
          tokenize (C.dropWhile isSpaceChar rest)
      | isDigit c ->
          let (num, remaining) = C.span isDigit input
           in case C.readInteger num of
                Just (value, _) -> TNum value : tokenize remaining
                Nothing -> tokenize remaining
      | isIdentStart c ->
          let (ident, remaining) = C.span isIdentChar input
           in TIdent ident : tokenize remaining
      | c == '(' ->
          TOpenParen : tokenize rest
      | c == ')' ->
          TCloseParen : tokenize rest
      | otherwise ->
          let (op, remaining) = C.span isOpChar input
           in if C.null op
                then tokenize rest
                else TOp op : tokenize remaining

parseExpr :: [Token] -> (Integer, [Token])
parseExpr = parseOr

binary :: ([Token] -> (Integer, [Token])) -> [ByteString] -> [Token] -> (Integer, [Token])
binary next ops ts =
  let (v1, ts1) = next ts
   in go v1 ts1
  where
    go v1 (TOp op : ts2)
      | op `elem` ops =
          let (v2, ts3) = next ts2
           in go (apply op v1 v2) ts3
    go v1 ts2 = (v1, ts2)

    apply "||" a b = if a /= 0 || b /= 0 then 1 else 0
    apply "&&" a b = if a /= 0 && b /= 0 then 1 else 0
    apply "==" a b = if a == b then 1 else 0
    apply "!=" a b = if a /= b then 1 else 0
    apply "<" a b = if a < b then 1 else 0
    apply ">" a b = if a > b then 1 else 0
    apply "<=" a b = if a <= b then 1 else 0
    apply ">=" a b = if a >= b then 1 else 0
    apply "+" a b = a + b
    apply "-" a b = a - b
    apply "*" a b = a * b
    apply "/" a b = if b == 0 then 0 else a `div` b
    apply "%" a b = if b == 0 then 0 else a `mod` b
    apply _ a _ = a

parseOr, parseAnd, parseEq, parseRel, parseAdd, parseMul :: [Token] -> (Integer, [Token])
parseOr = binary parseAnd ["||"]
parseAnd = binary parseEq ["&&"]
parseEq = binary parseRel ["==", "!="]
parseRel = binary parseAdd ["<", ">", "<=", ">="]
parseAdd = binary parseMul ["+", "-"]
parseMul = binary parseUnary ["*", "/", "%"]

parseUnary :: [Token] -> (Integer, [Token])
parseUnary (TOp "!" : ts) = let (v, ts') = parseUnary ts in (if v == 0 then 1 else 0, ts')
parseUnary (TOp "-" : ts) = let (v, ts') = parseUnary ts in (-v, ts')
parseUnary ts = parseAtom ts

parseAtom :: [Token] -> (Integer, [Token])
parseAtom (TNum n : ts) = (n, ts)
parseAtom (TIdent _ : ts) = (0, ts)
parseAtom (TOpenParen : ts) =
  let (v, ts1) = parseExpr ts
   in case ts1 of
        TCloseParen : ts2 -> (v, ts2)
        _ -> (v, ts1)
parseAtom ts = (0, ts)

replaceDefined :: Map ByteString MacroDef -> ByteString -> ByteString
replaceDefined macros = go
  where
    go txt =
      case C.uncons txt of
        Nothing -> ""
        Just (c, rest)
          | "defined" `C.isPrefixOf` txt && not (nextCharIsIdent (C.drop 7 txt)) ->
              expandDefined (C.dropWhile isSpaceChar (C.drop 7 txt))
          | otherwise ->
              C.cons c (go rest)

    expandDefined rest =
      case C.uncons rest of
        Just ('(', restAfterOpen) ->
          let rest' = C.dropWhile isSpaceChar restAfterOpen
              (name, restAfterName0) = C.span isIdentChar rest'
              restAfterName = C.dropWhile isSpaceChar restAfterName0
           in case C.uncons restAfterName of
                Just (')', restAfterClose) ->
                  boolLiteral (M.member name macros) <> go restAfterClose
                _ ->
                  boolLiteral False <> go restAfterName
        _ ->
          let (name, restAfterName) = C.span isIdentChar rest
           in if C.null name
                then boolLiteral False <> go rest
                else boolLiteral (M.member name macros) <> go restAfterName

    boolLiteral True = " 1 "
    boolLiteral False = " 0 "

    nextCharIsIdent remaining =
      case C.uncons remaining of
        Just (c, _) -> isIdentChar c
        Nothing -> False

replaceRemainingWithZero :: ByteString -> ByteString
replaceRemainingWithZero = go
  where
    go txt =
      case C.uncons txt of
        Nothing -> ""
        Just (c, rest)
          | isIdentStart c ->
              let (_, remaining) = C.span isIdentChar txt
               in " 0 " <> go remaining
          | otherwise ->
              C.cons c (go rest)

builderToBytes :: BSB.Builder -> ByteString
builderToBytes = BSL.toStrict . BSB.toLazyByteString

trimSpacesBytes :: ByteString -> ByteString
trimSpacesBytes = C.dropWhileEnd isSpaceChar . C.dropWhile isSpaceChar
