{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Aihc.Cpp (preprocess, Config(..), defaultConfig, Step(..), Result(..), IncludeRequest(..), IncludeKind(..), Diagnostic(..), Severity(..)) where {
import Aihc.Cpp.Cursor (Cursor(..), atEnd, findNewline, fromByteString, peekByte, peekByteAt, skipNewline, skipWhile, sliceBytes);
import Aihc.Cpp.Evaluator (evalCondition);
import Aihc.Cpp.Parser (Directive(..), isSpaceChar, parseDirective);
import Aihc.Cpp.Scanner (expandLineBySpanMultiline, lineScanFinalCDepth, lineScanFinalHsDepth, lineScanSpans, scanLine, scanLineDepthOnly);
import Aihc.Cpp.Types (CondFrame(..), Config(..), Continuation, Diagnostic(..), EngineState(..), IncludeKind(..), IncludeRequest(..), LineContext(..), MacroDef(..), Result(..), Severity(..), Step(..), currentActive, defaultConfig, defineMacro, emptyState, mkFrame, setMacroTable, undefMacro);
import Data.ByteString (ByteString);
import qualified Data.ByteString as BS;
import qualified Data.ByteString.Builder as BSB;
import qualified Data.ByteString.Char8 as C;
import qualified Data.ByteString.Lazy as BSL;
import qualified Data.Map.Strict as M;
import Data.Maybe (fromMaybe);
import qualified Data.Set as S;
import Data.Text (Text);
import qualified Data.Text.Encoding as TE;
import qualified Data.Text.Encoding.Error as TEE;
import System.FilePath (takeDirectory, (</>));
preprocess :: Config -> ByteString -> Step;
preprocess cfg input = let {
  cursor = fromByteString input} in processFile (configInputFile cfg) cursor False [] 1 initialState finish where {
  initialState = let {
  st0 = emitLine (linePragma 1 (configInputFile cfg)) (emptyState (configInputFile cfg))} in setMacroTable (M.map ObjectMacro (configMacros cfg)) st0;
  finish st = let {
  out = BSL.toStrict (BSB.toLazyByteString (stOutput st));
  outWithTrailingNewline = if BS.null out then out else out <> "\n"} in Done Result {resultOutput = outWithTrailingNewline, resultDiagnostics = reverse (stDiagnosticsRev st)}};
nextLine :: Cursor -> (ByteString, Int, Cursor);
nextLine cur = let {
  eol = findNewline cur;
  lineStart = curPos cur;
  lineEnd = curPos eol;
  rest = fromMaybe eol (skipNewline eol)} in if lineEnd > lineStart && peekByteAt (lineEnd - 1) cur == Just 0x5C && isDirectiveLine cur lineEnd then joinContinuationLines cur lineStart lineEnd rest else let {
  lineText = sliceBytes lineStart lineEnd cur} in if hasGccStringContinuation emptyQuoteState lineText then joinStringContinuationLines cur lineStart lineEnd rest else (lineText, 1, rest);
isDirectiveLine :: Cursor -> Int -> Bool;
isDirectiveLine cur lineEnd = let {
  trimmed = skipWhile isSpaceOrTab cur} in curPos trimmed < lineEnd && peekByte trimmed == Just 0x23 where {
  isSpaceOrTab b = b == 0x20 || b == 0x09};
joinContinuationLines :: Cursor -> Int -> Int -> Cursor -> (ByteString, Int, Cursor);
joinContinuationLines origCur lineStart firstLineEnd firstRest = let {
  buf = curBuf origCur;
  firstSegment = BSB.byteString (sliceBS lineStart (firstLineEnd - 1) buf)} in go firstSegment 1 firstRest where {
  go !acc !spanCount !rest | atEnd rest = let {
  joined = BSL.toStrict (BSB.toLazyByteString acc)} in (joined, spanCount, rest) | otherwise = let {
  eol = findNewline rest;
  segStart = curPos rest;
  segEnd = curPos eol;
  rest' = fromMaybe eol (skipNewline eol)} in if segEnd > segStart && peekByteAt (segEnd - 1) origCur == Just 0x5C then let {
  segment = BSB.byteString (sliceBS segStart (segEnd - 1) (curBuf origCur))} in go (acc <> segment) (spanCount + 1) rest' else let {
  segment = BSB.byteString (sliceBS segStart segEnd (curBuf origCur));
  joined = BSL.toStrict (BSB.toLazyByteString (acc <> segment))} in (joined, spanCount + 1, rest')};
sliceBS :: Int -> Int -> ByteString -> ByteString;
sliceBS start end bs = BS.take (end - start) (BS.drop start bs);
data QuoteState = QuoteState { qsInString :: !Bool, qsInChar :: !Bool, qsEscaped :: !Bool };
emptyQuoteState :: QuoteState;
emptyQuoteState = QuoteState False False False;
hasGccStringContinuation :: QuoteState -> ByteString -> Bool;
hasGccStringContinuation st lineText = "\\\\" `C.isSuffixOf` lineText && qsInString (scanQuoteState st lineText);
joinStringContinuationLines :: Cursor -> Int -> Int -> Cursor -> (ByteString, Int, Cursor);
joinStringContinuationLines origCur lineStart firstLineEnd firstRest = let {
  buf = curBuf origCur;
  firstSegment = sliceBytes lineStart (firstLineEnd - 1) origCur;
  firstBytes = BSB.byteString (sliceBS lineStart (firstLineEnd - 1) buf)} in go firstBytes 1 firstRest (scanQuoteState emptyQuoteState firstSegment) where {
  go !acc !spanCount !rest !quoteState | atEnd rest = let {
  joined = BSL.toStrict (BSB.toLazyByteString acc)} in (joined, spanCount, rest) | otherwise = let {
  eol = findNewline rest;
  segStart = curPos rest;
  segEnd = curPos eol;
  rest' = fromMaybe eol (skipNewline eol);
  segmentText = sliceBytes segStart segEnd origCur} in if hasGccStringContinuation quoteState segmentText then let {
  segmentBytes = BSB.byteString (sliceBS segStart (segEnd - 1) (curBuf origCur));
  scannedText = sliceBytes segStart (segEnd - 1) origCur} in go (acc <> segmentBytes) (spanCount + 1) rest' (scanQuoteState quoteState scannedText) else let {
  segmentBytes = BSB.byteString (sliceBS segStart segEnd (curBuf origCur));
  joined = BSL.toStrict (BSB.toLazyByteString (acc <> segmentBytes))} in (joined, spanCount + 1, rest')};
scanQuoteState :: QuoteState -> ByteString -> QuoteState;
scanQuoteState = go where {
  go st txt = case C.uncons txt of {
  Nothing -> st;
  Just (c, rest) | qsInString st -> if qsEscaped st && isSpaceChar c then go st {qsEscaped = False} (dropStringGapClose rest) else go st {qsInString = qsEscaped st || c /= '"', qsEscaped = c == '\\' && not (qsEscaped st)} rest | qsInChar st -> go st {qsInChar = qsEscaped st || c /= '\'', qsEscaped = c == '\\' && not (qsEscaped st)} rest | c == '"' -> go (QuoteState True False False) rest | c == '\'' -> go (QuoteState False True False) rest | otherwise -> go st {qsEscaped = False} rest};
  dropStringGapClose txt = let {
  afterSpace = C.dropWhile isSpaceChar txt} in case C.uncons afterSpace of {
  Just ('\\', rest) -> rest;
  _ -> afterSpace}};
processFile :: FilePath -> Cursor -> Bool -> [CondFrame] -> Int -> EngineState -> Continuation -> Step;
processFile _ cursor trailingNl _ _ st k | atEnd cursor = if trailingNl then k (emitBlankLines 1 st) else k st;
processFile filePath cursor trailingNl stack !lineNo st k = let {
  (lineText, lineSpan, restCursor) = nextLine cursor;
  hasTrailingNl = trailingNl && not (atEnd restCursor) || (trailingNl && atEnd restCursor);
  startsInBlockComment = stHsBlockCommentDepth st > 0 || stCBlockCommentDepth st > 0;
  parsedDirective = if startsInBlockComment then Nothing else parseDirective lineText;
  isActive = currentActive stack;
  nextLineNo = lineNo + lineSpan} in if not isActive && not (stSkippingDanglingElse st) then let {
  (finalHs, finalC) = scanLineDepthOnly (stHsBlockCommentDepth st) (stCBlockCommentDepth st) lineText;
  advanceSt st' = st' {stCurrentLine = nextLineNo, stHsBlockCommentDepth = finalHs, stCBlockCommentDepth = finalC};
  continueInactive st' = processFile filePath restCursor hasTrailingNl stack nextLineNo (advanceSt st') k;
  continueInactiveWith stack' st' = processFile filePath restCursor hasTrailingNl stack' nextLineNo (advanceSt st') k} in case parsedDirective of {
  Nothing -> continueInactive (emitBlankLines lineSpan st);
  Just directive -> let {
  ctx = LineContext {lcFilePath = filePath, lcLineNo = lineNo, lcLineSpan = lineSpan, lcNextLineNo = nextLineNo, lcRestCursor = restCursor, lcStack = stack, lcContinue = continueInactive, lcContinueWith = continueInactiveWith, lcDone = k}} in handleDirective ctx st directive} else let {
  lineScan = scanLine (stHsBlockCommentDepth st) (stCBlockCommentDepth st) lineText;
  advanceLineState st' = st' {stCurrentLine = nextLineNo, stHsBlockCommentDepth = lineScanFinalHsDepth lineScan, stCBlockCommentDepth = lineScanFinalCDepth lineScan};
  continue st' = processFile filePath restCursor hasTrailingNl stack nextLineNo (advanceLineState st') k;
  continueWith stack' st' = processFile filePath restCursor hasTrailingNl stack' nextLineNo (advanceLineState st') k;
  ctx = LineContext {lcFilePath = filePath, lcLineNo = lineNo, lcLineSpan = lineSpan, lcNextLineNo = nextLineNo, lcRestCursor = restCursor, lcStack = stack, lcContinue = continue, lcContinueWith = continueWith, lcDone = k}} in if stSkippingDanglingElse st then recoverDanglingElse ctx parsedDirective st else case parsedDirective of {
  Nothing -> let {
  (expanded, extraConsumed) = expandLineBySpanMultiline st (lineScanSpans lineScan) restCursor} in if extraConsumed > 0 then let {
  (remainingCursor, totalExtraSpan) = skipNLines extraConsumed restCursor;
  (finalHs, finalC) = scanConsumedLines (lineScanFinalHsDepth lineScan) (lineScanFinalCDepth lineScan) restCursor extraConsumed;
  nextLineNo' = nextLineNo + totalExtraSpan;
  st' = (emitLine expanded st) {stCurrentLine = nextLineNo', stHsBlockCommentDepth = finalHs, stCBlockCommentDepth = finalC}} in processFile filePath remainingCursor hasTrailingNl stack nextLineNo' st' k else continue (emitLine expanded st);
  Just directive -> handleDirective ctx st directive};
skipNLines :: Int -> Cursor -> (Cursor, Int);
skipNLines 0 cur = (cur, 0);
skipNLines n cur = go 0 0 cur where {
  go !count !consumed !c | count >= n = (c, consumed) | atEnd c = (c, consumed) | otherwise = let {
  eol = findNewline c;
  rest = fromMaybe eol (skipNewline eol)} in go (count + 1) (consumed + 1) rest};
scanConsumedLines :: Int -> Int -> Cursor -> Int -> (Int, Int);
scanConsumedLines !hsDepth !cDepth _cur 0 = (hsDepth, cDepth);
scanConsumedLines !hsDepth !cDepth cur remaining | atEnd cur = (hsDepth, cDepth) | otherwise = let {
  eol = findNewline cur;
  lineText = sliceBytes (curPos cur) (curPos eol) cur;
  (hsDepth', cDepth') = scanLineDepthOnly hsDepth cDepth lineText;
  rest = fromMaybe eol (skipNewline eol)} in scanConsumedLines hsDepth' cDepth' rest (remaining - 1);
recoverDanglingElse :: LineContext -> Maybe Directive -> EngineState -> Step;
recoverDanglingElse ctx parsedDirective st = case parsedDirective of {
  Just DirEndIf -> lcContinue ctx (addDiag Warning "unmatched #endif" (lcFilePath ctx) (lcLineNo ctx) (st {stSkippingDanglingElse = False}));
  Just _ -> lcContinue ctx st;
  Nothing -> lcContinue ctx (emitDirectiveBlank ctx st)};
handleDirective :: LineContext -> EngineState -> Directive -> Step;
handleDirective ctx st directive = case directive of {
  DirDefineObject name value -> mutateMacrosWhenActive ctx st (defineMacro name (ObjectMacro value));
  DirDefineFunction name params body -> mutateMacrosWhenActive ctx st (defineMacro name (FunctionMacro params body));
  DirUndef name -> mutateMacrosWhenActive ctx st (undefMacro name);
  DirInclude kind includeTarget -> handleIncludeDirective ctx st kind includeTarget;
  DirIf expr -> pushConditionalFrame ctx st (evalCondition st expr);
  DirIfDef name -> pushConditionalFrame ctx st (M.member name (stMacros st));
  DirIfNDef name -> pushConditionalFrame ctx st (not (M.member name (stMacros st)));
  DirElif expr -> handleElifDirective ctx st expr;
  DirElse -> handleElseDirective ctx st;
  DirEndIf -> case lcStack ctx of {
  [] -> lcContinue ctx (addDiag Warning "unmatched #endif" (lcFilePath ctx) (lcLineNo ctx) st);
  _ : rest -> continueBlankWithStack ctx rest st};
  DirWarning msg -> addDiagnosticWhenActive ctx Warning (messageText msg) st;
  DirError msg -> if currentActive (lcStack ctx) then lcDone ctx (emitDirectiveBlank ctx (addDiag Error (messageText msg) (lcFilePath ctx) (lcLineNo ctx) st)) else continueBlank ctx st;
  DirLine n mPath -> handleLineDirective ctx st n mPath;
  DirPragmaOnce -> handlePragmaOnceDirective ctx st;
  DirUnsupported name -> addDiagnosticWhenActive ctx Warning ("unsupported directive: " <> messageText name) st};
emitDirectiveBlank :: LineContext -> EngineState -> EngineState;
emitDirectiveBlank ctx = emitBlankLines (lcLineSpan ctx);
continueBlank :: LineContext -> EngineState -> Step;
continueBlank ctx st = lcContinue ctx (emitDirectiveBlank ctx st);
continueBlankWithStack :: LineContext -> [CondFrame] -> EngineState -> Step;
continueBlankWithStack ctx stack st = lcContinueWith ctx stack (emitDirectiveBlank ctx st);
mutateMacrosWhenActive :: LineContext -> EngineState -> (EngineState -> EngineState) -> Step;
mutateMacrosWhenActive ctx st mutate = if currentActive (lcStack ctx) then continueBlank ctx (mutate st) else continueBlank ctx st;
addDiagnosticWhenActive :: LineContext -> Severity -> Text -> EngineState -> Step;
addDiagnosticWhenActive ctx severity message st = if currentActive (lcStack ctx) then continueBlank ctx (addDiag severity message (lcFilePath ctx) (lcLineNo ctx) st) else continueBlank ctx st;
handlePragmaOnceDirective :: LineContext -> EngineState -> Step;
handlePragmaOnceDirective ctx st = if currentActive (lcStack ctx) then continueBlank ctx (st {stPragmaOnceFiles = S.insert (lcFilePath ctx) (stPragmaOnceFiles st)}) else continueBlank ctx st;
pushConditionalFrame :: LineContext -> EngineState -> Bool -> Step;
pushConditionalFrame ctx st cond = let {
  frame = mkFrame (currentActive (lcStack ctx)) cond} in continueBlankWithStack ctx (frame : lcStack ctx) st;
handleElifDirective :: LineContext -> EngineState -> ByteString -> Step;
handleElifDirective ctx st expr = case lcStack ctx of {
  [] -> continueBlank ctx (addDiag Error "#elif without matching #if" (lcFilePath ctx) (lcLineNo ctx) st);
  f : rest -> if frameInElse f then continueBlank ctx (addDiag Error "#elif after #else" (lcFilePath ctx) (lcLineNo ctx) st) else let {
  anyTaken = frameConditionTrue f;
  newCond = not anyTaken && evalCondition st expr;
  f' = f {frameConditionTrue = anyTaken || newCond, frameCurrentActive = frameOuterActive f && newCond}} in continueBlankWithStack ctx (f' : rest) st};
handleElseDirective :: LineContext -> EngineState -> Step;
handleElseDirective ctx st = case lcStack ctx of {
  [] -> lcContinue ctx (st {stSkippingDanglingElse = True});
  f : rest -> if frameInElse f then continueBlank ctx (addDiag Error "duplicate #else in conditional block" (lcFilePath ctx) (lcLineNo ctx) st) else let {
  newCurrent = frameOuterActive f && not (frameConditionTrue f);
  f' = f {frameInElse = True, frameCurrentActive = newCurrent}} in continueBlankWithStack ctx (f' : rest) st};
handleIncludeDirective :: LineContext -> EngineState -> IncludeKind -> ByteString -> Step;
handleIncludeDirective ctx st kind includeTarget | not (currentActive (lcStack ctx)) = continueBlank ctx st | S.member includeFilePath (stPragmaOnceFiles st) = continueBlank ctx st | otherwise = NeedInclude includeReq nextStep where {
  includePathText = C.unpack includeTarget;
  includeFilePath = case kind of {
  IncludeLocal -> takeDirectory (lcFilePath ctx) </> includePathText;
  IncludeSystem -> includePathText};
  includeReq = IncludeRequest {includePath = includePathText, includeKind = kind, includeFrom = lcFilePath ctx, includeLine = lcLineNo ctx};
  nextStep Nothing = lcContinue ctx (addDiag Error ("missing include: " <> messageText includeTarget) (lcFilePath ctx) (lcLineNo ctx) st);
  nextStep (Just includeBytes) = let {
  includeCursor = fromByteString includeBytes;
  includeHasTrailingNl = not (BS.null includeBytes) && BS.last includeBytes == 0x0A;
  stWithIncludePragma = emitLine (linePragma 1 includeFilePath) (st {stCurrentFile = includeFilePath, stCurrentLine = 1});
  resumeParent stAfterInclude = processFile (lcFilePath ctx) (lcRestCursor ctx) False (lcStack ctx) (lcNextLineNo ctx) (emitLine (linePragma (lcNextLineNo ctx) (lcFilePath ctx)) (stAfterInclude {stCurrentFile = lcFilePath ctx, stCurrentLine = lcNextLineNo ctx})) (lcDone ctx)} in processFile includeFilePath includeCursor includeHasTrailingNl [] 1 stWithIncludePragma resumeParent};
handleLineDirective :: LineContext -> EngineState -> Int -> Maybe FilePath -> Step;
handleLineDirective ctx st lineNumber maybePath | not (currentActive (lcStack ctx)) = continueBlank ctx st | otherwise = let {
  stWithFile = case maybePath of {
  Just path -> st {stCurrentFile = path};
  Nothing -> st};
  stWithLinePragma = emitLine (linePragma lineNumber (stCurrentFile stWithFile)) (stWithFile {stCurrentLine = lineNumber})} in processFile (lcFilePath ctx) (lcRestCursor ctx) False (lcStack ctx) lineNumber (stWithLinePragma {stCurrentLine = lineNumber}) (lcDone ctx);
emitLine :: ByteString -> EngineState -> EngineState;
emitLine line st = let {
  sep = if stOutputLineCount st > 0 then BSB.char8 '\n' else mempty} in st {stOutput = stOutput st <> sep <> BSB.byteString line, stOutputLineCount = stOutputLineCount st + 1};
emitBlankLines :: Int -> EngineState -> EngineState;
emitBlankLines n st | n <= 0 = st | otherwise = let {
  newlines = mconcat (replicate n (BSB.char8 '\n'))} in st {stOutput = stOutput st <> newlines, stOutputLineCount = stOutputLineCount st + n};
messageText :: ByteString -> Text;
messageText = TE.decodeUtf8With TEE.lenientDecode;
addDiag :: Severity -> Text -> FilePath -> Int -> EngineState -> EngineState;
addDiag sev msg filePath lineNo st = st {stDiagnosticsRev = Diagnostic {diagSeverity = sev, diagMessage = msg, diagFile = filePath, diagLine = lineNo} : stDiagnosticsRev st};
linePragma :: Int -> FilePath -> ByteString;
linePragma n path = "#line " <> C.pack (show n) <> " \"" <> C.pack path <> "\""
}
