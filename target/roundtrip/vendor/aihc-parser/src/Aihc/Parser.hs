{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
module Aihc.Parser (parseModule, ParserConfig(..), defaultConfig, ParseResult(..), formatParseErrors, parseExpr, parseSignatureType, parseType, parsePattern, parseDecl) where {
import Aihc.Parser.Internal.Common (TokParser, drainParseErrors, eofTok);
import Aihc.Parser.Internal.Decl (declParser);
import Aihc.Parser.Internal.Errors (parseErrorBundleToSpannedText, parseErrorsToSpannedText);
import Aihc.Parser.Internal.Expr (exprParser);
import Aihc.Parser.Internal.Module (moduleParser);
import Aihc.Parser.Internal.Pattern (patternParser);
import Aihc.Parser.Internal.Type (typeParser, typeSignatureParser);
import Aihc.Parser.Pretty ();
import Aihc.Parser.Syntax (Decl, Expr, Extension, Module(..), Pattern, SourceSpan, Type, applyImpliedExtensions, sourceSpanEndCol, sourceSpanSourceName, sourceSpanStartCol, sourceSpanStartLine, sourceSpanStartOffset, pattern SourceSpan);
import Aihc.Parser.Types;
import Data.ByteString qualified as BS;
import Data.List qualified as List;
import Data.Text (Text);
import Data.Text qualified as T;
import Data.Text.Encoding qualified as TE;
import Data.Word (Word8);
import Prettyprinter (Doc, colon, defaultLayoutOptions, layoutPretty, pretty, vcat);
import Prettyprinter.Render.String (renderString);
defaultConfig :: ParserConfig;
defaultConfig = ParserConfig {parserSourceName = "<input>", parserExtensions = []};
parseExpr :: ParserConfig -> Text -> ParseResult Expr;
parseExpr = runEntry mkTokStream (exprParser <* eofTok);
parsePattern :: ParserConfig -> Text -> ParseResult Pattern;
parsePattern = runEntry mkTokStream (patternParser <* eofTok);
parseSignatureType :: ParserConfig -> Text -> ParseResult Type;
parseSignatureType = runEntry mkTokStream (typeSignatureParser <* eofTok);
parseType :: ParserConfig -> Text -> ParseResult Type;
parseType = runEntry mkTokStream (typeParser <* eofTok);
parseDecl :: ParserConfig -> Text -> ParseResult Decl;
parseDecl = runEntry mkTokStream (declParser <* eofTok);
parseModule :: ParserConfig -> Text -> ([(SourceSpan, Text)], Module);
parseModule cfg input = case runTokStreamParser parser sourceName (mkTokStreamModule sourceName exts input) of {
  Left bundle -> (parseErrorBundleToSpannedText sourceName errorStream bundle, Module {moduleAnns = [], moduleHead = Nothing, moduleLanguagePragmas = [], moduleImports = [], moduleDecls = []});
  Right (errs, modu) -> (parseErrorsToSpannedText sourceName errorStream errs, modu)} where {
  sourceName = parserSourceName cfg;
  exts = applyImpliedExtensions (parserExtensions cfg);
  errorStream = rebuildStream (\(name, es, src) -> mkTokStreamModule name es src) (sourceName, exts, input);
  parser = do {
  modu <- moduleParser;
  errs <- drainParseErrors;
  pure (errs, modu)}};
runEntry :: (FilePath -> [Extension] -> Text -> TokStream) -> TokParser a -> ParserConfig -> Text -> ParseResult a;
runEntry mkStream parser cfg input = case runTokStreamParser parser sourceName (mkStream sourceName exts input) of {
  Left bundle -> ParseErr (parseErrorBundleToSpannedText sourceName (rebuildStream (\(name, es, src) -> mkStream name es src) (sourceName, exts, input)) bundle);
  Right parsed -> ParseOk parsed} where {
  sourceName = parserSourceName cfg;
  exts = applyImpliedExtensions (parserExtensions cfg)};
formatParseErrors :: FilePath -> Maybe Text -> [(SourceSpan, Text)] -> String;
formatParseErrors sourceName mSource errs = let {
  opts = defaultLayoutOptions;
  blocks = map (\(srcSpan, msg) -> renderString (layoutPretty opts $ case (srcSpan, mSource) of {
  (ss, Just source) -> vcat [renderSourceReference source ss, pretty msg];
  _ -> vcat [pretty sourceName, pretty msg]})) errs} in List.intercalate "\n\n" blocks;
renderSourceReference :: Text -> SourceSpan -> Doc ann;
renderSourceReference source srcSpan = let {
  SourceSpan {sourceSpanSourceName = renderedOrigin, sourceSpanStartLine = lineNo, sourceSpanStartCol = colNo, sourceSpanEndCol = endCol, sourceSpanStartOffset} = srcSpan;
  srcLine = extractSourceLineByOffset source sourceSpanStartOffset;
  lineNoText = show lineNo;
  markerPrefix = replicate (length lineNoText) ' ' ++ " | ";
  markerStart = max 0 (colNo - 1);
  markerLen = max 1 (endCol - colNo);
  marker = replicate markerStart ' ' ++ replicate markerLen '^';
  header = pretty renderedOrigin <> colon <> pretty lineNo <> colon <> pretty colNo <> colon} in vcat [header, pretty (lineNoText ++ " | " ++ srcLine), pretty (markerPrefix ++ marker)];
extractSourceLineByOffset :: Text -> Int -> String;
extractSourceLineByOffset source offset = let {
  bytes = TE.encodeUtf8 source;
  len = BS.length bytes;
  anchor = max 0 (min len offset);
  lineStart = scanBackward bytes anchor;
  lineEnd = scanForward bytes anchor} in T.unpack (TE.decodeUtf8 (BS.take (lineEnd - lineStart) (BS.drop lineStart bytes)));
scanBackward :: BS.ByteString -> Int -> Int;
scanBackward bytes = go where {
  go idx | idx <= 0 = 0 | isLineBreak (BS.index bytes (idx - 1)) = idx | otherwise = go (idx - 1)};
scanForward :: BS.ByteString -> Int -> Int;
scanForward bytes = go where {
  len = BS.length bytes;
  go idx | idx >= len = len | isLineBreak (BS.index bytes idx) = idx | otherwise = go (idx + 1)};
isLineBreak :: Word8 -> Bool;
isLineBreak w = w == 10 || w == 13
}
