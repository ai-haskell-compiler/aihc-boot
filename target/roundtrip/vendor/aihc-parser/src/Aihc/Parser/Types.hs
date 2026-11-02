{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
module Aihc.Parser.Types (TokStream(..), mkTokStream, mkTokStreamModule, mkTokStreamFromTokens, setTokStreamLayout, setTokStreamPendingPragmas, runTokStreamParser, ParserErrorComponent(..), FoundToken(..), mkFoundToken, ParseErrorBundle, ParseResult(..), rebuildStream, sourcePosSpan, ParserConfig(..)) where {
import Aihc.Parser.Lex (LayoutState(..), LexToken(..), LexTokenKind(..), Pragma, TokenOrigin(..), layoutTransition, mkInitialLayoutState, mkInitialLexerState, readModuleHeaderExtensions, scanAllTokens);
import Aihc.Parser.Syntax (Extension, ExtensionSet, SourceSpan, applyExtensionSetting, applyImpliedExtensions, mkExtensionSet, sourceSpanEndCol, sourceSpanEndLine, sourceSpanEndOffset, sourceSpanSourceName, sourceSpanStartCol, sourceSpanStartLine, sourceSpanStartOffset, pattern SourceSpan);
import Control.DeepSeq (NFData(..));
import Data.Text (Text);
import Data.Text qualified as T;
import GHC.Generics (Generic);
import Text.Megaparsec qualified as MP;
import Text.Megaparsec.Error qualified as MPE;
import Text.Megaparsec.Stream (Stream(..));
type ParseErrorBundle = MPE.ParseErrorBundle TokStream ParserErrorComponent;
data FoundToken = FoundToken { foundTokenText :: !Text, foundTokenKind :: !(Maybe LexTokenKind), foundTokenSpan :: !SourceSpan, foundTokenOrigin :: !TokenOrigin } deriving (Eq, Ord, Show, Generic, NFData);
data ParserErrorComponent = UnexpectedTokenExpecting { unexpectedFound :: Maybe FoundToken, unexpectedExpecting :: Text, unexpectedContext :: [Text] } deriving (Eq, Ord, Show, Generic);
instance MPE.ShowErrorComponent ParserErrorComponent where {
  showErrorComponent (UnexpectedTokenExpecting _ expecting _) = "expecting " <> T.unpack expecting};
mkFoundToken :: LexToken -> FoundToken;
mkFoundToken tok = FoundToken {foundTokenText = lexTokenText tok, foundTokenKind = Just (lexTokenKind tok), foundTokenSpan = lexTokenSpan tok, foundTokenOrigin = lexTokenOrigin tok};
data TokStream = TokStream { tokStreamRawTokens :: [LexToken], tokStreamLayoutState :: LayoutState, tokStreamBuffer :: [LexToken], tokStreamPendingPragmas :: [Pragma], tokStreamPrevToken :: Maybe LexToken, tokStreamExtensionSet :: ExtensionSet, tokStreamEOFEmitted :: !Bool, tokStreamNext :: Maybe (LexToken, TokStream) };
instance Show TokStream where {
  show ts = "TokStream { eofEmitted = " <> show (tokStreamEOFEmitted ts) <> ", pendingPragmas = " <> show (length (tokStreamPendingPragmas ts)) <> ", prevToken = " <> show (tokStreamPrevToken ts) <> ", extensionSet = " <> show (tokStreamExtensionSet ts) <> " }"};
instance NFData TokStream where {
  rnf ts = rnf (tokStreamPendingPragmas ts) `seq` rnf (tokStreamPrevToken ts) `seq` rnf (tokStreamExtensionSet ts) `seq` rnf (tokStreamEOFEmitted ts)};
mkTokStream :: FilePath -> [Extension] -> Text -> TokStream;
mkTokStream sourceName exts input = let {
  (env, lexSt) = mkInitialLexerState sourceName exts input} in normalizeTokStreamParts (scanAllTokens env lexSt) (mkInitialLayoutState False exts) [] [] Nothing (mkExtensionSet exts) False;
mkTokStreamModule :: FilePath -> [Extension] -> Text -> TokStream;
mkTokStreamModule sourceName baseExts input = let {
  (env, lexSt) = mkInitialLexerState sourceName effectiveExts input} in normalizeTokStreamParts (scanAllTokens env lexSt) (mkInitialLayoutState True effectiveExts) [] [] Nothing (mkExtensionSet effectiveExts) False where {
  headerSettings = readModuleHeaderExtensions input;
  effectiveExts = applyImpliedExtensions (foldr applyExtensionSetting baseExts headerSettings)};
mkTokStreamFromTokens :: [LexToken] -> TokStream;
mkTokStreamFromTokens toks = let {
  (env, lexSt) = mkInitialLexerState "<tokens>" [] ""} in normalizeTokStreamParts (scanAllTokens env lexSt) (mkInitialLayoutState False []) toks [] Nothing (mkExtensionSet []) False;
setTokStreamLayout :: LayoutState -> [LexToken] -> TokStream -> TokStream;
setTokStreamLayout layoutState buffer ts = buildTokStream (tokStreamRawTokens ts) layoutState buffer (tokStreamPendingPragmas ts) (tokStreamPrevToken ts) (tokStreamExtensionSet ts) (tokStreamEOFEmitted ts);
setTokStreamPendingPragmas :: [Pragma] -> TokStream -> TokStream;
setTokStreamPendingPragmas pendingPragmas ts = buildTokStream (tokStreamRawTokens ts) (tokStreamLayoutState ts) (tokStreamBuffer ts) pendingPragmas (tokStreamPrevToken ts) (tokStreamExtensionSet ts) (tokStreamEOFEmitted ts);
buildTokStream :: [LexToken] -> LayoutState -> [LexToken] -> [Pragma] -> Maybe LexToken -> ExtensionSet -> Bool -> TokStream;
buildTokStream rawTokens layoutState buffer pendingPragmas prevToken extensionSet eofEmitted = let {
  ts = TokStream {tokStreamRawTokens = rawTokens, tokStreamLayoutState = layoutState, tokStreamBuffer = buffer, tokStreamPendingPragmas = pendingPragmas, tokStreamPrevToken = prevToken, tokStreamExtensionSet = extensionSet, tokStreamEOFEmitted = eofEmitted, tokStreamNext = stepOne ts}} in ts;
normalizeTokStreamParts :: [LexToken] -> LayoutState -> [LexToken] -> [Pragma] -> Maybe LexToken -> ExtensionSet -> Bool -> TokStream;
normalizeTokStreamParts rawTokens layoutState buffer pendingPragmas prevToken extensionSet eofEmitted | eofEmitted = finish rawTokens layoutState buffer pendingPragmas | otherwise = go rawTokens layoutState buffer pendingPragmas where {
  finish rawTokens' layoutState' buffer' pendingPragmas' = buildTokStream rawTokens' layoutState' buffer' pendingPragmas' prevToken extensionSet eofEmitted;
  go rawTokens' layoutState' buffer' pendingPragmas' = case buffer' of {
  tok : rest -> case lexTokenKind tok of {
  TkPragma pragma' -> go rawTokens' layoutState' rest (pendingPragmas' <> [pragma']);
  TkLineComment -> go rawTokens' layoutState' rest pendingPragmas';
  TkBlockComment -> go rawTokens' layoutState' rest pendingPragmas';
  _ -> finish rawTokens' layoutState' buffer' pendingPragmas'};
  [] -> case rawTokens' of {
  [] -> finish [] layoutState' [] pendingPragmas';
  rawTok : rawRest -> case layoutTransition layoutState' rawTok of {
  (allToks, laySt') -> go rawRest laySt' allToks pendingPragmas'}}}};
stepOne :: TokStream -> Maybe (LexToken, TokStream);
stepOne ts | tokStreamEOFEmitted ts = Nothing | otherwise = case tokStreamBuffer ts of {
  tok : rest -> let {
  kind = lexTokenKind tok;
  isEOF = case kind of {
  TkEOF -> True;
  _ -> False};
  pendingPragmas = case lexTokenOrigin tok of {
  InsertedLayout -> tokStreamPendingPragmas ts;
  FromSource | TkSpecialSemicolon <- kind -> tokStreamPendingPragmas ts | otherwise -> []};
  !next = normalizeTokStreamParts (tokStreamRawTokens ts) (tokStreamLayoutState ts) rest pendingPragmas (Just tok) (tokStreamExtensionSet ts) isEOF} in Just (tok, next);
  [] -> Nothing};
rebuildStream :: (a -> TokStream) -> a -> TokStream;
rebuildStream build = build;
sourcePosSpan :: MP.SourcePos -> SourceSpan;
sourcePosSpan pos = SourceSpan {sourceSpanSourceName = T.pack (MP.sourceName pos), sourceSpanStartLine = MP.unPos (MP.sourceLine pos), sourceSpanStartCol = MP.unPos (MP.sourceColumn pos), sourceSpanEndLine = MP.unPos (MP.sourceLine pos), sourceSpanEndCol = MP.unPos (MP.sourceColumn pos), sourceSpanStartOffset = 0, sourceSpanEndOffset = 0};
runTokStreamParser :: MP.Parsec ParserErrorComponent TokStream a -> FilePath -> TokStream -> Either ParseErrorBundle a;
runTokStreamParser parser sourceName ts = snd (MP.runParser' parser initialState) where {
  initialState = MP.State {MP.stateInput = ts, MP.stateOffset = 0, MP.statePosState = MP.PosState {MP.pstateInput = detachedStream, MP.pstateOffset = 0, MP.pstateSourcePos = MP.initialPos sourceName, MP.pstateTabWidth = MP.defaultTabWidth, MP.pstateLinePrefix = ""}, MP.stateParseErrors = []};
  detachedStream = buildTokStream [] (tokStreamLayoutState ts) [] [] Nothing (tokStreamExtensionSet ts) True};
instance Stream TokStream where {
  type Token TokStream = LexToken;
  type Tokens TokStream = [LexToken];
  tokenToChunk _ tok = [tok];
  tokensToChunk _ = id;
  chunkToTokens _ = id;
  chunkLength _ = length;
  chunkEmpty _ = null;
  take1_ = tokStreamNext;
  takeN_ n ts | n <= 0 = Just ([], ts) | otherwise = go n [] ts where {
  go 0 acc s = Just (reverse acc, s);
  go k acc s = case tokStreamNext s of {
  Nothing | null acc -> Nothing | otherwise -> Just (reverse acc, s);
  Just (tok, s') -> go (k - 1) (tok : acc) s'}};
  takeWhile_ f = go [] where {
  go acc s = case tokStreamNext s of {
  Nothing -> (reverse acc, s);
  Just (tok, s') | f tok -> go (tok : acc) s' | otherwise -> (reverse acc, s)}}};
data ParserConfig = ParserConfig { parserSourceName :: FilePath, parserExtensions :: [Extension] } deriving (Eq, Show, Generic, NFData);
data ParseResult a = ParseOk a | ParseErr [(SourceSpan, Text)];
instance (NFData a) => NFData (ParseResult a) where {
  rnf parseResult = case parseResult of {
  ParseOk parsed -> rnf parsed;
  ParseErr errs -> rnf errs}}
}
