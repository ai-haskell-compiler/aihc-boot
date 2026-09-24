{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}

module Aihc.Parser.Types
  ( TokStream (..),
    mkTokStream,
    mkTokStreamModule,
    mkTokStreamFromTokens,
    setTokStreamLayout,
    setTokStreamPendingPragmas,
    runTokStreamParser,
    ParserErrorComponent (..),
    FoundToken (..),
    mkFoundToken,
    ParseErrorBundle,
    ParseResult (..),
    rebuildStream,
    sourcePosSpan,
    ParserConfig (..),
  )
where

import Aihc.Parser.Lex
  ( LayoutState (..),
    LexToken (..),
    LexTokenKind (..),
    Pragma,
    TokenOrigin (..),
    layoutTransition,
    mkInitialLayoutState,
    mkInitialLexerState,
    readModuleHeaderExtensions,
    scanAllTokens,
  )
import Aihc.Parser.Syntax (Extension, ExtensionSet, SourceSpan, applyExtensionSetting, applyImpliedExtensions, mkExtensionSet, sourceSpanEndCol, sourceSpanEndLine, sourceSpanEndOffset, sourceSpanSourceName, sourceSpanStartCol, sourceSpanStartLine, sourceSpanStartOffset, pattern SourceSpan)
import Control.DeepSeq (NFData (..))
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Error qualified as MPE
import Text.Megaparsec.Stream (Stream (..))

-- | Raw Megaparsec parse error bundle for low-level parser entry points.
type ParseErrorBundle = MPE.ParseErrorBundle TokStream ParserErrorComponent

data FoundToken = FoundToken
  { foundTokenText :: !Text,
    foundTokenKind :: !(Maybe LexTokenKind),
    foundTokenSpan :: !SourceSpan,
    foundTokenOrigin :: !TokenOrigin
  }
  deriving (Eq, Ord, Show, Generic, NFData)

data ParserErrorComponent
  = UnexpectedTokenExpecting
  { unexpectedFound :: Maybe FoundToken,
    unexpectedExpecting :: Text,
    unexpectedContext :: [Text]
  }
  deriving (Eq, Ord, Show, Generic)

instance MPE.ShowErrorComponent ParserErrorComponent where
  showErrorComponent (UnexpectedTokenExpecting _ expecting _) = "expecting " <> T.unpack expecting

mkFoundToken :: LexToken -> FoundToken
mkFoundToken tok =
  FoundToken
    { foundTokenText = lexTokenText tok,
      foundTokenKind = Just (lexTokenKind tok),
      foundTokenSpan = lexTokenSpan tok,
      foundTokenOrigin = lexTokenOrigin tok
    }

-- | Token stream backed by a shared lazy raw token list and a layout overlay.
--
-- The raw token list (@tokStreamRawTokens@) is produced lazily by scanning the
-- input once. Backtracking never causes characters to be re-scanned — it only
-- restores a pointer into this shared list. The layout overlay
-- (@tokStreamLayoutState@) runs 'layoutTransition' on each raw token to produce
-- virtual @{@, @;@, @}@ tokens. This allows the parser to influence layout
-- decisions (e.g., by closing implicit layout contexts via
-- 'closeImplicitLayoutContext').
data TokStream = TokStream
  { -- | Shared lazy list of raw tokens (never re-scanned on backtrack).
    tokStreamRawTokens :: [LexToken],
    -- | Layout engine state (context stack and pending layout).
    tokStreamLayoutState :: LayoutState,
    -- | Tokens ready for the parser, including virtual layout tokens.
    tokStreamBuffer :: [LexToken],
    -- | Hidden pragmas that appeared before the next source token.
    -- Parsers may inspect these explicitly, but they never participate in the
    -- ordinary token stream.
    tokStreamPendingPragmas :: [Pragma],
    tokStreamPrevToken :: Maybe LexToken,
    tokStreamExtensionSet :: ExtensionSet,
    -- | Whether this stream has already emitted TkEOF.
    -- After EOF is emitted, 'take1_' returns Nothing.
    tokStreamEOFEmitted :: !Bool,
    -- | Memoized result of stepping this stream by one token. Lookahead and
    -- backtracking step the same position many times; sharing the successor
    -- means the layout transition for each raw token runs once.
    --
    -- Never update the other fields with record syntax: build streams with
    -- 'buildTokStream' so this field stays consistent.
    tokStreamNext :: Maybe (LexToken, TokStream)
  }

-- -- Manual Eq instance — we skip tokStreamRawTokens since list position is not
-- -- efficiently comparable and Megaparsec tracks offset separately.
-- instance Eq TokStream where
--   a == b =
--     tokStreamLayoutState a == tokStreamLayoutState b
--       && tokStreamPendingPragmas a == tokStreamPendingPragmas b
--       && tokStreamPrevToken a == tokStreamPrevToken b
--       && tokStreamExtensions a == tokStreamExtensions b
--       && tokStreamEOFEmitted a == tokStreamEOFEmitted b

-- -- Manual Ord instance
-- instance Ord TokStream where
--   compare a b =
--     compare (tokStreamEOFEmitted a) (tokStreamEOFEmitted b)

-- Manual Show instance
instance Show TokStream where
  show ts =
    "TokStream { eofEmitted = "
      <> show (tokStreamEOFEmitted ts)
      <> ", pendingPragmas = "
      <> show (length (tokStreamPendingPragmas ts))
      <> ", prevToken = "
      <> show (tokStreamPrevToken ts)
      <> ", extensionSet = "
      <> show (tokStreamExtensionSet ts)
      <> " }"

-- Manual NFData instance — don't force the lazy raw token list.
instance NFData TokStream where
  rnf ts =
    rnf (tokStreamPendingPragmas ts) `seq`
      rnf (tokStreamPrevToken ts) `seq`
        rnf (tokStreamExtensionSet ts) `seq`
          rnf (tokStreamEOFEmitted ts)

-- | Create a TokStream for parsing expressions/declarations (no module layout).
mkTokStream :: FilePath -> [Extension] -> Text -> TokStream
mkTokStream sourceName exts input =
  let (env, lexSt) = mkInitialLexerState sourceName exts input
   in normalizeTokStreamParts
        (scanAllTokens env lexSt)
        (mkInitialLayoutState False exts)
        []
        []
        Nothing
        (mkExtensionSet exts)
        False

-- | Create a TokStream for parsing full modules (with module-body layout).
-- Also bootstraps LANGUAGE pragma extensions from the module header.
mkTokStreamModule :: FilePath -> [Extension] -> Text -> TokStream
mkTokStreamModule sourceName baseExts input =
  let (env, lexSt) = mkInitialLexerState sourceName effectiveExts input
   in normalizeTokStreamParts
        (scanAllTokens env lexSt)
        (mkInitialLayoutState True effectiveExts)
        []
        []
        Nothing
        (mkExtensionSet effectiveExts)
        False
  where
    headerSettings = readModuleHeaderExtensions input
    effectiveExts = applyImpliedExtensions (foldr applyExtensionSetting baseExts headerSettings)

-- | Create a TokStream from pre-lexed tokens (for testing/compatibility).
-- Layout tokens must already be inserted in the token list.
mkTokStreamFromTokens :: [LexToken] -> TokStream
mkTokStreamFromTokens toks =
  let (env, lexSt) = mkInitialLexerState "<tokens>" [] ""
   in normalizeTokStreamParts
        (scanAllTokens env lexSt)
        (mkInitialLayoutState False [])
        toks
        []
        Nothing
        (mkExtensionSet [])
        False

-- | Replace the layout state and buffer of a stream. The result is a fresh
-- stream with a consistent memoized successor.
setTokStreamLayout :: LayoutState -> [LexToken] -> TokStream -> TokStream
setTokStreamLayout layoutState buffer ts =
  buildTokStream
    (tokStreamRawTokens ts)
    layoutState
    buffer
    (tokStreamPendingPragmas ts)
    (tokStreamPrevToken ts)
    (tokStreamExtensionSet ts)
    (tokStreamEOFEmitted ts)

-- | Replace the pending hidden pragmas of a stream.
setTokStreamPendingPragmas :: [Pragma] -> TokStream -> TokStream
setTokStreamPendingPragmas pendingPragmas ts =
  buildTokStream
    (tokStreamRawTokens ts)
    (tokStreamLayoutState ts)
    (tokStreamBuffer ts)
    pendingPragmas
    (tokStreamPrevToken ts)
    (tokStreamExtensionSet ts)
    (tokStreamEOFEmitted ts)

-- | Build a stream whose memoized successor is computed from its fields.
buildTokStream :: [LexToken] -> LayoutState -> [LexToken] -> [Pragma] -> Maybe LexToken -> ExtensionSet -> Bool -> TokStream
buildTokStream rawTokens layoutState buffer pendingPragmas prevToken extensionSet eofEmitted =
  let ts =
        TokStream
          { tokStreamRawTokens = rawTokens,
            tokStreamLayoutState = layoutState,
            tokStreamBuffer = buffer,
            tokStreamPendingPragmas = pendingPragmas,
            tokStreamPrevToken = prevToken,
            tokStreamExtensionSet = extensionSet,
            tokStreamEOFEmitted = eofEmitted,
            tokStreamNext = stepOne ts
          }
   in ts

-- | Advance through layout and hidden tokens until the stream is ready for
-- 'stepOne'. Keeping the stream fields separate lets a token step normalize
-- its successor without first allocating an intermediate 'TokStream'.
normalizeTokStreamParts :: [LexToken] -> LayoutState -> [LexToken] -> [Pragma] -> Maybe LexToken -> ExtensionSet -> Bool -> TokStream
normalizeTokStreamParts rawTokens layoutState buffer pendingPragmas prevToken extensionSet eofEmitted
  | eofEmitted = finish rawTokens layoutState buffer pendingPragmas
  | otherwise = go rawTokens layoutState buffer pendingPragmas
  where
    finish rawTokens' layoutState' buffer' pendingPragmas' =
      buildTokStream rawTokens' layoutState' buffer' pendingPragmas' prevToken extensionSet eofEmitted

    go rawTokens' layoutState' buffer' pendingPragmas' =
      case buffer' of
        tok : rest ->
          case lexTokenKind tok of
            TkPragma pragma' ->
              go rawTokens' layoutState' rest (pendingPragmas' <> [pragma'])
            TkLineComment -> go rawTokens' layoutState' rest pendingPragmas'
            TkBlockComment -> go rawTokens' layoutState' rest pendingPragmas'
            _ -> finish rawTokens' layoutState' buffer' pendingPragmas'
        [] ->
          case rawTokens' of
            [] -> finish [] layoutState' [] pendingPragmas'
            rawTok : rawRest ->
              case layoutTransition layoutState' rawTok of
                (allToks, laySt') -> go rawRest laySt' allToks pendingPragmas'

-- | Step one token from the stream. This is the core primitive used by all
-- Stream methods; its result is memoized in 'tokStreamNext'.
--
-- Tokens are produced in two phases:
--
-- 1. Drain any buffered tokens from the layout state (virtual braces/semicolons
--    from 'layoutTransition' or 'closeImplicitLayoutContext').
-- 2. Fetch the next raw token from the shared lazy list and run
--    'layoutTransition' to produce virtual layout tokens and the raw token
--    itself. The first token is returned; the rest are buffered.
--
-- Because the raw token list is a shared lazy list, backtracking to an earlier
-- stream position is O(1) and never re-scans characters.
stepOne :: TokStream -> Maybe (LexToken, TokStream)
stepOne ts
  | tokStreamEOFEmitted ts = Nothing
  | otherwise =
      case tokStreamBuffer ts of
        -- Drain buffered tokens first (virtual braces/semicolons)
        tok : rest ->
          let kind = lexTokenKind tok
              isEOF = case kind of
                TkEOF -> True
                _ -> False
              pendingPragmas =
                case lexTokenOrigin tok of
                  InsertedLayout -> tokStreamPendingPragmas ts
                  FromSource
                    | TkSpecialSemicolon <- kind -> tokStreamPendingPragmas ts
                    | otherwise -> []
              -- The successor is demanded as soon as the parser asks for the
              -- token after this one, which is what almost always happens, so
              -- building it here avoids a thunk per token.
              !next =
                normalizeTokStreamParts
                  (tokStreamRawTokens ts)
                  (tokStreamLayoutState ts)
                  rest
                  pendingPragmas
                  (Just tok)
                  (tokStreamExtensionSet ts)
                  isEOF
           in Just (tok, next)
        [] ->
          Nothing

-- | Build a stream a second time, for locating errors after a parse.
--
-- Error rendering walks a stream from offset 0. Reusing the stream that was
-- parsed would keep its memoized successor chain alive for the whole parse,
-- which 'runTokStreamParser' is careful to avoid. This function is not
-- inlined so that GHC cannot share the rebuilt stream with the parsed one by
-- common-subexpression elimination.
rebuildStream :: (a -> TokStream) -> a -> TokStream
rebuildStream build = build
{-# NOINLINE rebuildStream #-}

-- | The zero-width span at a Megaparsec source position, such as the initial
-- position of a parse. Used when a stream has no token to locate something at.
sourcePosSpan :: MP.SourcePos -> SourceSpan
sourcePosSpan pos =
  SourceSpan
    { sourceSpanSourceName = T.pack (MP.sourceName pos),
      sourceSpanStartLine = MP.unPos (MP.sourceLine pos),
      sourceSpanStartCol = MP.unPos (MP.sourceColumn pos),
      sourceSpanEndLine = MP.unPos (MP.sourceLine pos),
      sourceSpanEndCol = MP.unPos (MP.sourceColumn pos),
      sourceSpanStartOffset = 0,
      sourceSpanEndOffset = 0
    }

-- | Run a token parser from the start of a stream.
--
-- The position state that Megaparsec keeps for error rendering is detached
-- from the stream, so the memoized successor chain can be collected as the
-- parser advances.
runTokStreamParser :: MP.Parsec ParserErrorComponent TokStream a -> FilePath -> TokStream -> Either ParseErrorBundle a
runTokStreamParser parser sourceName ts =
  snd (MP.runParser' parser initialState)
  where
    initialState =
      MP.State
        { MP.stateInput = ts,
          MP.stateOffset = 0,
          MP.statePosState =
            MP.PosState
              { MP.pstateInput = detachedStream,
                MP.pstateOffset = 0,
                MP.pstateSourcePos = MP.initialPos sourceName,
                MP.pstateTabWidth = MP.defaultTabWidth,
                MP.pstateLinePrefix = ""
              },
          MP.stateParseErrors = []
        }
    detachedStream = buildTokStream [] (tokStreamLayoutState ts) [] [] Nothing (tokStreamExtensionSet ts) True

instance Stream TokStream where
  type Token TokStream = LexToken
  type Tokens TokStream = [LexToken]

  tokenToChunk _ tok = [tok]
  tokensToChunk _ = id
  chunkToTokens _ = id
  chunkLength _ = length
  chunkEmpty _ = null

  take1_ = tokStreamNext

  takeN_ n ts
    | n <= 0 = Just ([], ts)
    | otherwise = go n [] ts
    where
      go 0 acc s = Just (reverse acc, s)
      go k acc s =
        case tokStreamNext s of
          Nothing
            | null acc -> Nothing
            | otherwise -> Just (reverse acc, s)
          Just (tok, s') -> go (k - 1) (tok : acc) s'

  takeWhile_ f =
    go []
    where
      go acc s =
        case tokStreamNext s of
          Nothing -> (reverse acc, s)
          Just (tok, s')
            | f tok -> go (tok : acc) s'
            | otherwise ->
                -- Put the non-matching token back by using the pre-step state
                (reverse acc, s)

data ParserConfig = ParserConfig
  { parserSourceName :: FilePath,
    parserExtensions :: [Extension]
  }
  deriving (Eq, Show, Generic, NFData)

data ParseResult a
  = ParseOk a
  | ParseErr [(SourceSpan, Text)]

instance (NFData a) => NFData (ParseResult a) where
  rnf parseResult =
    case parseResult of
      ParseOk parsed -> rnf parsed
      ParseErr errs -> rnf errs
