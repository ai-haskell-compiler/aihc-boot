module Aihc.Parser.Internal.Errors
  ( parseErrorBundleToSpannedText,
    parseErrorsToSpannedText,
  )
where

import Aihc.Parser.Lex (LexToken (..), TokenOrigin (..))
import Aihc.Parser.Syntax (SourceSpan, sourceSpanEndCol, sourceSpanEndLine, sourceSpanEndOffset, sourceSpanStartCol, sourceSpanStartLine, sourceSpanStartOffset)
import Aihc.Parser.Types (FoundToken (..), ParseErrorBundle, ParserErrorComponent (..), TokStream (..), sourcePosSpan)
import Data.List qualified as List
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Prettyprinter (Doc, defaultLayoutOptions, layoutPretty, pretty, vcat)
import Prettyprinter.Render.Text qualified as RText
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Error (ErrorFancy (..), ErrorItem (..))
import Text.Megaparsec.Error qualified as MPE

-- | Render the errors of a failed parse, each with a source span.
--
-- The stream is a fresh stream over the same input, positioned at offset 0.
-- It must not be the stream that was parsed: holding that one keeps its
-- memoized successor chain alive for the whole parse (see
-- 'Aihc.Parser.Types.runTokStreamParser').
parseErrorBundleToSpannedText :: FilePath -> TokStream -> ParseErrorBundle -> [(SourceSpan, Text)]
parseErrorBundleToSpannedText sourceName stream bundle =
  parseErrorsToSpannedText sourceName stream (NE.toList (MPE.bundleErrors bundle))

-- | Render parse errors, each with a source span. See
-- 'parseErrorBundleToSpannedText' for the stream argument.
parseErrorsToSpannedText :: FilePath -> TokStream -> [MPE.ParseError TokStream ParserErrorComponent] -> [(SourceSpan, Text)]
parseErrorsToSpannedText sourceName stream errs =
  [ (fromMaybe (spanAtOffset sourceName stream (MP.errorOffset err)) mSpan, RText.renderStrict (layoutPretty defaultLayoutOptions doc))
  | err <- List.sortOn MP.errorOffset errs,
    (mSpan, doc) <- renderParseErrors err
  ]

-- | The span of the token at an offset of a stream that starts at offset 0.
-- This is where the parser stood when it raised an error at that offset, so
-- it locates errors that carry no token of their own, such as one raised with
-- 'fail'. Past the last token the span is the zero-width end of that token; a
-- stream with no tokens at all gives the zero-width start of the input.
spanAtOffset :: FilePath -> TokStream -> Int -> SourceSpan
spanAtOffset sourceName = go
  where
    go stream n =
      case tokStreamNext stream of
        Just (tok, rest)
          | n > 0 -> go rest (n - 1)
          | otherwise -> lexTokenSpan tok
        Nothing ->
          case tokStreamPrevToken stream of
            Just prev -> spanEnd (lexTokenSpan prev)
            Nothing -> sourcePosSpan (MP.initialPos sourceName)
    spanEnd sp =
      sp
        { sourceSpanStartLine = sourceSpanEndLine sp,
          sourceSpanStartCol = sourceSpanEndCol sp,
          sourceSpanStartOffset = sourceSpanEndOffset sp
        }

-- | Render an error's messages, each with the span of the token it names, if
-- it names one.
renderParseErrors :: MPE.ParseError TokStream ParserErrorComponent -> [(Maybe SourceSpan, Doc ann)]
renderParseErrors err =
  case err of
    MPE.TrivialError _ mUnexpected expected ->
      let mSpan = trivialUnexpectedSpan mUnexpected
       in [(mSpan, vcat (map pretty (renderTrivialError mUnexpected expected)))]
    MPE.FancyError _ fancySet ->
      map renderFancyError (Set.toAscList fancySet)
  where
    trivialUnexpectedSpan :: Maybe (ErrorItem LexToken) -> Maybe SourceSpan
    trivialUnexpectedSpan mItem =
      case mItem of
        Just (Tokens ts) -> Just (lexTokenSpan (NE.head ts))
        _ -> Nothing

    renderFancyError :: ErrorFancy ParserErrorComponent -> (Maybe SourceSpan, Doc ann)
    renderFancyError fancy =
      case fancy of
        ErrorCustom custom ->
          ( customFoundSpan custom,
            vcat (map pretty (customMessageLines custom))
          )
        ErrorFail message -> (Nothing, pretty message)
        _ ->
          ( Nothing,
            pretty (show fancy)
          )

    customFoundSpan :: ParserErrorComponent -> Maybe SourceSpan
    customFoundSpan (UnexpectedTokenExpecting (Just found) _ _) =
      Just (foundTokenSpan found)
    customFoundSpan _ = Nothing

    customMessageLines :: ParserErrorComponent -> [String]
    customMessageLines e@(UnexpectedTokenExpecting mFound _ contexts) =
      [maybe "unexpected end of input" renderUnexpectedToken mFound, MPE.showErrorComponent e]
        <> map (\context -> "context: " <> T.unpack context) contexts

renderTrivialError :: Maybe (ErrorItem LexToken) -> Set.Set (ErrorItem LexToken) -> [String]
renderTrivialError mUnexpected expected =
  maybe [] (\item -> ["unexpected " <> renderErrorItem item]) mUnexpected
    <> ["expecting " <> renderExpectedItems (Set.toAscList expected) | not (Set.null expected)]

renderErrorItem :: ErrorItem LexToken -> String
renderErrorItem item =
  case item of
    Tokens toks -> unwords (map (T.unpack . lexTokenText) (NE.toList toks))
    Label label -> NE.toList label
    EndOfInput -> "end of input"

renderExpectedItems :: [ErrorItem LexToken] -> String
renderExpectedItems items =
  case map renderErrorItem items of
    [] -> ""
    [item] -> item
    [itemA, itemB] -> itemA <> " or " <> itemB
    rendered -> List.intercalate ", " (init rendered) <> ", or " <> last rendered

renderUnexpectedToken :: FoundToken -> String
renderUnexpectedToken found =
  "unexpected " <> tokenDescriptor found

tokenDescriptor :: FoundToken -> String
tokenDescriptor found =
  case foundTokenOrigin found of
    InsertedLayout -> "end of input"
    FromSource ->
      "'" <> T.unpack (foundTokenText found) <> "'"
