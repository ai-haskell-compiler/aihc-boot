{-# LANGUAGE OverloadedStrings #-}

module Aihc.Parser.Internal.Common
  ( TokParser,
    label,
    region,
    expectedTok,
    peekToken,
    peekTokenMaybe,
    peekTokenKind,
    nextTokenIs,
    tokenKindDispatch,
    optionalTok,
    optionalTokThen,
    eofTok,
    varIdTok,
    tokenSatisfy,
    hiddenPragma,
    optionalHiddenPragma,
    moduleNameParser,
    nameToUnqualified,
    mkUnqualifiedNameAt,
    mkNameAt,
    identifierName,
    identifierNameWithTokenParser,
    identifierNameParser,
    identifierUnqualifiedNameParser,
    identifierTextParser,
    lowerIdentifierParser,
    tyVarNameParser,
    implicitParamNameParser,
    constructorNameParser,
    constructorUnqualifiedNameParser,
    constructorOperatorUnqualifiedNameParser,
    binderNameParser,
    recordFieldNameParser,
    operatorNameParser,
    operatorUnqualifiedNameParser,
    operatorTextParser,
    constructorInfixOperatorNameParser,
    stringTextParser,
    consumedSpan,
    inputStartSpan,
    withSpan,
    withSpanAnn,
    optionalSuffix,
    parens,
    braces,
    closeAndExpectRBrace,
    thQuoteParser,
    skipSemicolons,
    bracedSemiSep,
    bracedSemiSep1,
    plainSemiSep,
    plainSemiSep1,
    contextItemParserWith,
    contextItemsParserWith,
    contextParserWith,
    typedSignaturePrefixParser,
    typedBindingOrSignatureParser,
    functionHeadParserWith,
    functionHeadParserWithBinder,
    functionBindValue,
    functionBindDecl,
    isExtensionEnabled,
    thAnyEnabled,
    asPatternParser,
    tupleDelimsParser,
    recordFieldsWithWildcardsParser,
    closeImplicitLayout,
    layoutSepEndBy,
    layoutSepBy1,
    drainParseErrors,
    lazy,
    startsWithContextType,
    startsWithTypeSig,
    startsWithAsPattern,
    startsWithTypeBinder,
    isConLikeName,
    isConLikeNameType,
    liftCheck,
    infixOperatorParser,
    startsInfixOperator,
    foldInfixL,
    foldInfixR,
  )
where

import Aihc.Parser.Lex (LayoutState (..), LexToken (..), LexTokenKind (..), TokenOrigin (..), closeImplicitLayoutContext)
import Aihc.Parser.Syntax
import Aihc.Parser.Types (ParserErrorComponent (..), TokStream (..), mkFoundToken, setTokStreamLayout, setTokStreamPendingPragmas, sourcePosSpan, tokStreamExtensionSet)
import Control.Monad (guard)
import Data.Char (isUpper)
import Data.Functor (($>))
import Data.List.NonEmpty qualified as NE
import Data.Maybe (catMaybes)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Text.Megaparsec (Parsec, anySingle, lookAhead, (<|>))
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Error qualified as MPE
import Text.Megaparsec.Internal qualified as MPI

type TokParser = Parsec ParserErrorComponent TokStream

-- | Replace whatever error a parser reports with one that names what was
-- expected here and what was found instead.
--
-- Written as a primitive rather than as 'MP.observing' followed by a case:
-- the labelled parsers are the hot ones — every expression, every right-hand
-- side, every type — and this way a successful parse hands its result
-- straight to the caller's continuation, with no 'Either' box and no bind.
-- The failure continuations see the state the failure happened in, so the
-- token that was found is read from that state rather than looked ahead for.
label :: Text -> TokParser a -> TokParser a
label expected parser =
  MPI.ParsecT $ \s cok cerr eok eerr ->
    MPI.unParser parser s cok (relabel cerr) eok (relabel eerr)
  where
    relabel report err errState =
      case err of
        MPE.TrivialError off _ _ ->
          report
            ( MPE.FancyError
                off
                ( Set.singleton
                    ( MPE.ErrorCustom
                        UnexpectedTokenExpecting
                          { unexpectedFound =
                              mkFoundToken . fst <$> tokStreamNext (MP.stateInput errState),
                            unexpectedExpecting = expected,
                            unexpectedContext = []
                          }
                    )
                )
            )
            errState
        _ -> report err errState

region :: Text -> TokParser a -> TokParser a
region context =
  MP.region addContextToError
  where
    addContextToError err =
      case err of
        MPE.FancyError off fancySet ->
          MPE.FancyError off (Set.map appendContext fancySet)
        _ -> err
    appendContext fancyErr =
      case fancyErr of
        MPE.ErrorCustom custom ->
          case custom of
            UnexpectedTokenExpecting found expecting contexts ->
              MPE.ErrorCustom (UnexpectedTokenExpecting found expecting (contexts <> [context]))
        _ -> fancyErr

-- | Match a specific token kind exactly.
expectedTok :: LexTokenKind -> TokParser ()
expectedTok expected =
  tokenSatisfy (renderTokenKind expected) $ \tok ->
    if lexTokenKind tok == expected then Just () else Nothing
{-# INLINE expectedTok #-}

-- | The next token, without consuming it.
--
-- Fails at the end of the stream with the same error as @lookAhead
-- anySingle@, which is what every dispatch point used before: this is that
-- parser with the 'MP.lookAhead' state save and restore and the 'MP.token'
-- machinery replaced by a read of the stream's memoized successor.
peekToken :: TokParser LexToken
peekToken =
  MPI.ParsecT $ \s _ _ eok eerr ->
    case tokStreamNext (MP.stateInput s) of
      Just (tok, _) -> eok tok s mempty
      Nothing -> eerr (MPE.TrivialError (MP.stateOffset s) (Just MPE.EndOfInput) Set.empty) s
{-# INLINE peekToken #-}

-- | The next token, or 'Nothing' at the end of the stream.
peekTokenMaybe :: TokParser (Maybe LexToken)
peekTokenMaybe =
  MPI.ParsecT $ \s _ _ eok _ ->
    eok (fst <$> tokStreamNext (MP.stateInput s)) s mempty
{-# INLINE peekTokenMaybe #-}

-- | The kind of the next token, without consuming it and without building a
-- parse error when there is none.
peekTokenKind :: TokParser (Maybe LexTokenKind)
peekTokenKind = MPI.ParsecT $ \s _ _ eok _ -> eok (nextTokenKind s) s mempty
{-# INLINE peekTokenKind #-}

-- | Whether the next token has the given kind, without consuming it.
nextTokenIs :: LexTokenKind -> TokParser Bool
nextTokenIs expected =
  MPI.ParsecT $ \s _ _ eok _ -> eok (nextTokenKind s == Just expected) s mempty
{-# INLINE nextTokenIs #-}

-- | Run the parser that the next token's kind selects.
--
-- 'Nothing' means the stream is exhausted, which only happens once 'TkEOF'
-- has been consumed.
tokenKindDispatch :: (Maybe LexTokenKind -> TokParser a) -> TokParser a
tokenKindDispatch select =
  MPI.ParsecT $ \s cok cerr eok eerr ->
    MPI.unParser (select (nextTokenKind s)) s cok cerr eok eerr
{-# INLINE tokenKindDispatch #-}

nextTokenKind :: MP.State TokStream e -> Maybe LexTokenKind
nextTokenKind s =
  case tokStreamNext (MP.stateInput s) of
    Just (tok, _) -> Just (lexTokenKind tok)
    Nothing -> Nothing
{-# INLINE nextTokenKind #-}

-- | Consume the next token if it has the given kind, reporting whether it
-- did.
--
-- Behaves exactly like @MP.optional (expectedTok expected)@ — including at
-- the end of the stream, where both leave the input alone — but decides on
-- the peeked token instead of recovering from a failed parse.
optionalTok :: LexTokenKind -> TokParser Bool
optionalTok expected =
  tokenKindDispatch $ \mKind ->
    if mKind == Just expected
      then True <$ expectedTok expected
      else pure False
{-# INLINE optionalTok #-}

-- | Like @MP.optional (expectedTok expected *> parser)@, but decided on the
-- peeked token.
--
-- The behaviour is identical, including when @parser@ fails after the token
-- was consumed: the failure propagates, because input was consumed either
-- way.  Pass @pure ()@ for a bare optional token.
optionalTokThen :: LexTokenKind -> TokParser a -> TokParser (Maybe a)
optionalTokThen expected parser =
  tokenKindDispatch $ \mKind ->
    if mKind == Just expected
      then Just <$> (expectedTok expected *> parser)
      else pure Nothing
{-# INLINE optionalTokThen #-}

-- | Match the end-of-file token.
--
-- The lexer emits a 'TkEOF' token at the end of input. This parser consumes
-- that token, ensuring the entire input has been processed.
eofTok :: TokParser ()
eofTok =
  tokenSatisfy "end of input" $ \tok ->
    if lexTokenKind tok == TkEOF then Just () else Nothing

-- | Match a specific variable identifier (contextual keyword).
varIdTok :: Text -> TokParser ()
varIdTok expected =
  tokenSatisfy ("identifier '" <> T.unpack expected <> "'") $ \tok ->
    case lexTokenKind tok of
      TkVarId ident | ident == expected -> Just ()
      _ -> Nothing

renderTokenKind :: LexTokenKind -> String
renderTokenKind tk = case tk of
  TkSpecialLParen -> "symbol '('"
  TkSpecialRParen -> "symbol ')'"
  TkSpecialUnboxedLParen -> "symbol '(#'"
  TkSpecialUnboxedRParen -> "symbol '#)'"
  TkSpecialComma -> "symbol ','"
  TkSpecialSemicolon -> "symbol ';'"
  TkSpecialLBracket -> "symbol '['"
  TkSpecialRBracket -> "symbol ']'"
  TkSpecialBacktick -> "symbol '`'"
  TkSpecialLBrace -> "symbol '{'"
  TkSpecialRBrace -> "symbol '}'"
  TkReservedDotDot -> "operator '..'"
  TkReservedColon -> "operator ':'"
  TkReservedDoubleColon -> "operator '::'"
  TkReservedEquals -> "operator '='"
  TkReservedBackslash -> "operator '\\'"
  TkReservedPipe -> "operator '|'"
  TkReservedLeftArrow -> "operator '<-'"
  TkReservedRightArrow -> "operator '->'"
  TkReservedAt -> "operator '@'"
  TkReservedDoubleArrow -> "operator '=>'"
  TkArrowTail -> "operator '-<'"
  TkArrowTailReverse -> "operator '>-'"
  TkDoubleArrowTail -> "operator '-<<'"
  TkDoubleArrowTailReverse -> "operator '>>-'"
  TkBananaOpen -> "operator '(|'"
  TkBananaClose -> "operator '|)'"
  TkPrefixBang -> "bang pattern '!'"
  TkPrefixTilde -> "irrefutable pattern '~'"
  TkTypeApp -> "type application '@'"
  TkTHExpQuoteOpen -> "TH expression quote '[|'"
  TkTHExpQuoteClose -> "TH expression quote close '|]'"
  TkTHTypedQuoteOpen -> "TH typed quote '[||'"
  TkTHTypedQuoteClose -> "TH typed quote close '||]'"
  TkTHDeclQuoteOpen -> "TH declaration quote '[d|'"
  TkTHTypeQuoteOpen -> "TH type quote '[t|'"
  TkTHPatQuoteOpen -> "TH pattern quote '[p|'"
  TkTHQuoteTick -> "TH name quote '''"
  TkTHTypeQuoteTick -> "TH type name quote ''''"
  TkTHSplice -> "TH splice '$'"
  TkTHTypedSplice -> "TH typed splice '$$'"
  TkImplicitParam name -> "implicit parameter " <> show name
  TkVarSym op -> "operator '" <> show op <> "'"
  TkConSym op -> "operator '" <> show op <> "'"
  TkKeywordModule -> "keyword 'module'"
  TkKeywordWhere -> "keyword 'where'"
  TkKeywordDo -> "keyword 'do'"
  TkKeywordData -> "keyword 'data'"
  TkKeywordImport -> "keyword 'import'"
  TkKeywordCase -> "keyword 'case'"
  TkKeywordOf -> "keyword 'of'"
  TkKeywordLet -> "keyword 'let'"
  TkKeywordIn -> "keyword 'in'"
  TkKeywordIf -> "keyword 'if'"
  TkKeywordThen -> "keyword 'then'"
  TkKeywordElse -> "keyword 'else'"
  TkKeywordProc -> "keyword 'proc'"
  TkKeywordPattern -> "keyword 'pattern'"
  TkKeywordRec -> "keyword 'rec'"
  TkKeywordBy -> "keyword 'by'"
  TkKeywordUsing -> "keyword 'using'"
  _ -> show tk

tokenSatisfy :: String -> (LexToken -> Maybe a) -> TokParser a
tokenSatisfy expectedLabel f =
  MP.token f expectedItems
  where
    expectedItems =
      Set.singleton $
        if null expectedLabel
          then MPE.EndOfInput
          else MPE.Label (NE.fromList expectedLabel)
{-# INLINE tokenSatisfy #-}

hiddenPragma :: String -> (Pragma -> Maybe a) -> TokParser a
hiddenPragma expectedLabel f = do
  mResult <- optionalHiddenPragma f
  case mResult of
    Just result -> pure result
    Nothing -> fail expectedLabel

optionalHiddenPragma :: (Pragma -> Maybe a) -> TokParser (Maybe a)
optionalHiddenPragma f = do
  pst <- MP.getParserState
  case spanNoMatch (tokStreamPendingPragmas (MP.stateInput pst)) of
    (ignored, pragmaTok : rest)
      | Just result <- f pragmaTok -> do
          MP.updateParserState $ \st ->
            st {MP.stateInput = setTokStreamPendingPragmas (ignored <> rest) (MP.stateInput st)}
          pure (Just result)
      | otherwise -> pure Nothing
    _ -> pure Nothing
  where
    spanNoMatch pragmas =
      case pragmas of
        pragmaTok : rest
          | Just _ <- f pragmaTok -> ([], pragmaTok : rest)
          | otherwise ->
              let (ignored, remaining) = spanNoMatch rest
               in (pragmaTok : ignored, remaining)
        [] -> ([], [])

moduleNameParser :: TokParser Text
moduleNameParser =
  label "module name" $
    tokenSatisfy "module name" $ \tok ->
      case lexTokenKind tok of
        TkConId ident | isModuleName ident -> Just ident
        TkQConId modName name | isModuleName (modName <> "." <> name) -> Just (modName <> "." <> name)
        _ -> Nothing

-- | The 'Name' an identifier token denotes, or 'Nothing' if the token is not
-- an identifier.
--
-- Exposed as a plain function rather than only as a parser so that callers
-- which also need the token itself (for its span) can build their result
-- inside a single token match, instead of pairing the two up and taking the
-- pair apart again in a monadic bind.
identifierName :: LexToken -> Maybe Name
identifierName tok =
  case lexTokenKind tok of
    TkVarId ident -> Just (mkNameAt tok Nothing NameVarId ident)
    TkConId ident -> Just (mkNameAt tok Nothing NameConId ident)
    TkQVarId modName ident -> Just (mkNameAt tok (Just modName) NameVarId ident)
    TkQConId modName ident -> Just (mkNameAt tok (Just modName) NameConId ident)
    _ -> Nothing

identifierNameWithTokenParser :: TokParser (LexToken, Name)
identifierNameWithTokenParser =
  tokenSatisfy "identifier" $ \tok -> (,) tok <$> identifierName tok

identifierNameParser :: TokParser Name
identifierNameParser =
  tokenSatisfy "identifier" identifierName

identifierUnqualifiedNameParser :: TokParser UnqualifiedName
identifierUnqualifiedNameParser =
  tokenSatisfy "unqualified identifier" $ \tok ->
    case lexTokenKind tok of
      TkVarId ident -> Just (mkUnqualifiedNameAt tok NameVarId ident)
      TkConId ident -> Just (mkUnqualifiedNameAt tok NameConId ident)
      _ -> Nothing

identifierTextParser :: TokParser Text
identifierTextParser = renderName <$> identifierNameParser

lowerIdentifierParser :: TokParser Text
lowerIdentifierParser =
  tokenSatisfy "lowercase identifier" $ \tok ->
    case lexTokenKind tok of
      TkVarId ident -> Just ident
      TkQVarId modName ident -> Just (modName <> "." <> ident)
      _ -> Nothing

tyVarNameParser :: TokParser Text
tyVarNameParser =
  lowerIdentifierParser
    <|> (expectedTok TkKeywordUnderscore $> "_")

implicitParamNameParser :: TokParser Text
implicitParamNameParser =
  tokenSatisfy "implicit parameter" $ \tok ->
    case lexTokenKind tok of
      TkImplicitParam name -> Just name
      _ -> Nothing

constructorNameParser :: TokParser Name
constructorNameParser =
  tokenSatisfy "constructor identifier" $ \tok ->
    case lexTokenKind tok of
      TkConId ident -> Just (mkNameAt tok Nothing NameConId ident)
      TkQConId modName ident -> Just (mkNameAt tok (Just modName) NameConId ident)
      _ -> Nothing

constructorUnqualifiedNameParser :: TokParser UnqualifiedName
constructorUnqualifiedNameParser =
  tokenSatisfy "unqualified constructor identifier" $ \tok ->
    case lexTokenKind tok of
      TkConId ident -> Just (mkUnqualifiedNameAt tok NameConId ident)
      _ -> Nothing

constructorOperatorUnqualifiedNameParser :: TokParser UnqualifiedName
constructorOperatorUnqualifiedNameParser =
  tokenSatisfy "unqualified constructor operator" $ \tok ->
    case lexTokenKind tok of
      TkConSym op -> Just (mkUnqualifiedNameAt tok NameConSym op)
      TkReservedColon -> Just (mkUnqualifiedNameAt tok NameConSym ":")
      _ -> Nothing

binderNameParser :: TokParser UnqualifiedName
binderNameParser =
  identifierUnqualifiedNameParser
    <|> parens operatorUnqualifiedNameParser

recordFieldNameParser :: TokParser Name
recordFieldNameParser =
  identifierNameParser
    <|> parens operatorNameParser

operatorTextParser :: TokParser Text
operatorTextParser = renderName <$> operatorNameParser

operatorNameParser :: TokParser Name
operatorNameParser =
  tokenSatisfy "operator" $ \tok ->
    case lexTokenKind tok of
      TkVarSym op -> Just (mkNameAt tok Nothing NameVarSym op)
      TkConSym op -> Just (mkNameAt tok Nothing NameConSym op)
      TkQVarSym modName op -> Just (mkNameAt tok (Just modName) NameVarSym op)
      TkQConSym modName op -> Just (mkNameAt tok (Just modName) NameConSym op)
      TkReservedAt -> Just (mkNameAt tok Nothing NameVarSym "@")
      _ -> Nothing

operatorUnqualifiedNameParser :: TokParser UnqualifiedName
operatorUnqualifiedNameParser =
  tokenSatisfy "unqualified operator" $ \tok ->
    case lexTokenKind tok of
      TkVarSym op -> Just (mkUnqualifiedNameAt tok NameVarSym op)
      TkConSym op -> Just (mkUnqualifiedNameAt tok NameConSym op)
      TkReservedRightArrow -> Just (mkUnqualifiedNameAt tok NameVarSym "->")
      TkReservedLeftArrow -> Just (mkUnqualifiedNameAt tok NameVarSym "<-")
      TkReservedDoubleArrow -> Just (mkUnqualifiedNameAt tok NameVarSym "=>")
      TkReservedEquals -> Just (mkUnqualifiedNameAt tok NameVarSym "=")
      TkReservedPipe -> Just (mkUnqualifiedNameAt tok NameVarSym "|")
      TkReservedDotDot -> Just (mkUnqualifiedNameAt tok NameVarSym "..")
      TkReservedDoubleColon -> Just (mkUnqualifiedNameAt tok NameVarSym "::")
      TkReservedColon -> Just (mkUnqualifiedNameAt tok NameConSym ":")
      TkReservedAt -> Just (mkUnqualifiedNameAt tok NameVarSym "@")
      _ -> Nothing

-- | Parse an infix operator name (varop) for function definitions.
-- Per Haskell Report section 4.4.3, funlhs uses 'varop' which is:
--   varop → varsym | ` varid `
-- This excludes constructor operators (consym) and qualified operators.
-- Note: Whitespace-sensitive lexing (GHC proposal 0229) now distinguishes
-- TkVarSym "!" (infix operator) from TkPrefixBang (bang pattern), so we
-- can accept all VarSym operators here.
infixOperatorNameParser :: TokParser UnqualifiedName
infixOperatorNameParser =
  symbolicOperatorParser <|> backtickIdentifierParser
  where
    symbolicOperatorParser =
      tokenSatisfy "variable operator" $ \tok ->
        case lexTokenKind tok of
          TkVarSym op -> Just (mkUnqualifiedNameAt tok NameVarSym op)
          _ -> Nothing
    backtickIdentifierParser = do
      expectedTok TkSpecialBacktick
      op <- varIdNameParser
      expectedTok TkSpecialBacktick
      pure op
    varIdNameParser =
      tokenSatisfy "variable identifier" $ \tok ->
        case lexTokenKind tok of
          TkVarId name -> Just (mkUnqualifiedNameAt tok NameVarId name)
          _ -> Nothing

-- | Parse an infix constructor operator name (conop) for pattern synonym where clauses.
-- Per Haskell Report, pattern synonym where-clause equations use the constructor
-- name in infix position: @pat ConOp pat = expr@.
-- This is the constructor counterpart of 'infixOperatorNameParser'.
--   conop → consym | ` conid `
constructorInfixOperatorNameParser :: TokParser UnqualifiedName
constructorInfixOperatorNameParser =
  symbolicConstructorOperatorParser <|> backtickConstructorIdentifierParser
  where
    symbolicConstructorOperatorParser =
      tokenSatisfy "constructor operator" $ \tok ->
        case lexTokenKind tok of
          TkConSym op -> Just (mkUnqualifiedNameAt tok NameConSym op)
          TkReservedColon -> Just (mkUnqualifiedNameAt tok NameConSym ":")
          _ -> Nothing
    backtickConstructorIdentifierParser = do
      expectedTok TkSpecialBacktick
      op <- constructorIdentifierNameParser
      expectedTok TkSpecialBacktick
      pure op
    constructorIdentifierNameParser =
      tokenSatisfy "constructor identifier" $ \tok ->
        case lexTokenKind tok of
          TkConId name -> Just (mkUnqualifiedNameAt tok NameConId name)
          _ -> Nothing

mkUnqualifiedNameAt :: LexToken -> NameType -> Text -> UnqualifiedName
mkUnqualifiedNameAt tok ty txt =
  UnqualifiedName ty txt [mkAnnotation (lexTokenSpan tok)]

mkNameAt :: LexToken -> Maybe Text -> NameType -> Text -> Name
mkNameAt tok qualifier ty txt =
  Name qualifier ty txt [mkAnnotation (lexTokenSpan tok)]

nameToUnqualified :: Name -> UnqualifiedName
nameToUnqualified name =
  UnqualifiedName (nameType name) (nameText name) (nameAnns name)

stringTextParser :: TokParser Text
stringTextParser =
  tokenSatisfy "string literal" $ \tok ->
    case lexTokenKind tok of
      TkString txt -> Just txt
      _ -> Nothing

-- | The span of the tokens consumed between two stream positions, each given
-- as the stream and its offset: from the first token at the start position to
-- the last token consumed before the end position. The result is lazy in both
-- positions, so a caller can attach the span of a deferred parse (see 'lazy')
-- without forcing that parse.
--
-- A parser that consumed nothing gets the zero-width span at the point where
-- it stands. A stream with no tokens at all has no token to stand at; the
-- lexer never builds one, since it always ends the stream with 'TkEOF', but a
-- stream built from an explicit token list can be empty. The span is then the
-- zero-width span at the given start of the input, which the parser state
-- knows along with the source name.
consumedSpan :: MP.SourcePos -> TokStream -> Int -> TokStream -> Int -> SourceSpan
consumedSpan inputStart startInput startOffset endInput endOffset =
  case (inputStartSpan startInput, lexTokenSpan <$> tokStreamPrevToken endInput) of
    (Just next, Just prev)
      | endOffset > startOffset -> mergeSourceSpans next prev
      | otherwise -> emptySpanAtStart next
    (Just next, Nothing) -> emptySpanAtStart next
    (Nothing, Just prev) -> emptySpanAtEnd prev
    (Nothing, Nothing) -> sourcePosSpan inputStart
  where
    emptySpanAtStart sp =
      sp
        { sourceSpanEndLine = sourceSpanStartLine sp,
          sourceSpanEndCol = sourceSpanStartCol sp,
          sourceSpanEndOffset = sourceSpanStartOffset sp
        }
    emptySpanAtEnd sp =
      sp
        { sourceSpanStartLine = sourceSpanEndLine sp,
          sourceSpanStartCol = sourceSpanEndCol sp,
          sourceSpanStartOffset = sourceSpanEndOffset sp
        }

-- | Run a parser and combine its result with the span of the consumed tokens.
--
-- The parser state is read once at each end, and the fields the span needs
-- are taken out strictly, so the span thunk holds positions rather than the
-- state. The state's position state references the initial input, and holding
-- it would keep every token of the file alive until the tree is forced.
withSpanAnn :: (SourceSpan -> a -> b) -> TokParser a -> TokParser b
withSpanAnn f parser = do
  startState <- MP.getParserState
  let !startInput = MP.stateInput startState
      !startOffset = MP.stateOffset startState
      !inputStart = MP.pstateSourcePos (MP.statePosState startState)
  out <- parser
  endState <- MP.getParserState
  let !endInput = MP.stateInput endState
      !endOffset = MP.stateOffset endState
  pure (f (consumedSpan inputStart startInput startOffset endInput endOffset) out)
{-# INLINE withSpanAnn #-}

-- | Run a parser whose result takes the span of the consumed tokens.
withSpan :: TokParser (SourceSpan -> a) -> TokParser a
withSpan = withSpanAnn (\parserSpan out -> out parserSpan)
{-# INLINE withSpan #-}

-- | The span of the next token, if there is one.
inputStartSpan :: TokStream -> Maybe SourceSpan
inputStartSpan ts
  | tokStreamEOFEmitted ts = Nothing
  | tok : _ <- tokStreamBuffer ts = Just (lexTokenSpan tok)
  | rawTok : _ <- tokStreamRawTokens ts = Just (lexTokenSpan rawTok)
  | otherwise = Nothing
{-# INLINE inputStartSpan #-}

optionalSuffix :: TokParser b -> (a -> b -> a) -> TokParser a -> TokParser a
optionalSuffix suffixParser attach parser = do
  base <- parser
  mSuffix <- MP.optional suffixParser
  pure $
    case mSuffix of
      Just suffix -> attach base suffix
      Nothing -> base

parens :: TokParser a -> TokParser a
parens parser = expectedTok TkSpecialLParen *> parser <* expectedTok TkSpecialRParen

braces :: TokParser a -> TokParser a
braces parser = expectedTok TkSpecialLBrace *> parser <* closeAndExpectRBrace

-- | Parse a delimited construct with an annotation wrapper.
-- Used for Template Haskell quotes: @open body close@.
thQuoteParser :: (SourceSpan -> c -> c) -> LexTokenKind -> LexTokenKind -> TokParser a -> (a -> c) -> TokParser c
thQuoteParser ann openTok closeTok bodyParser ctor =
  withSpanAnn ann $ do
    expectedTok openTok
    body <- bodyParser
    expectedTok closeTok
    pure (ctor body)

-- | Expect a @}@ token, closing implicit layout contexts if needed.
-- This implements the parse-error rule for closing braces: if @}@ is not found
-- but there is an implicit layout context, close it (which buffers a virtual @}@)
-- and consume that virtual @}@.
closeAndExpectRBrace :: TokParser ()
closeAndExpectRBrace =
  expectedTok TkSpecialRBrace <|> do
    closed <- closeImplicitLayout
    if closed then expectedTok TkSpecialRBrace else MP.empty

skipSemicolons :: TokParser ()
skipSemicolons = MP.skipMany (expectedTok TkSpecialSemicolon)

bracedSemiSep :: TokParser a -> TokParser [a]
bracedSemiSep = braces . layoutSemiSep

bracedSemiSep1 :: TokParser a -> TokParser [a]
bracedSemiSep1 = braces . layoutSemiSep1

-- | Zero-or-more variant of 'plainSemiSep1'.
-- Parses zero or more items separated by semicolons (no surrounding braces).
plainSemiSep :: TokParser a -> TokParser [a]
plainSemiSep = layoutSemiSep

plainSemiSep1 :: TokParser a -> TokParser [a]
plainSemiSep1 = layoutSemiSep1

layoutSemiSep :: TokParser a -> TokParser [a]
layoutSemiSep parser =
  catMaybes <$> MP.sepBy (MP.optional parser) (expectedTok TkSpecialSemicolon)

layoutSemiSep1 :: TokParser a -> TokParser [a]
layoutSemiSep1 parser = do
  items <- layoutSemiSep parser
  case items of
    [] -> MP.empty
    _ -> pure items

contextItemParserWith :: TokParser Type -> TokParser Type -> TokParser Type
contextItemParserWith typeParser typeAtomParser =
  withSpanAnn (TAnn . mkAnnotation) $
    MP.try parenthesizedContextItemParser <|> MP.try kindSigContextItemParser <|> bareContextItemParser
  where
    bareContextItemParser =
      do
        name <- implicitParamNameParser
        expectedTok TkReservedDoubleColon
        TImplicitParam name <$> typeParser
        <|> do
          expectedTok TkKeywordUnderscore
          pure TWildcard
        <|> constraintTypeParser
    parenthesizedContextItemParser = do
      expectedTok TkSpecialLParen
      item <- contextItemParserWith typeParser typeAtomParser
      expectedTok TkSpecialRParen
      guardNotFollowedByConstraintInfixOp
      pure (TParen item)
      where
        guardNotFollowedByConstraintInfixOp = do
          isFollowed <-
            fmap (either (const False) (const True))
              . MP.observing
              . MP.try
              . MP.lookAhead
              $ constraintTypeInfixOperatorParser
          guard (not isFollowed)
    -- \| Parse a type followed by `::` and another type (kind annotation).
    -- This handles cases like `(c :: Type -> Constraint)` in superclass contexts,
    -- both as standalone parenthesized constraints and as items in comma-separated lists.
    -- Uses lookahead to check for `::` at top bracket depth to avoid ambiguity.
    -- IMPORTANT: Uses `constraintTypeAppParser` (not `typeParser`) for the left side
    -- to avoid a parsing cycle: typeParser -> contextTypeParser -> constraintsParserWith
    -- -> constraintParserWith -> kindSigConstraintParser -> typeParser.
    kindSigContextItemParser :: TokParser Type
    kindSigContextItemParser = do
      guard =<< hasKindSignatureAtTopLevel
      ty <- constraintTypeAppParser
      expectedTok TkReservedDoubleColon
      TKindSig ty <$> kindTypeParser

    -- \| Lookahead: check if there's a `::` at the top bracket depth.
    -- This avoids ambiguity with the bare constraint parser.
    --
    -- The scan stops at the first token that cannot be part of a context
    -- item. Without these stops, a context-less head such as
    -- @instance C T where ...@ scans through the whole instance body.
    hasKindSignatureAtTopLevel :: TokParser Bool
    hasKindSignatureAtTopLevel = MP.lookAhead (go 0)
      where
        go :: Int -> TokParser Bool
        go depth = do
          tok <- anySingle
          case lexTokenKind tok of
            TkEOF -> pure False
            TkReservedDoubleColon | depth == 0 -> pure True
            TkReservedRightArrow | depth == 0 -> pure False
            TkReservedDoubleArrow | depth == 0 -> pure False
            TkSpecialComma | depth == 0 -> pure False
            TkSpecialLParen -> go (depth + 1)
            TkSpecialRParen
              | depth > 0 -> go (depth - 1)
              | otherwise -> pure False
            TkSpecialUnboxedLParen -> go (depth + 1)
            TkSpecialUnboxedRParen
              | depth > 0 -> go (depth - 1)
              | otherwise -> pure False
            TkSpecialLBracket -> go (depth + 1)
            TkSpecialRBracket
              | depth > 0 -> go (depth - 1)
              | otherwise -> pure False
            TkSpecialLBrace
              | lexTokenOrigin tok == InsertedLayout -> pure False
            TkSpecialRBrace
              | lexTokenOrigin tok == InsertedLayout -> pure False
            TkSpecialSemicolon -> pure False
            TkReservedEquals -> pure False
            TkReservedPipe -> pure False
            TkReservedLeftArrow -> pure False
            TkKeywordWhere -> pure False
            TkKeywordDeriving -> pure False
            TkKeywordInstance -> pure False
            TkKeywordClass -> pure False
            TkKeywordData -> pure False
            TkKeywordNewtype -> pure False
            TkKeywordType -> pure False
            TkKeywordImport -> pure False
            TkKeywordModule -> pure False
            TkKeywordLet -> pure False
            TkKeywordIn -> pure False
            TkKeywordDo -> pure False
            TkKeywordOf -> pure False
            TkKeywordThen -> pure False
            TkKeywordElse -> pure False
            _ -> go depth
    constraintTypeParser = do
      first <- constraintTypeAppParser
      rest <- MP.many ((,) <$> constraintTypeInfixOperatorParser <*> constraintTypeAppParser)
      pure (foldInfixR buildInfixType first rest)
    constraintTypeAppParser = do
      first <- typeAtomParser
      rest <- MP.many constraintTypeAppArgParser
      pure (foldl applyConstraintAppArg first rest)
    constraintTypeAppArgParser =
      (Left <$> MP.try (expectedTok TkTypeApp *> (typeAtomParser >>= rejectBareConstraintImplicitParam)))
        <|> (Right <$> (typeAtomParser >>= rejectBareConstraintImplicitParam))
    applyConstraintAppArg fn (Left ty) = TTypeApp fn ty
    applyConstraintAppArg fn (Right ty) = TApp fn ty
    rejectBareConstraintImplicitParam ty =
      case peelTypeAnn ty of
        TImplicitParam {} -> fail "implicit parameter type must be parenthesized"
        _ -> pure ty
    -- \| Parse a type expression that can appear as a kind annotation.
    -- Handles function types (e.g., Type -> Constraint) and type application,
    -- but NOT context types (C a => ...) to avoid parsing cycles.
    kindTypeParser = do
      first <- constraintTypeAppParser
      rest <- MP.many ((,) <$> constraintTypeInfixOperatorParser <*> constraintTypeAppParser)
      let baseType = foldInfixR buildInfixType first rest
      mRhs <- optionalTokThen TkReservedRightArrow kindTypeParser
      case mRhs of
        Just rhs ->
          pure (TFun ArrowUnrestricted baseType rhs)
        Nothing -> pure baseType
    buildInfixType lhs ((op, promoted), rhs) =
      TInfix lhs op promoted rhs
    constraintTypeInfixOperatorParser =
      MP.try promotedInfixOperatorParser <|> backtickConstraintOperatorParser <|> unpromotedInfixOperatorParser
    backtickConstraintOperatorParser = MP.try $ do
      expectedTok TkSpecialBacktick
      op <- constraintOperatorIdentifierParser
      expectedTok TkSpecialBacktick
      pure (op, Unpromoted)
    constraintOperatorIdentifierParser =
      tokenSatisfy "constraint operator identifier" $ \tok ->
        case lexTokenKind tok of
          TkVarId name -> Just (mkNameAt tok Nothing NameVarId name)
          TkConId name -> Just (mkNameAt tok Nothing NameConId name)
          _ -> Nothing
    unpromotedInfixOperatorParser =
      tokenSatisfy "type infix operator" $ \tok ->
        case lexTokenKind tok of
          TkVarSym op
            | op /= "."
                && op /= "!" ->
                Just (mkNameAt tok Nothing NameVarSym op, Unpromoted)
          TkConSym op -> Just (mkNameAt tok Nothing NameConSym op, Unpromoted)
          TkQVarSym modName op ->
            Just (mkNameAt tok (Just modName) NameVarSym op, Unpromoted)
          TkQConSym modName op -> Just (mkNameAt tok (Just modName) NameConSym op, Unpromoted)
          _ -> Nothing
    promotedInfixOperatorParser = do
      expectedTok (TkVarSym "'")
      expectedTok TkReservedColon
      pure (qualifyName Nothing (mkUnqualifiedName NameConSym ":"), Promoted)

contextItemsParserWith :: TokParser Type -> TokParser Type -> TokParser [Type]
contextItemsParserWith typeParser typeAtomParser =
  MP.try parenthesizedContextItemsParser <|> fmap pure (contextItemParserWith typeParser typeAtomParser)
  where
    parenthesizedContextItemsParser = withSpanAnn annotateSingleItem $ do
      items <- parens (listContextItemParser `MP.sepEndBy` expectedTok TkSpecialComma)
      guardNotFollowedByConstraintInfixOp
      case items of
        [] -> fail "empty constraint list in parens"
        [item] -> pure [TParen item]
        _ -> pure items
    -- A single item keeps its parentheses as a node that spans the whole
    -- parenthesized list.
    annotateSingleItem sp items =
      case items of
        [item] -> [typeAnnSpan sp item]
        _ -> items
    listContextItemParser =
      MP.try quantifiedContextItemParser <|> contextItemParserWith typeParser typeAtomParser
    -- \| Extension form (QuantifiedConstraints):
    --
    -- > context item -> ['forall' binders '.'] [context '=>'] constraint
    --
    -- 'contextItemParserWith' cannot read these two forms. Without this
    -- alternative the comma-separated list fails, and the enclosing
    -- parentheses fall back to 'typeAtomParser', which reads the full list as
    -- one tuple type. The generic type parser reads both forms.
    quantifiedContextItemParser = do
      guard =<< startsQuantifiedConstraint
      typeParser
    -- \| Look ahead for a 'forall' or a '=>' that belongs to this list item.
    -- The scan stops at the comma that ends the item and at the closing
    -- parenthesis of the list.
    startsQuantifiedConstraint :: TokParser Bool
    startsQuantifiedConstraint = MP.lookAhead (go (0 :: Int))
      where
        go depth = do
          tok <- anySingle
          case lexTokenKind tok of
            TkEOF -> pure False
            TkKeywordForall | depth == 0 -> pure True
            TkReservedDoubleArrow | depth == 0 -> pure True
            TkSpecialComma | depth == 0 -> pure False
            TkSpecialLParen -> go (depth + 1)
            TkSpecialRParen
              | depth > 0 -> go (depth - 1)
              | otherwise -> pure False
            TkSpecialUnboxedLParen -> go (depth + 1)
            TkSpecialUnboxedRParen
              | depth > 0 -> go (depth - 1)
              | otherwise -> pure False
            TkSpecialLBracket -> go (depth + 1)
            TkSpecialRBracket
              | depth > 0 -> go (depth - 1)
              | otherwise -> pure False
            TkSpecialLBrace
              | lexTokenOrigin tok == InsertedLayout -> pure False
            TkSpecialRBrace
              | lexTokenOrigin tok == InsertedLayout -> pure False
            TkSpecialSemicolon -> pure False
            TkReservedEquals -> pure False
            TkReservedPipe -> pure False
            TkKeywordWhere -> pure False
            _ -> go depth
    guardNotFollowedByConstraintInfixOp = do
      isFollowed <-
        fmap (either (const False) (const True))
          . MP.observing
          . MP.try
          . MP.lookAhead
          $ constraintInfixOpStartParser
      guard (not isFollowed)
    constraintInfixOpStartParser =
      tokenSatisfy "constraint infix operator" $ \tok ->
        case lexTokenKind tok of
          TkVarSym op
            | op /= "."
                && op /= "!" ->
                Just ()
          TkConSym _ -> Just ()
          TkQVarSym _ _ -> Just ()
          TkQConSym _ _ -> Just ()
          TkSpecialBacktick -> Just ()
          _ -> Nothing

contextParserWith :: TokParser Type -> TokParser Type -> TokParser [Type]
contextParserWith = contextItemsParserWith

-- | Parse the shared @vars :: type@ prefix used by type signatures and typed
-- bindings.
typedSignaturePrefixParser :: TokParser ty -> TokParser ([UnqualifiedName], ty)
typedSignaturePrefixParser typeParser = do
  names <- binderNameParser `MP.sepBy1` expectedTok TkSpecialComma
  expectedTok TkReservedDoubleColon
  ty <- typeParser
  pure (names, ty)

-- | Parse either a plain type signature or a typed binding that must be
-- reinterpreted when followed by @=@ or guarded RHS syntax.
typedBindingOrSignatureParser ::
  TokParser ty ->
  ([UnqualifiedName] -> ty -> a) ->
  (UnqualifiedName -> ty -> TokParser a) ->
  String ->
  TokParser a
typedBindingOrSignatureParser typeParser signatureCtor bindingCtor singleBinderMsg = do
  (names, ty) <- typedSignaturePrefixParser typeParser
  nextKind <- lexTokenKind <$> peekToken
  if nextKind == TkReservedEquals || nextKind == TkReservedPipe
    then case names of
      [name] -> bindingCtor name ty
      _ -> fail singleBinderMsg
    else pure (signatureCtor names ty)

functionHeadParserWith :: TokParser Pattern -> TokParser Pattern -> TokParser (MatchHeadForm, UnqualifiedName, [Pattern])
functionHeadParserWith = functionHeadParserWithBinder functionBinderNameParser infixOperatorNameParser

functionHeadParserWithBinder :: TokParser UnqualifiedName -> TokParser UnqualifiedName -> TokParser Pattern -> TokParser Pattern -> TokParser (MatchHeadForm, UnqualifiedName, [Pattern])
functionHeadParserWithBinder binderParser infixOpParser infixPatternParser prefixPatternParser = do
  (firstKind, secondKind) <-
    lookAhead $ do
      first <- lexTokenKind <$> anySingle
      second <- lexTokenKind <$> anySingle
      pure (first, second)
  case firstKind of
    TkVarId {}
      | startsPrefixHead secondKind -> prefixHeadParser
      | otherwise -> MP.try infixHeadParser <|> prefixHeadParser
    TkSpecialLParen ->
      MP.try parenthesizedInfixHeadParser
        <|> MP.try infixHeadParser
        <|> prefixHeadParser
    _ -> infixHeadParser
  where
    startsPrefixHead kind =
      case kind of
        TkVarId {} -> True
        TkConId {} -> True
        TkQConId {} -> True
        TkInteger {} -> True
        TkFloat {} -> True
        TkChar {} -> True
        TkCharHash {} -> True
        TkString {} -> True
        TkStringHash {} -> True
        TkSpecialLParen -> True
        TkSpecialUnboxedLParen -> True
        TkSpecialLBracket -> True
        TkKeywordUnderscore -> True
        TkPrefixBang -> True
        TkPrefixTilde -> True
        TkPrefixMinus -> True
        TkQuasiQuote {} -> True
        TkTypeApp -> True
        TkReservedEquals -> True
        TkReservedPipe -> True
        _ -> False

    prefixHeadParser = do
      name <- binderParser
      pats <- MP.many prefixPatternParser
      pure (MatchHeadPrefix, name, pats)

    infixHeadParser = do
      lhsPat <- infixPatternParser
      op <- infixOpParser
      rhsPat <- infixPatternParser
      pure (MatchHeadInfix, op, [lhsPat, rhsPat])

    parenthesizedInfixHeadParser = do
      expectedTok TkSpecialLParen
      lhsPat <- infixPatternParser
      op <- infixOpParser
      rhsPat <- infixPatternParser
      expectedTok TkSpecialRParen
      tailPats <- MP.many prefixPatternParser
      pure (MatchHeadInfix, op, [lhsPat, rhsPat] <> tailPats)

functionBinderNameParser :: TokParser UnqualifiedName
functionBinderNameParser =
  variableIdentifierParser <|> parens variableOperatorParser
  where
    variableIdentifierParser =
      tokenSatisfy "function binder" $ \tok ->
        case lexTokenKind tok of
          TkVarId ident -> Just (mkUnqualifiedNameAt tok NameVarId ident)
          _ -> Nothing
    variableOperatorParser =
      tokenSatisfy "variable operator" $ \tok ->
        case lexTokenKind tok of
          TkVarSym ident -> Just (mkUnqualifiedNameAt tok NameVarSym ident)
          TkReservedAt -> Just (mkUnqualifiedNameAt tok NameVarSym "@")
          _ -> Nothing

functionBindValue :: MatchHeadForm -> UnqualifiedName -> [Pattern] -> Rhs Expr -> ValueDecl
functionBindValue _headForm name [] rhs =
  -- Zero-argument bindings (e.g. @x = 5@, @x | g = 5@) are pattern bindings,
  -- not function bindings. 'FunctionBind' is reserved for declarations with
  -- at least one argument pattern.
  PatternBind NoMultiplicityTag (PVar name) rhs
functionBindValue headForm name pats rhs =
  FunctionBind
    name
    [ Match
        { matchAnns = [],
          matchHeadForm = headForm,
          matchPats = pats,
          matchRhs = rhs
        }
    ]

functionBindDecl :: MatchHeadForm -> UnqualifiedName -> [Pattern] -> Rhs Expr -> Decl
functionBindDecl headForm name pats rhs =
  DeclValue (functionBindValue headForm name pats rhs)

isModuleName :: Text -> Bool
isModuleName name =
  case T.splitOn "." name of
    [] -> False
    segments -> all isConstructorIdentifier segments

isConstructorIdentifier :: Text -> Bool
isConstructorIdentifier txt =
  case T.uncons txt of
    Just (c, _) -> isUpper c
    Nothing -> False

isExtensionEnabled :: Extension -> TokParser Bool
isExtensionEnabled ext =
  memberExtension ext . tokStreamExtensionSet <$> MP.getInput
{-# INLINE isExtensionEnabled #-}

-- | Check whether any Template Haskell extension is enabled (quotes or full TH).
thAnyEnabled :: TokParser Bool
thAnyEnabled = do
  thEnabled <- isExtensionEnabled TemplateHaskellQuotes
  thFullEnabled <- isExtensionEnabled TemplateHaskell
  pure (thEnabled || thFullEnabled)

asPatternParser :: TokParser Pattern -> TokParser Pattern
asPatternParser bodyParser = withSpanAnn (PAnn . mkAnnotation) $ do
  name <- MP.try (binderNameParser <* expectedTok TkReservedAt)
  PAs name <$> bodyParser

-- | Match a tuple opening delimiter and report the closer that must match it.
--
-- A single token match rather than two alternatives: this parser is tried at
-- very many positions where the next token is neither @(@ nor @(#@, and one
-- token test rejects those positions instead of two.
tupleDelimsParser :: TokParser (TupleFlavor, LexTokenKind)
tupleDelimsParser =
  tokenSatisfy "symbol '(' or '(#'" $ \tok ->
    case lexTokenKind tok of
      TkSpecialLParen -> Just (Boxed, TkSpecialRParen)
      TkSpecialUnboxedLParen -> Just (Unboxed, TkSpecialUnboxedRParen)
      _ -> Nothing

recordFieldsWithWildcardsParser :: TokParser [a] -> TokParser ([a], Bool)
recordFieldsWithWildcardsParser fieldsParser = do
  rwcEnabled <- isExtensionEnabled RecordWildCards
  fields <- fieldsParser
  if rwcEnabled
    then do
      hasDotDot <- optionalTok TkReservedDotDot
      if not hasDotDot
        then pure (fields, False)
        else do
          _ <- optionalTok TkSpecialComma
          pure (fields, True)
    else pure (fields, False)

-- | Signal to the layout engine that a virtual close brace should be inserted.
-- This implements the parse-error rule: when the parser encounters a token that
-- is illegal in the current context but @}@ would be legal, it calls this to
-- close the innermost implicit layout context.
--
-- Returns @True@ if a layout was closed, @False@ if there was no implicit
-- layout context to close.
closeImplicitLayout :: TokParser Bool
closeImplicitLayout = do
  pst <- MP.getParserState
  let ts = MP.stateInput pst
  case closeImplicitLayoutContext (tokStreamLayoutState ts) of
    Nothing -> pure False
    Just laySt' -> do
      let inserted = layoutBuffer laySt'
          laySt'' = laySt' {layoutBuffer = []}
      MP.updateParserState
        ( \s ->
            let input = MP.stateInput s
             in s {MP.stateInput = setTokStreamLayout laySt'' (inserted <> tokStreamBuffer input) input}
        )
      pure True

-- | Like Megaparsec's 'MP.sepEndBy' but implements the parse-error rule for
-- the separator. When the separator fails, we try closing an implicit layout
-- context and retrying — this handles cases like:
--
-- @R { f = case y of A -> 1, g = 2 }@
--
-- where the comma is a record field separator but appears inside the implicit
-- @case@ layout.
layoutSepEndBy :: TokParser a -> TokParser sep -> TokParser [a]
layoutSepEndBy p sep = layoutSepEndBy1 p sep <|> pure []

layoutSepEndBy1 :: TokParser a -> TokParser sep -> TokParser [a]
layoutSepEndBy1 p sep = do
  x <- p
  rest <- MP.option [] $ do
    _ <- layoutSep sep
    layoutSepEndBy p sep
  pure (x : rest)

-- | Like Megaparsec's 'MP.sepBy1' but implements the parse-error rule for
-- the separator.
layoutSepBy1 :: TokParser a -> TokParser sep -> TokParser [a]
layoutSepBy1 p sep = do
  x <- p
  rest <- MP.many $ do
    _ <- layoutSep sep
    p
  pure (x : rest)

-- | Try to match a separator token. If that fails, try closing an implicit
-- layout context and then matching the separator. This implements the
-- parse-error rule: if a token is illegal in the current context but would
-- be legal after inserting a virtual @}@, insert the @}@ and retry.
layoutSep :: TokParser sep -> TokParser sep
layoutSep sep =
  MP.try sep <|> do
    closed <- closeImplicitLayout
    if closed then sep else MP.empty

-- | Drain all registered parse errors from the parser state, returning them
-- and resetting the error list to empty. This prevents 'runParser' from
-- converting a successful parse into a failure due to registered errors
-- (from 'MP.registerParseError' / 'MP.withRecovery').
drainParseErrors :: TokParser [MPE.ParseError TokStream ParserErrorComponent]
drainParseErrors = do
  st <- MP.getParserState
  let errs = MP.stateParseErrors st
  MP.updateParserState (\s -> s {MP.stateParseErrors = []})
  pure errs

-- | Run a terminal parser from a copy of the current state when its result is
-- demanded. The outer parser does not advance. Recovered errors are retained
-- in the returned state; a complete failure returns 'mempty'.
lazy :: (Monoid a) => TokParser a -> TokParser (a, MP.State TokStream ParserErrorComponent)
lazy parser = do
  initialState <- MP.getParserState
  let ~(value, finalState) =
        case MP.runParser' captureResult initialState of
          (_, Right parsed) -> parsed
          (failedState, Left bundle) ->
            ( mempty,
              failedState
                { MP.stateParseErrors =
                    reverse (NE.toList (MPE.bundleErrors bundle))
                }
            )
  pure (value, finalState)
  where
    captureResult = do
      value <- parser
      finalState <- MP.getParserState
      MP.updateParserState (\state -> state {MP.stateParseErrors = []})
      pure (value, finalState)

-- | Non-consuming lookahead dispatch for optional context types.
-- Uses scanning to probe for @=>@ at top bracket depth.
-- Returns 'True' when the input looks like a context.
startsWithContextType :: TokParser Bool
startsWithContextType = MP.lookAhead (go [])
  where
    go :: [LexTokenKind] -> TokParser Bool
    go [] = do
      tok <- anySingle
      case lexTokenKind tok of
        TkEOF -> pure False
        TkReservedDoubleArrow -> pure True
        TkReservedDoubleColon -> pure False
        TkReservedRightArrow -> pure False
        TkReservedEquals -> pure False
        TkSpecialComma -> pure False
        TkSpecialSemicolon -> pure False
        TkReservedPipe -> pure False
        TkSpecialRParen -> pure False
        TkSpecialUnboxedRParen -> pure False
        TkSpecialRBracket -> pure False
        TkSpecialRBrace -> pure False
        TkTHExpQuoteClose -> pure False
        TkTHTypedQuoteClose -> pure False
        TkSpecialLParen -> go [TkSpecialRParen]
        TkSpecialUnboxedLParen -> go [TkSpecialUnboxedRParen]
        TkSpecialLBracket -> go [TkSpecialRBracket]
        TkTHExpQuoteOpen -> go [TkTHExpQuoteClose]
        TkTHTypedQuoteOpen -> go [TkTHTypedQuoteClose]
        TkTHDeclQuoteOpen -> go [TkTHExpQuoteClose]
        TkTHTypeQuoteOpen -> go [TkTHExpQuoteClose]
        TkTHPatQuoteOpen -> go [TkTHExpQuoteClose]
        TkSpecialLBrace -> go [TkSpecialRBrace]
        -- Keywords that cannot appear inside a type expression: stop scanning.
        -- This also prevents an enclosing expression form (such as if/then/else)
        -- from being mistaken for a later top-level context arrow.
        TkKeywordThen -> pure False
        TkKeywordElse -> pure False
        TkKeywordOf -> pure False
        TkKeywordIn -> pure False
        TkKeywordInstance -> pure False
        TkKeywordWhere -> pure False
        TkKeywordDeriving -> pure False
        TkKeywordClass -> pure False
        TkKeywordData -> pure False
        TkKeywordNewtype -> pure False
        _ -> go []
    go stack@(expectedClose : rest) = do
      tok <- anySingle
      case lexTokenKind tok of
        TkEOF -> pure False
        kind
          | kind == expectedClose ->
              case rest of
                [] -> go []
                _ -> go rest
        TkSpecialLParen -> go (TkSpecialRParen : stack)
        TkSpecialUnboxedLParen -> go (TkSpecialUnboxedRParen : stack)
        TkSpecialLBracket -> go (TkSpecialRBracket : stack)
        TkTHExpQuoteOpen -> go (TkTHExpQuoteClose : stack)
        TkTHTypedQuoteOpen -> go (TkTHTypedQuoteClose : stack)
        TkTHDeclQuoteOpen -> go (TkTHExpQuoteClose : stack)
        TkTHTypeQuoteOpen -> go (TkTHExpQuoteClose : stack)
        TkTHPatQuoteOpen -> go (TkTHExpQuoteClose : stack)
        TkSpecialLBrace -> go (TkSpecialRBrace : stack)
        _ -> go stack

-- | Non-consuming lookahead: does the input start with @name1, name2, ... ::@?
-- Used by declaration parsers to dispatch to the type-signature path without
-- 'MP.try', eliminating backtracking over the name list.
startsWithTypeSig :: TokParser Bool
startsWithTypeSig =
  fmap (either (const False) (const True)) . MP.observing . MP.try . MP.lookAhead $ do
    _ <- sigBinderNameParser
    let moreNames = (expectedTok TkSpecialComma *> sigBinderNameParser *> moreNames) <|> pure ()
    moreNames
    expectedTok TkReservedDoubleColon
  where
    sigBinderNameParser =
      binderNameParser
        <|> parens sigOperatorParser

    sigOperatorParser =
      tokenSatisfy "signature operator" $ \tok ->
        case lexTokenKind tok of
          TkVarSym op -> Just (mkUnqualifiedNameAt tok NameVarSym op)
          TkConSym op -> Just (mkUnqualifiedNameAt tok NameConSym op)
          TkReservedColon -> Just (mkUnqualifiedNameAt tok NameConSym ":")
          _ -> Nothing

-- | Non-consuming lookahead: does the input start with @name \@@?
startsWithAsPattern :: TokParser Bool
startsWithAsPattern =
  fmap (either (const False) (const True)) . MP.observing . MP.try . MP.lookAhead $ do
    _ <- binderNameParser
    expectedTok TkReservedAt

-- | Non-consuming lookahead: does the input start with a type binder (@\@@var or @\@@_)?
-- 'TypeAbstractions' implies 'TypeApplications', so the lexer always emits 'TkTypeApp' (not
-- 'TkReservedAt') for @\@@ preceded by whitespace. All valid type binder positions have
-- whitespace before @\@@, so only 'TkTypeApp' is checked. Accepting 'TkReservedAt' here
-- would produce false positives for as-patterns such as @x\@p@.
startsWithTypeBinder :: TokParser Bool
startsWithTypeBinder =
  fmap (either (const False) (const True)) . MP.observing . MP.try . MP.lookAhead $ do
    expectedTok TkTypeApp
    _ <- lowerIdentifierParser <|> (expectedTok TkKeywordUnderscore $> "_")
    pure ()

-- | Check whether a name looks like a constructor (starts with uppercase or ':').
isConLikeName :: Name -> Bool
isConLikeName = isConLikeNameType . nameType

-- | Check whether a name type is constructor-like.
isConLikeNameType :: NameType -> Bool
isConLikeNameType NameConId = True
isConLikeNameType NameConSym = True
isConLikeNameType _ = False

-- | Lift an @Either Text a@ into the parser, converting @Left@ into a parse error.
liftCheck :: Either Text a -> TokParser a
liftCheck (Right a) = pure a
liftCheck (Left msg) = fail (T.unpack msg)

-- | Whether a token can start an infix operator, symbolic or backticked.
-- Mirrors the token cases of 'infixOperatorParser'.
startsInfixOperator :: LexTokenKind -> Bool
startsInfixOperator kind =
  case kind of
    TkVarSym {} -> True
    TkConSym {} -> True
    TkPrefixPercent -> True
    TkQVarSym {} -> True
    TkQConSym {} -> True
    TkMinusOperator -> True
    TkReservedColon -> True
    TkSpecialBacktick -> True
    _ -> False

-- | Parse an infix operator.
infixOperatorParser :: TokParser Name
infixOperatorParser =
  symbolicOperatorParser <|> backtickIdentifierOperatorParser
  where
    symbolicOperatorParser =
      tokenSatisfy "infix operator" $ \tok ->
        case lexTokenKind tok of
          TkVarSym op -> Just (mkNameAt tok Nothing NameVarSym op)
          TkConSym op -> Just (mkNameAt tok Nothing NameConSym op)
          TkPrefixPercent -> Just (mkNameAt tok Nothing NameVarSym "%")
          TkQVarSym modName op -> Just (mkNameAt tok (Just modName) NameVarSym op)
          TkQConSym modName op -> Just (mkNameAt tok (Just modName) NameConSym op)
          -- TkMinusOperator is minus when LexicalNegation is enabled but used as infix
          TkMinusOperator -> Just (mkNameAt tok Nothing NameVarSym "-")
          -- Reserved operators that can be used as infix operators
          TkReservedColon -> Just (mkNameAt tok Nothing NameConSym ":")
          _ -> Nothing

    backtickIdentifierOperatorParser =
      expectedTok TkSpecialBacktick *> identifierNameParser <* expectedTok TkSpecialBacktick

-- | Build a left-associated infix chain from a left operand and a list
-- of @(operator, operand)@ pairs.  Given @lhs@ and
-- @[(op1, a), (op2, b), (op3, c)]@ this produces
-- @((lhs \`op1\` a) \`op2\` b) \`op3\` c@.
--
-- This matches GHC's parsed expression AST before any later fixity
-- reassociation pass has run.
foldInfixL :: (a -> (op, a) -> a) -> a -> [(op, a)] -> a
foldInfixL = foldl

-- | Build a right-associated infix chain from a left operand and a list
-- of @(operator, operand)@ pairs.  Given @lhs@ and
-- @[(op1, a), (op2, b), (op3, c)]@ this produces
-- @lhs \`op1\` (a \`op2\` (b \`op3\` c))@.
foldInfixR :: (a -> (op, a) -> a) -> a -> [(op, a)] -> a
foldInfixR _ lhs [] = lhs
foldInfixR build lhs ((op, rhs) : rest) =
  build lhs (op, foldInfixR build rhs rest)
