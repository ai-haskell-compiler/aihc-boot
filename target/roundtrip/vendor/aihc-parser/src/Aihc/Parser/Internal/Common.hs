{-# LANGUAGE OverloadedStrings #-}
module Aihc.Parser.Internal.Common (TokParser, label, region, expectedTok, peekToken, peekTokenMaybe, peekTokenKind, nextTokenIs, tokenKindDispatch, optionalTok, optionalTokThen, eofTok, varIdTok, tokenSatisfy, hiddenPragma, optionalHiddenPragma, moduleNameParser, nameToUnqualified, mkUnqualifiedNameAt, mkNameAt, identifierName, identifierNameWithTokenParser, identifierNameParser, identifierUnqualifiedNameParser, identifierTextParser, lowerIdentifierParser, tyVarNameParser, implicitParamNameParser, constructorNameParser, constructorUnqualifiedNameParser, constructorOperatorUnqualifiedNameParser, binderNameParser, recordFieldNameParser, operatorNameParser, operatorUnqualifiedNameParser, operatorTextParser, constructorInfixOperatorNameParser, stringTextParser, consumedSpan, inputStartSpan, withSpan, withSpanAnn, optionalSuffix, parens, braces, closeAndExpectRBrace, thQuoteParser, skipSemicolons, bracedSemiSep, bracedSemiSep1, plainSemiSep, plainSemiSep1, contextItemParserWith, contextItemsParserWith, contextParserWith, typedSignaturePrefixParser, typedBindingOrSignatureParser, functionHeadParserWith, functionHeadParserWithBinder, functionBindValue, functionBindDecl, isExtensionEnabled, thAnyEnabled, asPatternParser, tupleDelimsParser, recordFieldsWithWildcardsParser, closeImplicitLayout, layoutSepEndBy, layoutSepBy1, drainParseErrors, lazy, startsWithContextType, startsWithTypeSig, startsWithAsPattern, startsWithTypeBinder, isConLikeName, isConLikeNameType, liftCheck, infixOperatorParser, startsInfixOperator, foldInfixL, foldInfixR) where {
import Aihc.Parser.Lex (LayoutState(..), LexToken(..), LexTokenKind(..), TokenOrigin(..), closeImplicitLayoutContext);
import Aihc.Parser.Syntax;
import Aihc.Parser.Types (ParserErrorComponent(..), TokStream(..), mkFoundToken, setTokStreamLayout, setTokStreamPendingPragmas, sourcePosSpan, tokStreamExtensionSet);
import Control.Monad (guard);
import Data.Char (isUpper);
import Data.Functor (($>));
import Data.List.NonEmpty qualified as NE;
import Data.Maybe (catMaybes);
import Data.Set qualified as Set;
import Data.Text (Text);
import Data.Text qualified as T;
import Text.Megaparsec (Parsec, anySingle, lookAhead, (<|>));
import Text.Megaparsec qualified as MP;
import Text.Megaparsec.Error qualified as MPE;
import Text.Megaparsec.Internal qualified as MPI;
type TokParser = Parsec ParserErrorComponent TokStream;
label :: Text -> TokParser a -> TokParser a;
label expected parser = MPI.ParsecT $ \s cok cerr eok eerr -> MPI.unParser parser s cok (relabel cerr) eok (relabel eerr) where {
  relabel report err errState = case err of {
  MPE.TrivialError off _ _ -> report (MPE.FancyError off (Set.singleton (MPE.ErrorCustom UnexpectedTokenExpecting {unexpectedFound = mkFoundToken . fst <$> tokStreamNext (MP.stateInput errState), unexpectedExpecting = expected, unexpectedContext = []}))) errState;
  _ -> report err errState}};
region :: Text -> TokParser a -> TokParser a;
region context = MP.region addContextToError where {
  addContextToError err = case err of {
  MPE.FancyError off fancySet -> MPE.FancyError off (Set.map appendContext fancySet);
  _ -> err};
  appendContext fancyErr = case fancyErr of {
  MPE.ErrorCustom custom -> case custom of {
  UnexpectedTokenExpecting found expecting contexts -> MPE.ErrorCustom (UnexpectedTokenExpecting found expecting (contexts <> [context]))};
  _ -> fancyErr}};
expectedTok :: LexTokenKind -> TokParser ();
expectedTok expected = tokenSatisfy (renderTokenKind expected) $ \tok -> if lexTokenKind tok == expected then Just () else Nothing;
peekToken :: TokParser LexToken;
peekToken = MPI.ParsecT $ \s _ _ eok eerr -> case tokStreamNext (MP.stateInput s) of {
  Just (tok, _) -> eok tok s mempty;
  Nothing -> eerr (MPE.TrivialError (MP.stateOffset s) (Just MPE.EndOfInput) Set.empty) s};
peekTokenMaybe :: TokParser (Maybe LexToken);
peekTokenMaybe = MPI.ParsecT $ \s _ _ eok _ -> eok (fst <$> tokStreamNext (MP.stateInput s)) s mempty;
peekTokenKind :: TokParser (Maybe LexTokenKind);
peekTokenKind = MPI.ParsecT $ \s _ _ eok _ -> eok (nextTokenKind s) s mempty;
nextTokenIs :: LexTokenKind -> TokParser Bool;
nextTokenIs expected = MPI.ParsecT $ \s _ _ eok _ -> eok (nextTokenKind s == Just expected) s mempty;
tokenKindDispatch :: (Maybe LexTokenKind -> TokParser a) -> TokParser a;
tokenKindDispatch select = MPI.ParsecT $ \s cok cerr eok eerr -> MPI.unParser (select (nextTokenKind s)) s cok cerr eok eerr;
nextTokenKind :: MP.State TokStream e -> Maybe LexTokenKind;
nextTokenKind s = case tokStreamNext (MP.stateInput s) of {
  Just (tok, _) -> Just (lexTokenKind tok);
  Nothing -> Nothing};
optionalTok :: LexTokenKind -> TokParser Bool;
optionalTok expected = tokenKindDispatch $ \mKind -> if mKind == Just expected then True <$ expectedTok expected else pure False;
optionalTokThen :: LexTokenKind -> TokParser a -> TokParser (Maybe a);
optionalTokThen expected parser = tokenKindDispatch $ \mKind -> if mKind == Just expected then Just <$> (expectedTok expected *> parser) else pure Nothing;
eofTok :: TokParser ();
eofTok = tokenSatisfy "end of input" $ \tok -> if lexTokenKind tok == TkEOF then Just () else Nothing;
varIdTok :: Text -> TokParser ();
varIdTok expected = tokenSatisfy ("identifier '" <> T.unpack expected <> "'") $ \tok -> case lexTokenKind tok of {
  TkVarId ident | ident == expected -> Just ();
  _ -> Nothing};
renderTokenKind :: LexTokenKind -> String;
renderTokenKind tk = case tk of {
  TkSpecialLParen -> "symbol '('";
  TkSpecialRParen -> "symbol ')'";
  TkSpecialUnboxedLParen -> "symbol '(#'";
  TkSpecialUnboxedRParen -> "symbol '#)'";
  TkSpecialComma -> "symbol ','";
  TkSpecialSemicolon -> "symbol ';'";
  TkSpecialLBracket -> "symbol '['";
  TkSpecialRBracket -> "symbol ']'";
  TkSpecialBacktick -> "symbol '`'";
  TkSpecialLBrace -> "symbol '{'";
  TkSpecialRBrace -> "symbol '}'";
  TkReservedDotDot -> "operator '..'";
  TkReservedColon -> "operator ':'";
  TkReservedDoubleColon -> "operator '::'";
  TkReservedEquals -> "operator '='";
  TkReservedBackslash -> "operator '\\'";
  TkReservedPipe -> "operator '|'";
  TkReservedLeftArrow -> "operator '<-'";
  TkReservedRightArrow -> "operator '->'";
  TkReservedAt -> "operator '@'";
  TkReservedDoubleArrow -> "operator '=>'";
  TkArrowTail -> "operator '-<'";
  TkArrowTailReverse -> "operator '>-'";
  TkDoubleArrowTail -> "operator '-<<'";
  TkDoubleArrowTailReverse -> "operator '>>-'";
  TkBananaOpen -> "operator '(|'";
  TkBananaClose -> "operator '|)'";
  TkPrefixBang -> "bang pattern '!'";
  TkPrefixTilde -> "irrefutable pattern '~'";
  TkTypeApp -> "type application '@'";
  TkTHExpQuoteOpen -> "TH expression quote '[|'";
  TkTHExpQuoteClose -> "TH expression quote close '|]'";
  TkTHTypedQuoteOpen -> "TH typed quote '[||'";
  TkTHTypedQuoteClose -> "TH typed quote close '||]'";
  TkTHDeclQuoteOpen -> "TH declaration quote '[d|'";
  TkTHTypeQuoteOpen -> "TH type quote '[t|'";
  TkTHPatQuoteOpen -> "TH pattern quote '[p|'";
  TkTHQuoteTick -> "TH name quote '''";
  TkTHTypeQuoteTick -> "TH type name quote ''''";
  TkTHSplice -> "TH splice '$'";
  TkTHTypedSplice -> "TH typed splice '$$'";
  TkImplicitParam name -> "implicit parameter " <> show name;
  TkVarSym op -> "operator '" <> show op <> "'";
  TkConSym op -> "operator '" <> show op <> "'";
  TkKeywordModule -> "keyword 'module'";
  TkKeywordWhere -> "keyword 'where'";
  TkKeywordDo -> "keyword 'do'";
  TkKeywordData -> "keyword 'data'";
  TkKeywordImport -> "keyword 'import'";
  TkKeywordCase -> "keyword 'case'";
  TkKeywordOf -> "keyword 'of'";
  TkKeywordLet -> "keyword 'let'";
  TkKeywordIn -> "keyword 'in'";
  TkKeywordIf -> "keyword 'if'";
  TkKeywordThen -> "keyword 'then'";
  TkKeywordElse -> "keyword 'else'";
  TkKeywordProc -> "keyword 'proc'";
  TkKeywordPattern -> "keyword 'pattern'";
  TkKeywordRec -> "keyword 'rec'";
  TkKeywordBy -> "keyword 'by'";
  TkKeywordUsing -> "keyword 'using'";
  _ -> show tk};
tokenSatisfy :: String -> (LexToken -> Maybe a) -> TokParser a;
tokenSatisfy expectedLabel f = MP.token f expectedItems where {
  expectedItems = Set.singleton $ if null expectedLabel then MPE.EndOfInput else MPE.Label (NE.fromList expectedLabel)};
hiddenPragma :: String -> (Pragma -> Maybe a) -> TokParser a;
hiddenPragma expectedLabel f = do {
  mResult <- optionalHiddenPragma f;
  case mResult of {
  Just result -> pure result;
  Nothing -> fail expectedLabel}};
optionalHiddenPragma :: (Pragma -> Maybe a) -> TokParser (Maybe a);
optionalHiddenPragma f = do {
  pst <- MP.getParserState;
  case spanNoMatch (tokStreamPendingPragmas (MP.stateInput pst)) of {
  (ignored, pragmaTok : rest) | Just result <- f pragmaTok -> do {
  MP.updateParserState $ \st -> st {MP.stateInput = setTokStreamPendingPragmas (ignored <> rest) (MP.stateInput st)};
  pure (Just result)} | otherwise -> pure Nothing;
  _ -> pure Nothing}} where {
  spanNoMatch pragmas = case pragmas of {
  pragmaTok : rest | Just _ <- f pragmaTok -> ([], pragmaTok : rest) | otherwise -> let {
  (ignored, remaining) = spanNoMatch rest} in (pragmaTok : ignored, remaining);
  [] -> ([], [])}};
moduleNameParser :: TokParser Text;
moduleNameParser = label "module name" $ tokenSatisfy "module name" $ \tok -> case lexTokenKind tok of {
  TkConId ident | isModuleName ident -> Just ident;
  TkQConId modName name | isModuleName (modName <> "." <> name) -> Just (modName <> "." <> name);
  _ -> Nothing};
identifierName :: LexToken -> Maybe Name;
identifierName tok = case lexTokenKind tok of {
  TkVarId ident -> Just (mkNameAt tok Nothing NameVarId ident);
  TkConId ident -> Just (mkNameAt tok Nothing NameConId ident);
  TkQVarId modName ident -> Just (mkNameAt tok (Just modName) NameVarId ident);
  TkQConId modName ident -> Just (mkNameAt tok (Just modName) NameConId ident);
  _ -> Nothing};
identifierNameWithTokenParser :: TokParser (LexToken, Name);
identifierNameWithTokenParser = tokenSatisfy "identifier" $ \tok -> (,) tok <$> identifierName tok;
identifierNameParser :: TokParser Name;
identifierNameParser = tokenSatisfy "identifier" identifierName;
identifierUnqualifiedNameParser :: TokParser UnqualifiedName;
identifierUnqualifiedNameParser = tokenSatisfy "unqualified identifier" $ \tok -> case lexTokenKind tok of {
  TkVarId ident -> Just (mkUnqualifiedNameAt tok NameVarId ident);
  TkConId ident -> Just (mkUnqualifiedNameAt tok NameConId ident);
  _ -> Nothing};
identifierTextParser :: TokParser Text;
identifierTextParser = renderName <$> identifierNameParser;
lowerIdentifierParser :: TokParser Text;
lowerIdentifierParser = tokenSatisfy "lowercase identifier" $ \tok -> case lexTokenKind tok of {
  TkVarId ident -> Just ident;
  TkQVarId modName ident -> Just (modName <> "." <> ident);
  _ -> Nothing};
tyVarNameParser :: TokParser Text;
tyVarNameParser = lowerIdentifierParser <|> (expectedTok TkKeywordUnderscore $> "_");
implicitParamNameParser :: TokParser Text;
implicitParamNameParser = tokenSatisfy "implicit parameter" $ \tok -> case lexTokenKind tok of {
  TkImplicitParam name -> Just name;
  _ -> Nothing};
constructorNameParser :: TokParser Name;
constructorNameParser = tokenSatisfy "constructor identifier" $ \tok -> case lexTokenKind tok of {
  TkConId ident -> Just (mkNameAt tok Nothing NameConId ident);
  TkQConId modName ident -> Just (mkNameAt tok (Just modName) NameConId ident);
  _ -> Nothing};
constructorUnqualifiedNameParser :: TokParser UnqualifiedName;
constructorUnqualifiedNameParser = tokenSatisfy "unqualified constructor identifier" $ \tok -> case lexTokenKind tok of {
  TkConId ident -> Just (mkUnqualifiedNameAt tok NameConId ident);
  _ -> Nothing};
constructorOperatorUnqualifiedNameParser :: TokParser UnqualifiedName;
constructorOperatorUnqualifiedNameParser = tokenSatisfy "unqualified constructor operator" $ \tok -> case lexTokenKind tok of {
  TkConSym op -> Just (mkUnqualifiedNameAt tok NameConSym op);
  TkReservedColon -> Just (mkUnqualifiedNameAt tok NameConSym ":");
  _ -> Nothing};
binderNameParser :: TokParser UnqualifiedName;
binderNameParser = identifierUnqualifiedNameParser <|> parens operatorUnqualifiedNameParser;
recordFieldNameParser :: TokParser Name;
recordFieldNameParser = identifierNameParser <|> parens operatorNameParser;
operatorTextParser :: TokParser Text;
operatorTextParser = renderName <$> operatorNameParser;
operatorNameParser :: TokParser Name;
operatorNameParser = tokenSatisfy "operator" $ \tok -> case lexTokenKind tok of {
  TkVarSym op -> Just (mkNameAt tok Nothing NameVarSym op);
  TkConSym op -> Just (mkNameAt tok Nothing NameConSym op);
  TkQVarSym modName op -> Just (mkNameAt tok (Just modName) NameVarSym op);
  TkQConSym modName op -> Just (mkNameAt tok (Just modName) NameConSym op);
  TkReservedAt -> Just (mkNameAt tok Nothing NameVarSym "@");
  _ -> Nothing};
operatorUnqualifiedNameParser :: TokParser UnqualifiedName;
operatorUnqualifiedNameParser = tokenSatisfy "unqualified operator" $ \tok -> case lexTokenKind tok of {
  TkVarSym op -> Just (mkUnqualifiedNameAt tok NameVarSym op);
  TkConSym op -> Just (mkUnqualifiedNameAt tok NameConSym op);
  TkReservedRightArrow -> Just (mkUnqualifiedNameAt tok NameVarSym "->");
  TkReservedLeftArrow -> Just (mkUnqualifiedNameAt tok NameVarSym "<-");
  TkReservedDoubleArrow -> Just (mkUnqualifiedNameAt tok NameVarSym "=>");
  TkReservedEquals -> Just (mkUnqualifiedNameAt tok NameVarSym "=");
  TkReservedPipe -> Just (mkUnqualifiedNameAt tok NameVarSym "|");
  TkReservedDotDot -> Just (mkUnqualifiedNameAt tok NameVarSym "..");
  TkReservedDoubleColon -> Just (mkUnqualifiedNameAt tok NameVarSym "::");
  TkReservedColon -> Just (mkUnqualifiedNameAt tok NameConSym ":");
  TkReservedAt -> Just (mkUnqualifiedNameAt tok NameVarSym "@");
  _ -> Nothing};
infixOperatorNameParser :: TokParser UnqualifiedName;
infixOperatorNameParser = symbolicOperatorParser <|> backtickIdentifierParser where {
  symbolicOperatorParser = tokenSatisfy "variable operator" $ \tok -> case lexTokenKind tok of {
  TkVarSym op -> Just (mkUnqualifiedNameAt tok NameVarSym op);
  _ -> Nothing};
  backtickIdentifierParser = do {
  expectedTok TkSpecialBacktick;
  op <- varIdNameParser;
  expectedTok TkSpecialBacktick;
  pure op};
  varIdNameParser = tokenSatisfy "variable identifier" $ \tok -> case lexTokenKind tok of {
  TkVarId name -> Just (mkUnqualifiedNameAt tok NameVarId name);
  _ -> Nothing}};
constructorInfixOperatorNameParser :: TokParser UnqualifiedName;
constructorInfixOperatorNameParser = symbolicConstructorOperatorParser <|> backtickConstructorIdentifierParser where {
  symbolicConstructorOperatorParser = tokenSatisfy "constructor operator" $ \tok -> case lexTokenKind tok of {
  TkConSym op -> Just (mkUnqualifiedNameAt tok NameConSym op);
  TkReservedColon -> Just (mkUnqualifiedNameAt tok NameConSym ":");
  _ -> Nothing};
  backtickConstructorIdentifierParser = do {
  expectedTok TkSpecialBacktick;
  op <- constructorIdentifierNameParser;
  expectedTok TkSpecialBacktick;
  pure op};
  constructorIdentifierNameParser = tokenSatisfy "constructor identifier" $ \tok -> case lexTokenKind tok of {
  TkConId name -> Just (mkUnqualifiedNameAt tok NameConId name);
  _ -> Nothing}};
mkUnqualifiedNameAt :: LexToken -> NameType -> Text -> UnqualifiedName;
mkUnqualifiedNameAt tok ty txt = UnqualifiedName ty txt [mkAnnotation (lexTokenSpan tok)];
mkNameAt :: LexToken -> Maybe Text -> NameType -> Text -> Name;
mkNameAt tok qualifier ty txt = Name qualifier ty txt [mkAnnotation (lexTokenSpan tok)];
nameToUnqualified :: Name -> UnqualifiedName;
nameToUnqualified name = UnqualifiedName (nameType name) (nameText name) (nameAnns name);
stringTextParser :: TokParser Text;
stringTextParser = tokenSatisfy "string literal" $ \tok -> case lexTokenKind tok of {
  TkString txt -> Just txt;
  _ -> Nothing};
consumedSpan :: MP.SourcePos -> TokStream -> Int -> TokStream -> Int -> SourceSpan;
consumedSpan inputStart startInput startOffset endInput endOffset = case (inputStartSpan startInput, lexTokenSpan <$> tokStreamPrevToken endInput) of {
  (Just next, Just prev) | endOffset > startOffset -> mergeSourceSpans next prev | otherwise -> emptySpanAtStart next;
  (Just next, Nothing) -> emptySpanAtStart next;
  (Nothing, Just prev) -> emptySpanAtEnd prev;
  (Nothing, Nothing) -> sourcePosSpan inputStart} where {
  emptySpanAtStart sp = sp {sourceSpanEndLine = sourceSpanStartLine sp, sourceSpanEndCol = sourceSpanStartCol sp, sourceSpanEndOffset = sourceSpanStartOffset sp};
  emptySpanAtEnd sp = sp {sourceSpanStartLine = sourceSpanEndLine sp, sourceSpanStartCol = sourceSpanEndCol sp, sourceSpanStartOffset = sourceSpanEndOffset sp}};
withSpanAnn :: (SourceSpan -> a -> b) -> TokParser a -> TokParser b;
withSpanAnn f parser = do {
  startState <- MP.getParserState;
  let {
  !startInput = MP.stateInput startState;
  !startOffset = MP.stateOffset startState;
  !inputStart = MP.pstateSourcePos (MP.statePosState startState)};
  out <- parser;
  endState <- MP.getParserState;
  let {
  !endInput = MP.stateInput endState;
  !endOffset = MP.stateOffset endState};
  pure (f (consumedSpan inputStart startInput startOffset endInput endOffset) out)};
withSpan :: TokParser (SourceSpan -> a) -> TokParser a;
withSpan = withSpanAnn (\parserSpan out -> out parserSpan);
inputStartSpan :: TokStream -> Maybe SourceSpan;
inputStartSpan ts | tokStreamEOFEmitted ts = Nothing | tok : _ <- tokStreamBuffer ts = Just (lexTokenSpan tok) | rawTok : _ <- tokStreamRawTokens ts = Just (lexTokenSpan rawTok) | otherwise = Nothing;
optionalSuffix :: TokParser b -> (a -> b -> a) -> TokParser a -> TokParser a;
optionalSuffix suffixParser attach parser = do {
  base <- parser;
  mSuffix <- MP.optional suffixParser;
  pure $ case mSuffix of {
  Just suffix -> attach base suffix;
  Nothing -> base}};
parens :: TokParser a -> TokParser a;
parens parser = expectedTok TkSpecialLParen *> parser <* expectedTok TkSpecialRParen;
braces :: TokParser a -> TokParser a;
braces parser = expectedTok TkSpecialLBrace *> parser <* closeAndExpectRBrace;
thQuoteParser :: (SourceSpan -> c -> c) -> LexTokenKind -> LexTokenKind -> TokParser a -> (a -> c) -> TokParser c;
thQuoteParser ann openTok closeTok bodyParser ctor = withSpanAnn ann $ do {
  expectedTok openTok;
  body <- bodyParser;
  expectedTok closeTok;
  pure (ctor body)};
closeAndExpectRBrace :: TokParser ();
closeAndExpectRBrace = expectedTok TkSpecialRBrace <|> do {
  closed <- closeImplicitLayout;
  if closed then expectedTok TkSpecialRBrace else MP.empty};
skipSemicolons :: TokParser ();
skipSemicolons = MP.skipMany (expectedTok TkSpecialSemicolon);
bracedSemiSep :: TokParser a -> TokParser [a];
bracedSemiSep = braces . layoutSemiSep;
bracedSemiSep1 :: TokParser a -> TokParser [a];
bracedSemiSep1 = braces . layoutSemiSep1;
plainSemiSep :: TokParser a -> TokParser [a];
plainSemiSep = layoutSemiSep;
plainSemiSep1 :: TokParser a -> TokParser [a];
plainSemiSep1 = layoutSemiSep1;
layoutSemiSep :: TokParser a -> TokParser [a];
layoutSemiSep parser = catMaybes <$> MP.sepBy (MP.optional parser) (expectedTok TkSpecialSemicolon);
layoutSemiSep1 :: TokParser a -> TokParser [a];
layoutSemiSep1 parser = do {
  items <- layoutSemiSep parser;
  case items of {
  [] -> MP.empty;
  _ -> pure items}};
contextItemParserWith :: TokParser Type -> TokParser Type -> TokParser Type;
contextItemParserWith typeParser typeAtomParser = withSpanAnn (TAnn . mkAnnotation) $ MP.try parenthesizedContextItemParser <|> MP.try kindSigContextItemParser <|> bareContextItemParser where {
  bareContextItemParser = do {
  name <- implicitParamNameParser;
  expectedTok TkReservedDoubleColon;
  TImplicitParam name <$> typeParser} <|> do {
  expectedTok TkKeywordUnderscore;
  pure TWildcard} <|> constraintTypeParser;
  parenthesizedContextItemParser = do {
  expectedTok TkSpecialLParen;
  item <- contextItemParserWith typeParser typeAtomParser;
  expectedTok TkSpecialRParen;
  guardNotFollowedByConstraintInfixOp;
  pure (TParen item)} where {
  guardNotFollowedByConstraintInfixOp = do {
  isFollowed <- fmap (either (const False) (const True)) . MP.observing . MP.try . MP.lookAhead $ constraintTypeInfixOperatorParser;
  guard (not isFollowed)}};
  kindSigContextItemParser :: TokParser Type;
  kindSigContextItemParser = do {
  guard =<< hasKindSignatureAtTopLevel;
  ty <- constraintTypeAppParser;
  expectedTok TkReservedDoubleColon;
  TKindSig ty <$> kindTypeParser};
  hasKindSignatureAtTopLevel :: TokParser Bool;
  hasKindSignatureAtTopLevel = MP.lookAhead (go 0) where {
  go :: Int -> TokParser Bool;
  go depth = do {
  tok <- anySingle;
  case lexTokenKind tok of {
  TkEOF -> pure False;
  TkReservedDoubleColon | depth == 0 -> pure True;
  TkReservedRightArrow | depth == 0 -> pure False;
  TkReservedDoubleArrow | depth == 0 -> pure False;
  TkSpecialComma | depth == 0 -> pure False;
  TkSpecialLParen -> go (depth + 1);
  TkSpecialRParen | depth > 0 -> go (depth - 1) | otherwise -> pure False;
  TkSpecialUnboxedLParen -> go (depth + 1);
  TkSpecialUnboxedRParen | depth > 0 -> go (depth - 1) | otherwise -> pure False;
  TkSpecialLBracket -> go (depth + 1);
  TkSpecialRBracket | depth > 0 -> go (depth - 1) | otherwise -> pure False;
  TkSpecialLBrace | lexTokenOrigin tok == InsertedLayout -> pure False;
  TkSpecialRBrace | lexTokenOrigin tok == InsertedLayout -> pure False;
  TkSpecialSemicolon -> pure False;
  TkReservedEquals -> pure False;
  TkReservedPipe -> pure False;
  TkReservedLeftArrow -> pure False;
  TkKeywordWhere -> pure False;
  TkKeywordDeriving -> pure False;
  TkKeywordInstance -> pure False;
  TkKeywordClass -> pure False;
  TkKeywordData -> pure False;
  TkKeywordNewtype -> pure False;
  TkKeywordType -> pure False;
  TkKeywordImport -> pure False;
  TkKeywordModule -> pure False;
  TkKeywordLet -> pure False;
  TkKeywordIn -> pure False;
  TkKeywordDo -> pure False;
  TkKeywordOf -> pure False;
  TkKeywordThen -> pure False;
  TkKeywordElse -> pure False;
  _ -> go depth}}};
  constraintTypeParser = do {
  first <- constraintTypeAppParser;
  rest <- MP.many ((,) <$> constraintTypeInfixOperatorParser <*> constraintTypeAppParser);
  pure (foldInfixR buildInfixType first rest)};
  constraintTypeAppParser = do {
  first <- typeAtomParser;
  rest <- MP.many constraintTypeAppArgParser;
  pure (foldl applyConstraintAppArg first rest)};
  constraintTypeAppArgParser = (Left <$> MP.try (expectedTok TkTypeApp *> (typeAtomParser >>= rejectBareConstraintImplicitParam))) <|> (Right <$> (typeAtomParser >>= rejectBareConstraintImplicitParam));
  applyConstraintAppArg fn (Left ty) = TTypeApp fn ty;
  applyConstraintAppArg fn (Right ty) = TApp fn ty;
  rejectBareConstraintImplicitParam ty = case peelTypeAnn ty of {
  TImplicitParam {} -> fail "implicit parameter type must be parenthesized";
  _ -> pure ty};
  kindTypeParser = do {
  first <- constraintTypeAppParser;
  rest <- MP.many ((,) <$> constraintTypeInfixOperatorParser <*> constraintTypeAppParser);
  let {
  baseType = foldInfixR buildInfixType first rest};
  mRhs <- optionalTokThen TkReservedRightArrow kindTypeParser;
  case mRhs of {
  Just rhs -> pure (TFun ArrowUnrestricted baseType rhs);
  Nothing -> pure baseType}};
  buildInfixType lhs ((op, promoted), rhs) = TInfix lhs op promoted rhs;
  constraintTypeInfixOperatorParser = MP.try promotedInfixOperatorParser <|> backtickConstraintOperatorParser <|> unpromotedInfixOperatorParser;
  backtickConstraintOperatorParser = MP.try $ do {
  expectedTok TkSpecialBacktick;
  op <- constraintOperatorIdentifierParser;
  expectedTok TkSpecialBacktick;
  pure (op, Unpromoted)};
  constraintOperatorIdentifierParser = tokenSatisfy "constraint operator identifier" $ \tok -> case lexTokenKind tok of {
  TkVarId name -> Just (mkNameAt tok Nothing NameVarId name);
  TkConId name -> Just (mkNameAt tok Nothing NameConId name);
  _ -> Nothing};
  unpromotedInfixOperatorParser = tokenSatisfy "type infix operator" $ \tok -> case lexTokenKind tok of {
  TkVarSym op | op /= "." && op /= "!" -> Just (mkNameAt tok Nothing NameVarSym op, Unpromoted);
  TkConSym op -> Just (mkNameAt tok Nothing NameConSym op, Unpromoted);
  TkQVarSym modName op -> Just (mkNameAt tok (Just modName) NameVarSym op, Unpromoted);
  TkQConSym modName op -> Just (mkNameAt tok (Just modName) NameConSym op, Unpromoted);
  _ -> Nothing};
  promotedInfixOperatorParser = do {
  expectedTok (TkVarSym "'");
  expectedTok TkReservedColon;
  pure (qualifyName Nothing (mkUnqualifiedName NameConSym ":"), Promoted)}};
contextItemsParserWith :: TokParser Type -> TokParser Type -> TokParser [Type];
contextItemsParserWith typeParser typeAtomParser = MP.try parenthesizedContextItemsParser <|> fmap pure (contextItemParserWith typeParser typeAtomParser) where {
  parenthesizedContextItemsParser = withSpanAnn annotateSingleItem $ do {
  items <- parens (listContextItemParser `MP.sepEndBy` expectedTok TkSpecialComma);
  guardNotFollowedByConstraintInfixOp;
  case items of {
  [] -> fail "empty constraint list in parens";
  [item] -> pure [TParen item];
  _ -> pure items}};
  annotateSingleItem sp items = case items of {
  [item] -> [typeAnnSpan sp item];
  _ -> items};
  listContextItemParser = MP.try quantifiedContextItemParser <|> contextItemParserWith typeParser typeAtomParser;
  quantifiedContextItemParser = do {
  guard =<< startsQuantifiedConstraint;
  typeParser};
  startsQuantifiedConstraint :: TokParser Bool;
  startsQuantifiedConstraint = MP.lookAhead (go (0 :: Int)) where {
  go depth = do {
  tok <- anySingle;
  case lexTokenKind tok of {
  TkEOF -> pure False;
  TkKeywordForall | depth == 0 -> pure True;
  TkReservedDoubleArrow | depth == 0 -> pure True;
  TkSpecialComma | depth == 0 -> pure False;
  TkSpecialLParen -> go (depth + 1);
  TkSpecialRParen | depth > 0 -> go (depth - 1) | otherwise -> pure False;
  TkSpecialUnboxedLParen -> go (depth + 1);
  TkSpecialUnboxedRParen | depth > 0 -> go (depth - 1) | otherwise -> pure False;
  TkSpecialLBracket -> go (depth + 1);
  TkSpecialRBracket | depth > 0 -> go (depth - 1) | otherwise -> pure False;
  TkSpecialLBrace | lexTokenOrigin tok == InsertedLayout -> pure False;
  TkSpecialRBrace | lexTokenOrigin tok == InsertedLayout -> pure False;
  TkSpecialSemicolon -> pure False;
  TkReservedEquals -> pure False;
  TkReservedPipe -> pure False;
  TkKeywordWhere -> pure False;
  _ -> go depth}}};
  guardNotFollowedByConstraintInfixOp = do {
  isFollowed <- fmap (either (const False) (const True)) . MP.observing . MP.try . MP.lookAhead $ constraintInfixOpStartParser;
  guard (not isFollowed)};
  constraintInfixOpStartParser = tokenSatisfy "constraint infix operator" $ \tok -> case lexTokenKind tok of {
  TkVarSym op | op /= "." && op /= "!" -> Just ();
  TkConSym _ -> Just ();
  TkQVarSym _ _ -> Just ();
  TkQConSym _ _ -> Just ();
  TkSpecialBacktick -> Just ();
  _ -> Nothing}};
contextParserWith :: TokParser Type -> TokParser Type -> TokParser [Type];
contextParserWith = contextItemsParserWith;
typedSignaturePrefixParser :: TokParser ty -> TokParser ([UnqualifiedName], ty);
typedSignaturePrefixParser typeParser = do {
  names <- binderNameParser `MP.sepBy1` expectedTok TkSpecialComma;
  expectedTok TkReservedDoubleColon;
  ty <- typeParser;
  pure (names, ty)};
typedBindingOrSignatureParser :: TokParser ty -> ([UnqualifiedName] -> ty -> a) -> (UnqualifiedName -> ty -> TokParser a) -> String -> TokParser a;
typedBindingOrSignatureParser typeParser signatureCtor bindingCtor singleBinderMsg = do {
  (names, ty) <- typedSignaturePrefixParser typeParser;
  nextKind <- lexTokenKind <$> peekToken;
  if nextKind == TkReservedEquals || nextKind == TkReservedPipe then case names of {
  [name] -> bindingCtor name ty;
  _ -> fail singleBinderMsg} else pure (signatureCtor names ty)};
functionHeadParserWith :: TokParser Pattern -> TokParser Pattern -> TokParser (MatchHeadForm, UnqualifiedName, [Pattern]);
functionHeadParserWith = functionHeadParserWithBinder functionBinderNameParser infixOperatorNameParser;
functionHeadParserWithBinder :: TokParser UnqualifiedName -> TokParser UnqualifiedName -> TokParser Pattern -> TokParser Pattern -> TokParser (MatchHeadForm, UnqualifiedName, [Pattern]);
functionHeadParserWithBinder binderParser infixOpParser infixPatternParser prefixPatternParser = do {
  (firstKind, secondKind) <- lookAhead $ do {
  first <- lexTokenKind <$> anySingle;
  second <- lexTokenKind <$> anySingle;
  pure (first, second)};
  case firstKind of {
  TkVarId {} | startsPrefixHead secondKind -> prefixHeadParser | otherwise -> MP.try infixHeadParser <|> prefixHeadParser;
  TkSpecialLParen -> MP.try parenthesizedInfixHeadParser <|> MP.try infixHeadParser <|> prefixHeadParser;
  _ -> infixHeadParser}} where {
  startsPrefixHead kind = case kind of {
  TkVarId {} -> True;
  TkConId {} -> True;
  TkQConId {} -> True;
  TkInteger {} -> True;
  TkFloat {} -> True;
  TkChar {} -> True;
  TkCharHash {} -> True;
  TkString {} -> True;
  TkStringHash {} -> True;
  TkSpecialLParen -> True;
  TkSpecialUnboxedLParen -> True;
  TkSpecialLBracket -> True;
  TkKeywordUnderscore -> True;
  TkPrefixBang -> True;
  TkPrefixTilde -> True;
  TkPrefixMinus -> True;
  TkQuasiQuote {} -> True;
  TkTypeApp -> True;
  TkReservedEquals -> True;
  TkReservedPipe -> True;
  _ -> False};
  prefixHeadParser = do {
  name <- binderParser;
  pats <- MP.many prefixPatternParser;
  pure (MatchHeadPrefix, name, pats)};
  infixHeadParser = do {
  lhsPat <- infixPatternParser;
  op <- infixOpParser;
  rhsPat <- infixPatternParser;
  pure (MatchHeadInfix, op, [lhsPat, rhsPat])};
  parenthesizedInfixHeadParser = do {
  expectedTok TkSpecialLParen;
  lhsPat <- infixPatternParser;
  op <- infixOpParser;
  rhsPat <- infixPatternParser;
  expectedTok TkSpecialRParen;
  tailPats <- MP.many prefixPatternParser;
  pure (MatchHeadInfix, op, [lhsPat, rhsPat] <> tailPats)}};
functionBinderNameParser :: TokParser UnqualifiedName;
functionBinderNameParser = variableIdentifierParser <|> parens variableOperatorParser where {
  variableIdentifierParser = tokenSatisfy "function binder" $ \tok -> case lexTokenKind tok of {
  TkVarId ident -> Just (mkUnqualifiedNameAt tok NameVarId ident);
  _ -> Nothing};
  variableOperatorParser = tokenSatisfy "variable operator" $ \tok -> case lexTokenKind tok of {
  TkVarSym ident -> Just (mkUnqualifiedNameAt tok NameVarSym ident);
  TkReservedAt -> Just (mkUnqualifiedNameAt tok NameVarSym "@");
  _ -> Nothing}};
functionBindValue :: MatchHeadForm -> UnqualifiedName -> [Pattern] -> Rhs Expr -> ValueDecl;
functionBindValue _headForm name [] rhs = PatternBind NoMultiplicityTag (PVar name) rhs;
functionBindValue headForm name pats rhs = FunctionBind name [Match {matchAnns = [], matchHeadForm = headForm, matchPats = pats, matchRhs = rhs}];
functionBindDecl :: MatchHeadForm -> UnqualifiedName -> [Pattern] -> Rhs Expr -> Decl;
functionBindDecl headForm name pats rhs = DeclValue (functionBindValue headForm name pats rhs);
isModuleName :: Text -> Bool;
isModuleName name = case T.splitOn "." name of {
  [] -> False;
  segments -> all isConstructorIdentifier segments};
isConstructorIdentifier :: Text -> Bool;
isConstructorIdentifier txt = case T.uncons txt of {
  Just (c, _) -> isUpper c;
  Nothing -> False};
isExtensionEnabled :: Extension -> TokParser Bool;
isExtensionEnabled ext = memberExtension ext . tokStreamExtensionSet <$> MP.getInput;
thAnyEnabled :: TokParser Bool;
thAnyEnabled = do {
  thEnabled <- isExtensionEnabled TemplateHaskellQuotes;
  thFullEnabled <- isExtensionEnabled TemplateHaskell;
  pure (thEnabled || thFullEnabled)};
asPatternParser :: TokParser Pattern -> TokParser Pattern;
asPatternParser bodyParser = withSpanAnn (PAnn . mkAnnotation) $ do {
  name <- MP.try (binderNameParser <* expectedTok TkReservedAt);
  PAs name <$> bodyParser};
tupleDelimsParser :: TokParser (TupleFlavor, LexTokenKind);
tupleDelimsParser = tokenSatisfy "symbol '(' or '(#'" $ \tok -> case lexTokenKind tok of {
  TkSpecialLParen -> Just (Boxed, TkSpecialRParen);
  TkSpecialUnboxedLParen -> Just (Unboxed, TkSpecialUnboxedRParen);
  _ -> Nothing};
recordFieldsWithWildcardsParser :: TokParser [a] -> TokParser ([a], Bool);
recordFieldsWithWildcardsParser fieldsParser = do {
  rwcEnabled <- isExtensionEnabled RecordWildCards;
  fields <- fieldsParser;
  if rwcEnabled then do {
  hasDotDot <- optionalTok TkReservedDotDot;
  if not hasDotDot then pure (fields, False) else do {
  _ <- optionalTok TkSpecialComma;
  pure (fields, True)}} else pure (fields, False)};
closeImplicitLayout :: TokParser Bool;
closeImplicitLayout = do {
  pst <- MP.getParserState;
  let {
  ts = MP.stateInput pst};
  case closeImplicitLayoutContext (tokStreamLayoutState ts) of {
  Nothing -> pure False;
  Just laySt' -> do {
  let {
  inserted = layoutBuffer laySt';
  laySt'' = laySt' {layoutBuffer = []}};
  MP.updateParserState (\s -> let {
  input = MP.stateInput s} in s {MP.stateInput = setTokStreamLayout laySt'' (inserted <> tokStreamBuffer input) input});
  pure True}}};
layoutSepEndBy :: TokParser a -> TokParser sep -> TokParser [a];
layoutSepEndBy p sep = layoutSepEndBy1 p sep <|> pure [];
layoutSepEndBy1 :: TokParser a -> TokParser sep -> TokParser [a];
layoutSepEndBy1 p sep = do {
  x <- p;
  rest <- MP.option [] $ do {
  _ <- layoutSep sep;
  layoutSepEndBy p sep};
  pure (x : rest)};
layoutSepBy1 :: TokParser a -> TokParser sep -> TokParser [a];
layoutSepBy1 p sep = do {
  x <- p;
  rest <- MP.many $ do {
  _ <- layoutSep sep;
  p};
  pure (x : rest)};
layoutSep :: TokParser sep -> TokParser sep;
layoutSep sep = MP.try sep <|> do {
  closed <- closeImplicitLayout;
  if closed then sep else MP.empty};
drainParseErrors :: TokParser [MPE.ParseError TokStream ParserErrorComponent];
drainParseErrors = do {
  st <- MP.getParserState;
  let {
  errs = MP.stateParseErrors st};
  MP.updateParserState (\s -> s {MP.stateParseErrors = []});
  pure errs};
lazy :: (Monoid a) => TokParser a -> TokParser (a, MP.State TokStream ParserErrorComponent);
lazy parser = do {
  initialState <- MP.getParserState;
  let {
  ~(value, finalState) = case MP.runParser' captureResult initialState of {
  (_, Right parsed) -> parsed;
  (failedState, Left bundle) -> (mempty, failedState {MP.stateParseErrors = reverse (NE.toList (MPE.bundleErrors bundle))})}};
  pure (value, finalState)} where {
  captureResult = do {
  value <- parser;
  finalState <- MP.getParserState;
  MP.updateParserState (\state -> state {MP.stateParseErrors = []});
  pure (value, finalState)}};
startsWithContextType :: TokParser Bool;
startsWithContextType = MP.lookAhead (go []) where {
  go :: [LexTokenKind] -> TokParser Bool;
  go [] = do {
  tok <- anySingle;
  case lexTokenKind tok of {
  TkEOF -> pure False;
  TkReservedDoubleArrow -> pure True;
  TkReservedDoubleColon -> pure False;
  TkReservedRightArrow -> pure False;
  TkReservedEquals -> pure False;
  TkSpecialComma -> pure False;
  TkSpecialSemicolon -> pure False;
  TkReservedPipe -> pure False;
  TkSpecialRParen -> pure False;
  TkSpecialUnboxedRParen -> pure False;
  TkSpecialRBracket -> pure False;
  TkSpecialRBrace -> pure False;
  TkTHExpQuoteClose -> pure False;
  TkTHTypedQuoteClose -> pure False;
  TkSpecialLParen -> go [TkSpecialRParen];
  TkSpecialUnboxedLParen -> go [TkSpecialUnboxedRParen];
  TkSpecialLBracket -> go [TkSpecialRBracket];
  TkTHExpQuoteOpen -> go [TkTHExpQuoteClose];
  TkTHTypedQuoteOpen -> go [TkTHTypedQuoteClose];
  TkTHDeclQuoteOpen -> go [TkTHExpQuoteClose];
  TkTHTypeQuoteOpen -> go [TkTHExpQuoteClose];
  TkTHPatQuoteOpen -> go [TkTHExpQuoteClose];
  TkSpecialLBrace -> go [TkSpecialRBrace];
  TkKeywordThen -> pure False;
  TkKeywordElse -> pure False;
  TkKeywordOf -> pure False;
  TkKeywordIn -> pure False;
  TkKeywordInstance -> pure False;
  TkKeywordWhere -> pure False;
  TkKeywordDeriving -> pure False;
  TkKeywordClass -> pure False;
  TkKeywordData -> pure False;
  TkKeywordNewtype -> pure False;
  _ -> go []}};
  go stack@(expectedClose : rest) = do {
  tok <- anySingle;
  case lexTokenKind tok of {
  TkEOF -> pure False;
  kind | kind == expectedClose -> case rest of {
  [] -> go [];
  _ -> go rest};
  TkSpecialLParen -> go (TkSpecialRParen : stack);
  TkSpecialUnboxedLParen -> go (TkSpecialUnboxedRParen : stack);
  TkSpecialLBracket -> go (TkSpecialRBracket : stack);
  TkTHExpQuoteOpen -> go (TkTHExpQuoteClose : stack);
  TkTHTypedQuoteOpen -> go (TkTHTypedQuoteClose : stack);
  TkTHDeclQuoteOpen -> go (TkTHExpQuoteClose : stack);
  TkTHTypeQuoteOpen -> go (TkTHExpQuoteClose : stack);
  TkTHPatQuoteOpen -> go (TkTHExpQuoteClose : stack);
  TkSpecialLBrace -> go (TkSpecialRBrace : stack);
  _ -> go stack}}};
startsWithTypeSig :: TokParser Bool;
startsWithTypeSig = fmap (either (const False) (const True)) . MP.observing . MP.try . MP.lookAhead $ do {
  _ <- sigBinderNameParser;
  let {
  moreNames = (expectedTok TkSpecialComma *> sigBinderNameParser *> moreNames) <|> pure ()};
  moreNames;
  expectedTok TkReservedDoubleColon} where {
  sigBinderNameParser = binderNameParser <|> parens sigOperatorParser;
  sigOperatorParser = tokenSatisfy "signature operator" $ \tok -> case lexTokenKind tok of {
  TkVarSym op -> Just (mkUnqualifiedNameAt tok NameVarSym op);
  TkConSym op -> Just (mkUnqualifiedNameAt tok NameConSym op);
  TkReservedColon -> Just (mkUnqualifiedNameAt tok NameConSym ":");
  _ -> Nothing}};
startsWithAsPattern :: TokParser Bool;
startsWithAsPattern = fmap (either (const False) (const True)) . MP.observing . MP.try . MP.lookAhead $ do {
  _ <- binderNameParser;
  expectedTok TkReservedAt};
startsWithTypeBinder :: TokParser Bool;
startsWithTypeBinder = fmap (either (const False) (const True)) . MP.observing . MP.try . MP.lookAhead $ do {
  expectedTok TkTypeApp;
  _ <- lowerIdentifierParser <|> (expectedTok TkKeywordUnderscore $> "_");
  pure ()};
isConLikeName :: Name -> Bool;
isConLikeName = isConLikeNameType . nameType;
isConLikeNameType :: NameType -> Bool;
isConLikeNameType NameConId = True;
isConLikeNameType NameConSym = True;
isConLikeNameType _ = False;
liftCheck :: Either Text a -> TokParser a;
liftCheck (Right a) = pure a;
liftCheck (Left msg) = fail (T.unpack msg);
startsInfixOperator :: LexTokenKind -> Bool;
startsInfixOperator kind = case kind of {
  TkVarSym {} -> True;
  TkConSym {} -> True;
  TkPrefixPercent -> True;
  TkQVarSym {} -> True;
  TkQConSym {} -> True;
  TkMinusOperator -> True;
  TkReservedColon -> True;
  TkSpecialBacktick -> True;
  _ -> False};
infixOperatorParser :: TokParser Name;
infixOperatorParser = symbolicOperatorParser <|> backtickIdentifierOperatorParser where {
  symbolicOperatorParser = tokenSatisfy "infix operator" $ \tok -> case lexTokenKind tok of {
  TkVarSym op -> Just (mkNameAt tok Nothing NameVarSym op);
  TkConSym op -> Just (mkNameAt tok Nothing NameConSym op);
  TkPrefixPercent -> Just (mkNameAt tok Nothing NameVarSym "%");
  TkQVarSym modName op -> Just (mkNameAt tok (Just modName) NameVarSym op);
  TkQConSym modName op -> Just (mkNameAt tok (Just modName) NameConSym op);
  TkMinusOperator -> Just (mkNameAt tok Nothing NameVarSym "-");
  TkReservedColon -> Just (mkNameAt tok Nothing NameConSym ":");
  _ -> Nothing};
  backtickIdentifierOperatorParser = expectedTok TkSpecialBacktick *> identifierNameParser <* expectedTok TkSpecialBacktick};
foldInfixL :: (a -> (op, a) -> a) -> a -> [(op, a)] -> a;
foldInfixL = foldl;
foldInfixR :: (a -> (op, a) -> a) -> a -> [(op, a)] -> a;
foldInfixR _ lhs [] = lhs;
foldInfixR build lhs ((op, rhs) : rest) = build lhs (op, foldInfixR build rhs rest)
}
