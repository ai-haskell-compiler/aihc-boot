{-# LANGUAGE OverloadedStrings #-}
module Aihc.Parser.Internal.Pattern (patternParser, patternParserWithTypeSigParser, caseAltPatternParser, patParser, lpatParser, apatParser, literalParser) where {
import Aihc.Parser.Internal.CheckPattern (checkPattern);
import Aihc.Parser.Internal.Common;
import Aihc.Parser.Internal.Expr (atomExprParser, exprParser);
import Aihc.Parser.Internal.Type (typeParser);
import Aihc.Parser.Lex (LexToken(..), LexTokenKind(..), lexTokenKind, lexTokenSpan, lexTokenText);
import Aihc.Parser.Syntax;
import Aihc.Parser.Types (ParserErrorComponent(..), TokStream(..), mkFoundToken);
import Text.Megaparsec (anySingle, lookAhead, (<|>));
import Text.Megaparsec qualified as MP;
patternParser :: TokParser Pattern;
patternParser = patternParserWithTypeSigParser typeParser;
patternParserWithTypeSigParser :: TokParser Type -> TokParser Pattern;
patternParserWithTypeSigParser typeSigParser = label "pattern" $ optionalSuffix (expectedTok TkReservedDoubleColon *> typeSigParser) PTypeSig patParser;
caseAltPatternParser :: TokParser Pattern;
caseAltPatternParser = patParser;
patParser :: TokParser Pattern;
patParser = do {
  lhs <- lpatParser;
  rest <- MP.many ((,) <$> conOperatorParser <*> lpatParser);
  pure (foldInfixL buildInfixPattern lhs rest)};
buildInfixPattern :: Pattern -> (Name, Pattern) -> Pattern;
buildInfixPattern lhs (op, rhs) = PInfix lhs op rhs;
conOperatorParser :: TokParser Name;
conOperatorParser = symbolicConOp <|> backtickConOp where {
  symbolicConOp = tokenSatisfy "constructor operator" $ \tok -> case lexTokenKind tok of {
  TkConSym op -> Just (mkNameAt tok Nothing NameConSym op);
  TkQConSym modName op -> Just (mkNameAt tok (Just modName) NameConSym op);
  TkReservedColon -> Just (mkNameAt tok Nothing NameConSym ":");
  _ -> Nothing};
  backtickConOp = MP.try $ expectedTok TkSpecialBacktick *> constructorNameParser <* expectedTok TkSpecialBacktick};
lpatParser :: TokParser Pattern;
lpatParser = negativeLiteralPatternParser <|> do {
  first <- apatParser;
  if isPatternAppHead first then foldl buildPatternApp first <$> MP.many apatParser else pure first};
buildPatternApp :: Pattern -> Pattern -> Pattern;
buildPatternApp lhs rhs = case peelPatternAnn lhs of {
  PCon name typeArgs args -> PCon name typeArgs (args <> [rhs]);
  PBuiltinCon con typeArgs args -> PBuiltinCon con typeArgs (args <> [rhs]);
  _ -> lhs};
apatParser :: TokParser Pattern;
apatParser = label "pattern atom" $ asApatParser <|> nonAsApatParser;
asApatParser :: TokParser Pattern;
asApatParser = asPatternParser apatParser;
nonAsApatParser :: TokParser Pattern;
nonAsApatParser = do {
  tok <- peekToken;
  case lexTokenKind tok of {
  TkTypeApp -> do {
  typeAbstractionsEnabled <- isExtensionEnabled TypeAbstractions;
  if typeAbstractionsEnabled then typeBinderPatternParser else varOrConPatternParser};
  TkPrefixBang -> strictPatternParser;
  TkPrefixTilde -> irrefutablePatternParser;
  TkKeywordType -> do {
  explicitNamespacesEnabled <- isExtensionEnabled ExplicitNamespaces;
  requiredTypeArgumentsEnabled <- isExtensionEnabled RequiredTypeArguments;
  if explicitNamespacesEnabled || requiredTypeArgumentsEnabled then explicitTypePatternParser else varOrConPatternParser};
  TkQuasiQuote {} -> quasiQuotePatternParser;
  TkTHSplice -> do {
  thAny <- thAnyEnabled;
  if thAny then thSplicePatternParser else varOrConPatternParser};
  TkKeywordUnderscore -> wildcardPatternParser;
  TkInteger {} -> literalPatternParser;
  TkFloat {} -> literalPatternParser;
  TkChar {} -> literalPatternParser;
  TkCharHash {} -> literalPatternParser;
  TkString {} -> literalPatternParser;
  TkStringHash {} -> literalPatternParser;
  TkSpecialLBracket -> listPatternParser;
  TkSpecialLParen -> parenOrTuplePatternParser;
  TkSpecialUnboxedLParen -> parenOrTuplePatternParser;
  _ -> varOrConPatternParser}};
typeBinderPatternParser :: TokParser Pattern;
typeBinderPatternParser = withSpanAnn (PAnn . mkAnnotation) $ do {
  expectedTok TkTypeApp;
  PTypeBinder <$> visibleTypeBinderCoreParser};
explicitTypePatternParser :: TokParser Pattern;
explicitTypePatternParser = withSpanAnn (PAnn . mkAnnotation) $ do {
  expectedTok TkKeywordType;
  PTypeSyntax TypeSyntaxExplicitNamespace <$> typeParser};
strictPatternParser :: TokParser Pattern;
strictPatternParser = withSpanAnn (PAnn . mkAnnotation) $ do {
  expectedTok TkPrefixBang;
  PStrict <$> apatParser};
irrefutablePatternParser :: TokParser Pattern;
irrefutablePatternParser = withSpanAnn (PAnn . mkAnnotation) $ do {
  expectedTok TkPrefixTilde;
  PIrrefutable <$> apatParser};
negativeLiteralPatternParser :: TokParser Pattern;
negativeLiteralPatternParser = MP.try $ withSpanAnn (PAnn . mkAnnotation) $ do {
  expectedTok (TkVarSym "-");
  PNegLit <$> numericLiteralParser};
numericLiteralParser :: TokParser Literal;
numericLiteralParser = intLiteralParser <|> floatLiteralParser;
wildcardPatternParser :: TokParser Pattern;
wildcardPatternParser = tokenSatisfy "wildcard" $ \tok -> case lexTokenKind tok of {
  TkKeywordUnderscore -> Just (PAnn (mkAnnotation (lexTokenSpan tok)) PWildcard);
  _ -> Nothing};
literalPatternParser :: TokParser Pattern;
literalPatternParser = tokenSatisfy "literal" $ \tok -> do {
  lit <- literalFromToken tok;
  let {
  ann = mkAnnotation (lexTokenSpan tok)};
  pure (PAnn ann (PLit (LitAnn ann lit)))};
quasiQuotePatternParser :: TokParser Pattern;
quasiQuotePatternParser = tokenSatisfy "quasi quote" $ \tok -> case lexTokenKind tok of {
  TkQuasiQuote quoter body -> Just (PAnn (mkAnnotation (lexTokenSpan tok)) (PQuasiQuote quoter body));
  _ -> Nothing};
literalParser :: TokParser Literal;
literalParser = intLiteralParser <|> floatLiteralParser <|> charLiteralParser <|> stringLiteralParser;
tokenLiteralParser :: String -> (LexToken -> Maybe Literal) -> TokParser Literal;
tokenLiteralParser expected matchToken = tokenSatisfy expected $ \tok -> LitAnn (mkAnnotation (lexTokenSpan tok)) <$> matchToken tok;
literalFromToken :: LexToken -> Maybe Literal;
literalFromToken tok = intLiteralFromToken tok <|> floatLiteralFromToken tok <|> charLiteralFromToken tok <|> stringLiteralFromToken tok;
intLiteralParser :: TokParser Literal;
intLiteralParser = tokenLiteralParser "integer literal" intLiteralFromToken;
intLiteralFromToken :: LexToken -> Maybe Literal;
intLiteralFromToken tok = case lexTokenKind tok of {
  TkInteger i nt -> Just (LitInt i nt (lexTokenText tok));
  _ -> Nothing};
floatLiteralParser :: TokParser Literal;
floatLiteralParser = tokenLiteralParser "floating literal" floatLiteralFromToken;
floatLiteralFromToken :: LexToken -> Maybe Literal;
floatLiteralFromToken tok = case lexTokenKind tok of {
  TkFloat x ft -> Just (LitFloat x ft (lexTokenText tok));
  _ -> Nothing};
charLiteralParser :: TokParser Literal;
charLiteralParser = tokenLiteralParser "character literal" charLiteralFromToken;
charLiteralFromToken :: LexToken -> Maybe Literal;
charLiteralFromToken tok = case lexTokenKind tok of {
  TkChar x -> Just (LitChar x (lexTokenText tok));
  TkCharHash x txt -> Just (LitCharHash x txt);
  _ -> Nothing};
stringLiteralParser :: TokParser Literal;
stringLiteralParser = tokenLiteralParser "string literal" stringLiteralFromToken;
stringLiteralFromToken :: LexToken -> Maybe Literal;
stringLiteralFromToken tok = case lexTokenKind tok of {
  TkString x -> Just (LitString x (lexTokenText tok));
  TkStringHash x txt -> Just (LitStringHash x txt);
  _ -> Nothing};
thSplicePatternParser :: TokParser Pattern;
thSplicePatternParser = withSpanAnn (PAnn . mkAnnotation) $ do {
  expectedTok TkTHSplice;
  tok <- peekToken;
  case lexTokenKind tok of {
  TkKeywordType -> MP.empty;
  _ -> PSplice <$> atomExprParser}};
visibleTypeBinderCoreParser :: TokParser TyVarBinder;
visibleTypeBinderCoreParser = withSpan $ (do {
  ident <- tyVarNameParser;
  pure (\span' -> TyVarBinder [mkAnnotation span'] ident Nothing TyVarBSpecified TyVarBInvisible)}) <|> (do {
  expectedTok TkSpecialLParen;
  ident <- tyVarNameParser;
  expectedTok TkReservedDoubleColon;
  kind <- typeParser;
  expectedTok TkSpecialRParen;
  pure (\span' -> TyVarBinder [mkAnnotation span'] ident (Just kind) TyVarBSpecified TyVarBInvisible)});
varOrConPatternParser :: TokParser Pattern;
varOrConPatternParser = do {
  (tok, name) <- identifierNameWithTokenParser;
  mNextTok <- peekTokenMaybe;
  let {
  ann = mkAnnotation (lexTokenSpan tok)};
  case mNextTok of {
  Just nextTok | isConLikeName name && lexTokenKind nextTok == TkSpecialLBrace -> do {
  (fields, hasWildcard) <- braces recordPatternFieldListParser;
  endInput <- MP.getInput;
  let {
  startSpan = lexTokenSpan tok;
  recordSpan = maybe startSpan (mergeSourceSpans startSpan . lexTokenSpan) (tokStreamPrevToken endInput)};
  pure (PAnn (mkAnnotation recordSpan) (PRecord name fields hasWildcard))};
  _ -> pure $ PAnn ann $ if isConLikeName name then PCon name [] [] else PVar (nameToUnqualified name)}};
recordFieldPatternParser :: TokParser (RecordField Pattern);
recordFieldPatternParser = do {
  field <- recordFieldNameParser;
  hasEquals <- optionalTok TkReservedEquals;
  if hasEquals then do {
  pat <- subpatternWithBareViewParser;
  pure (RecordField field pat False)} else pure (RecordField field (PVar (nameToUnqualified field)) True)};
recordPatternFieldListParser :: TokParser ([RecordField Pattern], Bool);
recordPatternFieldListParser = recordFieldsWithWildcardsParser (recordFieldPatternParser `MP.sepEndBy` expectedTok TkSpecialComma);
listPatternParser :: TokParser Pattern;
listPatternParser = withSpanAnn (PAnn . mkAnnotation) $ do {
  expectedTok TkSpecialLBracket;
  elems <- listPatternElementParser `MP.sepBy` expectedTok TkSpecialComma;
  expectedTok TkSpecialRBracket;
  pure (PList elems)} where {
  listPatternElementParser :: TokParser Pattern;
  listPatternElementParser = subpatternWithBareViewParser};
subpatternWithBareViewParser :: TokParser Pattern;
subpatternWithBareViewParser = do {
  mResult <- MP.optional . MP.try $ do {
  expr <- exprParser;
  tok <- peekToken;
  case lexTokenKind tok of {
  TkReservedRightArrow -> pure (Left expr);
  TkSpecialComma -> Right <$> liftCheck (checkPattern expr);
  TkSpecialRBracket -> Right <$> liftCheck (checkPattern expr);
  TkSpecialRBrace -> Right <$> liftCheck (checkPattern expr);
  TkSpecialRParen -> Right <$> liftCheck (checkPattern expr);
  TkSpecialUnboxedRParen -> Right <$> liftCheck (checkPattern expr);
  TkReservedPipe -> Right <$> liftCheck (checkPattern expr);
  _ -> fail "incomplete element parse"}};
  case mResult of {
  Just (Left expr) -> do {
  expectedTok TkReservedRightArrow;
  PView expr <$> subpatternWithBareViewParser};
  Just (Right pat) -> pure pat;
  Nothing -> patternParser}};
parenOrTuplePatternParser :: TokParser Pattern;
parenOrTuplePatternParser = withSpanAnn (PAnn . mkAnnotation) $ do {
  (tupleFlavor, closeTok) <- tupleDelimsParser;
  mNextTok <- peekTokenMaybe;
  case fmap lexTokenKind mNextTok of {
  Just nextKind | nextKind == closeTok -> unitPatternParser tupleFlavor closeTok | nextKind == TkSpecialComma -> tupleConstructorPatternParser tupleFlavor closeTok | tupleFlavor == Unboxed && nextKind == TkReservedPipe -> parseUnboxedSumPatLeadingBars closeTok;
  _ -> do {
  mView <- if tupleFlavor == Boxed then viewPatternParser closeTok else pure Nothing;
  case mView of {
  Just mkView -> pure mkView;
  Nothing -> tupleOrParenPatternParser tupleFlavor closeTok}}}} where {
  parenOrSymConParser isBareOperator inner = do {
  case peelPatternAnn inner of {
  PVar name | isBareOperator, unqualifiedNameType name == NameVarSym -> pure inner;
  PCon con [] [] | isBareOperator, nameType con == NameConSym -> do {
  mBrace <- MP.optional . lookAhead $ anySingle;
  case fmap lexTokenKind mBrace of {
  Just TkSpecialLBrace -> do {
  (fields, hasWildcard) <- braces recordPatternFieldListParser;
  pure (PRecord con fields hasWildcard)};
  _ -> pure inner}};
  _ -> pure (PParen inner)}};
  unitPatternParser tupleFlavor closeTok = do {
  expectedTok closeTok;
  pure (PTuple tupleFlavor [])};
  tupleConstructorPatternParser tupleFlavor closeTok = do {
  commas <- MP.some (expectedTok TkSpecialComma);
  expectedTok closeTok;
  pure (PBuiltinCon (BuiltinTuple tupleFlavor (length commas + 1)) [] [])};
  viewPatternParser :: LexTokenKind -> TokParser (Maybe Pattern);
  viewPatternParser closeTok = MP.optional . MP.try $ do {
  expr <- exprParser <* lookAhead (expectedTok TkReservedRightArrow);
  expectedTok TkReservedRightArrow;
  inner <- subpatternWithBareViewParser;
  expectedTok closeTok;
  pure (PParen (PView expr inner))};
  parenPatElementParser :: TokParser (Bool, Pattern);
  parenPatElementParser = do {
  tok <- peekToken;
  case lexTokenKind tok of {
  TkPrefixBang -> (False,) <$> patternParser;
  TkPrefixTilde -> (False,) <$> patternParser;
  TkVarSym {} -> operatorOrExprPatternParser;
  TkConSym {} -> operatorOrExprPatternParser;
  TkQConSym {} -> operatorOrExprPatternParser;
  TkReservedColon -> operatorOrExprPatternParser;
  TkReservedAt -> operatorOrExprPatternParser;
  _ -> do {
  isAs <- startsWithAsPattern;
  if isAs then (False,) <$> patternParser else (False,) <$> subpatternWithBareViewParser}}} where {
  operatorOrExprPatternParser :: TokParser (Bool, Pattern);
  operatorOrExprPatternParser = do {
  mNext <- MP.optional . lookAhead . MP.try $ do {
  _ <- anySingle;
  peekToken};
  case fmap lexTokenKind mNext of {
  Just TkSpecialRParen -> (True,) <$> operatorPatternParser;
  Just TkSpecialUnboxedRParen -> (True,) <$> operatorPatternParser;
  Just TkSpecialComma -> (True,) <$> operatorPatternParser;
  Just TkReservedPipe -> (True,) <$> operatorPatternParser;
  _ -> (False,) <$> subpatternWithBareViewParser}};
  operatorPatternParser :: TokParser Pattern;
  operatorPatternParser = do {
  tok' <- anySingle;
  let {
  ann = mkAnnotation (lexTokenSpan tok')};
  case lexTokenKind tok' of {
  TkVarSym op -> pure (PAnn ann (PVar (mkUnqualifiedNameAt tok' NameVarSym op)));
  TkConSym op -> pure (PAnn ann (PCon (mkNameAt tok' Nothing NameConSym op) [] []));
  TkQConSym modName op -> pure (PAnn ann (PCon (mkNameAt tok' (Just modName) NameConSym op) [] []));
  TkReservedColon -> pure (PAnn ann (PCon (mkNameAt tok' Nothing NameConSym ":") [] []));
  TkReservedAt -> pure (PAnn ann (PVar (mkUnqualifiedNameAt tok' NameVarSym "@")));
  _ -> MP.customFailure UnexpectedTokenExpecting {unexpectedFound = Just (mkFoundToken tok'), unexpectedExpecting = "operator token", unexpectedContext = []}}}};
  tupleOrParenPatternParser tupleFlavor closeTok = do {
  (isBareOp, first) <- parenPatElementParser;
  hasComma <- optionalTok TkSpecialComma;
  if hasComma then do {
  (_, second) <- parenPatElementParser;
  more <- MP.many (expectedTok TkSpecialComma *> (snd <$> parenPatElementParser));
  expectedTok closeTok;
  pure (PTuple tupleFlavor (first : second : more))} else do {
  hasPipe <- if tupleFlavor == Unboxed then optionalTok TkReservedPipe else pure False;
  if hasPipe then do {
  trailingBars <- MP.many (expectedTok TkReservedPipe);
  expectedTok closeTok;
  let {
  arity = 2 + length trailingBars};
  pure (PUnboxedSum 0 arity first)} else do {
  expectedTok closeTok;
  if tupleFlavor == Boxed then parenOrSymConParser isBareOp first else pure (PTuple Unboxed [first])}}};
  parseUnboxedSumPatLeadingBars closeTok = do {
  _ <- expectedTok TkReservedPipe;
  leadingBars <- MP.many (MP.try (expectedTok TkReservedPipe));
  let {
  altIdx = 1 + length leadingBars};
  (_, inner) <- parenPatElementParser;
  trailingBars <- MP.many (expectedTok TkReservedPipe);
  expectedTok closeTok;
  let {
  arity = altIdx + 1 + length trailingBars};
  pure (PUnboxedSum altIdx arity inner)}};
isPatternAppHead :: Pattern -> Bool;
isPatternAppHead pat = case peelPatternAnn pat of {
  PCon {} -> True;
  PBuiltinCon {} -> True;
  PVar name -> isConLikeNameType (unqualifiedNameType name);
  _ -> False}
}
