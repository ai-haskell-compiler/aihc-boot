{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Aihc.Parser.Internal.Expr (exprParser, atomExprParser, cmdArrAppLhsParser, lexpParser, equationRhsParser, caseRhsParserWithBodyParser, parseLetDeclsParser, parseLetDeclsStmtParser) where {
import Aihc.Parser.Internal.CheckPattern (checkPattern);
import Aihc.Parser.Internal.Cmd (cmdParser);
import Aihc.Parser.Internal.Common;
import Aihc.Parser.Internal.Decl (declParser, fixityDeclParser, pragmaDeclParser);
import Aihc.Parser.Internal.Pattern (apatParser, caseAltPatternParser, patParser, patternParser);
import Aihc.Parser.Internal.Type (typeAtomParser, typeParser, typeSignatureParser);
import Aihc.Parser.Lex (LexToken(..), LexTokenKind(..), lexTokenKind, lexTokenSpan, lexTokenText);
import Aihc.Parser.Syntax;
import Aihc.Parser.Types (ParserErrorComponent(..), mkFoundToken);
import Control.Monad (guard);
import Data.Functor (($>));
import Data.Maybe (fromMaybe);
import Data.Text (Text);
import Text.Megaparsec (anySingle, lookAhead, (<|>));
import Text.Megaparsec qualified as MP;
exprOrPatternBindParser :: TokParser Expr -> TokParser Expr -> (Pattern -> Expr -> a) -> (Expr -> a) -> TokParser a;
exprOrPatternBindParser exprP rhsP bindCtor exprCtor = do {
  lhs <- MP.try exprOrPattern <|> patternBind;
  case lhs of {
  Left pat -> bindCtor pat <$> rhsP;
  Right expr -> pure (exprCtor expr)}} where {
  exprOrPattern = do {
  expr <- exprP;
  mKind <- peekTokenKind;
  case mKind of {
  Just TkReservedAt -> MP.empty;
  Just TkPrefixBang -> MP.empty;
  Just TkPrefixTilde -> MP.empty;
  Just TkReservedDoubleColon -> MP.empty;
  _ -> pure ()};
  hasArrow <- optionalTok TkReservedLeftArrow;
  if hasArrow then Left <$> liftCheck (checkPattern expr) else pure (Right expr)};
  patternBind = do {
  pat <- patternParser;
  expectedTok TkReservedLeftArrow;
  pure (Left pat)}};
exprParser :: TokParser Expr;
exprParser = exprParserWithTypeSigParser typeSignatureParser;
exprParserWithTypeSigParser :: TokParser Type -> TokParser Expr;
exprParserWithTypeSigParser typeSigParser = label "expression" $ exprCoreParserWithTypeSigParser typeSigParser;
exprCoreParserWithoutTypeSig :: TokParser Expr;
exprCoreParserWithoutTypeSig = do {
  mSCC <- optionalHiddenPragma getSCCPragma;
  case mSCC of {
  Just sccPragma -> EPragma sccPragma <$> exprCoreParserWithoutTypeSig;
  Nothing -> exprCoreParserWithoutTypeSigBody}};
exprCoreParserWithoutTypeSigBody :: TokParser Expr;
exprCoreParserWithoutTypeSigBody = exprInfixChainParser lexpParser;
exprCoreParserWithTypeSigParser :: TokParser Type -> TokParser Expr;
exprCoreParserWithTypeSigParser typeSigParser = do {
  optionalSuffix (expectedTok TkReservedDoubleColon *> typeSigParser) ETypeSig exprCoreParserWithoutTypeSig};
cmdArrAppLhsParser :: TokParser Expr;
cmdArrAppLhsParser = exprInfixChainParser (lexpParserWith CmdArrAppLhsAtom);
data AtomContext = NormalExprAtom | CmdArrAppLhsAtom deriving (Eq);
maybeViewPattern :: Expr -> TokParser Expr;
maybeViewPattern lhs = do {
  hasArrow <- optionalTok TkReservedRightArrow;
  if hasArrow then EViewPat lhs <$> texprParser else pure lhs};
texprParser :: TokParser Expr;
texprParser = exprParser >>= maybeViewPattern;
ifExprParser :: TokParser Expr;
ifExprParser = do {
  expectedTok TkKeywordIf;
  multiWayIfExprParser <|> classicIfExprParser};
classicIfExprParser :: TokParser Expr;
classicIfExprParser = withSpanAnn (EAnn . mkAnnotation) $ do {
  cond <- exprParser;
  skipSemicolons;
  expectedTok TkKeywordThen;
  yes <- exprParser;
  skipSemicolons;
  expectedTok TkKeywordElse;
  EIf cond yes <$> exprParser};
multiWayIfExprParser :: TokParser Expr;
multiWayIfExprParser = withSpanAnn (EAnn . mkAnnotation) $ do {
  EMultiWayIf <$> braces (MP.some multiWayIfAlternative)};
multiWayIfAlternative :: TokParser (GuardedRhs Expr);
multiWayIfAlternative = withSpan $ do {
  expectedTok TkReservedPipe;
  guards <- layoutSepBy1 (guardQualifierParser RhsArrowCase) (expectedTok TkSpecialComma);
  expectedTok TkReservedRightArrow;
  body <- exprParser;
  pure $ \span' -> GuardedRhs {guardedRhsAnns = [mkAnnotation span'], guardedRhsGuards = guards, guardedRhsBody = body}};
doExprParser :: TokParser Expr;
doExprParser = withSpanAnn (EAnn . mkAnnotation) $ do {
  expectedTok TkKeywordDo;
  stmts <- bracedSemiSep1 doStmtParser;
  pure (EDo stmts DoPlain)};
mdoExprParser :: TokParser Expr;
mdoExprParser = withSpanAnn (EAnn . mkAnnotation) $ do {
  expectedTok TkKeywordMdo;
  stmts <- bracedSemiSep1 doStmtParser;
  pure (EDo stmts DoMdo)};
qualifiedDoExprParser :: TokParser Expr;
qualifiedDoExprParser = withSpanAnn (EAnn . mkAnnotation) $ do {
  modName <- tokenSatisfy "qualified do" $ \tok -> case lexTokenKind tok of {
  TkQualifiedDo m -> Just m;
  _ -> Nothing};
  stmts <- bracedSemiSep1 doStmtParser;
  pure (EDo stmts (DoQualified modName))};
qualifiedMdoExprParser :: TokParser Expr;
qualifiedMdoExprParser = withSpanAnn (EAnn . mkAnnotation) $ do {
  modName <- tokenSatisfy "qualified mdo" $ \tok -> case lexTokenKind tok of {
  TkQualifiedMdo m -> Just m;
  _ -> Nothing};
  stmts <- bracedSemiSep1 doStmtParser;
  pure (EDo stmts (DoQualifiedMdo modName))};
procExprParser :: TokParser Expr;
procExprParser = withSpanAnn (EAnn . mkAnnotation) $ do {
  expectedTok TkKeywordProc;
  pat <- region "while parsing proc pattern" apatParser;
  expectedTok TkReservedRightArrow;
  body <- region "while parsing proc body" cmdParser;
  pure (EProc pat body)};
doStmtParser :: TokParser (DoStmt Expr);
doStmtParser = do {
  tok <- peekToken;
  case lexTokenKind tok of {
  TkKeywordLet -> MP.try doLetStmtParser <|> doBindOrExprStmtParser;
  TkKeywordRec -> doRecStmtParser;
  _ -> doBindOrExprStmtParser}};
doBindOrExprStmtParser :: TokParser (DoStmt Expr);
doBindOrExprStmtParser = withSpanAnn (DoAnn . mkAnnotation) $ exprOrPatternBindParser exprParser (region "while parsing '<-' binding" exprParser) DoBind DoExpr;
parseLetDeclsParser :: TokParser [Decl];
parseLetDeclsParser = expectedTok TkKeywordLet *> bracedDeclsMaybeEmpty;
parseLetDeclsStmtParser :: TokParser [Decl];
parseLetDeclsStmtParser = parseLetDeclsParser <* MP.notFollowedBy (expectedTok TkKeywordIn);
bracedDeclsMaybeEmpty :: TokParser [Decl];
bracedDeclsMaybeEmpty = bracedSemiSep localDeclsParser;
doLetStmtParser :: TokParser (DoStmt Expr);
doLetStmtParser = withSpanAnn (DoAnn . mkAnnotation) $ do {
  DoLetDecls <$> parseLetDeclsStmtParser};
doRecStmtParser :: TokParser (DoStmt Expr);
doRecStmtParser = withSpanAnn (DoAnn . mkAnnotation) $ do {
  expectedTok TkKeywordRec;
  DoRecStmt <$> bracedSemiSep1 doStmtParser};
exprInfixChainParser :: TokParser Expr -> TokParser Expr;
exprInfixChainParser lexp = do {
  lhs <- lexp;
  rest <- MP.many infixOperatorAndOperand;
  pure (foldInfixL buildInfix lhs rest)} where {
  infixOperatorAndOperand = do {
  mKind <- peekTokenKind;
  case mKind of {
  Just kind | startsInfixOperator kind -> (,) <$> infixOperatorParser <*> region "after infix operator" lexp;
  _ -> MP.empty}}};
lexpParser :: TokParser Expr;
lexpParser = lexpParserWith NormalExprAtom;
lexpParserWith :: AtomContext -> TokParser Expr;
lexpParserWith atomContext = do {
  mSCC <- optionalHiddenPragma getSCCPragma;
  case mSCC of {
  Just sccPragma -> EPragma sccPragma <$> lexpParserWith atomContext;
  Nothing -> lexpBaseParserWith atomContext (appExprParserWith (atomOrRecordExprParserWith atomContext))}};
lexpBaseParserWith :: AtomContext -> TokParser Expr -> TokParser Expr;
lexpBaseParserWith atomContext appParser = do {
  tok <- peekToken;
  case lexTokenKind tok of {
  kind | Just blockParser <- lexpBlockParserForToken atomContext kind -> blockParser;
  TkVarSym "-" -> negateOrApp;
  TkMinusOperator -> negateOrApp;
  TkPrefixMinus -> negateOrApp;
  _ -> appParser}} where {
  negateOrApp = MP.try negateExprParser <|> appParser};
reportLexpParser :: TokParser Expr -> TokParser Expr;
reportLexpParser appParser = lexpBlockParser <|> appParser;
lexpBlockParser :: TokParser Expr;
lexpBlockParser = lexpBlockParserWith NormalExprAtom;
lexpBlockParserWith :: AtomContext -> TokParser Expr;
lexpBlockParserWith atomContext = do {
  tok <- peekToken;
  fromMaybe MP.empty (lexpBlockParserForToken atomContext (lexTokenKind tok))};
lexpBlockParserForToken :: AtomContext -> LexTokenKind -> Maybe (TokParser Expr);
lexpBlockParserForToken atomContext kind = case kind of {
  TkKeywordDo -> Just doExprParser;
  TkKeywordMdo -> Just mdoExprParser;
  TkQualifiedDo {} -> Just qualifiedDoExprParser;
  TkQualifiedMdo {} -> Just qualifiedMdoExprParser;
  TkKeywordIf -> Just ifExprParser;
  TkKeywordCase -> Just caseExprParser;
  TkKeywordLet -> Just letExprParser;
  TkKeywordProc | atomContext == NormalExprAtom -> Just procExprParser;
  TkReservedBackslash -> Just lambdaExprParser;
  _ -> Nothing};
getSCCPragma :: Pragma -> Maybe Pragma;
getSCCPragma p = case pragmaType p of {
  PragmaSCC _ -> Just p;
  _ -> Nothing};
buildInfix :: Expr -> (Name, Expr) -> Expr;
buildInfix lhs (op, rhs) = EInfix lhs op rhs;
intExprParser :: TokParser Expr;
intExprParser = tokenExprParser "integer literal" $ \tok -> case lexTokenKind tok of {
  TkInteger i nt -> Just (EInt i nt (lexTokenText tok));
  _ -> Nothing};
floatExprParser :: TokParser Expr;
floatExprParser = tokenExprParser "floating literal" $ \tok -> case lexTokenKind tok of {
  TkFloat x ft -> Just (EFloat x ft (lexTokenText tok));
  _ -> Nothing};
tokenExprParser :: String -> (LexToken -> Maybe Expr) -> TokParser Expr;
tokenExprParser expected matchToken = tokenSatisfy expected $ \tok -> EAnn (mkAnnotation (lexTokenSpan tok)) <$> matchToken tok;
charExprParser :: TokParser Expr;
charExprParser = tokenExprParser "character literal" $ \tok -> case lexTokenKind tok of {
  TkChar x -> Just (EChar x (lexTokenText tok));
  TkCharHash x txt -> Just (ECharHash x txt);
  _ -> Nothing};
stringExprParser :: TokParser Expr;
stringExprParser = tokenExprParser "string literal" $ \tok -> case lexTokenKind tok of {
  TkString x -> Just (EString x (lexTokenText tok));
  TkStringHash x txt -> Just (EStringHash x txt);
  _ -> Nothing};
overloadedLabelExprParser :: TokParser Expr;
overloadedLabelExprParser = tokenExprParser "overloaded label" $ \tok -> case lexTokenKind tok of {
  TkOverloadedLabel lbl repr -> Just (EOverloadedLabel lbl repr);
  _ -> Nothing};
appExprParser :: TokParser Expr;
appExprParser = appExprParserWith atomOrRecordExprParser;
appExprParserWith :: TokParser Expr -> TokParser Expr;
appExprParserWith atomParser = do {
  startState <- MP.getParserState;
  let {
  !startInput = MP.stateInput startState;
  !startOffset = MP.stateOffset startState;
  !inputStart = MP.pstateSourcePos (MP.statePosState startState)};
  first <- atomParser;
  rest <- MP.many appArg;
  case rest of {
  [] -> pure first;
  _ -> do {
  endState <- MP.getParserState;
  let {
  !endInput = MP.stateInput endState;
  !endOffset = MP.stateOffset endState;
  appSpan = consumedSpan inputStart startInput startOffset endInput endOffset};
  pure (EAnn (mkAnnotation appSpan) (foldl applyArg first rest))}}} where {
  appArg :: TokParser (Either Type Expr);
  appArg = (Left <$> typeAppArg) <|> (Right <$> appExprArgParser);
  typeAppArg :: TokParser Type;
  typeAppArg = do {
  hasAt <- nextTokenIs TkTypeApp;
  if hasAt then MP.try (expectedTok TkTypeApp *> typeAtomParser) else MP.empty};
  appExprArgParser :: TokParser Expr;
  appExprArgParser = do {
  tok <- peekToken;
  case lexTokenKind tok of {
  TkKeywordType -> MP.empty;
  _ -> atomParser}};
  applyArg :: Expr -> Either Type Expr -> Expr;
  applyArg fn (Left ty) = ETypeApp fn ty;
  applyArg fn (Right arg) = EApp fn arg};
atomOrRecordExprParser :: TokParser Expr;
atomOrRecordExprParser = atomOrRecordExprParserWith NormalExprAtom;
atomOrRecordExprParserWith :: AtomContext -> TokParser Expr;
atomOrRecordExprParserWith atomContext = do {
  tok <- peekToken;
  if startsAtomOnlyForm atomContext (lexTokenKind tok) then atomExprParserWith atomContext else recordExprParser} where {
  recordExprParser :: TokParser Expr;
  recordExprParser = do {
  base <- recordBaseAtomExprParserWith atomContext;
  applyRecordSuffixes base};
  applyRecordSuffixes :: Expr -> TokParser Expr;
  applyRecordSuffixes e = do {
  hasBrace <- nextTokenIs TkSpecialLBrace;
  if not hasBrace then applyRecordDotSuffixes e else do {
  (fields, hasWildcard) <- recordBracesParser;
  let {
  result = case peelExprAnn e of {
  EVar name | isConLikeName name -> ERecordCon name (map normalizeField fields) hasWildcard;
  _ -> ERecordUpd e (map normalizeField fields)}};
  applyRecordSuffixes result}};
  applyRecordDotSuffixes :: Expr -> TokParser Expr;
  applyRecordDotSuffixes e = do {
  hasDot <- nextTokenIs TkRecordDot;
  if not hasDot || not (recordDotMayFollow e) then pure e else do {
  recordDotEnabled <- isExtensionEnabled OverloadedRecordDot;
  if not recordDotEnabled then pure e else do {
  expectedTok TkRecordDot;
  fieldName <- recordFieldNameParser;
  applyRecordSuffixes (EGetField e fieldName)}}};
  normalizeField :: (Name, Maybe Expr, SourceSpan) -> RecordField Expr;
  normalizeField (fieldName, mExpr, sp) = case mExpr of {
  Just expr' -> RecordField fieldName expr' False;
  Nothing -> RecordField fieldName (EAnn (mkAnnotation sp) (EVar fieldName)) True};
  recordDotMayFollow :: Expr -> Bool;
  recordDotMayFollow expr = case expr of {
  EAnn _ sub -> recordDotMayFollow sub;
  ETypeSyntax TypeSyntaxExplicitNamespace _ -> False;
  _ -> True}};
recordBracesParser :: TokParser ([(Name, Maybe Expr, SourceSpan)], Bool);
recordBracesParser = braces $ recordFieldsWithWildcardsParser (layoutSepEndBy recordFieldBindingParser (expectedTok TkSpecialComma));
recordFieldBindingParser :: TokParser (Name, Maybe Expr, SourceSpan);
recordFieldBindingParser = withSpan $ do {
  fieldName <- recordFieldNameParser;
  mAssign <- optionalTokThen TkReservedEquals exprParser;
  pure (fieldName,mAssign,)};
recordBaseAtomExprParserWith :: AtomContext -> TokParser Expr;
recordBaseAtomExprParserWith atomContext = do {
  tok <- peekToken;
  case lexTokenKind tok of {
  TkImplicitParam {} -> implicitParamExprParser;
  _ -> simpleAtomExprParserWith atomContext}};
startsAtomOnlyForm :: AtomContext -> LexTokenKind -> Bool;
startsAtomOnlyForm atomContext kind = case kind of {
  TkKeywordType -> True;
  TkReservedBackslash -> True;
  TkKeywordLet -> True;
  TkKeywordDo -> True;
  TkKeywordMdo -> True;
  TkQualifiedDo {} -> True;
  TkQualifiedMdo {} -> True;
  TkKeywordCase -> True;
  TkKeywordIf -> True;
  TkKeywordProc -> atomContext == NormalExprAtom;
  _ -> False};
atomExprParser :: TokParser Expr;
atomExprParser = atomExprParserWith NormalExprAtom;
atomExprParserWith :: AtomContext -> TokParser Expr;
atomExprParserWith atomContext = do {
  tok <- peekToken;
  case lexTokenKind tok of {
  TkImplicitParam {} -> implicitParamExprParser;
  TkKeywordType -> do {
  explicitNamespacesEnabled <- isExtensionEnabled ExplicitNamespaces;
  requiredTypeArgumentsEnabled <- isExtensionEnabled RequiredTypeArguments;
  if explicitNamespacesEnabled || requiredTypeArgumentsEnabled then explicitTypeExprParser else simpleAtomExprParserWith atomContext};
  TkReservedBackslash -> lambdaExprParser;
  TkKeywordLet -> blockAtom letExprParser;
  TkKeywordDo -> blockAtom doExprParser;
  TkKeywordMdo -> blockAtom mdoExprParser;
  TkQualifiedDo {} -> blockAtom qualifiedDoExprParser;
  TkQualifiedMdo {} -> blockAtom qualifiedMdoExprParser;
  TkKeywordCase -> blockAtom caseExprParser;
  TkKeywordIf -> blockAtom ifExprParser;
  TkKeywordProc | atomContext == NormalExprAtom -> blockAtom procExprParser;
  _ -> simpleAtomExprParserWith atomContext}} where {
  blockAtom parser = do {
  blockArgsEnabled <- isExtensionEnabled BlockArguments;
  if blockArgsEnabled then parser else simpleAtomExprParserWith atomContext}};
simpleAtomExprParserWith :: AtomContext -> TokParser Expr;
simpleAtomExprParserWith atomContext = do {
  tok <- peekToken;
  case lexTokenKind tok of {
  TkPrefixMinus -> prefixNegateAtomExprParserWith atomContext;
  TkSpecialLParen -> do {
  mAfterParen <- lookAhead (MP.optional (anySingle *> anySingle));
  case lexTokenKind <$> mAfterParen of {
  Just afterParen | startsParenOperator afterParen -> MP.try parenOperatorExprParser <|> parenExprParser;
  _ -> parenExprParser}};
  TkSpecialUnboxedLParen -> parenExprParser;
  TkSpecialLBracket -> listExprParser;
  TkInteger {} -> intExprParser;
  TkFloat {} -> floatExprParser;
  TkChar {} -> charExprParser;
  TkCharHash {} -> charExprParser;
  TkString {} -> stringExprParser;
  TkStringHash {} -> stringExprParser;
  TkOverloadedLabel {} -> overloadedLabelExprParser;
  TkKeywordUnderscore -> wildcardExprParser;
  TkVarId {} -> varExprParser;
  TkConId {} -> varExprParser;
  TkQVarId {} -> varExprParser;
  TkQConId {} -> varExprParser;
  TkQuasiQuote {} -> quasiQuoteExprParser;
  TkTHExpQuoteOpen -> thAtom thQuoteExprParser;
  TkTHTypedQuoteOpen -> thAtom thQuoteExprParser;
  TkTHDeclQuoteOpen -> thAtom thQuoteExprParser;
  TkTHTypeQuoteOpen -> thAtom thQuoteExprParser;
  TkTHPatQuoteOpen -> thAtom thQuoteExprParser;
  TkTHQuoteTick -> thAtom thNameQuoteExprParser;
  TkTHTypeQuoteTick -> thAtom thNameQuoteExprParser;
  TkTHTypedSplice -> thAtom thTypedSpliceParser;
  TkTHSplice -> thAtom thUntypedSpliceParser;
  _ -> varExprParser}} where {
  thAtom parser = do {
  thAny <- thAnyEnabled;
  if thAny then parser else varExprParser}};
explicitTypeExprParser :: TokParser Expr;
explicitTypeExprParser = withSpanAnn (EAnn . mkAnnotation) $ do {
  expectedTok TkKeywordType;
  ETypeSyntax TypeSyntaxExplicitNamespace <$> typeParser};
prefixNegateAtomExprParserWith :: AtomContext -> TokParser Expr;
prefixNegateAtomExprParserWith atomContext = withSpanAnn (EAnn . mkAnnotation) $ do {
  prefixMinusTokenParser;
  ENegate <$> atomExprParserWith atomContext};
negateExprParser :: TokParser Expr;
negateExprParser = withSpanAnn (EAnn . mkAnnotation) $ do {
  _ <- minusTokenValueParser;
  ENegate <$> negateOperandParser};
negateOperandParser :: TokParser Expr;
negateOperandParser = reportLexpParser appExprParser;
minusTokenValueParser :: TokParser LexToken;
minusTokenValueParser = tokenSatisfy "minus operator" $ \tok -> case lexTokenKind tok of {
  TkVarSym "-" -> Just tok;
  TkMinusOperator -> Just tok;
  TkPrefixMinus -> Just tok;
  _ -> Nothing};
prefixMinusTokenParser :: TokParser ();
prefixMinusTokenParser = tokenSatisfy "prefix minus" $ \tok -> case lexTokenKind tok of {
  TkPrefixMinus -> Just ();
  _ -> Nothing};
startsParenOperator :: LexTokenKind -> Bool;
startsParenOperator kind = case kind of {
  TkVarSym {} -> True;
  TkConSym {} -> True;
  TkQVarSym {} -> True;
  TkQConSym {} -> True;
  TkMinusOperator -> True;
  TkReservedColon -> True;
  TkReservedAt -> True;
  _ -> False};
parenOperatorExprParser :: TokParser Expr;
parenOperatorExprParser = withSpanAnn (EAnn . mkAnnotation) $ EVar <$> parens operatorExprNameParser;
operatorExprNameParser :: TokParser Name;
operatorExprNameParser = tokenSatisfy "operator" $ \tok -> case lexTokenKind tok of {
  TkVarSym sym -> Just (mkNameAt tok Nothing NameVarSym sym);
  TkConSym sym -> Just (mkNameAt tok Nothing NameConSym sym);
  TkQVarSym modName sym -> Just (mkNameAt tok (Just modName) NameVarSym sym);
  TkQConSym modName sym -> Just (mkNameAt tok (Just modName) NameConSym sym);
  TkMinusOperator -> Just (mkNameAt tok Nothing NameVarSym "-");
  TkReservedColon -> Just (mkNameAt tok Nothing NameConSym ":");
  TkReservedAt -> Just (mkNameAt tok Nothing NameVarSym "@");
  _ -> Nothing};
rhsParser :: TokParser (Rhs Expr);
rhsParser = label "right-hand side" (caseRhsParserWithBodyParser exprParser);
equationRhsParser :: TokParser (Rhs Expr);
equationRhsParser = label "equation right-hand side" (rhsParserWithBodyParser RhsArrowEquation exprParser);
caseRhsParserWithBodyParser :: TokParser body -> TokParser (Rhs body);
caseRhsParserWithBodyParser = rhsParserWithBodyParser RhsArrowCase;
data RhsArrowKind = RhsArrowCase | RhsArrowEquation;
rhsArrowText :: RhsArrowKind -> Text;
rhsArrowText RhsArrowCase = "->";
rhsArrowText RhsArrowEquation = "=";
rhsArrowTok :: RhsArrowKind -> TokParser ();
rhsArrowTok RhsArrowCase = expectedTok TkReservedRightArrow;
rhsArrowTok RhsArrowEquation = expectedTok TkReservedEquals;
rhsParserWithBodyParser :: RhsArrowKind -> TokParser body -> TokParser (Rhs body);
rhsParserWithBodyParser arrowKind bodyParser = do {
  tok <- peekToken;
  case lexTokenKind tok of {
  TkReservedPipe -> guardedRhssParserWithBodyParser arrowKind bodyParser;
  TkReservedRightArrow | RhsArrowCase <- arrowKind -> unguardedRhsParserWithBodyParser arrowKind bodyParser;
  TkReservedEquals | RhsArrowEquation <- arrowKind -> unguardedRhsParserWithBodyParser arrowKind bodyParser;
  _ -> MP.customFailure UnexpectedTokenExpecting {unexpectedFound = Just (mkFoundToken tok), unexpectedExpecting = rhsArrowText arrowKind <> " or guarded right-hand side", unexpectedContext = []}}};
unguardedRhsParserWithBodyParser :: RhsArrowKind -> TokParser body -> TokParser (Rhs body);
unguardedRhsParserWithBodyParser arrowKind bodyParser = withSpan $ do {
  rhsArrowTok arrowKind;
  body <- region (rhsContextText arrowKind) bodyParser;
  whereDecls <- MP.optional whereClauseParser;
  pure (\span' -> UnguardedRhs [mkAnnotation span'] body whereDecls)};
rhsContextText :: RhsArrowKind -> Text;
rhsContextText RhsArrowCase = "while parsing case alternative right-hand side";
rhsContextText RhsArrowEquation = "while parsing equation right-hand side";
guardedRhssParserWithBodyParser :: RhsArrowKind -> TokParser body -> TokParser (Rhs body);
guardedRhssParserWithBodyParser arrowKind bodyParser = withSpan $ do {
  grhss <- MP.some (guardedRhsParserWithBodyParser arrowKind bodyParser);
  whereDecls <- MP.optional whereClauseParser;
  pure (\span' -> GuardedRhss [mkAnnotation span'] grhss whereDecls)};
guardedRhsParserWithBodyParser :: RhsArrowKind -> TokParser body -> TokParser (GuardedRhs body);
guardedRhsParserWithBodyParser arrowKind bodyParser = withSpan $ do {
  expectedTok TkReservedPipe;
  guards <- layoutSepBy1 (guardQualifierParser arrowKind) (expectedTok TkSpecialComma);
  rhsArrowTok arrowKind;
  body <- bodyParser;
  pure $ \span' -> GuardedRhs {guardedRhsAnns = [mkAnnotation span'], guardedRhsGuards = guards, guardedRhsBody = body}};
guardQualifierParser :: RhsArrowKind -> TokParser GuardQualifier;
guardQualifierParser arrowKind = do {
  tok <- peekToken;
  case lexTokenKind tok of {
  TkKeywordLet -> MP.try guardLetParser <|> guardBindOrExprParser arrowKind;
  _ -> guardBindOrExprParser arrowKind}};
guardBindOrExprParser :: RhsArrowKind -> TokParser GuardQualifier;
guardBindOrExprParser arrowKind = withSpanAnn (GuardAnn . mkAnnotation) $ exprOrPatternBindParser (guardExprParser arrowKind) exprParser GuardPat GuardExpr;
guardLetParser :: TokParser GuardQualifier;
guardLetParser = withSpanAnn (GuardAnn . mkAnnotation) $ do {
  GuardLet <$> parseLetDeclsStmtParser};
guardExprParser :: RhsArrowKind -> TokParser Expr;
guardExprParser RhsArrowEquation = exprParserWithTypeSigParser typeParser;
guardExprParser RhsArrowCase = exprCoreParserWithoutTypeSig;
caseAltParser :: TokParser (CaseAlt Expr);
caseAltParser = withSpan $ do {
  pat <- region "while parsing case alternative" caseAltPatternParser;
  rhs <- region "while parsing case alternative" rhsParser;
  pure $ \span' -> CaseAlt {caseAltAnns = [mkAnnotation span'], caseAltPattern = pat, caseAltRhs = rhs}};
lambdaCaseAltParser :: TokParser LambdaCaseAlt;
lambdaCaseAltParser = withSpan $ do {
  pats <- region "while parsing lambda-cases alternative" (MP.many apatParser);
  rhs <- region "while parsing lambda-cases alternative" rhsParser;
  pure $ \span' -> LambdaCaseAlt {lambdaCaseAltAnns = [mkAnnotation span'], lambdaCaseAltPats = pats, lambdaCaseAltRhs = rhs}};
caseExprParser :: TokParser Expr;
caseExprParser = withSpanAnn (EAnn . mkAnnotation) $ do {
  expectedTok TkKeywordCase;
  scrutinee <- region "while parsing case expression" exprParser;
  expectedTok TkKeywordOf;
  ECase scrutinee <$> bracedAlts} where {
  bracedAlts = bracedSemiSep caseAltParser};
parenExprParser :: TokParser Expr;
parenExprParser = withSpanAnn (EAnn . mkAnnotation) $ do {
  (tupleFlavor, closeTok) <- tupleDelimsParser;
  closedImmediately <- optionalTok closeTok;
  if closedImmediately then pure (ETuple tupleFlavor []) else if tupleFlavor == Boxed then MP.try (parseNegateParen closeTok) <|> parseBoxedContent closeTok else MP.try (parseUnboxedSumExprLeadingBars closeTok) <|> parseTupleOrParen tupleFlavor closeTok} where {
  parseNegateParen closeTok = do {
  minusTok <- minusTokenValueParser;
  guard (parenNegateAllowed minusTok);
  negOperand <- negateOperandParser;
  let {
  negBase = ENegate negOperand};
  rest <- MP.many (MP.try ((,) <$> infixOperatorParser <*> region "after infix operator" lexpParser));
  let {
  withInfix = foldInfixL buildInfix negBase rest};
  mTypeSig <- optionalTokThen TkReservedDoubleColon typeSignatureParser;
  let {
  typed = case mTypeSig of {
  Just ty -> ETypeSig withInfix ty;
  Nothing -> withInfix}};
  finalExpr <- maybeViewPattern typed;
  expectedTok closeTok;
  pure $ case lexTokenKind minusTok of {
  TkPrefixMinus -> finalExpr;
  _ -> EParen finalExpr}};
  parenNegateAllowed minusTok = case lexTokenKind minusTok of {
  TkPrefixMinus -> True;
  TkVarSym "-" -> True;
  TkMinusOperator -> False;
  _ -> False};
  parseBoxedContent closeTok = MP.try (parseProjectionSection closeTok) <|> MP.try parseSectionR <|> do {
  mBase <- MP.optional (MP.try negateExprParser <|> lexpParser);
  case mBase of {
  Nothing -> finishBoxed closeTok Nothing;
  Just base -> do {
  mOp <- MP.optional infixOperatorParser;
  case mOp of {
  Nothing -> do {
  mArrowSection <- MP.optional (MP.try (arrowSectionOperatorParser <* expectedTok closeTok));
  case mArrowSection of {
  Just op -> pure (EParen (ESectionL base op));
  Nothing -> do {
  mTypeSig <- optionalTokThen TkReservedDoubleColon typeSignatureParser;
  let {
  typed = case mTypeSig of {
  Just ty -> ETypeSig base ty;
  Nothing -> base}};
  finalExpr <- maybeViewPattern typed;
  finishBoxed closeTok (Just finalExpr)}}};
  Just op -> do {
  closedAfterOperator <- optionalTok closeTok;
  if closedAfterOperator then pure (EParen (ESectionL base op)) else do {
  rhs <- region "after infix operator" lexpParser;
  more <- MP.many (MP.try ((,) <$> infixOperatorParser <*> region "after infix operator" lexpParser));
  let {
  fullInfix = foldInfixL buildInfix base ((op, rhs) : more)};
  mTrailingOp <- MP.optional infixOperatorParser;
  case mTrailingOp of {
  Just trailOp -> do {
  expectedTok closeTok;
  pure (EParen (ESectionL fullInfix trailOp))};
  Nothing -> do {
  mTypeSig <- optionalTokThen TkReservedDoubleColon typeSignatureParser;
  let {
  typed = case mTypeSig of {
  Just ty -> ETypeSig fullInfix ty;
  Nothing -> fullInfix}};
  finalExpr <- maybeViewPattern typed;
  finishBoxed closeTok (Just finalExpr)}}}}}}}} where {
  parseProjectionSection tok = do {
  recordDotEnabled <- isExtensionEnabled OverloadedRecordDot;
  guard recordDotEnabled;
  fields <- MP.some (expectedTok TkRecordDot *> recordFieldNameParser);
  expectedTok tok;
  pure (EParen (EGetFieldProjection fields))};
  parseSectionR = do {
  op <- reservedAtSectionOperatorParser <|> infixOperatorParser <|> arrowSectionOperatorParser;
  rhs <- exprParser;
  expectedTok closeTok;
  pure (EParen (ESectionR op rhs))};
  reservedAtSectionOperatorParser = do {
  expectedTok TkReservedAt;
  pure (qualifyName Nothing (mkUnqualifiedName NameVarSym "@"))};
  arrowSectionOperatorParser = tokenSatisfy "operator" $ \tok -> case lexTokenKind tok of {
  TkArrowTail -> Just (qualifyName Nothing (mkUnqualifiedName NameVarSym "-<"));
  TkDoubleArrowTail -> Just (qualifyName Nothing (mkUnqualifiedName NameVarSym "-<<"));
  _ -> Nothing}};
  finishBoxed closeTok mFirst = do {
  hasComma <- optionalTok TkSpecialComma;
  case (mFirst, hasComma) of {
  (Just e, False) -> do {
  expectedTok closeTok;
  pure (EParen e)};
  (_, True) -> do {
  rest <- parseTupleElems closeTok;
  pure (ETuple Boxed (mFirst : rest))};
  (Nothing, False) -> fail "expected expression or closing paren"}};
  parseTupleOrParen tupleFlavor closeTok = do {
  first <- MP.optional texprParser;
  hasComma <- optionalTok TkSpecialComma;
  case (first, hasComma) of {
  (Just e, False) -> case tupleFlavor of {
  Boxed -> do {
  expectedTok closeTok;
  pure (EParen e)};
  Unboxed -> do {
  hasPipe <- optionalTok TkReservedPipe;
  if hasPipe then do {
  trailingBars <- MP.many (expectedTok TkReservedPipe);
  expectedTok closeTok;
  let {
  arity = 2 + length trailingBars};
  pure (EUnboxedSum 0 arity e)} else do {
  expectedTok closeTok;
  pure (ETuple Unboxed [Just e])}}};
  (_, True) -> do {
  rest <- parseTupleElems closeTok;
  pure (ETuple tupleFlavor (first : rest))};
  (Nothing, False) -> fail "expected expression or closing paren"}};
  parseTupleElems closeTok = do {
  e <- MP.optional texprParser;
  hasComma <- optionalTok TkSpecialComma;
  if hasComma then (e :) <$> parseTupleElems closeTok else do {
  expectedTok closeTok;
  pure [e]}};
  parseUnboxedSumExprLeadingBars closeTok = do {
  _ <- expectedTok TkReservedPipe;
  leadingBars <- MP.many (expectedTok TkReservedPipe);
  let {
  altIdx = 1 + length leadingBars};
  inner <- texprParser;
  trailingBars <- MP.many (expectedTok TkReservedPipe);
  expectedTok closeTok;
  let {
  arity = altIdx + 1 + length trailingBars};
  pure (EUnboxedSum altIdx arity inner)}};
listExprParser :: TokParser Expr;
listExprParser = withSpanAnn (EAnn . mkAnnotation) $ do {
  expectedTok TkSpecialLBracket;
  closedImmediately <- optionalTok TkSpecialRBracket;
  if closedImmediately then pure (EList []) else do {
  first <- exprParser;
  parseListTail first}};
parseListTail :: Expr -> TokParser Expr;
parseListTail first = listCompTailParser <|> arithFromToTailParser <|> commaTailParser <|> singletonTailParser where {
  listCompTailParser = do {
  expectedTok TkReservedPipe;
  firstGroup <- compStmtParser `MP.sepBy1` expectedTok TkSpecialComma;
  moreGroups <- MP.many (expectedTok TkReservedPipe *> (compStmtParser `MP.sepBy1` expectedTok TkSpecialComma));
  expectedTok TkSpecialRBracket;
  pure $ case moreGroups of {
  [] -> EListComp first firstGroup;
  _ -> EListCompParallel first (firstGroup : moreGroups)}};
  arithFromToTailParser = do {
  expectedTok TkReservedDotDot;
  mTo <- MP.optional exprParser;
  expectedTok TkSpecialRBracket;
  pure $ EArithSeq $ case mTo of {
  Nothing -> ArithSeqFrom first;
  Just toExpr -> ArithSeqFromTo first toExpr}};
  commaTailParser = do {
  expectedTok TkSpecialComma;
  second <- exprParser;
  arithFromThenTailParser second <|> listTailParser second};
  arithFromThenTailParser second = do {
  expectedTok TkReservedDotDot;
  mTo <- MP.optional exprParser;
  expectedTok TkSpecialRBracket;
  pure $ EArithSeq $ case mTo of {
  Nothing -> ArithSeqFromThen first second;
  Just toExpr -> ArithSeqFromThenTo first second toExpr}};
  listTailParser second = do {
  rest <- MP.many (expectedTok TkSpecialComma *> exprParser);
  expectedTok TkSpecialRBracket;
  pure (EList (first : second : rest))};
  singletonTailParser = do {
  expectedTok TkSpecialRBracket;
  pure (EList [first])}};
compStmtParser :: TokParser CompStmt;
compStmtParser = do {
  tok <- peekToken;
  case lexTokenKind tok of {
  TkKeywordLet -> MP.try compLetStmtParser <|> compGenOrGuardParser;
  TkKeywordThen -> compTransformStmtParser <|> compGenOrGuardParser;
  _ -> compGenOrGuardParser}};
compTransformStmtParser :: TokParser CompStmt;
compTransformStmtParser = MP.try $ withSpanAnn (CompAnn . mkAnnotation) $ do {
  enabled <- isExtensionEnabled TransformListComp;
  guard enabled;
  expectedTok TkKeywordThen;
  tok <- peekToken;
  case lexTokenKind tok of {
  TkVarId "group" -> compGroupStmtParser;
  _ -> compThenStmtParser}};
compGroupStmtParser :: TokParser CompStmt;
compGroupStmtParser = do {
  varIdTok "group";
  tok <- peekToken;
  case lexTokenKind tok of {
  TkKeywordBy -> do {
  expectedTok TkKeywordBy;
  e <- compTransformExprParser;
  expectedTok TkKeywordUsing;
  CompGroupByUsing e <$> exprParser};
  TkKeywordUsing -> do {
  expectedTok TkKeywordUsing;
  CompGroupUsing <$> exprParser};
  _ -> fail "expected 'by' or 'using' after 'group'"}};
compThenStmtParser :: TokParser CompStmt;
compThenStmtParser = do {
  f <- compTransformExprParser;
  hasBy <- optionalTok TkKeywordBy;
  if hasBy then CompThenBy f <$> exprParser else pure (CompThen f)};
compTransformExprParser :: TokParser Expr;
compTransformExprParser = label "expression" $ optionalSuffix (expectedTok TkReservedDoubleColon *> typeSignatureParser) ETypeSig compTransformExprWithoutTypeSigParser;
compTransformExprWithoutTypeSigParser :: TokParser Expr;
compTransformExprWithoutTypeSigParser = exprInfixChainParser compTransformLexpParser;
compTransformLexpParser :: TokParser Expr;
compTransformLexpParser = compTransformLambdaExprParser <|> lexpBlockParser <|> MP.try compTransformNegateExprParser <|> compTransformAppExprParser;
compTransformLambdaExprParser :: TokParser Expr;
compTransformLambdaExprParser = withSpanAnn (EAnn . mkAnnotation) $ do {
  expectedTok TkReservedBackslash;
  lambdaCaseParser <|> lambdaCasesParser <|> lambdaPatsParser} where {
  lambdaCaseParser = do {
  expectedTok TkKeywordCase;
  ELambdaCase <$> bracedSemiSep compTransformCaseAltParser};
  lambdaCasesParser = do {
  lambdaCaseEnabled <- isExtensionEnabled LambdaCase;
  guard lambdaCaseEnabled;
  varIdTok "cases";
  ELambdaCases <$> bracedSemiSep compTransformLambdaCaseAltParser};
  lambdaPatsParser = do {
  pats <- MP.some apatParser;
  expectedTok TkReservedRightArrow;
  body <- region "while parsing lambda body" compTransformExprParser;
  pure (ELambdaPats pats body)}};
compTransformCaseAltParser :: TokParser (CaseAlt Expr);
compTransformCaseAltParser = withSpan $ do {
  pat <- region "while parsing case alternative" caseAltPatternParser;
  rhs <- region "while parsing case alternative" (caseRhsParserWithBodyParser compTransformExprParser);
  pure $ \span' -> CaseAlt {caseAltAnns = [mkAnnotation span'], caseAltPattern = pat, caseAltRhs = rhs}};
compTransformLambdaCaseAltParser :: TokParser LambdaCaseAlt;
compTransformLambdaCaseAltParser = withSpan $ do {
  pats <- region "while parsing lambda-cases alternative" (MP.many apatParser);
  rhs <- region "while parsing lambda-cases alternative" (caseRhsParserWithBodyParser compTransformExprParser);
  pure $ \span' -> LambdaCaseAlt {lambdaCaseAltAnns = [mkAnnotation span'], lambdaCaseAltPats = pats, lambdaCaseAltRhs = rhs}};
compTransformAppExprParser :: TokParser Expr;
compTransformAppExprParser = appExprParserWith compTransformAtomOrRecordExprParser;
compTransformNegateExprParser :: TokParser Expr;
compTransformNegateExprParser = withSpanAnn (EAnn . mkAnnotation) $ do {
  _ <- minusTokenValueParser;
  ENegate <$> compTransformNegateOperandParser};
compTransformNegateOperandParser :: TokParser Expr;
compTransformNegateOperandParser = reportLexpParser compTransformAppExprParser;
compTransformAtomOrRecordExprParser :: TokParser Expr;
compTransformAtomOrRecordExprParser = do {
  tok <- peekToken;
  case lexTokenKind tok of {
  TkKeywordBy -> MP.empty;
  TkKeywordUsing -> MP.empty;
  _ -> atomOrRecordExprParser}};
compGenOrGuardParser :: TokParser CompStmt;
compGenOrGuardParser = withSpanAnn (CompAnn . mkAnnotation) $ exprOrPatternBindParser exprParser (region "while parsing '<-' generator" exprParser) CompGen CompGuard;
compLetStmtParser :: TokParser CompStmt;
compLetStmtParser = withSpanAnn (CompAnn . mkAnnotation) $ do {
  CompLetDecls <$> parseLetDeclsStmtParser};
lambdaExprParser :: TokParser Expr;
lambdaExprParser = withSpanAnn (EAnn . mkAnnotation) $ do {
  expectedTok TkReservedBackslash;
  lambdaCaseParser <|> lambdaCasesParser <|> lambdaPatsParser} where {
  lambdaCaseParser = do {
  expectedTok TkKeywordCase;
  ELambdaCase <$> bracedCaseAlts};
  lambdaCasesParser = do {
  lambdaCaseEnabled <- isExtensionEnabled LambdaCase;
  guard lambdaCaseEnabled;
  varIdTok "cases";
  ELambdaCases <$> bracedLambdaCaseAlts};
  lambdaPatsParser = do {
  pats <- MP.some apatParser;
  expectedTok TkReservedRightArrow;
  body <- region "while parsing lambda body" exprParser;
  pure (ELambdaPats pats body)};
  bracedCaseAlts = bracedSemiSep caseAltParser;
  bracedLambdaCaseAlts = bracedSemiSep lambdaCaseAltParser};
letExprParser :: TokParser Expr;
letExprParser = withSpanAnn (EAnn . mkAnnotation) $ do {
  decls <- parseLetDeclsParser;
  expectedTok TkKeywordIn;
  ELetDecls decls <$> exprParser};
whereClauseParser :: TokParser [Decl];
whereClauseParser = do {
  expectedTok TkKeywordWhere;
  bracedDeclsMaybeEmpty};
localDeclsParser :: TokParser Decl;
localDeclsParser = do {
  typeSigPrefix <- startsWithTypeSig;
  pragmaDeclParser <|> implicitParamDeclParser <|> fixityDeclParser <|> (if typeSigPrefix then localTypeSigDeclsParser else MP.empty) <|> localFunctionDeclParser <|> localPatternDeclParser};
localTypeSigDeclsParser :: TokParser Decl;
localTypeSigDeclsParser = withSpanAnn (DeclAnn . mkAnnotation) $ typedBindingOrSignatureParser typeParser DeclTypeSig (\name ty -> do {
  rhs <- equationRhsParser;
  let {
  pat = PTypeSig (PVar name) ty};
  pure (DeclValue (PatternBind NoMultiplicityTag pat rhs))}) "local typed bindings with '=' or guards require exactly one binder";
localFunctionDeclParser :: TokParser Decl;
localFunctionDeclParser = withSpanAnn (DeclAnn . mkAnnotation) $ do {
  (headForm, name, pats) <- MP.try $ do {
  headParts <- functionHeadParserWith patParser apatParser;
  lookAhead (expectedTok TkReservedEquals <|> expectedTok TkReservedPipe);
  pure headParts};
  functionBindDecl headForm name pats <$> equationRhsParser};
localPatternDeclParser :: TokParser Decl;
localPatternDeclParser = withSpanAnn (DeclAnn . mkAnnotation) $ do {
  linearEnabled <- isExtensionEnabled LinearTypes;
  multTag <- if linearEnabled then MP.option NoMultiplicityTag (MP.try localMultiplicityTagParser) else pure NoMultiplicityTag;
  pat <- patternParser;
  DeclValue . PatternBind multTag pat <$> equationRhsParser};
localMultiplicityTagParser :: TokParser MultiplicityTag;
localMultiplicityTagParser = do {
  expectedTok TkPrefixPercent;
  tok <- peekToken;
  case lexTokenKind tok of {
  TkInteger 1 _ -> anySingle $> LinearMultiplicityTag;
  _ -> ExplicitMultiplicityTag <$> typeAtomParser}};
implicitParamDeclParser :: TokParser Decl;
implicitParamDeclParser = withSpanAnn (DeclAnn . mkAnnotation) $ do {
  name <- implicitParamNameParser;
  expectedTok TkReservedEquals;
  rhsExpr <- exprParser;
  whereDecls <- MP.optional whereClauseParser;
  pure $ DeclImplicitParam name rhsExpr whereDecls};
varExprParser :: TokParser Expr;
varExprParser = tokenSatisfy "identifier" $ \tok -> case identifierName tok of {
  Just name -> Just (EAnn (mkAnnotation (lexTokenSpan tok)) (EVar name));
  Nothing -> Nothing};
implicitParamExprParser :: TokParser Expr;
implicitParamExprParser = tokenSatisfy "implicit parameter" $ \tok -> case lexTokenKind tok of {
  TkImplicitParam name -> Just $ EAnn (mkAnnotation (lexTokenSpan tok)) (EImplicitParam name);
  _ -> Nothing};
wildcardExprParser :: TokParser Expr;
wildcardExprParser = tokenSatisfy "wildcard" $ \tok -> case lexTokenKind tok of {
  TkKeywordUnderscore -> Just $ EAnn (mkAnnotation (lexTokenSpan tok)) (EVar (mkNameAt tok Nothing NameVarId "_"));
  _ -> Nothing};
thQuoteExprParser :: TokParser Expr;
thQuoteExprParser = thExpQuoteParser <|> thTypedQuoteParser <|> thDeclQuoteParser <|> thTypeQuoteParser <|> thPatQuoteParser;
thExpQuoteParser :: TokParser Expr;
thExpQuoteParser = thQuoteParser (EAnn . mkAnnotation) TkTHExpQuoteOpen TkTHExpQuoteClose exprParser ETHExpQuote;
thTypedQuoteParser :: TokParser Expr;
thTypedQuoteParser = thQuoteParser (EAnn . mkAnnotation) TkTHTypedQuoteOpen TkTHTypedQuoteClose exprParser ETHTypedQuote;
thDeclQuoteParser :: TokParser Expr;
thDeclQuoteParser = thQuoteParser (EAnn . mkAnnotation) TkTHDeclQuoteOpen TkTHExpQuoteClose (bracedSemiSep declParser <|> plainSemiSep declParser) ETHDeclQuote;
thTypeQuoteParser :: TokParser Expr;
thTypeQuoteParser = thQuoteParser (EAnn . mkAnnotation) TkTHTypeQuoteOpen TkTHExpQuoteClose typeParser ETHTypeQuote;
thPatQuoteParser :: TokParser Expr;
thPatQuoteParser = thQuoteParser (EAnn . mkAnnotation) TkTHPatQuoteOpen TkTHExpQuoteClose patParser ETHPatQuote;
thUntypedSpliceParser :: TokParser Expr;
thUntypedSpliceParser = withSpanAnn (EAnn . mkAnnotation) $ do {
  expectedTok TkTHSplice;
  ETHSplice <$> compactSpliceBodyParser};
thTypedSpliceParser :: TokParser Expr;
thTypedSpliceParser = withSpanAnn (EAnn . mkAnnotation) $ do {
  expectedTok TkTHTypedSplice;
  ETHTypedSplice <$> compactSpliceBodyParser};
compactSpliceBodyParser :: TokParser Expr;
compactSpliceBodyParser = do {
  tok <- peekToken;
  case lexTokenKind tok of {
  TkReservedBackslash -> MP.empty;
  TkKeywordLet -> MP.empty;
  TkKeywordDo -> MP.empty;
  TkKeywordMdo -> MP.empty;
  TkQualifiedDo {} -> MP.empty;
  TkQualifiedMdo {} -> MP.empty;
  TkKeywordCase -> MP.empty;
  TkKeywordIf -> MP.empty;
  TkKeywordProc -> MP.empty;
  TkKeywordType -> MP.empty;
  _ -> atomExprParser}};
thNameQuoteExprParser :: TokParser Expr;
thNameQuoteExprParser = thValueNameQuoteParser <|> thTypeNameQuoteParser;
thValueNameQuoteParser :: TokParser Expr;
thValueNameQuoteParser = withSpanAnn (EAnn . mkAnnotation) $ do {
  expectedTok TkTHQuoteTick;
  body <- atomExprParser;
  guard (isTHValueNameQuoteBody body);
  pure (ETHNameQuote body)};
isTHValueNameQuoteBody :: Expr -> Bool;
isTHValueNameQuoteBody expr = case peelExprAnn expr of {
  EVar {} -> True;
  EList [] -> True;
  ETuple _ elems -> all isTupleConstructorSlot elems;
  _ -> False} where {
  isTupleConstructorSlot Nothing = True;
  isTupleConstructorSlot Just {} = False};
thTypeNameQuoteParser :: TokParser Expr;
thTypeNameQuoteParser = withSpanAnn (EAnn . mkAnnotation) $ do {
  expectedTok TkTHTypeQuoteTick;
  body <- typeAtomParser;
  guard (isTHTypeNameQuoteBody body);
  pure (ETHTypeNameQuote body)};
isTHTypeNameQuoteBody :: Type -> Bool;
isTHTypeNameQuoteBody ty = case peelTypeAnn ty of {
  TVar {} -> True;
  TCon {} -> True;
  TBuiltinCon {} -> True;
  TTuple _ _ [] -> True;
  _ -> False};
quasiQuoteExprParser :: TokParser Expr;
quasiQuoteExprParser = tokenSatisfy "quasi quote" $ \tok -> case lexTokenKind tok of {
  TkQuasiQuote quoter body -> Just (EAnn (mkAnnotation (lexTokenSpan tok)) (EQuasiQuote quoter body));
  _ -> Nothing}
}
