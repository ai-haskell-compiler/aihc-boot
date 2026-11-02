{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
module Aihc.Parser.Internal.Decl (declParser, fixityDeclParser, pragmaDeclParser, typeSigDeclParser) where {
import Aihc.Parser.Internal.Common;
import Aihc.Parser.Internal.Expr (equationRhsParser, exprParser);
import Aihc.Parser.Internal.Import (warningPragmaParser);
import Aihc.Parser.Internal.Pattern (apatParser, lpatParser, patParser, patternParser);
import Aihc.Parser.Internal.Type (arrowKindParser, forallTelescopeParser, typeAppParser, typeAtomParser, typeInfixOperatorParser, typeInfixParser, typeParser, typeSignatureParser);
import Aihc.Parser.Lex (LexTokenKind(..), lexTokenKind, pattern TkVarFamily, pattern TkVarRole);
import Aihc.Parser.Syntax;
import Aihc.Parser.Types (ParserErrorComponent(..), TokStream(..), mkFoundToken);
import Control.Monad (when);
import Data.Char (isLower);
import Data.Functor (($>));
import Data.Maybe (fromMaybe);
import Data.Text (Text);
import Data.Text qualified as T;
import Text.Megaparsec (anySingle, lookAhead, (<|>));
import Text.Megaparsec qualified as MP;
instanceOverlapPragmaParser :: TokParser Pragma;
instanceOverlapPragmaParser = hiddenPragma "instance overlap pragma" $ \p -> case pragmaType p of {
  PragmaInstanceOverlap _ -> Just p;
  _ -> Nothing};
anyPragmaParser :: String -> TokParser Pragma;
anyPragmaParser expectedLabel = hiddenPragma expectedLabel Just;
declParser :: TokParser Decl;
declParser = do {
  pst <- MP.getParserState;
  case tokStreamPendingPragmas (MP.stateInput pst) of {
  [] -> ordinaryDeclParser;
  _ -> pragmaDeclParser <|> ordinaryDeclParser}};
ordinaryDeclParser :: TokParser Decl;
ordinaryDeclParser = do {
  (tok, nextTok) <- lookAhead ((,) <$> anySingle <*> anySingle);
  let {
  tokKind = lexTokenKind tok;
  nextTokKind = lexTokenKind nextTok;
  valueDecl = do {
  exprFallback <- exprDeclEnabled;
  if exprFallback then MP.try valueDeclParser <|> exprDeclParser else valueDeclParser};
  sigOrValueDecl = do {
  typeSigPrefix <- startsWithTypeSig;
  if typeSigPrefix then typeSigOrPatternTypeSigDeclParser else valueDecl};
  fallbackDecl = do {
  typeSigPrefix <- startsWithTypeSig;
  exprFallback <- exprDeclEnabled;
  (if typeSigPrefix then typeSigOrPatternTypeSigDeclParser else MP.empty) <|> MP.try valueDeclParser <|> patternBindDeclParser <|> (if exprFallback then exprDeclParser else MP.empty)}};
  case tokKind of {
  TkKeywordData -> case nextTokKind of {
  TkVarFamily -> dataFamilyDeclParser;
  TkKeywordInstance -> dataFamilyInstParser;
  _ -> dataDeclParser};
  TkKeywordClass -> classDeclParser;
  TkKeywordDefault -> defaultDeclParser;
  TkKeywordDeriving -> standaloneDerivingDeclParser;
  TkKeywordForeign -> foreignDeclParser;
  TkKeywordInfix -> fixityDeclParser;
  TkKeywordInfixl -> fixityDeclParser;
  TkKeywordInfixr -> fixityDeclParser;
  TkKeywordInstance -> instanceDeclParser;
  TkKeywordNewtype | nextTokKind == TkKeywordInstance -> newtypeFamilyInstParser;
  TkKeywordNewtype -> newtypeDeclParser;
  TkKeywordType -> case nextTokKind of {
  TkVarRole -> roleAnnotationDeclParser;
  TkVarFamily -> typeFamilyDeclParser;
  TkKeywordData -> typeDataDeclParser;
  TkKeywordInstance -> typeFamilyInstParser;
  _ -> typeDeclarationParser};
  TkKeywordPattern -> patternSynonymParser;
  TkPragmaOpen "RULES" -> rulesDeclParser;
  TkVarId {} -> case nextTokKind of {
  TkReservedDoubleColon -> sigOrValueDecl;
  TkSpecialComma -> sigOrValueDecl;
  TkReservedEquals -> valueDecl;
  _ | patternBindMayFollowVarId nextTokKind -> nonBareVarPatternBindDeclParser <|> valueDecl | otherwise -> valueDecl};
  _ -> fallbackDecl}};
patternBindMayFollowVarId :: LexTokenKind -> Bool;
patternBindMayFollowVarId kind = case kind of {
  TkReservedAt -> True;
  TkConSym {} -> True;
  TkQConSym {} -> True;
  TkReservedColon -> True;
  TkSpecialBacktick -> True;
  _ -> False};
nonBareVarPatternBindDeclParser :: TokParser Decl;
nonBareVarPatternBindDeclParser = MP.try $ withSpanAnn (DeclAnn . mkAnnotation) $ do {
  pat <- region "while parsing pattern binding" patternParser;
  case pat of {
  PVar {} -> fail "bare variable bindings are parsed as function declarations";
  _ -> do {
  DeclValue . PatternBind NoMultiplicityTag pat <$> equationRhsParser}}};
pragmaDeclParser :: TokParser Decl;
pragmaDeclParser = withSpanAnn (DeclAnn . mkAnnotation) $ DeclPragma <$> anyPragmaParser "pragma declaration";
rulesDeclParser :: TokParser Decl;
rulesDeclParser = withSpanAnn (DeclAnn . mkAnnotation) $ do {
  expectedTok (TkPragmaOpen "RULES");
  rules <- region "while parsing RULES pragma" (plainSemiSep ruleDeclParser);
  expectedTok TkPragmaClose;
  pure (DeclRules rules)};
ruleDeclParser :: TokParser RuleDecl;
ruleDeclParser = withSpan $ do {
  name <- stringTextParser;
  activation <- MP.optional ruleActivationParser;
  binders <- MP.option [] ruleForallParser;
  (typeBinders, termBinders) <- MP.option ([], binders) $ do {
  termBinders <- ruleForallParser;
  pure (map ruleBinderToTyVarBinder binders, termBinders)};
  lhs <- region "while parsing rule left-hand side" exprParser;
  expectedTok TkReservedEquals;
  rhs <- region "while parsing rule right-hand side" exprParser;
  pure $ \span' -> RuleDecl {ruleAnns = [mkAnnotation span'], ruleName = name, ruleActivation = activation, ruleTypeBinders = typeBinders, ruleBinders = termBinders, ruleLhs = lhs, ruleRhs = rhs}};
ruleActivationParser :: TokParser RuleActivation;
ruleActivationParser = do {
  expectedTok TkSpecialLBracket;
  activation <- (ruleTildeParser *> (RuleActiveBefore <$> rulePhaseParser <|> pure RuleNeverActive)) <|> (RuleActiveAfter <$> rulePhaseParser);
  expectedTok TkSpecialRBracket;
  pure activation};
ruleTildeParser :: TokParser ();
ruleTildeParser = tokenSatisfy "~" $ \tok -> case lexTokenKind tok of {
  TkPrefixTilde -> Just ();
  TkVarSym "~" -> Just ();
  _ -> Nothing};
rulePhaseParser :: TokParser Int;
rulePhaseParser = tokenSatisfy "phase number" $ \tok -> case lexTokenKind tok of {
  TkInteger n _ | n >= 0 && n <= fromIntegral (maxBound :: Int) -> Just (fromInteger n);
  _ -> Nothing};
ruleForallParser :: TokParser [RuleBinder];
ruleForallParser = do {
  expectedTok TkKeywordForall;
  binders <- MP.many ruleBinderParser;
  expectedTok (TkVarSym ".");
  pure binders};
ruleBinderParser :: TokParser RuleBinder;
ruleBinderParser = withSpan $ (do {
  name <- ruleBinderNameParser;
  pure (\span' -> RuleBinder [mkAnnotation span'] name Nothing)}) <|> (do {
  expectedTok TkSpecialLParen;
  name <- ruleBinderNameParser;
  expectedTok TkReservedDoubleColon;
  ty <- typeParser;
  expectedTok TkSpecialRParen;
  pure (\span' -> RuleBinder [mkAnnotation span'] name (Just ty))});
ruleBinderNameParser :: TokParser UnqualifiedName;
ruleBinderNameParser = tokenSatisfy "rule variable" $ \tok -> case lexTokenKind tok of {
  TkVarId ident -> Just (mkUnqualifiedNameAt tok NameVarId ident);
  _ -> Nothing};
ruleBinderToTyVarBinder :: RuleBinder -> TyVarBinder;
ruleBinderToTyVarBinder binder = TyVarBinder {tyVarBinderAnns = ruleBinderAnns binder, tyVarBinderName = unqualifiedNameText (ruleBinderName binder), tyVarBinderKind = ruleBinderType binder, tyVarBinderSpecificity = TyVarBSpecified, tyVarBinderVisibility = TyVarBVisible};
exprDeclEnabled :: TokParser Bool;
exprDeclEnabled = do {
  th <- isExtensionEnabled TemplateHaskell;
  thq <- isExtensionEnabled TemplateHaskellQuotes;
  qq <- isExtensionEnabled QuasiQuotes;
  pure (th || thq || qq)};
exprDeclParser :: TokParser Decl;
exprDeclParser = withSpanAnn (DeclAnn . mkAnnotation) $ DeclSplice <$> exprParser;
typeDeclarationParser :: TokParser Decl;
typeDeclarationParser = withSpanAnn (DeclAnn . mkAnnotation) $ do {
  expectedTok TkKeywordType;
  typeHead <- typeDeclHeadParser;
  nextTok <- anySingle;
  case lexTokenKind nextTok of {
  TkReservedDoubleColon -> do {
  if null (binderHeadParams typeHead) then DeclStandaloneKindSig (binderHeadName typeHead) <$> typeParser else fail "Standalone kind signatures cannot have type parameters."};
  TkReservedEquals -> do {
  body <- typeParser;
  pure $ DeclTypeSyn TypeSynDecl {typeSynHead = typeHead, typeSynBody = body}};
  _ -> MP.customFailure UnexpectedTokenExpecting {unexpectedFound = Just (mkFoundToken nextTok), unexpectedExpecting = "'::' or '=' after type declaration head", unexpectedContext = []}}};
roleAnnotationDeclParser :: TokParser Decl;
roleAnnotationDeclParser = withSpanAnn (DeclAnn . mkAnnotation) $ do {
  expectedTok TkKeywordType;
  expectedTok TkVarRole;
  typeName <- constructorUnqualifiedNameParser <|> parens constructorOperatorUnqualifiedNameParser;
  roles <- MP.many roleParser;
  pure $ DeclRoleAnnotation RoleAnnotation {roleAnnotationName = typeName, roleAnnotationRoles = roles}};
roleParser :: TokParser Role;
roleParser = (varIdTok "nominal" >> pure RoleNominal) <|> (varIdTok "representational" >> pure RoleRepresentational) <|> (varIdTok "phantom" >> pure RolePhantom) <|> (expectedTok TkKeywordUnderscore >> pure RoleInfer);
contextPrefixDispatchList :: TokParser [Type];
contextPrefixDispatchList = MP.option [] (MP.try (declContextParser <* expectedTok TkReservedDoubleArrow));
declHeadPreferringInlineKind :: TokParser head -> TokParser (Maybe [Type], head, Maybe Type);
declHeadPreferringInlineKind headParser = MP.try (do {
  head' <- headParser;
  inlineKind <- expectedTok TkReservedDoubleColon *> typeParser;
  pure (Nothing, head', Just inlineKind)}) <|> do {
  (context, head') <- declHeadWithOptionalContext headParser;
  inlineKind <- optionalTokThen TkReservedDoubleColon typeParser;
  pure (context, head', inlineKind)};
declHeadWithOptionalContext :: TokParser head -> TokParser (Maybe [Type], head);
declHeadWithOptionalContext headParser = MP.try ((,) . Just <$> (declContextParser <* expectedTok TkReservedDoubleArrow) <*> headParser) <|> ((Nothing,) <$> headParser);
explicitForallParser :: TokParser [TyVarBinder];
explicitForallParser = do {
  expectedTok TkKeywordForall;
  binders <- MP.some explicitForallBinderParser;
  expectedTok (TkVarSym ".");
  pure binders};
familyResultKindParser :: TokParser (Maybe Type);
familyResultKindParser = optionalTokThen TkReservedDoubleColon typeParser;
typeFamilyResultSigParser :: Bool -> TokParser (Maybe TypeFamilyResultSig);
typeFamilyResultSigParser explicitFamily = MP.optional (kindSigParser <|> namedSigParser) where {
  kindSigParser = TypeFamilyKindSig <$> (expectedTok TkReservedDoubleColon *> typeParser);
  namedSigParser = do {
  expectedTok TkReservedEquals;
  result <- namedResultBinderParser;
  mInjectivity <- MP.optional typeFamilyInjectivityParser;
  case mInjectivity of {
  Just injectivity -> pure $ TypeFamilyInjectiveSig result injectivity;
  Nothing | explicitFamily -> pure $ TypeFamilyTyVarSig result | otherwise -> fail "named result sig without injectivity requires explicit 'family' keyword"}};
  namedResultBinderParser = withSpan $ (do {
  ident <- lowerIdentifierParser;
  pure (\span' -> TyVarBinder [mkAnnotation span'] ident Nothing TyVarBSpecified TyVarBVisible)}) <|> (do {
  expectedTok TkSpecialLParen;
  ident <- lowerIdentifierParser;
  expectedTok TkReservedDoubleColon;
  kind <- typeParser;
  expectedTok TkSpecialRParen;
  pure (\span' -> TyVarBinder [mkAnnotation span'] ident (Just kind) TyVarBSpecified TyVarBVisible)})};
typeFamilyInjectivityParser :: TokParser TypeFamilyInjectivity;
typeFamilyInjectivityParser = withSpan $ do {
  expectedTok TkReservedPipe;
  result <- lowerIdentifierParser;
  expectedTok TkReservedRightArrow;
  determined <- MP.some lowerIdentifierParser;
  pure $ \span' -> TypeFamilyInjectivity {typeFamilyInjectivityAnns = [mkAnnotation span'], typeFamilyInjectivityResult = result, typeFamilyInjectivityDetermined = determined}};
typeFamilyDeclParser :: TokParser Decl;
typeFamilyDeclParser = withSpanAnn (DeclAnn . mkAnnotation) $ DeclTypeFamilyDecl <$> typeFamilyDeclBodyParser FamilyKeywordRequired;
closedTypeFamilyWhereParser :: TokParser [TypeFamilyEq];
closedTypeFamilyWhereParser = whereClauseItemsParser typeFamilyEqParser;
typeFamilyEqParser :: TokParser TypeFamilyEq;
typeFamilyEqParser = withSpan $ do {
  forallBinders <- MP.option [] explicitForallParser;
  (headForm, lhs) <- typeFamilyLhsParser;
  expectedTok TkReservedEquals;
  rhs <- typeParser;
  pure $ \span' -> TypeFamilyEq {typeFamilyEqAnns = [mkAnnotation span'], typeFamilyEqForall = forallBinders, typeFamilyEqHeadForm = headForm, typeFamilyEqLhs = lhs, typeFamilyEqRhs = rhs}};
dataFamilyDeclParser :: TokParser Decl;
dataFamilyDeclParser = withSpanAnn (DeclAnn . mkAnnotation) $ do {
  expectedTok TkKeywordData;
  varIdTok "family";
  head' <- typeDeclHeadParser;
  kind <- familyResultKindParser;
  pure $ DeclDataFamilyDecl DataFamilyDecl {dataFamilyDeclHead = head', dataFamilyDeclKind = kind}};
typeFamilyInstParser :: TokParser Decl;
typeFamilyInstParser = withSpanAnn (DeclAnn . mkAnnotation) $ do {
  expectedTok TkKeywordType;
  expectedTok TkKeywordInstance;
  forallBinders <- MP.option [] explicitForallParser;
  (headForm, lhs) <- typeFamilyLhsParser;
  expectedTok TkReservedEquals;
  rhs <- typeParser;
  pure $ DeclTypeFamilyInst TypeFamilyInst {typeFamilyInstForall = forallBinders, typeFamilyInstHeadForm = headForm, typeFamilyInstLhs = lhs, typeFamilyInstRhs = rhs}};
dataFamilyInstParser :: TokParser Decl;
dataFamilyInstParser = withSpanAnn (DeclAnn . mkAnnotation) $ do {
  expectedTok TkKeywordData;
  expectedTok TkKeywordInstance;
  forallBinders <- MP.option [] explicitForallParser;
  (_, head') <- typeFamilyLhsParser;
  kind <- familyResultKindParser;
  (constructors, derivingClauses) <- gadtDataDeclParser <|> traditionalDataDeclParser;
  pure $ DeclDataFamilyInst DataFamilyInst {dataFamilyInstIsNewtype = False, dataFamilyInstForall = forallBinders, dataFamilyInstHead = head', dataFamilyInstKind = kind, dataFamilyInstConstructors = constructors, dataFamilyInstDeriving = derivingClauses}};
newtypeFamilyInstParser :: TokParser Decl;
newtypeFamilyInstParser = withSpanAnn (DeclAnn . mkAnnotation) $ do {
  expectedTok TkKeywordNewtype;
  expectedTok TkKeywordInstance;
  forallBinders <- MP.option [] explicitForallParser;
  (_, head') <- typeFamilyLhsParser;
  kind <- familyResultKindParser;
  expectedTok TkReservedEquals;
  constructor <- dataConDeclParser;
  derivingClauses <- MP.many derivingClauseParser;
  pure $ DeclDataFamilyInst DataFamilyInst {dataFamilyInstIsNewtype = True, dataFamilyInstForall = forallBinders, dataFamilyInstHead = head', dataFamilyInstKind = kind, dataFamilyInstConstructors = [constructor], dataFamilyInstDeriving = derivingClauses}};
classTypeFamilyDeclParser :: TokParser ClassDeclItem;
classTypeFamilyDeclParser = withSpanAnn (ClassItemAnn . mkAnnotation) $ ClassItemTypeFamilyDecl <$> typeFamilyDeclBodyParser FamilyKeywordOptional;
classDataFamilyDeclParser :: TokParser ClassDeclItem;
classDataFamilyDeclParser = withSpanAnn (ClassItemAnn . mkAnnotation) $ do {
  expectedTok TkKeywordData;
  head' <- typeDeclHeadParser;
  kind <- familyResultKindParser;
  pure (ClassItemDataFamilyDecl DataFamilyDecl {dataFamilyDeclHead = head', dataFamilyDeclKind = kind})};
classDefaultTypeInstParser :: TokParser ClassDeclItem;
classDefaultTypeInstParser = classDefaultTypeInstParser' True;
classDefaultTypeInstShorthandParser :: TokParser ClassDeclItem;
classDefaultTypeInstShorthandParser = classDefaultTypeInstParser' False;
classDefaultTypeInstParser' :: Bool -> TokParser ClassDeclItem;
classDefaultTypeInstParser' requireInstance = withSpanAnn (ClassItemAnn . mkAnnotation) $ do {
  expectedTok TkKeywordType;
  when requireInstance (expectedTok TkKeywordInstance);
  forallBinders <- MP.option [] explicitForallParser;
  (headForm, lhs) <- typeFamilyLhsParser;
  expectedTok TkReservedEquals;
  rhs <- typeParser;
  pure (ClassItemDefaultTypeInst TypeFamilyInst {typeFamilyInstForall = forallBinders, typeFamilyInstHeadForm = headForm, typeFamilyInstLhs = lhs, typeFamilyInstRhs = rhs})};
instanceTypeFamilyInstParser :: TokParser InstanceDeclItem;
instanceTypeFamilyInstParser = withSpanAnn (InstanceItemAnn . mkAnnotation) $ do {
  expectedTok TkKeywordType;
  _ <- optionalTok TkKeywordInstance;
  forallBinders <- MP.option [] explicitForallParser;
  (headForm, lhs) <- typeFamilyLhsParser;
  expectedTok TkReservedEquals;
  rhs <- typeParser;
  pure $ InstanceItemTypeFamilyInst TypeFamilyInst {typeFamilyInstForall = forallBinders, typeFamilyInstHeadForm = headForm, typeFamilyInstLhs = lhs, typeFamilyInstRhs = rhs}};
instanceDataFamilyInstParser :: TokParser InstanceDeclItem;
instanceDataFamilyInstParser = withSpanAnn (InstanceItemAnn . mkAnnotation) $ do {
  expectedTok TkKeywordData;
  _ <- optionalTok TkKeywordInstance;
  (_, head') <- typeFamilyLhsParser;
  kind <- familyResultKindParser;
  (constructors, derivingClauses) <- gadtDataDeclParser <|> traditionalDataDeclParser;
  pure $ InstanceItemDataFamilyInst DataFamilyInst {dataFamilyInstIsNewtype = False, dataFamilyInstForall = [], dataFamilyInstHead = head', dataFamilyInstKind = kind, dataFamilyInstConstructors = constructors, dataFamilyInstDeriving = derivingClauses}};
instanceNewtypeFamilyInstParser :: TokParser InstanceDeclItem;
instanceNewtypeFamilyInstParser = withSpanAnn (InstanceItemAnn . mkAnnotation) $ do {
  expectedTok TkKeywordNewtype;
  _ <- optionalTok TkKeywordInstance;
  (_, head') <- typeFamilyLhsParser;
  kind <- familyResultKindParser;
  expectedTok TkReservedEquals;
  constructor <- dataConDeclParser;
  derivingClauses <- MP.many derivingClauseParser;
  pure $ InstanceItemDataFamilyInst DataFamilyInst {dataFamilyInstIsNewtype = True, dataFamilyInstForall = [], dataFamilyInstHead = head', dataFamilyInstKind = kind, dataFamilyInstConstructors = [constructor], dataFamilyInstDeriving = derivingClauses}};
typeSigDeclParser :: TokParser Decl;
typeSigDeclParser = withSpanAnn (DeclAnn . mkAnnotation) $ uncurry DeclTypeSig <$> typedSignaturePrefixParser typeSignatureParser;
typeSigOrPatternTypeSigDeclParser :: TokParser Decl;
typeSigOrPatternTypeSigDeclParser = withSpanAnn (DeclAnn . mkAnnotation) $ typedBindingOrSignatureParser typeSignatureParser DeclTypeSig (\name ty -> do {
  rhs <- equationRhsParser;
  let {
  pat = PTypeSig (PVar name) ty};
  pure (DeclValue (PatternBind NoMultiplicityTag pat rhs))}) "typed pattern bindings with '=' require exactly one binder";
defaultDeclParser :: TokParser Decl;
defaultDeclParser = withSpanAnn (DeclAnn . mkAnnotation) $ do {
  expectedTok TkKeywordDefault;
  DeclDefault <$> parens (typeParser `MP.sepEndBy` expectedTok TkSpecialComma)};
fixityDeclParser :: TokParser Decl;
fixityDeclParser = withSpanAnn (DeclAnn . mkAnnotation) $ do {
  (parsedAssoc, prec, mNamespace, ops) <- fixityDeclPartsParser;
  pure (DeclFixity parsedAssoc mNamespace prec ops)};
fixityDeclPartsParser :: TokParser (FixityAssoc, Maybe Int, Maybe IEEntityNamespace, [UnqualifiedName]);
fixityDeclPartsParser = do {
  assoc <- fixityAssocParser;
  prec <- MP.optional fixityPrecedenceParser;
  mNamespace <- MP.optional fixityNamespaceParser;
  ops <- fixityOperatorParser `MP.sepBy1` expectedTok TkSpecialComma;
  pure (assoc, prec, mNamespace, ops)};
fixityNamespaceParser :: TokParser IEEntityNamespace;
fixityNamespaceParser = (expectedTok TkKeywordType >> pure IEEntityNamespaceType) <|> (expectedTok TkKeywordData >> pure IEEntityNamespaceData);
fixityAssocParser :: TokParser FixityAssoc;
fixityAssocParser = (expectedTok TkKeywordInfix >> pure Infix) <|> (expectedTok TkKeywordInfixl >> pure InfixL) <|> (expectedTok TkKeywordInfixr >> pure InfixR);
fixityPrecedenceParser :: TokParser Int;
fixityPrecedenceParser = tokenSatisfy "fixity precedence" $ \tok -> case lexTokenKind tok of {
  TkInteger n _ | n <= 9 -> Just (fromInteger n);
  _ -> Nothing};
fixityOperatorParser :: TokParser UnqualifiedName;
fixityOperatorParser = symbolicOperatorParser <|> backtickIdentifierParser where {
  symbolicOperatorParser = tokenSatisfy "fixity operator" $ \tok -> case lexTokenKind tok of {
  TkVarSym op -> Just (mkUnqualifiedNameAt tok NameVarSym op);
  TkConSym op -> Just (mkUnqualifiedNameAt tok NameConSym op);
  TkReservedColon -> Just (mkUnqualifiedNameAt tok NameConSym ":");
  TkReservedRightArrow -> Just (mkUnqualifiedNameAt tok NameVarSym "->");
  _ -> Nothing};
  backtickIdentifierParser = do {
  expectedTok TkSpecialBacktick;
  op <- identifierUnqualifiedNameParser;
  expectedTok TkSpecialBacktick;
  pure op}};
classDeclParser :: TokParser Decl;
classDeclParser = withSpanAnn (DeclAnn . mkAnnotation) $ do {
  expectedTok TkKeywordClass;
  (context, classHead) <- declHeadWithOptionalContext classHeadParser;
  classFundeps <- MP.option [] (MP.try classFundepsParser);
  items <- MP.option [] classWhereClauseParser;
  pure $ DeclClass ClassDecl {classDeclContext = context, classDeclHead = classHead, classDeclFundeps = classFundeps, classDeclItems = items}};
classFundepsParser :: TokParser [FunctionalDependency];
classFundepsParser = do {
  expectedTok TkReservedPipe;
  classFundepParser `MP.sepBy1` expectedTok TkSpecialComma};
classFundepParser :: TokParser FunctionalDependency;
classFundepParser = withSpan $ do {
  determinedBy <- MP.many lowerIdentifierParser;
  expectedTok TkReservedRightArrow;
  determines <- MP.many lowerIdentifierParser;
  pure $ \span' -> FunctionalDependency {functionalDependencyAnns = [mkAnnotation span'], functionalDependencyDeterminers = determinedBy, functionalDependencyDetermined = determines}};
classWhereClauseParser :: TokParser [ClassDeclItem];
classWhereClauseParser = do {
  expectedTok TkKeywordWhere;
  bracedSemiSep classDeclItemParser <|> plainSemiSep1 classDeclItemParser <|> pure []};
whereClauseItemsParser :: TokParser a -> TokParser [a];
whereClauseItemsParser itemParser = do {
  expectedTok TkKeywordWhere;
  bracedSemiSep itemParser <|> plainSemiSep1 itemParser <|> pure []};
classDeclItemParser :: TokParser ClassDeclItem;
classDeclItemParser = classPragmaItemParser <|> do {
  (tok, nextTok) <- lookAhead ((,) <$> anySingle <*> anySingle);
  typeSigPrefix <- startsWithTypeSig;
  case lexTokenKind tok of {
  TkKeywordInfix -> classFixityItemParser;
  TkKeywordInfixl -> classFixityItemParser;
  TkKeywordInfixr -> classFixityItemParser;
  TkKeywordData -> classDataFamilyDeclParser;
  TkKeywordDefault -> classDefaultSigItemParser;
  TkKeywordType | lexTokenKind nextTok == TkKeywordInstance -> classDefaultTypeInstParser;
  TkKeywordType -> MP.try classTypeFamilyDeclParser <|> classDefaultTypeInstShorthandParser;
  _ -> (if typeSigPrefix then classTypeSigItemParser else classDefaultItemParser)}};
classPragmaItemParser :: TokParser ClassDeclItem;
classPragmaItemParser = withSpanAnn (ClassItemAnn . mkAnnotation) $ ClassItemPragma <$> anyPragmaParser "pragma declaration";
classTypeSigItemParser :: TokParser ClassDeclItem;
classTypeSigItemParser = typeSigItemParser (ClassItemAnn . mkAnnotation) ClassItemTypeSig;
classDefaultSigItemParser :: TokParser ClassDeclItem;
classDefaultSigItemParser = withSpanAnn (ClassItemAnn . mkAnnotation) $ do {
  expectedTok TkKeywordDefault;
  name <- binderNameParser;
  expectedTok TkReservedDoubleColon;
  ClassItemDefaultSig name <$> typeSignatureParser};
classFixityItemParser :: TokParser ClassDeclItem;
classFixityItemParser = fixityItemParser (ClassItemAnn . mkAnnotation) ClassItemFixity;
classDefaultItemParser :: TokParser ClassDeclItem;
classDefaultItemParser = valueItemParser (ClassItemAnn . mkAnnotation) ClassItemDefault;
instanceDeclParser :: TokParser Decl;
instanceDeclParser = withSpanAnn (DeclAnn . mkAnnotation) $ do {
  expectedTok TkKeywordInstance;
  overlapPragmas <- MP.option [] (fmap (: []) instanceOverlapPragmaParser);
  warningText <- MP.optional warningPragmaParser;
  forallBinders <- MP.optional explicitForallParser;
  (context, instanceHead) <- declHeadWithOptionalContext typeInfixParser;
  items <- MP.option [] instanceWhereClauseParser;
  pure $ DeclInstance InstanceDecl {instanceDeclPragmas = overlapPragmas, instanceDeclWarning = warningText, instanceDeclForall = fromMaybe [] forallBinders, instanceDeclContext = fromMaybe [] context, instanceDeclHead = instanceHead, instanceDeclItems = items}};
standaloneDerivingDeclParser :: TokParser Decl;
standaloneDerivingDeclParser = withSpanAnn (DeclAnn . mkAnnotation) $ do {
  expectedTok TkKeywordDeriving;
  strategy <- MP.optional derivingStrategyParser;
  expectedTok TkKeywordInstance;
  overlapPragmas <- MP.option [] (fmap (: []) instanceOverlapPragmaParser);
  warningText <- MP.optional warningPragmaParser;
  forallBinders <- MP.optional explicitForallParser;
  (context, derivingHead) <- declHeadWithOptionalContext typeInfixParser;
  pure $ DeclStandaloneDeriving StandaloneDerivingDecl {standaloneDerivingStrategy = strategy, standaloneDerivingPragmas = overlapPragmas, standaloneDerivingWarning = warningText, standaloneDerivingForall = fromMaybe [] forallBinders, standaloneDerivingContext = fromMaybe [] context, standaloneDerivingHead = derivingHead}};
instanceWhereClauseParser :: TokParser [InstanceDeclItem];
instanceWhereClauseParser = do {
  expectedTok TkKeywordWhere;
  bracedSemiSep instanceDeclItemParser <|> plainSemiSep1 instanceDeclItemParser <|> pure []};
instanceDeclItemParser :: TokParser InstanceDeclItem;
instanceDeclItemParser = instancePragmaItemParser <|> do {
  tok <- peekToken;
  typeSigPrefix <- startsWithTypeSig;
  case lexTokenKind tok of {
  TkKeywordInfix -> instanceFixityItemParser;
  TkKeywordInfixl -> instanceFixityItemParser;
  TkKeywordInfixr -> instanceFixityItemParser;
  TkKeywordData -> instanceDataFamilyInstParser;
  TkKeywordNewtype -> instanceNewtypeFamilyInstParser;
  TkKeywordType -> instanceTypeFamilyInstParser;
  _ -> (if typeSigPrefix then instanceTypeSigItemParser else instanceValueItemParser)}};
instancePragmaItemParser :: TokParser InstanceDeclItem;
instancePragmaItemParser = withSpanAnn (InstanceItemAnn . mkAnnotation) $ do {
  pragma <- anyPragmaParser "pragma declaration";
  pure (InstanceItemPragma pragma)};
instanceTypeSigItemParser :: TokParser InstanceDeclItem;
instanceTypeSigItemParser = typeSigItemParser (InstanceItemAnn . mkAnnotation) InstanceItemTypeSig;
instanceFixityItemParser :: TokParser InstanceDeclItem;
instanceFixityItemParser = fixityItemParser (InstanceItemAnn . mkAnnotation) InstanceItemFixity;
instanceValueItemParser :: TokParser InstanceDeclItem;
instanceValueItemParser = valueItemParser (InstanceItemAnn . mkAnnotation) InstanceItemBind;
typeSigItemParser :: (SourceSpan -> a -> a) -> ([UnqualifiedName] -> Type -> a) -> TokParser a;
typeSigItemParser ann ctor = withSpanAnn ann $ uncurry ctor <$> typedSignaturePrefixParser typeSignatureParser;
fixityItemParser :: (SourceSpan -> a -> a) -> (FixityAssoc -> Maybe IEEntityNamespace -> Maybe Int -> [UnqualifiedName] -> a) -> TokParser a;
fixityItemParser ann ctor = withSpanAnn ann $ do {
  (assoc, prec, mNamespace, ops) <- fixityDeclPartsParser;
  pure (ctor assoc mNamespace prec ops)};
valueItemParser :: (SourceSpan -> a -> a) -> (ValueDecl -> a) -> TokParser a;
valueItemParser ann ctor = withSpanAnn ann $ do {
  (headForm, name, pats) <- functionHeadParserWith patParser apatParser;
  ctor . functionBindValue headForm name pats <$> equationRhsParser};
foreignDeclParser :: TokParser Decl;
foreignDeclParser = withSpanAnn (DeclAnn . mkAnnotation) $ do {
  expectedTok TkKeywordForeign;
  direction <- foreignDirectionParser;
  callConv <- callConvParser;
  safety <- case direction of {
  ForeignImport -> MP.optional foreignSafetyParser;
  ForeignExport -> pure Nothing};
  entity <- MP.optional foreignEntityParser;
  name <- binderNameParser;
  expectedTok TkReservedDoubleColon;
  ty <- typeSignatureParser;
  pure $ DeclForeign ForeignDecl {foreignDirection = direction, foreignCallConv = callConv, foreignSafety = safety, foreignEntity = fromMaybe ForeignEntityOmitted entity, foreignName = name, foreignType = ty}};
foreignDirectionParser :: TokParser ForeignDirection;
foreignDirectionParser = (expectedTok TkKeywordImport >> pure ForeignImport) <|> (varIdTok "export" >> pure ForeignExport);
callConvParser :: TokParser CallConv;
callConvParser = (varIdTok "ccall" >> pure CCall) <|> (varIdTok "stdcall" >> pure StdCall) <|> (varIdTok "capi" >> pure CApi) <|> (varIdTok "prim" >> pure CPrim) <|> (varIdTok "javascript" >> pure JavaScript);
foreignSafetyParser :: TokParser ForeignSafety;
foreignSafetyParser = (varIdTok "safe" >> pure Safe) <|> (varIdTok "unsafe" >> pure Unsafe) <|> (varIdTok "interruptible" >> pure Interruptible);
foreignEntityParser :: TokParser ForeignEntitySpec;
foreignEntityParser = foreignEntityFromString <$> stringTextParser;
foreignEntityFromString :: Text -> ForeignEntitySpec;
foreignEntityFromString txt | txt == "dynamic" = ForeignEntityDynamic | txt == "wrapper" = ForeignEntityWrapper | txt == "static" = ForeignEntityStatic Nothing | Just rest <- T.stripPrefix "static " txt = ForeignEntityStatic (Just rest) | txt == "&" = ForeignEntityAddress Nothing | Just rest <- T.stripPrefix "&" txt = ForeignEntityAddress (Just rest) | otherwise = ForeignEntityNamed txt;
traditionalDataDeclParser :: TokParser ([DataConDecl], [DerivingClause]);
traditionalDataDeclParser = do {
  constructors <- optionalTokThen TkReservedEquals (dataConDeclParser `MP.sepBy1` expectedTok TkReservedPipe);
  derivingClauses <- MP.many derivingClauseParser;
  pure (fromMaybe [] constructors, derivingClauses)};
gadtDataDeclParser :: TokParser ([DataConDecl], [DerivingClause]);
gadtDataDeclParser = do {
  constructors <- gadtWhereClauseParser;
  derivingClauses <- MP.many derivingClauseParser;
  pure (constructors, derivingClauses)};
dataDeclParser :: TokParser Decl;
dataDeclParser = withSpanAnn (DeclAnn . mkAnnotation) $ do {
  expectedTok TkKeywordData;
  ctypePragma <- optionalHiddenPragma Just;
  (context, typeHead, inlineKind) <- declHeadPreferringInlineKind typeDeclHeadParser;
  (constructors, derivingClauses) <- gadtDataDeclParser <|> traditionalDataDeclParser;
  pure $ DeclData DataDecl {dataDeclCTypePragma = ctypePragma, dataDeclHead = typeHead, dataDeclContext = fromMaybe [] context, dataDeclKind = inlineKind, dataDeclConstructors = constructors, dataDeclDeriving = derivingClauses}};
typeDataDeclParser :: TokParser Decl;
typeDataDeclParser = withSpanAnn (DeclAnn . mkAnnotation) $ do {
  expectedTok TkKeywordType;
  expectedTok TkKeywordData;
  typeHead <- typeDeclHeadParser;
  inlineKind <- optionalTokThen TkReservedDoubleColon typeParser;
  constructors <- gadtStyleTypeDataDecl <|> traditionalStyleTypeDataDecl;
  pure $ DeclTypeData DataDecl {dataDeclCTypePragma = Nothing, dataDeclHead = typeHead, dataDeclContext = [], dataDeclKind = inlineKind, dataDeclConstructors = constructors, dataDeclDeriving = []}} where {
  traditionalStyleTypeDataDecl = fromMaybe [] <$> optionalTokThen TkReservedEquals (typeDataConDeclParser `MP.sepBy1` expectedTok TkReservedPipe);
  gadtStyleTypeDataDecl = gadtTypeDataWhereClauseParser};
typeDataConDeclParser :: TokParser DataConDecl;
typeDataConDeclParser = withSpanAnn (DataConAnn . mkAnnotation) $ do {
  (_forallVars, context) <- dataConQualifiersParser;
  MP.try (typeDataConPrefixParser context) <|> typeDataConInfixParser context};
typeDataConPrefixParser :: [Type] -> TokParser DataConDecl;
typeDataConPrefixParser context = do {
  conName <- constructorUnqualifiedNameParser <|> parens constructorOperatorUnqualifiedNameParser;
  args <- MP.many $ BangType [] [] False False <$> typeAtomParser;
  MP.notFollowedBy constructorOperatorParser;
  pure (PrefixCon [] context conName args)};
typeDataConInfixParser :: [Type] -> TokParser DataConDecl;
typeDataConInfixParser context = do {
  lhs <- typeDataConArgParser;
  op <- constructorOperatorUnqualifiedNameParser <|> backtickConstructorUnqualifiedParser;
  InfixCon [] context lhs op <$> typeDataConArgParser} where {
  backtickConstructorUnqualifiedParser = do {
  expectedTok TkSpecialBacktick;
  name <- constructorUnqualifiedNameParser;
  expectedTok TkSpecialBacktick;
  pure name}};
typeDataConArgParser :: TokParser BangType;
typeDataConArgParser = BangType [] [] False False <$> typeAtomParser;
gadtTypeDataWhereClauseParser :: TokParser [DataConDecl];
gadtTypeDataWhereClauseParser = whereClauseItemsParser gadtTypeDataConDeclParser;
gadtTypeDataConDeclParser :: TokParser DataConDecl;
gadtTypeDataConDeclParser = withSpanAnn (DataConAnn . mkAnnotation) $ do {
  names <- gadtConNameParser `MP.sepBy1` expectedTok TkSpecialComma;
  expectedTok TkReservedDoubleColon;
  forallBinders <- MP.many gadtForallParser;
  context <- contextPrefixDispatchList;
  GadtCon forallBinders context names <$> gadtTypeDataBodyParser};
gadtTypeDataBodyParser :: TokParser GadtBody;
gadtTypeDataBodyParser = do {
  firstTy <- typeAppParser;
  rest <- MP.many ((,) <$> arrowKindParser <*> typeAppParser);
  case rest of {
  [] -> pure (GadtPrefixBody [] firstTy);
  _ -> let {
  mkBang = BangType [] [] False False;
  allTys = firstTy : map snd rest;
  arrowKinds = map fst rest;
  argTys = init allTys;
  resultTy = last allTys;
  argsWithKinds = zip (map mkBang argTys) arrowKinds} in pure (GadtPrefixBody argsWithKinds resultTy)}};
dataConDeclParser :: TokParser DataConDecl;
dataConDeclParser = withSpanAnn (DataConAnn . mkAnnotation) $ do {
  (forallVars, context) <- dataConQualifiersParser;
  tok <- peekToken;
  case lexTokenKind tok of {
  TkSpecialUnboxedLParen -> MP.try (dataConInfixParser forallVars context) <|> unboxedConDeclParser forallVars context;
  TkSpecialLBracket -> MP.try (listConDeclParser forallVars context) <|> dataConInfixParser forallVars context;
  _ -> MP.try (dataConInfixParser forallVars context) <|> MP.try (boxedTupleConDeclParser forallVars context) <|> dataConRecordOrPrefixParser forallVars context}};
listConDeclParser :: [TyVarBinder] -> [Type] -> TokParser DataConDecl;
listConDeclParser forallVars context = do {
  expectedTok TkSpecialLBracket;
  expectedTok TkSpecialRBracket;
  pure (ListCon forallVars context)};
boxedTupleConDeclParser :: [TyVarBinder] -> [Type] -> TokParser DataConDecl;
boxedTupleConDeclParser forallVars context = do {
  expectedTok TkSpecialLParen;
  closedImmediately <- optionalTok TkSpecialRParen;
  if closedImmediately then pure (TupleCon forallVars context Boxed []) else do {
  firstField <- constructorArgParser;
  expectedTok TkSpecialComma;
  rest <- constructorArgParser `MP.sepBy1` expectedTok TkSpecialComma;
  expectedTok TkSpecialRParen;
  pure (TupleCon forallVars context Boxed (firstField : rest))}};
unboxedConDeclParser :: [TyVarBinder] -> [Type] -> TokParser DataConDecl;
unboxedConDeclParser forallVars context = do {
  expectedTok TkSpecialUnboxedLParen;
  closedImmediately <- optionalTok TkSpecialUnboxedRParen;
  if closedImmediately then pure (TupleCon forallVars context Unboxed []) else do {
  leadingPipes <- MP.many (MP.try (expectedTok TkReservedPipe));
  if not (null leadingPipes) then do {
  field <- constructorArgParser;
  trailingPipes <- MP.many (expectedTok TkReservedPipe);
  expectedTok TkSpecialUnboxedRParen;
  let {
  pos = length leadingPipes + 1;
  arity = length leadingPipes + 1 + length trailingPipes};
  pure (UnboxedSumCon forallVars context pos arity field)} else do {
  firstField <- constructorArgParser;
  mSep <- MP.optional (MP.try ((Left () <$ expectedTok TkSpecialComma) <|> (Right () <$ expectedTok TkReservedPipe)));
  case mSep of {
  Nothing -> do {
  expectedTok TkSpecialUnboxedRParen;
  pure (TupleCon forallVars context Unboxed [firstField])};
  Just (Left ()) -> do {
  rest <- constructorArgParser `MP.sepBy1` expectedTok TkSpecialComma;
  expectedTok TkSpecialUnboxedRParen;
  pure (TupleCon forallVars context Unboxed (firstField : rest))};
  Just (Right ()) -> do {
  trailingPipes <- MP.many (expectedTok TkReservedPipe);
  expectedTok TkSpecialUnboxedRParen;
  let {
  arity = 1 + 1 + length trailingPipes};
  pure (UnboxedSumCon forallVars context 1 arity firstField)}}}}};
newtypeDeclParser :: TokParser Decl;
newtypeDeclParser = withSpanAnn (DeclAnn . mkAnnotation) $ do {
  expectedTok TkKeywordNewtype;
  ctypePragma <- optionalHiddenPragma Just;
  (context, typeHead, inlineKind) <- declHeadPreferringInlineKind typeDeclHeadParser;
  (constructor, derivingClauses) <- gadtStyleNewtypeDecl <|> traditionalStyleNewtypeDecl;
  pure $ DeclNewtype NewtypeDecl {newtypeDeclCTypePragma = ctypePragma, newtypeDeclHead = typeHead, newtypeDeclContext = fromMaybe [] context, newtypeDeclKind = inlineKind, newtypeDeclConstructor = constructor, newtypeDeclDeriving = derivingClauses}} where {
  traditionalStyleNewtypeDecl = do {
  constructor <- optionalTokThen TkReservedEquals dataConDeclParser;
  derivingClauses <- MP.many derivingClauseParser;
  pure (constructor, derivingClauses)};
  gadtStyleNewtypeDecl = do {
  constructors <- gadtWhereClauseParser;
  case constructors of {
  [ctor] -> do {
  derivingClauses <- MP.many derivingClauseParser;
  pure (Just ctor, derivingClauses)};
  _ -> fail "newtype must have exactly one constructor"}}};
gadtWhereClauseParser :: TokParser [DataConDecl];
gadtWhereClauseParser = whereClauseItemsParser gadtConDeclParser;
gadtConDeclParser :: TokParser DataConDecl;
gadtConDeclParser = withSpanAnn (DataConAnn . mkAnnotation) $ do {
  names <- gadtConNameParser `MP.sepBy1` expectedTok TkSpecialComma;
  expectedTok TkReservedDoubleColon;
  forallBinders <- MP.many gadtForallParser;
  context <- contextPrefixDispatchList;
  GadtCon forallBinders context names <$> gadtBodyParser};
gadtConNameParser :: TokParser UnqualifiedName;
gadtConNameParser = constructorUnqualifiedNameParser <|> parens constructorOperatorUnqualifiedNameParser;
gadtForallParser :: TokParser ForallTelescope;
gadtForallParser = forallTelescopeParser;
gadtBodyParser :: TokParser GadtBody;
gadtBodyParser = gadtRecordBodyParser <|> gadtPrefixBodyParser;
gadtRecordBodyParser :: TokParser GadtBody;
gadtRecordBodyParser = do {
  fields <- recordFieldsParser;
  expectedTok TkReservedRightArrow;
  GadtRecordBody fields <$> gadtResultTypeParser};
gadtPrefixBodyParser :: TokParser GadtBody;
gadtPrefixBodyParser = do {
  firstBang <- gadtBangTypeParser;
  rest <- MP.many ((,) <$> gadtArrowParser <*> gadtBangTypeParser);
  case rest of {
  [] -> pure (GadtPrefixBody [] (bangType firstBang));
  _ -> let {
  allBangs = firstBang : map snd rest;
  arrowKinds = map fst rest;
  args = init allBangs;
  result = last allBangs;
  argsWithKinds = zip args arrowKinds} in pure (GadtPrefixBody argsWithKinds (bangType result))}};
gadtArrowParser :: TokParser ArrowKind;
gadtArrowParser = arrowKindParser;
gadtBangTypeParser :: TokParser BangType;
gadtBangTypeParser = bangTypeParserWith typeInfixParser;
gadtResultTypeParser :: TokParser Type;
gadtResultTypeParser = typeParser;
declContextParser :: TokParser [Type];
declContextParser = contextParserWith typeParser typeAtomParser;
declHeadParserWith :: TokParser UnqualifiedName -> TokParser (BinderHead UnqualifiedName);
declHeadParserWith opParser = MP.try parenthesizedInfixDeclHeadParser <|> MP.try parenthesizedPrefixDeclHeadParser <|> MP.try infixDeclHeadParser <|> prefixDeclHeadParser where {
  prefixDeclHeadParser = do {
  name <- constructorUnqualifiedNameParser <|> parens operatorUnqualifiedNameParser;
  params <- MP.many declTypeParamParser;
  pure (PrefixBinderHead name params)};
  infixDeclHeadParser = do {
  lhs <- declTypeParamParser;
  op <- opParser;
  rhs <- declTypeParamParser;
  pure (InfixBinderHead lhs op rhs [])};
  parenthesizedInfixDeclHeadParser = do {
  expectedTok TkSpecialLParen;
  lhs <- declTypeParamParser;
  op <- opParser;
  rhs <- declTypeParamParser;
  expectedTok TkSpecialRParen;
  tailParams <- MP.many declTypeParamParser;
  pure (InfixBinderHead lhs op rhs tailParams)};
  parenthesizedPrefixDeclHeadParser = do {
  expectedTok TkSpecialLParen;
  name <- constructorUnqualifiedNameParser;
  params <- MP.some declTypeParamParser;
  expectedTok TkSpecialRParen;
  tailParams <- MP.many declTypeParamParser;
  pure (PrefixBinderHead name (params <> tailParams))}};
typeDeclHeadParser :: TokParser (BinderHead UnqualifiedName);
typeDeclHeadParser = declHeadParserWith typeSynonymOperatorParser;
typeSynonymOperatorParser :: TokParser UnqualifiedName;
typeSynonymOperatorParser = symbolicTypeSynonymOperatorParser <|> backtickTypeSynonymIdentifierParser where {
  symbolicTypeSynonymOperatorParser = tokenSatisfy "type synonym operator" $ \tok -> case lexTokenKind tok of {
  TkVarSym op -> Just (mkUnqualifiedNameAt tok NameVarSym op);
  TkConSym op -> Just (mkUnqualifiedNameAt tok NameConSym op);
  TkReservedAt -> Just (mkUnqualifiedNameAt tok NameVarSym "@");
  _ -> Nothing};
  backtickTypeSynonymIdentifierParser = do {
  expectedTok TkSpecialBacktick;
  op <- identifierUnqualifiedNameParser;
  expectedTok TkSpecialBacktick;
  pure op}};
typeFamilyHeadParser :: TokParser (TypeHeadForm, Type, [TyVarBinder]);
typeFamilyHeadParser = withSpan $ do {
  binderHead <- declHeadParserWith (nameToUnqualified <$> typeFamilyOperatorParser);
  pure (`binderHeadToTypeFamilyHead` binderHead)};
typeFamilyOperatorParser :: TokParser Name;
typeFamilyOperatorParser = operatorNameParser <|> backtickTypeFamilyIdentifierParser where {
  backtickTypeFamilyIdentifierParser = do {
  expectedTok TkSpecialBacktick;
  op <- constructorNameParser;
  expectedTok TkSpecialBacktick;
  pure op}};
typeFamilyLhsParser :: TokParser (TypeHeadForm, Type);
typeFamilyLhsParser = do {
  lhs <- typeAppParser;
  hasInfixTail <- MP.optional (lookAhead typeInfixOperatorParser);
  case hasInfixTail of {
  Just _ -> do {
  rest <- typeHeadInfixTailParser;
  pure (TypeHeadInfix, foldInfixR buildInfixType lhs rest)};
  Nothing -> pure (TypeHeadPrefix, lhs)}} where {
  typeHeadInfixTailParser :: TokParser [((Name, TypePromotion), Type)];
  typeHeadInfixTailParser = MP.many $ MP.try $ do {
  op <- typeInfixOperatorParser;
  rhs <- typeAppParser;
  pure (op, rhs)};
  buildInfixType left ((op, promoted), right) = TInfix left op promoted right};
classHeadParser :: TokParser (BinderHead UnqualifiedName);
classHeadParser = declHeadParserWith (nameToUnqualified <$> typeFamilyOperatorParser);
binderHeadToTypeFamilyHead :: SourceSpan -> BinderHead UnqualifiedName -> (TypeHeadForm, Type, [TyVarBinder]);
binderHeadToTypeFamilyHead span' binderHead = case binderHead of {
  PrefixBinderHead name params -> (TypeHeadPrefix, typeAnnSpan span' (TCon (qualifyName Nothing name) Unpromoted), params);
  InfixBinderHead lhs op rhs tailParams -> (TypeHeadInfix, typeAnnSpan span' (TInfix lhsType opName Unpromoted rhsType), [lhs, rhs] <> tailParams) where {
  lhsType = TVar (mkUnqualifiedName NameVarId (tyVarBinderName lhs));
  rhsType = TVar (mkUnqualifiedName NameVarId (tyVarBinderName rhs));
  opName = qualifyName Nothing op}};
explicitForallBinderParser :: TokParser TyVarBinder;
explicitForallBinderParser = withSpan $ (do {
  ident <- typeParamBinderNameParser;
  pure (\span' -> TyVarBinder [mkAnnotation span'] ident Nothing TyVarBSpecified TyVarBVisible)}) <|> (do {
  expectedTok TkSpecialLParen;
  ident <- typeParamBinderNameParser;
  expectedTok TkReservedDoubleColon;
  kind <- typeParser;
  expectedTok TkSpecialRParen;
  pure (\span' -> TyVarBinder [mkAnnotation span'] ident (Just kind) TyVarBSpecified TyVarBVisible)});
declTypeParamParser :: TokParser TyVarBinder;
declTypeParamParser = MP.try invisibleDeclTypeParamParser <|> explicitForallBinderParser;
invisibleDeclTypeParamParser :: TokParser TyVarBinder;
invisibleDeclTypeParamParser = withSpan $ do {
  expectedTok TkTypeApp;
  (do {
  ident <- lowerIdentifierParser <|> (expectedTok TkKeywordUnderscore $> "_");
  pure (\span' -> TyVarBinder [mkAnnotation span'] ident Nothing TyVarBSpecified TyVarBInvisible)}) <|> do {
  expectedTok TkSpecialLParen;
  ident <- lowerIdentifierParser <|> (expectedTok TkKeywordUnderscore $> "_");
  expectedTok TkReservedDoubleColon;
  kind <- typeParser;
  expectedTok TkSpecialRParen;
  pure (\span' -> TyVarBinder [mkAnnotation span'] ident (Just kind) TyVarBSpecified TyVarBInvisible)}};
isTypeVarName :: Text -> Bool;
isTypeVarName name = case T.uncons name of {
  Just (c, _) -> c == '_' || isLower c;
  Nothing -> False};
typeParamBinderNameParser :: TokParser Text;
typeParamBinderNameParser = tokenSatisfy "type parameter binder" $ \tok -> case lexTokenKind tok of {
  TkVarId name | isTypeVarName name -> Just name;
  TkKeywordUnderscore -> Just "_";
  _ -> Nothing};
derivingClauseParser :: TokParser DerivingClause;
derivingClauseParser = do {
  expectedTok TkKeywordDeriving;
  strategy <- MP.optional derivingStrategyWithoutViaParser;
  classes <- parenClasses <|> singleClass;
  viaStrategy <- MP.optional (DerivingVia <$> derivingViaTypeParser);
  case (strategy, viaStrategy) of {
  (Just _, Just _) -> fail "deriving via cannot be combined with another deriving strategy";
  _ -> pure (DerivingClause (viaStrategy <|> strategy) classes)}} where {
  singleClass = Left <$> constructorNameParser;
  parenClasses = Right <$> parens (typeParser `MP.sepEndBy` expectedTok TkSpecialComma)};
derivingViaTypeParser :: TokParser Type;
derivingViaTypeParser = do {
  varIdTok "via";
  typeParser};
derivingStrategyParser :: TokParser DerivingStrategy;
derivingStrategyParser = derivingStrategyWithoutViaParser <|> (DerivingVia <$> MP.try derivingViaTypeParser);
derivingStrategyWithoutViaParser :: TokParser DerivingStrategy;
derivingStrategyWithoutViaParser = (varIdTok "stock" >> pure DerivingStock) <|> (expectedTok TkKeywordNewtype >> pure DerivingNewtype) <|> (varIdTok "anyclass" >> pure DerivingAnyclass);
dataConQualifiersParser :: TokParser ([TyVarBinder], [Type]);
dataConQualifiersParser = do {
  foralls <- MP.option [] forallBindersParser;
  context <- contextPrefixDispatchList;
  pure (foralls, context)};
data FamilyKeywordMode = FamilyKeywordRequired | FamilyKeywordOptional;
typeFamilyDeclBodyParser :: FamilyKeywordMode -> TokParser TypeFamilyDecl;
typeFamilyDeclBodyParser familyKeywordMode = do {
  expectedTok TkKeywordType;
  explicitFamilyKeyword <- case familyKeywordMode of {
  FamilyKeywordRequired -> expectedTok TkVarFamily $> True;
  FamilyKeywordOptional -> optionalTok TkVarFamily};
  (headForm, headType, params) <- typeFamilyHeadParser;
  resultSig <- typeFamilyResultSigParser explicitFamilyKeyword;
  equations <- case familyKeywordMode of {
  FamilyKeywordRequired -> MP.optional (MP.try closedTypeFamilyWhereParser);
  FamilyKeywordOptional -> pure Nothing};
  pure TypeFamilyDecl {typeFamilyDeclHeadForm = headForm, typeFamilyDeclExplicitFamilyKeyword = explicitFamilyKeyword, typeFamilyDeclHead = headType, typeFamilyDeclParams = params, typeFamilyDeclResultSig = resultSig, typeFamilyDeclEquations = equations}};
forallBindersParser :: TokParser [TyVarBinder];
forallBindersParser = do {
  expectedTok TkKeywordForall;
  binders <- MP.some explicitForallBinderParser;
  expectedTok (TkVarSym ".");
  pure binders};
dataConRecordOrPrefixParser :: [TyVarBinder] -> [Type] -> TokParser DataConDecl;
dataConRecordOrPrefixParser forallVars context = do {
  name <- constructorUnqualifiedNameParser <|> parens operatorUnqualifiedNameParser;
  mRecordFields <- MP.optional (MP.try recordFieldsParserAfterLayoutSemicolon);
  case mRecordFields of {
  Just fields -> pure (RecordCon forallVars context name fields);
  Nothing -> do {
  args <- MP.many constructorArgParser;
  MP.notFollowedBy constructorOperatorParser;
  pure (PrefixCon forallVars context name args)}}} where {
  recordFieldsParserAfterLayoutSemicolon = recordFieldsParser <|> (expectedTok TkSpecialSemicolon *> recordFieldsParser)};
dataConInfixParser :: [TyVarBinder] -> [Type] -> TokParser DataConDecl;
dataConInfixParser forallVars context = do {
  lhs <- infixConstructorArgParser;
  op <- constructorOperatorUnqualifiedNameParser <|> backtickConstructorUnqualifiedParser;
  InfixCon forallVars context lhs op <$> infixConstructorArgParser} where {
  backtickConstructorUnqualifiedParser = do {
  expectedTok TkSpecialBacktick;
  name <- constructorUnqualifiedNameParser;
  expectedTok TkSpecialBacktick;
  pure name}};
recordFieldsParser :: TokParser [FieldDecl];
recordFieldsParser = braces (recordFieldDeclParser `MP.sepEndBy` expectedTok TkSpecialComma);
recordFieldDeclParser :: TokParser FieldDecl;
recordFieldDeclParser = withSpan $ do {
  names <- binderNameParser `MP.sepBy1` expectedTok TkSpecialComma;
  linearEnabled <- isExtensionEnabled LinearTypes;
  mMult <- if linearEnabled then MP.optional fieldMultiplicityParser else pure Nothing;
  expectedTok TkReservedDoubleColon;
  fieldTy <- recordFieldBangTypeParser;
  pure $ \span' -> FieldDecl {fieldAnns = [mkAnnotation span'], fieldNames = names, fieldMultiplicity = mMult, fieldType = fieldTy}};
fieldMultiplicityParser :: TokParser Type;
fieldMultiplicityParser = do {
  expectedTok TkPrefixPercent;
  typeAtomParser};
constructorArgParser :: TokParser BangType;
constructorArgParser = MP.try $ MP.notFollowedBy derivingKeywordParser *> bangTypeParser;
infixConstructorArgParser :: TokParser BangType;
infixConstructorArgParser = MP.try $ MP.notFollowedBy derivingKeywordParser *> bangTypeParserWith typeAppParser;
derivingKeywordParser :: TokParser ();
derivingKeywordParser = tokenSatisfy "identifier \"deriving\"" $ \tok -> case lexTokenKind tok of {
  TkKeywordDeriving -> Just ();
  _ -> Nothing};
bangTypeParserWith :: TokParser Type -> TokParser BangType;
bangTypeParserWith typeP = withSpan $ do {
  pragmas <- MP.option [] (fmap (: []) unpackPragmaParser);
  strict <- MP.option False (expectedTok TkPrefixBang >> pure True);
  isLazy <- MP.option False (expectedTok TkPrefixTilde >> pure True);
  ty <- typeP;
  pure $ \span' -> BangType {bangAnns = [mkAnnotation span'], bangPragmas = pragmas, bangStrict = strict, bangLazy = isLazy, bangType = ty}};
bangTypeParser :: TokParser BangType;
bangTypeParser = bangTypeParserWith typeAtomParser;
recordFieldBangTypeParser :: TokParser BangType;
recordFieldBangTypeParser = bangTypeParserWith typeParser;
unpackPragmaParser :: TokParser Pragma;
unpackPragmaParser = hiddenPragma "source unpack pragma" $ \p -> case pragmaType p of {
  PragmaUnpack _ -> Just p;
  _ -> Nothing};
constructorOperatorParser :: TokParser Name;
constructorOperatorParser = symbolicConstructorOperatorParser <|> backtickConstructorIdentifierParser where {
  symbolicConstructorOperatorParser = (qualifyName Nothing <$> constructorOperatorUnqualifiedNameParser) <|> tokenSatisfy "qualified constructor operator" (\tok -> case lexTokenKind tok of {
  TkQConSym modName op -> Just (mkNameAt tok (Just modName) NameConSym op);
  _ -> Nothing});
  backtickConstructorIdentifierParser = do {
  expectedTok TkSpecialBacktick;
  op <- constructorNameParser;
  expectedTok TkSpecialBacktick;
  pure op}};
patternBindDeclParser :: TokParser Decl;
patternBindDeclParser = MP.try $ withSpanAnn (DeclAnn . mkAnnotation) $ do {
  pat <- region "while parsing pattern binding" patternParser;
  DeclValue . PatternBind NoMultiplicityTag pat <$> equationRhsParser};
valueDeclParser :: TokParser Decl;
valueDeclParser = withSpanAnn (DeclAnn . mkAnnotation) $ do {
  (headForm, name, pats) <- functionHeadParserWith patParser apatParser;
  functionBindDecl headForm name pats <$> equationRhsParser};
patternSynonymParser :: TokParser Decl;
patternSynonymParser = MP.try patternSynonymSigDeclParser <|> patternSynonymDeclParser;
patternSynonymSigDeclParser :: TokParser Decl;
patternSynonymSigDeclParser = withSpanAnn (DeclAnn . mkAnnotation) $ do {
  expectedTok TkKeywordPattern;
  names <- patSynNameParser `MP.sepBy1` expectedTok TkSpecialComma;
  expectedTok TkReservedDoubleColon;
  DeclPatSynSig names <$> typeSignatureParser};
patSynNameParser :: TokParser UnqualifiedName;
patSynNameParser = constructorUnqualifiedNameParser <|> do {
  op <- parens constructorOperatorParser;
  pure (nameToUnqualified op)};
patternSynonymDeclParser :: TokParser Decl;
patternSynonymDeclParser = withSpanAnn (DeclAnn . mkAnnotation) $ do {
  expectedTok TkKeywordPattern;
  (name, args) <- patSynLhsParser;
  (dir, pat) <- patSynDirAndPatParser name;
  pure $ DeclPatSyn PatSynDecl {patSynDeclName = name, patSynDeclArgs = args, patSynDeclPat = pat, patSynDeclDir = dir}};
patSynLhsParser :: TokParser (UnqualifiedName, PatSynArgs);
patSynLhsParser = MP.try patSynInfixLhsParser <|> patSynRecordOrPrefixLhsParser;
patSynInfixLhsParser :: TokParser (UnqualifiedName, PatSynArgs);
patSynInfixLhsParser = do {
  lhs <- lowerIdentifierParser;
  op <- constructorOperatorParser;
  rhs <- lowerIdentifierParser;
  pure (nameToUnqualified op, PatSynInfixArgs lhs rhs)};
patSynRecordOrPrefixLhsParser :: TokParser (UnqualifiedName, PatSynArgs);
patSynRecordOrPrefixLhsParser = do {
  name <- patSynNameParser;
  mFields <- MP.optional (MP.try patSynRecordFieldsParser);
  case mFields of {
  Just fields -> pure (name, PatSynRecordArgs fields);
  Nothing -> do {
  args <- MP.many lowerIdentifierParser;
  pure (name, PatSynPrefixArgs args)}}};
patSynRecordFieldsParser :: TokParser [Text];
patSynRecordFieldsParser = braces (lowerIdentifierParser `MP.sepEndBy` expectedTok TkSpecialComma);
patSynDirAndPatParser :: UnqualifiedName -> TokParser (PatSynDir, Pattern);
patSynDirAndPatParser name = (do {
  expectedTok TkReservedEquals;
  pat <- patternParser;
  pure (PatSynBidirectional, pat)}) <|> (do {
  expectedTok TkReservedLeftArrow;
  pat <- patternParser;
  mMatches <- MP.optional (patSynWhereClauseParser (renderUnqualifiedName name));
  case mMatches of {
  Nothing -> pure (PatSynUnidirectional, pat);
  Just matches -> pure (PatSynExplicitBidirectional matches, pat)}}) <|> do {
  mTok <- peekTokenMaybe;
  MP.customFailure UnexpectedTokenExpecting {unexpectedFound = mkFoundToken <$> mTok, unexpectedExpecting = "'=' or '<-' in pattern synonym declaration", unexpectedContext = []}};
patSynWhereClauseParser :: Text -> TokParser [Match];
patSynWhereClauseParser _name = whereClauseItemsParser patSynWhereMatch;
patSynWhereMatch :: TokParser Match;
patSynWhereMatch = withSpan $ do {
  (headForm, _name, pats) <- patSynWhereHeadParser;
  rhs <- equationRhsParser;
  pure $ \span' -> Match {matchAnns = [mkAnnotation span'], matchHeadForm = headForm, matchPats = pats, matchRhs = rhs}};
patSynWhereHeadParser :: TokParser (MatchHeadForm, UnqualifiedName, [Pattern]);
patSynWhereHeadParser = MP.try infixHeadParser <|> MP.try parenthesizedInfixHeadParser <|> prefixHeadParser where {
  prefixHeadParser = do {
  name <- patSynNameParser;
  pats <- MP.many apatParser;
  pure (MatchHeadPrefix, name, pats)};
  infixHeadParser = do {
  lhsPat <- lpatParser;
  op <- constructorInfixOperatorNameParser;
  rhsPat <- lpatParser;
  pure (MatchHeadInfix, op, [lhsPat, rhsPat])};
  parenthesizedInfixHeadParser = do {
  expectedTok TkSpecialLParen;
  lhsPat <- lpatParser;
  op <- constructorInfixOperatorNameParser;
  rhsPat <- lpatParser;
  expectedTok TkSpecialRParen;
  tailPats <- MP.many apatParser;
  pure (MatchHeadInfix, op, [lhsPat, rhsPat] <> tailPats)}}
}
