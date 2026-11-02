{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
module Aihc.Tc.Generate.Decl (tcModule, tcModuleScc, moduleBindings, defaultMethodName, TcBindingResult(..), tbName) where {
import Aihc.Parser.Syntax (Annotation, BangType(..), BinderHead(..), BuiltinCon(..), CallConv(..), CaseAlt(..), ClassDecl(..), ClassDeclItem(..), DataConDecl(..), DataDecl(..), DataFamilyDecl(..), DataFamilyInst(..), Decl(..), ExportSpec(..), Expr(..), Extension(..), FieldDecl(..), ForallTelescope(..), ForeignDecl(..), ForeignDirection(..), ForeignEntitySpec(..), ForeignSafety(..), FunctionalDependency(..), GadtBody(..), IEBundledMember(..), InstanceDecl(..), InstanceDeclItem(..), Literal(..), Match(..), MatchHeadForm(..), Module(..), Name(..), NameType(..), NewtypeDecl(..), PatSynArgs(..), PatSynDecl(..), PatSynDir(..), Pattern(..), Pragma(..), PragmaType(..), PragmaUnpackKind(..), RecordField(..), Rhs(..), Role(..), RoleAnnotation(..), RuleBinder(..), RuleDecl(..), SourceSpan, TupleFlavor(..), TyVarBinder, Type(..), TypeFamilyDecl(..), TypeFamilyEq(..), TypeFamilyInjectivity(..), TypeFamilyInst(..), TypeFamilyResultSig(..), TypeSynDecl(..), UnqualifiedName(..), ValueDecl(..), binderHeadName, binderHeadParams, fromAnnotation, gadtBodyResultType, instanceHeadName, instanceHeadTypes, mkAnnotation, mkUnqualifiedName, moduleExports, moduleName, nameText, peelClassDeclItemAnn, peelDeclAnn, peelInstanceDeclItemAnn, peelTypeHead, qualifyName, tyVarBinderKind, tyVarBinderName, unqualifiedNameAnns);
import Aihc.Resolve (Identifier(..), ModuleUnit(..), PackageId(..), ResolutionAnnotation(..), ResolutionNamespace(..), ResolvedName(..), VisibleTermIdentities(..));
import Aihc.Resolve.Traverse (annotationList);
import Aihc.Tc.Annotations (PendingTcAnnotation(..), TcAnnotation(..), TcClassAnnotation(..), TcClassMethodAnnotation(..), TcCoercedDeriving(..), TcDerivedInstance(..), TcDerivingPlan(..), TcDictBinderAnnotation(..), TcForeignAbiType(..), TcForeignCApi(..), TcForeignCApiKind(..), TcForeignEffect(..), TcForeignImportAnnotation(..), TcForeignImportInfo(..), TcForeignMarshal(..), TcForeignSafety(..), TcForeignTarget(..), TcInstanceAnnotation(..), TcInstanceMethodAnnotation(..), TcPatSynAnnotation(..), annotateDecl, annotateRhsCast, renderTcType);
import Aihc.Tc.Constraint;
import Aihc.Tc.Deriving (annotateAttachedDerivingTc, annotateStandaloneDerivingTc);
import Aihc.Tc.Deriving.Cast (checkCoercedInstance);
import Aihc.Tc.Deriving.Context (inferDerivingContexts, isContextFreeStockPlan, settleContextFreePlans, typeTyVars);
import Aihc.Tc.Deriving.Generate (generateDerivedInstances);
import Aihc.Tc.Env (AssociatedTypeInfo(..), CType(..), ClassInfo(..), DataConFieldInfo(..), DataConFieldUnpack(..), DataConInfo(..), DataConSourceForm(..), DataFamilyInstanceInfo(..), DataTypeInfo(..), FunDep(..), InstanceEnv, InstanceInfo(..), PatSynDirection(..), PatSynInfo(..), RecordHead(..), TyConFlavor(..), TyConInfo(..), TypeFamilyInstanceInfo(..), TypeSynonymInfo(..), addInstanceEnv, dataConArgTypes, dataFamilyAxiomName, dataFamilyRepresentationName, instanceClassTyCon, instanceEnvSince, typeFamilyAxiomKey, typeFamilyAxiomName);
import Aihc.Tc.Error (TcErrorKind(..));
import Aihc.Tc.Evidence (EvTerm(..));
import Aihc.Tc.Finalize (finalizeModuleTc);
import Aihc.Tc.FunDep (checkInstanceFunDeps);
import Aihc.Tc.Generalize (collectMetaVars, environmentMetaVars, generalizeAndCommit, generalizeAndCommitIgnoring, generalizeGroupAndCommitIgnoring, predMetaVars);
import Aihc.Tc.Generate.Bind (freeVarsDecl, freeVarsMatch, inferRhsWithLocals);
import Aihc.Tc.Generate.Expr (checkExpr, checkRhs, inferExpr);
import Aihc.Tc.Generate.Pattern;
import Aihc.Tc.Generate.PatternBranch (solvePatternBranch);
import Aihc.Tc.Instantiate (Instantiation(..), instantiate, instantiateWithArgs);
import Aihc.Tc.Kind (ParamInfo(..), TvKindEnv, checkRuntimeType, checkSurfaceType, classPredicateArgKinds, convertSurfaceTypeWithKinds, defaultKindMetas, explicitForallNames, freeTypeVars, freshKindMeta, hasWildcardType, isEmptyContext, makeParamEnv, makeParamEnvWith, scopedSigTyVars, sigToScheme, splitSigma, standaloneKindSigToScheme, surfacePredToPred, surfaceTypeSpan, takeVisibleArgumentKinds, tcTypeKind, tyConKindFromParams, tyConKindFromParamsWith, unifyKinds, unifyKindsAt, zonkKind);
import Aihc.Tc.Monad;
import Aihc.Tc.Solve (SolveResult(..), solveConstraints, solveWithImpls);
import Aihc.Tc.Solve.Defaulting (defaultAmbiguousMetas);
import Aihc.Tc.Solve.Dict (DictResult(..), isCallStackPred, reportUnsolvedDict, solveDict, solveDictWithGivens);
import Aihc.Tc.Solve.Equality (EqResult(..), solveEquality, solveGivenEquality);
import Aihc.Tc.Solve.InertSet (InertSet(..));
import Aihc.Tc.Solve.Injective (improveInjectivity);
import Aihc.Tc.TypeScheme (equivalentTypeSchemes, schemeToType, typeSchemeFromType);
import Aihc.Tc.Types;
import Aihc.Tc.Wiring (BuiltinDataCon(..), builtinDataCon, mkTcKinds);
import Aihc.Tc.Zonk (defaultPredKinds, defaultTyConKindScheme, defaultTyVarKinds, defaultTypeKinds, defaultTypeSchemeKinds, zonkType);
import Control.Applicative ((<|>));
import Control.Monad (filterM, foldM, forM, forM_, replicateM, unless, void, when, zipWithM, zipWithM_);
import Control.Monad.Trans.Class (lift);
import Control.Monad.Trans.State.Strict (get, modify');
import Data.Char (isAlpha, isAlphaNum, isSpace, ord);
import Data.Either (partitionEithers);
import Data.Graph (SCC(..), stronglyConnComp);
import Data.IntSet (IntSet);
import Data.IntSet qualified as IntSet;
import Data.List (elemIndex, find, mapAccumL, nub, nubBy, partition, (\\));
import Data.Map.Strict (Map);
import Data.Map.Strict qualified as Map;
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, listToMaybe, mapMaybe, maybeToList);
import Data.Set qualified as Set;
import Data.Text (Text);
import Data.Text qualified as T;
sourceSpanFromAnns :: [Annotation] -> Maybe SourceSpan;
sourceSpanFromAnns = listToMaybe . mapMaybe (fromAnnotation @SourceSpan);
peelDeclSpan :: Decl -> Maybe SourceSpan;
peelDeclSpan = go Nothing where {
  go ambient (DeclAnn ann inner) = go (fromAnnotation @SourceSpan ann <|> ambient) inner;
  go ambient _ = ambient};
data TcBindingResult = TcBindingResult { tbKey :: !TcTermKey, tbDisplayName :: !Text, tbType :: !TcType } deriving (Show, Read);
tbName :: TcBindingResult -> Text;
tbName = termKeyName . tbKey;
data UserSig = UserSig { userSigName :: !Text, userSigType :: !Type, userSigSpan :: !(Maybe SourceSpan) } deriving (Show);
data CheckedSig = CheckedSig { checkedSigName :: !Text, checkedSigScheme :: !TypeScheme, checkedSigSpan :: !(Maybe SourceSpan), checkedSigScopedNames :: ![Text], checkedSigPartial :: !Bool } deriving (Show);
moduleBindings :: TcWiring -> Module -> [TcBindingResult];
moduleBindings wiring modu = concatMap (declBindings wiring (mkTcKinds wiring) (resolvedModuleOrigin modu)) (moduleDecls modu);
resolvedModuleOrigin :: Module -> (Text, Text);
resolvedModuleOrigin resolvedModule = fromMaybe ("", fromMaybe "Main" (moduleName resolvedModule)) $ do {
  resolved <- listToMaybe (mapMaybe definitionResolution (moduleDecls resolvedModule));
  case resolutionTarget resolved of {
  ResolvedTopLevel packageId moduleName' _ -> pure (packageIdText packageId, moduleName');
  _ -> Nothing}};
definitionResolution :: Decl -> Maybe ResolutionAnnotation;
definitionResolution declaration = case peelDeclAnn declaration of {
  DeclValue (FunctionBind name _) -> nameResolution name;
  DeclValue (PatternBind _ pattern' _) -> patternResolution pattern';
  DeclData dataDeclaration -> nameResolution (binderHeadName (dataDeclHead dataDeclaration));
  DeclNewtype newtypeDeclaration -> nameResolution (binderHeadName (newtypeDeclHead newtypeDeclaration));
  DeclClass classDeclaration -> nameResolution (binderHeadName (classDeclHead classDeclaration));
  DeclDataFamilyDecl familyDeclaration -> nameResolution (binderHeadName (dataFamilyDeclHead familyDeclaration));
  DeclForeign foreignDeclaration -> nameResolution (foreignName foreignDeclaration);
  DeclTypeSyn typeSynDeclaration -> nameResolution (binderHeadName (typeSynHead typeSynDeclaration));
  DeclTypeData dataDeclaration -> nameResolution (binderHeadName (dataDeclHead dataDeclaration));
  DeclPatSyn patSynDeclaration -> nameResolution (patSynDeclName patSynDeclaration);
  DeclTypeSig names _ -> listToMaybe (mapMaybe nameResolution names);
  _ -> Nothing};
patternResolution :: Pattern -> Maybe ResolutionAnnotation;
patternResolution pattern' = case pattern' of {
  PVar name -> nameResolution name;
  PAnn _ inner -> patternResolution inner;
  PParen inner -> patternResolution inner;
  PStrict inner -> patternResolution inner;
  PIrrefutable inner -> patternResolution inner;
  PAs name _ -> nameResolution name;
  PTypeSig inner _ -> patternResolution inner;
  _ -> Nothing};
nameResolution :: UnqualifiedName -> Maybe ResolutionAnnotation;
nameResolution = listToMaybe . mapMaybe fromAnnotation . unqualifiedNameAnns;
typeToScheme :: TcType -> TypeScheme;
typeToScheme ty = let {
  (tyVars, body) = peelForAlls ty} in case body of {
  TcQualTy predicates result -> specifiedScheme tyVars predicates result;
  result -> specifiedScheme tyVars [] result};
originTermKey :: (Text, Text) -> Text -> TcTermKey;
originTermKey (package, moduleName') = TcTermGlobal (PackageId package) moduleName';
declBindings :: TcWiring -> TcKinds -> (Text, Text) -> Decl -> [TcBindingResult];
declBindings wiring kinds origin decl = case decl of {
  DeclAnn ann inner -> annotationBindings kinds origin ann inner <> declBindings wiring kinds origin inner;
  DeclData dataDecl -> concatMap (dataConBindings wiring origin) (dataDeclConstructors dataDecl) <> concatMap (recordSelectorBindings origin) (dataDeclConstructors dataDecl);
  DeclNewtype newtypeDecl -> maybe [] (\constructor -> dataConBindings wiring origin constructor <> recordSelectorBindings origin constructor) (newtypeDeclConstructor newtypeDecl);
  DeclDataFamilyInst familyInst -> concatMap (dataConBindings wiring origin) (dataFamilyInstConstructors familyInst) <> concatMap (recordSelectorBindings origin) (dataFamilyInstConstructors familyInst);
  _ -> []};
annotationBindings :: TcKinds -> (Text, Text) -> Annotation -> Decl -> [TcBindingResult];
annotationBindings kinds origin ann decl = tcAnnotationBindings origin ann decl <> classAnnotationBindings kinds origin ann decl <> instanceAnnotationBindings origin ann;
tcAnnotationBindings :: (Text, Text) -> Annotation -> Decl -> [TcBindingResult];
tcAnnotationBindings origin ann decl = case fromAnnotation ann of {
  Nothing -> [];
  Just tcAnn -> case decl of {
  DeclValue (FunctionBind binder _) -> let {
  (name, displayName) = binderBindingName binder} in [TcBindingResult (originTermKey origin name) displayName (tcAnnType tcAnn)];
  DeclValue (PatternBind _ pat _) -> [TcBindingResult (originTermKey origin name) displayName (fromMaybe (tcAnnType tcAnn) (binderCheckedType binder)) | binder <- patternBinderNames pat, let {
  (name, displayName) = binderBindingName binder}];
  DeclData dataDecl -> let {
  name = unqualifiedNameText (binderHeadName (dataDeclHead dataDecl))} in [TcBindingResult (originTermKey origin name) name (tcAnnType tcAnn)];
  DeclNewtype newtypeDecl -> let {
  name = unqualifiedNameText (binderHeadName (newtypeDeclHead newtypeDecl))} in [TcBindingResult (originTermKey origin name) name (tcAnnType tcAnn)];
  DeclDataFamilyDecl familyDecl -> let {
  name = unqualifiedNameText (binderHeadName (dataFamilyDeclHead familyDecl))} in [TcBindingResult (originTermKey origin name) name (tcAnnType tcAnn)];
  DeclForeign foreignDecl -> let {
  name = unqualifiedNameText (foreignName foreignDecl);
  displayName = renderBinderName (foreignName foreignDecl)} in [TcBindingResult (originTermKey origin name) displayName (tcAnnType tcAnn)];
  _ -> []}};
classAnnotationBindings :: TcKinds -> (Text, Text) -> Annotation -> Decl -> [TcBindingResult];
classAnnotationBindings kinds origin ann decl = case (fromAnnotation ann, decl) of {
  (Just classAnn, DeclClass {}) -> [TcBindingResult (originTermKey origin (tcClassMethodName method)) (tcClassMethodName method) (tcClassMethodType method) | method <- tcClassMethods classAnn] <> [TcBindingResult (originTermKey origin (defaultMethodName (tcClassMethodName method))) (defaultMethodName (tcClassMethodName method)) (classDefaultWorkerType kinds classAnn method) | method <- tcClassMethods classAnn, tcClassMethodName method `elem` tcClassDefaultMethods classAnn];
  _ -> []};
classDefaultWorkerType :: TcKinds -> TcClassAnnotation -> TcClassMethodAnnotation -> TcType;
classDefaultWorkerType kinds classAnnotation method = case lookup (tcClassMethodName method) (tcClassDefaultSignatures classAnnotation) of {
  Just signature | Just classPredicate <- constraintTypeToPred kinds (tcClassMethodDictType method) -> let {
  (tyVars, body) = peelForAlls signature;
  qualifiedBody = case body of {
  TcQualTy predicates result -> TcQualTy (classPredicate : predicates) result;
  result -> TcQualTy [classPredicate] result}} in foldr TcForAllTy qualifiedBody tyVars;
  _ -> tcClassMethodType method};
instanceAnnotationBindings :: (Text, Text) -> Annotation -> [TcBindingResult];
instanceAnnotationBindings origin ann = case fromAnnotation ann of {
  Just instAnn -> [TcBindingResult (originTermKey origin (tcInstanceDictName instAnn)) (tcInstanceDictName instAnn) (tcInstanceDictType instAnn)];
  Nothing -> []};
dataConBindings :: TcWiring -> (Text, Text) -> DataConDecl -> [TcBindingResult];
dataConBindings wiring origin dataConDecl = case dataConDecl of {
  DataConAnn ann inner -> case fromAnnotation ann of {
  Just tcAnn -> [TcBindingResult (originTermKey origin name) name (tcAnnType tcAnn) | name <- map (dataConIdentityName wiring) (dataConIdentities inner)];
  Nothing -> dataConBindings wiring origin inner};
  _ -> []};
recordSelectorBindings :: (Text, Text) -> DataConDecl -> [TcBindingResult];
recordSelectorBindings origin declaration = case declaration of {
  DataConAnn ann inner -> case fromAnnotation ann of {
  Just tcAnn -> selectorBindingsFromConstructorType (tcAnnType tcAnn) inner;
  Nothing -> recordSelectorBindings origin inner};
  _ -> []} where {
  selectorBindingsFromConstructorType constructorType inner = let {
  (typeVariables, qualifiedConstructor) = peelForAlls constructorType;
  body = case qualifiedConstructor of {
  TcQualTy _ result -> result;
  result -> result};
  (fieldTypes, resultType) = splitFunctionType body;
  (_, sourceFields, _) = dataConSourceLayout inner;
  universals = filter (`elem` closeOverKinds (typeTyVars resultType)) typeVariables;
  closeOverKinds variables = let {
  kindVariables = [variable | variable <- typeVariables, variable `notElem` variables, any (\universal -> variable `elem` typeTyVars (tvKind universal)) variables]} in if null kindVariables then variables else closeOverKinds (variables <> kindVariables)} in [TcBindingResult (originTermKey origin label) label (foldr TcForAllTy (TcFunTy resultType fieldType) universals) | ((maybeLabel, _), fieldType) <- zip sourceFields fieldTypes, all (`elem` universals) (typeTyVars fieldType), Just label <- [maybeLabel]]};
binderCheckedType :: UnqualifiedName -> Maybe TcType;
binderCheckedType name = tcAnnType <$> listToMaybe (mapMaybe fromAnnotation (unqualifiedNameAnns name));
data DataConIdentity = DeclaredDataCon UnqualifiedName | BuiltinDataConIdentity BuiltinDataCon;
dataConIdentities :: DataConDecl -> [DataConIdentity];
dataConIdentities dataConDecl = case dataConDecl of {
  DataConAnn _ inner -> dataConIdentities inner;
  PrefixCon _ _ name _ -> [DeclaredDataCon name];
  InfixCon _ _ _ name _ -> [DeclaredDataCon name];
  RecordCon _ _ name _ -> [DeclaredDataCon name];
  GadtCon _ _ names _ -> map DeclaredDataCon names;
  TupleCon _ _ flavor fields -> [BuiltinDataConIdentity (BuiltinTupleCon flavor (length fields))];
  UnboxedSumCon _ _ alternative arity _ -> [BuiltinDataConIdentity (BuiltinUnboxedSumCon alternative arity)];
  ListCon {} -> [BuiltinDataConIdentity BuiltinNilCon]};
dataConIdentityName :: TcWiring -> DataConIdentity -> Text;
dataConIdentityName wiring identity = case identity of {
  DeclaredDataCon name -> unqualifiedNameText name;
  BuiltinDataConIdentity builtin -> tyConName (builtinDataCon wiring builtin)};
dataConNames :: DataConDecl -> TcM [Text];
dataConNames declaration = do {
  wiring <- getWiring;
  pure (map (dataConIdentityName wiring) (dataConIdentities declaration))};
dataConIdentityKey :: TyCon -> DataConIdentity -> TcM TcTermKey;
dataConIdentityKey parent identity = case identity of {
  DeclaredDataCon name -> resolvedUnqualifiedTermKey name;
  BuiltinDataConIdentity builtin -> do {
  wiring <- getWiring;
  pure (tyConMemberTermKey parent (tyConName (builtinDataCon wiring builtin)))}};
dataConKeys :: TyCon -> DataConDecl -> TcM [TcTermKey];
dataConKeys parent = mapM (dataConIdentityKey parent) . dataConIdentities;
binderBindingName :: UnqualifiedName -> (Text, Text);
binderBindingName name = (unqualifiedNameText name, renderBinderName name);
tcModule :: ModuleUnit -> TcM Module;
tcModule unit = do {
  modules <- tcModuleScc [unit];
  case modules of {
  [result] -> pure result;
  _ -> pure (moduleUnitAst unit)}};
tcModuleScc :: [ModuleUnit] -> TcM [Module];
tcModuleScc sourceUnits = withPolyKindOrigins polyKindOrigins $ do {
  initialKeys <- globalStateKeys <$> lift get;
  let {
  modules = map moduleUnitAst units;
  moduleExtensions = map moduleUnitExtensions units;
  declarations = concatMap moduleDecls modules;
  standaloneKindSignatures = collectStandaloneKindSignatures declarations};
  mapM_ (atDecl predeclareTypeConstructor) declarations;
  mapM_ (atDecl predeclareTypeLevelDataConstructors) declarations;
  mapM_ (atDecl registerDeclaredRecordPatSyn) declarations;
  standaloneKindSchemes <- traverse standaloneKindSigToScheme standaloneKindSignatures;
  mapM_ (atDecl (registerTypeDeclHeader standaloneKindSchemes)) declarations;
  let {
  structuralDeclarations = [(resolvedModuleOrigin modu, declaration) | modu <- modules, declaration <- moduleDecls modu]};
  forM_ (structuralDeclGroups (filter (not . isInstanceDecl . snd) structuralDeclarations)) $ \group -> do {
  mapM_ (atDecl registerTypeSynonymBody . snd) group;
  mapM_ (atDecl checkTypeSynonymBody . snd) group;
  mapM_ (atDeclOf registerStructuralDecl) group;
  generalizeDeclarationKinds polyKindOrigins (Set.fromList (concatMap (declarationTypeKeys . snd) group))};
  mapM_ (atDecl registerNominalRoles) declarations;
  componentTyCons <- componentTyConKeys initialKeys;
  withComponentTyCons componentTyCons $ mapM_ (atDeclOf registerStructuralDecl) (filter (isInstanceDecl . snd) structuralDeclarations);
  defaultGlobalKindMetas initialKeys;
  structuralKeys <- globalStateKeys <$> lift get;
  derivingAnnotated <- zipWithM annotateModuleDerivingTc moduleExtensions modules;
  derivingRepresented <- mapM (registerDerivedInstances isContextFreeStockPlan . settleContextFreePlans) derivingAnnotated;
  derivingInferred <- inferDerivingContexts derivingRepresented;
  derivingFinalized <- mapM (registerDerivedInstances (not . isContextFreeStockPlan)) derivingInferred;
  defaultGlobalKindMetas structuralKeys;
  derivedKeys <- globalStateKeys <$> lift get;
  rawSigs <- mapM (collectUserSigs . moduleDecls) derivingFinalized;
  schemes <- zipWithM checkModuleSignatures moduleExtensions rawSigs;
  mapM_ (uncurry registerCheckedSig) (concatMap Map.toList schemes);
  pending <- zipWithM tcModuleBody schemes derivingFinalized;
  mapM_ checkBundledPatSyns derivingFinalized;
  defaultDeferredKindMetas;
  defaultGlobalKindMetas derivedKeys;
  annotated <- mapM annotatePendingModule pending;
  mapM finalizeModuleTc annotated} where {
  units = [unit {moduleUnitAst = hoistAssociatedDataFamilies (moduleUnitAst unit)} | unit <- sourceUnits];
  polyKindOrigins = [resolvedModuleOrigin (moduleUnitAst unit) | unit <- units, PolyKinds `elem` moduleUnitExtensions unit];
  atDeclOf check (origin, declaration) = atDecl (check origin) declaration};
atDecl :: (Decl -> TcM a) -> Decl -> TcM a;
atDecl check declaration = withAmbientSpan (peelDeclSpan declaration) (check declaration);
hoistAssociatedDataFamilies :: Module -> Module;
hoistAssociatedDataFamilies modu = modu {moduleDecls = concatMap hoistDecl (moduleDecls modu)} where {
  hoistDecl decl = case decl of {
  DeclAnn ann inner -> case hoistDecl inner of {
  inner' : hoisted -> DeclAnn ann inner' : hoisted;
  [] -> [decl]};
  DeclClass classDecl -> let {
  (items, families) = partitionEithers (map classItem (classDeclItems classDecl))} in DeclClass classDecl {classDeclItems = items} : families;
  DeclInstance instanceDecl -> let {
  (items, instances) = partitionEithers (map instanceItem (instanceDeclItems instanceDecl))} in DeclInstance instanceDecl {instanceDeclItems = items} : instances;
  _ -> [decl]};
  classItem item = case item of {
  ClassItemAnn ann inner -> either (Left . ClassItemAnn ann) (Right . DeclAnn ann) (classItem inner);
  ClassItemDataFamilyDecl familyDecl -> Right (DeclDataFamilyDecl familyDecl);
  _ -> Left item};
  instanceItem item = case item of {
  InstanceItemAnn ann inner -> either (Left . InstanceItemAnn ann) (Right . DeclAnn ann) (instanceItem inner);
  InstanceItemDataFamilyInst familyInst -> Right (DeclDataFamilyInst familyInst);
  _ -> Left item}};
registerNominalRoles :: Decl -> TcM ();
registerNominalRoles declaration = case peelDeclAnn declaration of {
  DeclRoleAnnotation annotation -> do {
  info <- lookupDeclaredTyCon (roleAnnotationName annotation);
  case info of {
  Just constructor -> lift $ modify' $ \state -> state {tcsDataTypes = Map.adjust (\dataType -> dataType {dtiNominalRoles = map (== RoleNominal) (roleAnnotationRoles annotation)}) (tyConKey (tciTyCon constructor)) (tcsDataTypes state)};
  Nothing -> pure ()}};
  _ -> pure ()};
checkModuleSignatures :: [Extension] -> Map TcTermKey UserSig -> TcM (Map TcTermKey CheckedSig);
checkModuleSignatures extensions signatures = do {
  checked <- traverse checkUserSig signatures;
  if PolyKinds `elem` extensions then traverse generalizeSignatureKinds checked else traverse (\signature -> do {
  scheme <- defaultTypeSchemeKinds (checkedSigScheme signature);
  pure signature {checkedSigScheme = scheme}}) checked};
generalizeTyVarKinds :: [TyVarId] -> TcM ();
generalizeTyVarKinds variables = do {
  kinds <- getKinds;
  reserved <- reservedKindMetas;
  variableKinds <- mapM (zonkKind . tvKind) variables;
  forM_ (zip [0 :: Int ..] (nub (concatMap collectMetaVars variableKinds))) $ \(index, Unique meta) -> do {
  metaKind <- readMetaTvKind (Unique meta) >>= zonkKind;
  tracked <- isTrackedKindMeta (Unique meta);
  when (tracked && metaKind == typeKind kinds && not (IntSet.member meta reserved)) $ do {
  variable <- freshSkolemTv ("k" <> T.pack (show index));
  writeMetaTv (Unique meta) (TcTyVar variable)}}};
generalizeSignatureKinds :: CheckedSig -> TcM CheckedSig;
generalizeSignatureKinds signature = do {
  let {
  Scheme inferred specified predicates body = checkedSigScheme signature};
  generalizeTyVarKinds (inferred <> specified);
  inferred' <- mapM defaultTyVarKinds inferred;
  specified' <- mapM defaultTyVarKinds specified;
  body' <- zonkType body;
  predicates' <- mapM defaultPredKinds predicates;
  pure signature {checkedSigScheme = withInventedKindVariables (Scheme inferred' specified' predicates' body')}};
registerCheckedSig :: TcTermKey -> CheckedSig -> TcM ();
registerCheckedSig key sig = extendTermKeyEnvPermanent key binder where {
  binder = TcIdBinder (flattenSchemeContexts (checkedSigScheme sig)) Closed};
flattenSchemeContexts :: TypeScheme -> TypeScheme;
flattenSchemeContexts scheme@(Scheme inferred specified predicates body) = case body of {
  TcForAllTy variable inner -> flattenSchemeContexts (Scheme inferred (specified <> [variable]) predicates inner);
  TcQualTy more inner -> flattenSchemeContexts (Scheme inferred specified (predicates <> more) inner);
  _ -> scheme};
data PendingModule = PendingModule { pendingSyntax :: !Module, pendingValueResults :: ![TcBindingResult] };
tcModuleBody :: Map TcTermKey CheckedSig -> Module -> TcM PendingModule;
tcModuleBody schemes m = withVisibleTerms visible $ do {
  declaredDefaults <- moduleDefaultTypes (moduleDecls m);
  localDefaultTypes declaredDefaults (tcModuleBodyWithDefaults schemes m)} where {
  visible = [TcTermGlobal package moduleName' name | VisibleTermIdentities identities <- mapMaybe fromAnnotation (moduleAnns m), (package, moduleName', name) <- identities]};
moduleDefaultTypes :: [Decl] -> TcM (Maybe [TcType]);
moduleDefaultTypes decls = case [tys | decl <- decls, DeclDefault tys <- [peelDeclAnn decl]] of {
  [] -> pure Nothing;
  groups -> Just <$> mapM checkDefaultType (last groups)} where {
  checkDefaultType ty = do {
  kinds <- getKinds;
  checkSurfaceType Map.empty ty (typeKind kinds)}};
tcModuleBodyWithDefaults :: Map TcTermKey CheckedSig -> Module -> TcM PendingModule;
tcModuleBodyWithDefaults schemes m = do {
  let {
  sourceGroups = zip [0 :: Int ..] (groupValueDecls (moduleDecls m))};
  grouped <- sortDeclGroups sourceGroups;
  groupResults <- mapM (\group -> withAmbientSpan (declGroupSourceSpan (snd group)) (tcDeclGroup schemes group)) grouped;
  let {
  valueResults = concatMap tcGroupBindingResults groupResults;
  checkedGroups = Map.fromList [(tcGroupId result, decls) | result <- groupResults, Just decls <- [tcGroupAnnotatedDecls result]];
  valueAnnotatedModule = m {moduleDecls = concatMap (renderCheckedGroup checkedGroups) sourceGroups}};
  classDecls <- mapM (atDecl tcClassDeclBodies) (moduleDecls valueAnnotatedModule);
  instanceHeaders <- mapM (atDecl (annotateInstanceHeaderTc (resolvedModuleOrigin m) False)) classDecls;
  instanceDecls <- mapM (atDecl tcInstanceDeclBodies) instanceHeaders;
  ruleDecls <- mapM (atDecl tcRulesDecl) instanceDecls;
  let {
  pendingModule = valueAnnotatedModule {moduleDecls = ruleDecls}};
  checkTopLevelUnliftedBindings sourceGroups groupResults;
  pure (PendingModule pendingModule valueResults)};
annotatePendingModule :: PendingModule -> TcM Module;
annotatePendingModule pending = do {
  annotateModuleTc (Map.fromList [(tbName result, tbType result) | result <- pendingValueResults pending]) (pendingSyntax pending)};
tcRulesDecl :: Decl -> TcM Decl;
tcRulesDecl decl = case decl of {
  DeclAnn ann inner -> DeclAnn ann <$> tcRulesDecl inner;
  DeclRules rules -> DeclRules <$> mapM tcRuleDecl rules;
  _ -> pure decl};
tcRuleDecl :: RuleDecl -> TcM RuleDecl;
tcRuleDecl rule = withAmbientSpan sp $ do {
  kinds <- getKinds;
  typeBinders <- foldM (ruleTypeBinder kinds) [] (ruleTypeBinders rule);
  let {
  scope = Map.fromList [(tvName tv, (tv, tvKind tv)) | tv <- typeBinders]};
  withImpliedScopedTyVars scope $ do {
  binders <- forM (ruleBinders rule) $ \binder -> do {
  ty <- case ruleBinderType binder of {
  Nothing -> freshMetaTv;
  Just sig -> do {
  scoped <- getScopedTyVars;
  checkRuntimeType scoped sig}};
  pure (binder, ty)};
  let {
  ruleTypeOf lhsTy = foldr (TcFunTy . snd) lhsTy binders};
  ((lhs', lhsTy, rhs', residualPreds), failed) <- withErrorTracking $ withRuleBinders binders $ do {
  (lhs', lhsTy, lhsCts) <- inferExpr (ruleLhs rule);
  (rhs', rhsTy, rhsCts) <- checkExpr lhsTy (ruleRhs rule);
  sidesEv <- freshEvVar;
  let {
  sidesCt = mkWantedEqCt TypeTrace {typeTraceType = rhsTy, typeTraceRole = ActualType, typeTraceOrigin = ExpressionTypeOrigin sp} TypeTrace {typeTraceType = lhsTy, typeTraceRole = ExpectedType, typeTraceOrigin = ExpressionTypeOrigin sp} sidesEv (UnifyOrigin sp) sp};
  solveResult <- solveWithImpls (lhsCts <> rhsCts <> [sidesCt]) [];
  residualPreds <- generalizableResidualPreds (ruleTypeOf lhsTy) solveResult;
  pure (lhs', lhsTy, rhs', residualPreds)};
  if failed then pure rule else do {
  Scheme inferred specified predicates body <- generalizeAndCommit (ruleTypeOf lhsTy) residualPreds;
  let {
  closed = schemeToType (Scheme [] (typeBinders <> inferred <> specified) predicates body)};
  binders' <- forM binders $ \(binder, ty) -> pure binder {ruleBinderAnns = mkAnnotation (PendingTcAnnotation ty [] [] 0 [] [] []) : ruleBinderAnns binder};
  pure rule {ruleAnns = mkAnnotation (PendingTcAnnotation closed [] [] 0 [] [] []) : ruleAnns rule, ruleBinders = binders', ruleLhs = lhs', ruleRhs = rhs'}}}} where {
  sp = sourceSpanFromAnns (ruleAnns rule);
  withRuleBinders binders action = foldr (\(binder, ty) -> extendResolvedTermEnv (ruleBinderName binder) (TcMonoIdBinder ty)) action binders;
  ruleTypeBinder kinds bound binder = do {
  kind <- case tyVarBinderKind binder of {
  Nothing -> pure (typeKind kinds);
  Just kindSyntax -> checkSurfaceType (Map.fromList [(tvName tv, (tv, tvKind tv)) | tv <- bound]) kindSyntax (typeKind kinds)};
  skolem <- freshSkolemTvOfKind (tyVarBinderName binder) kind;
  pure (bound <> [skolem])}};
data GlobalStateKeys = GlobalStateKeys { globalTerms :: !(Map TcTermKey TcBinder), globalTyCons :: !(Map TcTypeKey TyConInfo), globalDataTypes :: !(Map TcTypeKey DataTypeInfo), globalClasses :: !(Map TcTypeKey ClassInfo), globalInstances :: !InstanceEnv, globalDataFamilyInstances :: !(Map TcAxiomKey DataFamilyInstanceInfo), globalTypeFamilyInstances :: !(Map TcAxiomKey TypeFamilyInstanceInfo), globalPatSyns :: !(Map TcTermKey PatSynInfo) };
globalStateKeys :: TcState -> GlobalStateKeys;
globalStateKeys state = GlobalStateKeys {globalTerms = tcsGlobalTerms state, globalTyCons = tcsGlobalTyCons state, globalDataTypes = tcsDataTypes state, globalClasses = tcsClasses state, globalInstances = tcsInstances state, globalDataFamilyInstances = tcsDataFamilyInstances state, globalTypeFamilyInstances = tcsTypeFamilyInstances state, globalPatSyns = tcsPatSyns state};
defaultDeferredKindMetas :: TcM ();
defaultDeferredKindMetas = do {
  deferred <- takeDeferredKindMetas;
  mapM_ (defaultKindMetas . TcMetaTv) deferred};
componentTyConKeys :: GlobalStateKeys -> TcM (Set.Set TcTypeKey);
componentTyConKeys initialKeys = do {
  state <- lift get;
  pure (Map.keysSet (Map.difference (tcsGlobalTyCons state) (globalTyCons initialKeys)))};
reservedKindMetas :: TcM IntSet;
reservedKindMetas = do {
  keys <- getComponentTyCons;
  state <- lift get;
  let {
  owned = Map.restrictKeys (tcsGlobalTyCons state) keys};
  kinds <- mapM (zonkKind . typeSchemeBody . tciKindScheme) (Map.elems owned);
  pure (IntSet.fromList [key | Unique key <- concatMap collectMetaVars kinds])};
structuralDeclGroups :: [((Text, Text), Decl)] -> [[((Text, Text), Decl)]];
structuralDeclGroups declarations = map flatten (stronglyConnComp nodes) where {
  numbered = zip [0 :: Int ..] declarations;
  owners = Map.fromList [(key, index) | (index, (_, declaration)) <- numbered, key <- declarationTypeKeys declaration];
  nodes = [(declaration, index, dependencies (snd declaration)) | (index, declaration) <- numbered];
  signatures = collectStandaloneKindSignatures (map snd declarations);
  kindAnnotations declaration = concat [annotationList kind | key <- declarationTypeKeys declaration, Just kind <- [Map.lookup key signatures]];
  dependencies declaration = [owner | annotation <- annotationList declaration <> kindAnnotations declaration, Just resolution <- [fromAnnotation @ResolutionAnnotation annotation], resolutionNamespace resolution == ResolutionNamespaceType, ResolvedTopLevel package moduleName' name <- [resolutionTarget resolution], Just owner <- [Map.lookup (TcTypeKey (nameText name) package moduleName' ResolutionNamespaceType) owners]];
  flatten (AcyclicSCC declaration) = [declaration];
  flatten (CyclicSCC group) = group};
declarationTypeKeys :: Decl -> [TcTypeKey];
declarationTypeKeys declaration = case peelDeclAnn declaration of {
  DeclData info -> key (binderHeadName (dataDeclHead info));
  DeclNewtype info -> key (binderHeadName (newtypeDeclHead info));
  DeclTypeSyn info -> key (binderHeadName (typeSynHead info));
  DeclDataFamilyDecl info -> key (binderHeadName (dataFamilyDeclHead info));
  DeclTypeFamilyDecl info -> maybe [] key (typeFamilyHeadName (typeFamilyDeclHead info));
  DeclClass info -> key (binderHeadName (classDeclHead info)) <> concatMap (declarationTypeKeys . DeclTypeFamilyDecl) (classDeclTypeFamilies info);
  _ -> []} where {
  key = maybeToList . resolvedTypeKey};
generalizeDeclarationKinds :: [(Text, Text)] -> Set.Set TcTypeKey -> TcM ();
generalizeDeclarationKinds polyKindOrigins keys = do {
  state <- lift get;
  let {
  constructors = Map.filter (\info -> tciFlavor info `elem` [DataTyCon, NewtypeTyCon, SynonymTyCon] && (packageIdText (tyConPackageId (tciTyCon info)), tyConModuleName (tciTyCon info)) `elem` polyKindOrigins) (Map.restrictKeys (tcsGlobalTyCons state) keys)};
  generalized <- traverse generalizeDataKindInfo constructors;
  lift $ modify' (\current -> current {tcsGlobalTyCons = generalized `Map.union` tcsGlobalTyCons current})};
generalizeDataKindInfo :: TyConInfo -> TcM TyConInfo;
generalizeDataKindInfo info = do {
  scheme <- generalizeDataKind (tciKindScheme info);
  pure info {tciKindScheme = scheme}};
generalizeDataKind :: TypeScheme -> TcM TypeScheme;
generalizeDataKind (ForAll variables predicates body) = do {
  kind <- zonkKind body;
  let {
  metas = nub (collectMetaVars kind)};
  forM_ (zip [0 :: Int ..] metas) $ \(index, meta) -> do {
  variable <- freshSkolemTv ("k" <> T.pack (show index));
  writeMetaTv meta (TcTyVar variable)};
  kind' <- zonkKind kind;
  pure (specifiedScheme (uniqueKindVariables (variables <> freeKindVariables kind')) predicates kind')};
closeKindVariables :: [TyVarId] -> [TyVarId];
closeKindVariables variables = uniqueKindVariables (concatMap (\variable -> freeKindVariables (tvKind variable) <> [variable]) variables);
inventedKindVariablesFirst :: [TyVarId] -> [TyVarId];
inventedKindVariablesFirst variables = filter (not . given) ordered <> filter given ordered where {
  ordered = closeKindVariables variables;
  given variable = tvUnique variable `elem` map tvUnique variables};
withInventedKindVariables :: TypeScheme -> TypeScheme;
withInventedKindVariables (Scheme inferred specified predicates body) = Scheme (filter (not . isSpecified) ordered) (filter isSpecified ordered) predicates body where {
  ordered = closeKindVariables (inferred <> specified);
  isSpecified variable = tvUnique variable `elem` map tvUnique specified};
uniqueKindVariables :: [TyVarId] -> [TyVarId];
uniqueKindVariables = nubBy (\left right -> tvUnique left == tvUnique right);
freeKindVariables :: TcType -> [TyVarId];
freeKindVariables ty = case ty of {
  TcTyVar variable -> freeKindVariables (tvKind variable) <> [variable];
  TcMetaTv {} -> [];
  TcArrowTy -> [];
  TcTyLit {} -> [];
  TcTyCon _ arguments -> concatMap freeKindVariables arguments;
  TcFunTy argument result -> freeKindVariables argument <> freeKindVariables result;
  TcAppTy function argument -> freeKindVariables function <> freeKindVariables argument;
  TcForAllTy variable body -> filter (/= variable) (freeKindVariables body);
  TcQualTy _ body -> freeKindVariables body};
defaultGlobalKindMetas :: GlobalStateKeys -> TcM ();
defaultGlobalKindMetas initialKeys = do {
  state <- lift get;
  tyCons <- traverseNewMap globalTyCons defaultTyConInfoKinds (tcsGlobalTyCons state);
  terms <- traverseNewMap globalTerms defaultBinderKinds (tcsGlobalTerms state);
  dataTypes <- traverseNewMap globalDataTypes defaultDataTypeKinds (tcsDataTypes state);
  classes <- traverseNewMap globalClasses defaultClassKinds (tcsClasses state);
  let {
  previousInstances = globalInstances initialKeys};
  newInstances <- mapM defaultInstanceKinds (instanceEnvSince (tcsInstances state) previousInstances);
  let {
  instances = foldr addInstanceEnv previousInstances newInstances};
  dataFamilyInstances <- traverseNewMap globalDataFamilyInstances defaultDataFamilyInstanceKinds (tcsDataFamilyInstances state);
  typeFamilyInstances <- traverseNewMap globalTypeFamilyInstances defaultTypeFamilyInstanceKinds (tcsTypeFamilyInstances state);
  patSyns <- traverseNewMap globalPatSyns defaultPatSynKinds (tcsPatSyns state);
  lift $ modify' $ \current -> current {tcsGlobalTerms = terms, tcsPatSyns = patSyns, tcsGlobalTyCons = tyCons, tcsDataTypes = dataTypes, tcsClasses = classes, tcsInstances = instances, tcsDataFamilyInstances = dataFamilyInstances, tcsTypeFamilyInstances = typeFamilyInstances}} where {
  traverseNewMap selectPrevious transform current | Map.size current == Map.size previous = pure current | otherwise = do {
  defaulted <- traverse transform (Map.difference current previous);
  pure (Map.union defaulted current)} where {
  previous = selectPrevious initialKeys};
  defaultPatSynKinds info = do {
  scheme <- defaultTypeSchemeKinds (psiScheme info);
  required <- mapM defaultPredKinds (psiReqTheta info);
  provided <- mapM defaultPredKinds (psiProvTheta info);
  pure info {psiScheme = scheme, psiReqTheta = required, psiProvTheta = provided}};
  defaultBinderKinds binder = case binder of {
  TcIdBinder scheme closedness -> do {
  defaulted <- defaultTypeSchemeKinds scheme;
  pure (TcIdBinder (withInventedKindVariables defaulted) closedness)};
  TcMonoIdBinder ty -> TcMonoIdBinder <$> defaultTypeKinds ty};
  defaultTyConInfoKinds info = do {
  defaulted <- defaultTyConKindScheme (tciKindScheme info);
  let {
  ForAll variables predicates body = defaulted};
  let {
  kindScheme = specifiedScheme (closeKindVariables variables) predicates body};
  synonym <- traverse defaultTypeSynonymKinds (tciTypeSynonym info);
  pure info {tciKindScheme = kindScheme, tciTypeSynonym = synonym}};
  defaultTypeSynonymKinds synonym = TypeSynonymInfo <$> mapM defaultTyVarKinds (tsiParams synonym) <*> traverse defaultTypeKinds (tsiBody synonym);
  defaultDataTypeKinds info = do {
  tyVars <- mapM defaultTyVarKinds (dtiTyVars info);
  resultKind <- defaultKindMetas (dtiResultKind info);
  constructors <- mapM defaultDataConKinds (dtiConstructors info);
  pure info {dtiTyVars = tyVars, dtiResultKind = resultKind, dtiConstructors = constructors}};
  defaultDataConKinds info = do {
  universalTyVars <- mapM defaultTyVarKinds (dciUnivTyVars info);
  existentialTyVars <- mapM defaultTyVarKinds (dciExTyVars info);
  predicates <- mapM defaultPredKinds (dciTheta info);
  fields <- mapM defaultDataConFieldKinds (dciFields info);
  resultType <- defaultTypeKinds (dciResTy info);
  let {
  universals = inventedKindVariablesFirst universalTyVars};
  pure info {dciUnivTyVars = universals, dciExTyVars = filter (\tyVar -> tvUnique tyVar `notElem` map tvUnique universals) (inventedKindVariablesFirst existentialTyVars), dciTheta = predicates, dciFields = fields, dciResTy = resultType}};
  defaultDataConFieldKinds field = do {
  fieldType' <- defaultTypeKinds (dcfiType field);
  pure field {dcfiType = fieldType'}};
  defaultClassKinds info = do {
  kindTyVars <- mapM defaultTyVarKinds (ciKindTyVars info);
  tyVars <- mapM defaultTyVarKinds (ciTyVars info);
  superClassTypes <- mapM defaultTypeKinds (ciSuperClassTypes info);
  methods <- mapM (traverse defaultTypeSchemeKinds) (ciMethods info);
  defaultSignatures <- mapM (traverse defaultTypeSchemeKinds) (ciDefaultSignatures info);
  pure info {ciKindTyVars = kindTyVars, ciTyVars = tyVars, ciSuperClassTypes = superClassTypes, ciMethods = methods, ciDefaultSignatures = defaultSignatures}};
  defaultInstanceKinds info = InstanceInfo (iiClassName info) (iiDictName info) (iiDictOrigin info) <$> defaultTypeKinds (iiDictType info) <*> mapM defaultTyVarKinds (iiTyVars info) <*> mapM defaultPredKinds (iiContext info) <*> mapM defaultTypeKinds (iiHead info);
  defaultDataFamilyInstanceKinds info = do {
  familyType <- defaultTypeKinds (dfiiFamilyType info);
  tyVars <- mapM defaultTyVarKinds (dfiiTyVars info);
  pure info {dfiiFamilyType = familyType, dfiiTyVars = tyVars}};
  defaultTypeFamilyInstanceKinds info = do {
  let {
  (package, moduleName') = tfiiOrigin info};
  polyKinds <- isPolyKindOrigin (packageIdText package, moduleName');
  when polyKinds (generalizeTyVarKinds (tfiiTyVars info));
  tyVars <- mapM defaultTyVarKinds (tfiiTyVars info);
  left <- defaultTypeKinds (tfiiLeft info);
  right <- defaultTypeKinds (tfiiRight info);
  pure info {tfiiTyVars = orderTyVarsByKind (closeKindVariables tyVars), tfiiLeft = left, tfiiRight = right}}};
data TcDeclGroupResult = TcDeclGroupResult { tcGroupId :: !Int, tcGroupBindingResults :: ![TcBindingResult], tcGroupAnnotatedDecls :: !(Maybe [Decl]) };
checkTopLevelUnliftedBindings :: [(Int, DeclGroup)] -> [TcDeclGroupResult] -> TcM ();
checkTopLevelUnliftedBindings sourceGroups results = forM_ results $ \result -> case Map.lookup (tcGroupId result) groupsById of {
  Just group | Just sourceSpan <- declGroupSourceSpan group -> forM_ (tcGroupBindingResults result) $ \binding -> do {
  kind <- tcTypeKind (tbType binding);
  when (isUnliftedKind kind) $ emitError (Just sourceSpan) (TopLevelUnliftedBinding (tbDisplayName binding) (tbType binding))};
  _ -> pure ()} where {
  groupsById = Map.fromList sourceGroups;
  isUnliftedKind kind = case runtimeRepFromKind kind of {
  Right (BoxedRep Lifted) -> False;
  Right _ -> True;
  Left _ -> False}};
declGroupSourceSpan :: DeclGroup -> Maybe SourceSpan;
declGroupSourceSpan group = case group of {
  SingleDecl decl -> peelDeclSpan decl;
  MergedFunctionBind sourceSpan _ _ _ -> sourceSpan};
renderCheckedGroup :: Map Int [Decl] -> (Int, DeclGroup) -> [Decl];
renderCheckedGroup checkedGroups (groupId, group) = fromMaybe (renderDeclGroup group) (Map.lookup groupId checkedGroups);
annotateModuleTc :: Map Text TcType -> Module -> TcM Module;
annotateModuleTc checkedValueTypes m = do {
  let {
  classMethods = collectClassMethodNames (moduleDecls m)};
  decls <- mapM (annotateDeclTc (resolvedModuleOrigin m) classMethods checkedValueTypes False) (moduleDecls m);
  pure (m {moduleDecls = decls})};
annotateModuleDerivingTc :: [Extension] -> Module -> TcM Module;
annotateModuleDerivingTc extensions modu = do {
  declarations <- mapM (annotateDeclDerivingTc extensions) (moduleDecls modu);
  pure modu {moduleDecls = declarations}};
registerDerivedInstances :: (TcDerivingPlan -> Bool) -> Module -> TcM Module;
registerDerivedInstances selected modu = do {
  generated <- generateDerivedInstances selected origin modu;
  mapM_ (registerStructuralDecl origin) generated;
  pure modu {moduleDecls = moduleDecls modu <> generated}} where {
  origin = resolvedModuleOrigin modu};
annotateDeclDerivingTc :: [Extension] -> Decl -> TcM Decl;
annotateDeclDerivingTc extensions = go Nothing where {
  go declSpan decl = case decl of {
  DeclAnn annotation inner -> DeclAnn annotation <$> go (fromAnnotation annotation <|> declSpan) inner;
  DeclData dataDecl -> annotateAttachedDerivingTc extensions DataTyCon (dataDeclHead dataDecl) (dataDeclDeriving dataDecl) decl;
  DeclNewtype newtypeDecl -> annotateAttachedDerivingTc extensions NewtypeTyCon (newtypeDeclHead newtypeDecl) (newtypeDeclDeriving newtypeDecl) decl;
  DeclStandaloneDeriving derivingDecl -> annotateStandaloneDerivingTc extensions derivingDecl;
  DeclDataFamilyInst familyInst | not (null (dataFamilyInstDeriving familyInst)) -> do {
  emitWarning declSpan (OtherError "deriving clauses on data family instances are not supported yet; no instance is derived");
  pure decl};
  _ -> pure decl}};
annotateDeclTc :: (Text, Text) -> Map Text [Text] -> Map Text TcType -> Bool -> Decl -> TcM Decl;
annotateDeclTc origin classMethods checkedValueTypes derived decl = case decl of {
  DeclAnn ann _ | Just _ <- fromAnnotation @TcInstanceAnnotation ann -> pure decl;
  DeclAnn ann inner -> DeclAnn ann <$> annotateDeclTc origin classMethods checkedValueTypes (derived || isDerivedInstanceAnn ann) inner;
  DeclValue valueDecl | valueDeclWasChecked checkedValueTypes valueDecl -> do {
  (ty, valueDecl') <- annotateValueDeclTc checkedValueTypes valueDecl;
  pure (annotateDeclAt (valueDeclSpan valueDecl) (TcAnnotation ty [] [] [] [] []) (DeclValue valueDecl'))} | otherwise -> pure decl;
  DeclData dataDecl -> annotateDataDeclTc dataDecl;
  DeclNewtype newtypeDecl -> annotateNewtypeDeclTc newtypeDecl;
  DeclTypeSyn typeSynDecl -> annotateTypeSynDeclTc typeSynDecl;
  DeclDataFamilyDecl familyDecl -> annotateDataFamilyDeclTc familyDecl;
  DeclDataFamilyInst familyInst -> annotateDataFamilyInstTc familyInst;
  DeclTypeFamilyDecl familyDecl -> annotateTypeFamilyDeclTc familyDecl;
  DeclTypeFamilyInst familyInst -> annotateTypeFamilyInstTc origin familyInst;
  DeclForeign foreignDecl | isForeignImport foreignDecl -> annotateForeignDeclTc foreignDecl;
  DeclClass classDecl -> annotateClassDeclTc classDecl;
  DeclInstance instanceDecl -> annotateInstanceDeclTc origin derived instanceDecl;
  DeclStandaloneDeriving {} -> pure decl;
  DeclPatSyn patSynDecl | Just ty <- Map.lookup (unqualifiedNameText (patSynDeclName patSynDecl)) checkedValueTypes -> pure (annotateDeclAt (patSynBinderSpan (patSynDeclName patSynDecl)) (TcAnnotation ty [] [] [] [] []) decl);
  _ -> pure decl};
annotateInstanceHeaderTc :: (Text, Text) -> Bool -> Decl -> TcM Decl;
annotateInstanceHeaderTc origin derived decl = case decl of {
  DeclAnn ann inner | Just (TcCoercedDeriving plan) <- fromAnnotation ann, DeclInstance instanceDecl <- peelDeclAnn inner -> maybe id (DeclAnn . mkAnnotation) (tcDerivingSourceSpan plan) <$> annotateInstanceDeclWithPlan origin True (Just plan) instanceDecl;
  DeclAnn ann inner -> DeclAnn ann <$> annotateInstanceHeaderTc origin (derived || isDerivedInstanceAnn ann) inner;
  DeclInstance instanceDecl -> annotateInstanceDeclTc origin derived instanceDecl;
  _ -> pure decl};
isDerivedInstanceAnn :: Annotation -> Bool;
isDerivedInstanceAnn ann = isJust (fromAnnotation @TcDerivedInstance ann);
valueDeclWasChecked :: Map Text TcType -> ValueDecl -> Bool;
valueDeclWasChecked checkedValueTypes valueDecl = any (`Map.member` checkedValueTypes) (valueDeclBinderNames valueDecl);
valueDeclBinderNames :: ValueDecl -> [Text];
valueDeclBinderNames valueDecl = case valueDecl of {
  FunctionBind binder _ -> [unqualifiedNameText binder];
  PatternBind _ pat _ -> patternBinders pat};
annotateClassDeclTc :: ClassDecl -> TcM Decl;
annotateClassDeclTc classDecl = do {
  let {
  className = unqualifiedNameText (binderHeadName (classDeclHead classDecl))};
  classInfo <- lookupDeclaredClass (binderHeadName (classDeclHead classDecl));
  case classInfo of {
  Nothing -> missingTypeInfo ("class " <> T.unpack className);
  Just info -> do {
  kinds <- getKinds;
  methods <- zipWithM (annotateClassMethod (ciTyCon info)) [0 :: Int ..] (classDeclMethodNames classDecl);
  items <- mapM (annotateClassDefaultItem (ciTyCon info)) (classDeclItems classDecl);
  pure (DeclAnn (mkAnnotation TcClassAnnotation {tcClassTyCon = ciTyCon info, tcClassKindTyVars = ciKindTyVars info, tcClassTyVars = ciTyVars info, tcClassSuperClasses = map (constraintTypeDictBinder kinds) (ciSuperClassTypes info), tcClassMethods = methods, tcClassDefaultMethods = ciDefaultMethods info, tcClassDefaultSignatures = [(methodName, schemeToType signature) | (methodName, signature) <- ciDefaultSignatures info], tcClassAssociatedTypes = ciAssociatedTypes info, tcClassFunDeps = ciFunDeps info}) (DeclClass (classDecl {classDeclItems = items})))}}};
annotateClassDefaultItem :: TyCon -> ClassDeclItem -> TcM ClassDeclItem;
annotateClassDefaultItem classTyCon item = case item of {
  ClassItemAnn ann inner -> ClassItemAnn ann <$> annotateClassDefaultItem classTyCon inner;
  ClassItemDefault valueDecl -> case valueDeclBinderName valueDecl of {
  Just (methodName, _) -> do {
  methodTy <- bindingType (tyConMemberTermKey classTyCon (defaultMethodName methodName));
  pure (ClassItemAnn (mkAnnotation (TcInstanceMethodAnnotation methodName methodTy)) item)};
  Nothing -> pure item};
  ClassItemTypeFamilyDecl familyDecl -> case typeFamilyHeadName (typeFamilyDeclHead familyDecl) of {
  Nothing -> pure item;
  Just familyBinder -> do {
  ty <- tyConBindingType familyBinder;
  let {
  annotatedHead = annotateTypeFamilyHead (TcAnnotation ty [] [] [] [] []) (typeFamilyDeclHead familyDecl)};
  pure (ClassItemTypeFamilyDecl (familyDecl {typeFamilyDeclHead = annotatedHead}))}};
  _ -> pure item};
annotateClassMethod :: TyCon -> Int -> Text -> TcM TcClassMethodAnnotation;
annotateClassMethod classTyCon index methodName = do {
  methodTy <- bindingType (tyConMemberTermKey classTyCon methodName);
  let {
  (tvs, _) = peelForAlls methodTy};
  dictTy <- selectorDictTypeTc methodName methodTy;
  pure TcClassMethodAnnotation {tcClassMethodName = methodName, tcClassMethodType = methodTy, tcClassMethodTyVars = tvs, tcClassMethodDictType = dictTy, tcClassMethodIndex = index}};
annotateDataDeclTc :: DataDecl -> TcM Decl;
annotateDataDeclTc dataDecl = do {
  let {
  binder = binderHeadName (dataDeclHead dataDecl);
  tyName = unqualifiedNameText binder};
  ty <- tyConBindingType binder;
  parent <- mkDeclaredTyCon binder tyName (length (binderHeadParams (dataDeclHead dataDecl)));
  constructors <- mapM (annotateDataConDeclTc parent) (dataDeclConstructors dataDecl);
  let {
  annotatedHead = annotateBinderHeadName (TcAnnotation ty [] [] [] [] []) (dataDeclHead dataDecl)};
  pure (DeclData (dataDecl {dataDeclHead = annotatedHead, dataDeclConstructors = constructors}))};
annotateNewtypeDeclTc :: NewtypeDecl -> TcM Decl;
annotateNewtypeDeclTc newtypeDecl = do {
  let {
  binder = binderHeadName (newtypeDeclHead newtypeDecl);
  tyName = unqualifiedNameText binder};
  ty <- tyConBindingType binder;
  parent <- mkDeclaredTyCon binder tyName (length (binderHeadParams (newtypeDeclHead newtypeDecl)));
  constructor <- mapM (annotateDataConDeclTc parent) (newtypeDeclConstructor newtypeDecl);
  let {
  annotatedHead = annotateBinderHeadName (TcAnnotation ty [] [] [] [] []) (newtypeDeclHead newtypeDecl)};
  pure (DeclNewtype (newtypeDecl {newtypeDeclHead = annotatedHead, newtypeDeclConstructor = constructor}))};
annotateTypeSynDeclTc :: TypeSynDecl -> TcM Decl;
annotateTypeSynDeclTc typeSynDecl = do {
  ty <- tyConBindingType (binderHeadName (typeSynHead typeSynDecl));
  let {
  annotatedHead = annotateBinderHeadName (TcAnnotation ty [] [] [] [] []) (typeSynHead typeSynDecl)};
  pure (DeclTypeSyn (typeSynDecl {typeSynHead = annotatedHead}))};
annotateDataFamilyDeclTc :: DataFamilyDecl -> TcM Decl;
annotateDataFamilyDeclTc familyDecl = do {
  ty <- tyConBindingType (binderHeadName (dataFamilyDeclHead familyDecl));
  let {
  annotatedHead = annotateBinderHeadName (TcAnnotation ty [] [] [] [] []) (dataFamilyDeclHead familyDecl)};
  pure (DeclDataFamilyDecl (familyDecl {dataFamilyDeclHead = annotatedHead}))};
annotateTypeFamilyDeclTc :: TypeFamilyDecl -> TcM Decl;
annotateTypeFamilyDeclTc familyDecl = case typeFamilyHeadName (typeFamilyDeclHead familyDecl) of {
  Nothing -> pure (DeclTypeFamilyDecl familyDecl);
  Just familyBinder -> do {
  ty <- tyConBindingType familyBinder;
  let {
  annotatedHead = annotateTypeFamilyHead (TcAnnotation ty [] [] [] [] []) (typeFamilyDeclHead familyDecl)};
  pure (DeclTypeFamilyDecl (familyDecl {typeFamilyDeclHead = annotatedHead}))}};
annotateTypeFamilyHead :: TcAnnotation -> Type -> Type;
annotateTypeFamilyHead tcAnn ty = case peelTypeHead ty of {
  TCon name promo -> TCon (annotateName tcAnn name) promo;
  TInfix left name promo right -> TInfix left (annotateName tcAnn name) promo right;
  TApp function argument -> TApp (annotateTypeFamilyHead tcAnn function) argument;
  TTypeApp function argument -> TTypeApp (annotateTypeFamilyHead tcAnn function) argument;
  other -> other};
annotateName :: TcAnnotation -> Name -> Name;
annotateName tcAnn name = name {nameAnns = nameAnns name <> [mkAnnotation tcAnn]};
annotateTypeFamilyInstTc :: (Text, Text) -> TypeFamilyInst -> TcM Decl;
annotateTypeFamilyInstTc (packageName, moduleName') familyInst = do {
  familyInstances <- getTypeFamilyInstances;
  let {
  expectedKey = TcAxiomKey (PackageId packageName) moduleName' (sourceTypeFamilyAxiomName (packageName, moduleName') (typeFamilyInstLhs familyInst))};
  case find ((== expectedKey) . typeFamilyAxiomKey) familyInstances of {
  Just familyInstance -> pure (DeclAnn (mkAnnotation familyInstance) (DeclTypeFamilyInst familyInst));
  Nothing -> pure (DeclTypeFamilyInst familyInst)}};
annotateDataFamilyInstTc :: DataFamilyInst -> TcM Decl;
annotateDataFamilyInstTc familyInst = do {
  parent <- dataFamilyInstHeadTyCon familyInst;
  constructors <- mapM (annotateRegisteredDataConDeclTc parent) (dataFamilyInstConstructors familyInst);
  let {
  annotated = DeclDataFamilyInst (familyInst {dataFamilyInstConstructors = constructors})};
  constructorNames <- concat <$> mapM dataConNames constructors;
  familyInstances <- getDataFamilyInstances;
  case constructorNames of {
  firstConstructor : _ -> case find (elem firstConstructor . dfiiConstructorNames) familyInstances of {
  Just familyInstance -> pure (DeclAnn (mkAnnotation familyInstance) annotated);
  Nothing -> pure annotated};
  [] -> pure annotated}};
annotateRegisteredDataConDeclTc :: TyCon -> DataConDecl -> TcM DataConDecl;
annotateRegisteredDataConDeclTc parent dataConDecl = do {
  keys <- dataConKeys parent dataConDecl;
  case keys of {
  [] -> pure dataConDecl;
  key : _ -> do {
  maybeBinder <- lookupTermKey key;
  case maybeBinder of {
  Just (TcIdBinder scheme _) -> annotateWithType (schemeToType scheme);
  Just (TcMonoIdBinder ty) -> annotateWithType ty;
  Nothing -> pure dataConDecl}}}} where {
  annotateWithType ty = do {
  zonkedTy <- zonkType ty;
  pure (DataConAnn (mkAnnotation (TcAnnotation zonkedTy [] [] [] [] [])) dataConDecl)}};
annotateBinderHeadName :: TcAnnotation -> BinderHead UnqualifiedName -> BinderHead UnqualifiedName;
annotateBinderHeadName tcAnn head' = case head' of {
  PrefixBinderHead name params -> PrefixBinderHead (annotateUnqualifiedName tcAnn name) params;
  InfixBinderHead lhs name rhs params -> InfixBinderHead lhs (annotateUnqualifiedName tcAnn name) rhs params};
annotateUnqualifiedName :: TcAnnotation -> UnqualifiedName -> UnqualifiedName;
annotateUnqualifiedName tcAnn name = name {unqualifiedNameAnns = unqualifiedNameAnns name <> [mkAnnotation tcAnn]};
annotateDataConDeclTc :: TyCon -> DataConDecl -> TcM DataConDecl;
annotateDataConDeclTc parent dataConDecl = do {
  keys <- dataConKeys parent dataConDecl;
  case keys of {
  [] -> pure dataConDecl;
  key : _ -> do {
  ty <- dataConBindingType key;
  selectors <- annotateRecordSelectorNames parent dataConDecl;
  pure (DataConAnn (mkAnnotation (TcAnnotation ty [] [] [] [] [])) selectors)}}};
annotateRecordSelectorNames :: TyCon -> DataConDecl -> TcM DataConDecl;
annotateRecordSelectorNames parent declaration = case declaration of {
  RecordCon forallVars context constructor fields -> RecordCon forallVars context constructor <$> mapM annotateField fields;
  GadtCon forallBinders context constructors (GadtRecordBody fields result) -> GadtCon forallBinders context constructors . (`GadtRecordBody` result) <$> mapM annotateField fields;
  _ -> pure declaration} where {
  annotateField field = do {
  names <- mapM annotateSelectorName (fieldNames field);
  pure field {fieldNames = names}};
  annotateSelectorName name = do {
  ty <- bindingType (tyConMemberTermKey parent (unqualifiedNameText name));
  pure (annotateUnqualifiedName (TcAnnotation ty [] [] [] [] []) name)}};
dataConBindingType :: TcTermKey -> TcM TcType;
dataConBindingType key = do {
  mBinder <- lookupTermKey key;
  case mBinder of {
  Just (TcIdBinder scheme _) -> zonkType (schemeToType scheme);
  Just (TcMonoIdBinder ty) -> zonkType ty;
  Nothing -> missingTypeInfo ("data constructor " <> T.unpack (termKeyName key))}};
annotateForeignDeclTc :: ForeignDecl -> TcM Decl;
annotateForeignDeclTc foreignDecl = do {
  key <- resolvedUnqualifiedTermKey (foreignName foreignDecl);
  ty <- bindingType key;
  let {
  sourceSpan = unqualifiedNameSpan (foreignName foreignDecl);
  annotated = annotateDeclAt sourceSpan (TcAnnotation ty [] [] [] [] []) (DeclForeign foreignDecl)};
  case foreignCallConv foreignDecl of {
  callConv | callConv == CCall || callConv == CApi -> do {
  let {
  declaredName = unqualifiedNameText (foreignName foreignDecl);
  capi = callConv == CApi};
  checkedPlan <- case foreignEntity foreignDecl of {
  ForeignEntityWrapper | not capi -> checkForeignWrapper sourceSpan declaredName ty;
  ForeignEntityDynamic | not capi -> do {
  checkForeignDynamic sourceSpan ty;
  checkForeignImportType sourceSpan TcForeignDynamic declaredName ty};
  _ -> do {
  entity <- checkForeignEntity sourceSpan capi declaredName (foreignEntity foreignDecl);
  plan <- checkForeignImportType sourceSpan (foreignEntityTarget entity) (foreignEntityName entity) ty;
  checkForeignTarget sourceSpan plan {tcForeignCApi = foreignCApiFor capi entity}}};
  registerForeignImport key (TcForeignCCallImport (foreignSafetyMark (foreignSafety foreignDecl)) checkedPlan);
  pure (DeclAnn (mkAnnotation checkedPlan) annotated)};
  CPrim -> do {
  checkPrimitiveImportType sourceSpan key ty;
  registerForeignImport key TcForeignPrimImport;
  pure annotated};
  _ -> pure annotated}};
checkPrimitiveImportType :: Maybe SourceSpan -> TcTermKey -> TcType -> TcM ();
checkPrimitiveImportType sourceSpan key declaredType = do {
  wiring <- getWiring;
  case key of {
  TcTermGlobal package declaredModule name -> do {
  let {
  canonicalIdentity@(canonicalPackage, canonicalModule, canonicalName) = tcWiringPrimitiveTerm wiring name;
  canonicalKey = TcTermGlobal canonicalPackage canonicalModule canonicalName;
  canonicalLabel = T.unpack (canonicalModule <> "." <> canonicalName)};
  when ((package, declaredModule, name) /= canonicalIdentity) $ if Set.member canonicalIdentity (tcWiringRestrictedPrimitiveTerms wiring) then emitError sourceSpan (OtherError ("foreign primitive " <> T.unpack name <> " is only accepted at the configured identity " <> canonicalLabel)) else do {
  canonical <- lookupTermKey canonicalKey;
  case canonical of {
  Just binder -> do {
  let {
  canonicalType = case binder of {
  TcIdBinder scheme _ -> schemeToType scheme;
  TcMonoIdBinder ty -> ty}};
  unless (equivalentTypeSchemes (typeSchemeFromType canonicalType) (typeSchemeFromType declaredType)) $ emitError sourceSpan (OtherError ("foreign import prim " <> T.unpack name <> " must repeat the type of " <> canonicalLabel <> ": expected " <> renderTcType canonicalType <> ", got " <> renderTcType declaredType))};
  Nothing -> pure ()}}};
  _ -> pure ()}};
registerForeignImport :: TcTermKey -> TcForeignImportInfo -> TcM ();
registerForeignImport key info = lift $ modify' $ \state -> state {tcsForeignImports = Map.insert key info (tcsForeignImports state)};
foreignSafetyMark :: Maybe ForeignSafety -> TcForeignSafety;
foreignSafetyMark safety = case safety of {
  Nothing -> TcForeignSafe;
  Just Safe -> TcForeignSafe;
  Just Unsafe -> TcForeignUnsafe;
  Just Interruptible -> TcForeignInterruptible};
foreignCApiFor :: Bool -> ForeignEntity -> Maybe TcForeignCApi;
foreignCApiFor capi entity | not capi = Nothing | foreignEntityTarget entity == TcForeignAddress = Nothing | otherwise = Just TcForeignCApi {tcForeignCApiHeader = foreignEntityHeader entity, tcForeignCApiKind = if foreignEntityIsValue entity then TcForeignCApiValue else TcForeignCApiFunction};
checkForeignEntity :: Maybe SourceSpan -> Bool -> Text -> ForeignEntitySpec -> TcM ForeignEntity;
checkForeignEntity sourceSpan capi declaredName entity = case resolveForeignEntity capi declaredName entity of {
  Right resolved -> pure resolved;
  Left message -> do {
  emitError sourceSpan (OtherError message);
  pure (callEntity declaredName)}};
resolveForeignEntity :: Bool -> Text -> ForeignEntitySpec -> Either String ForeignEntity;
resolveForeignEntity capi declaredName entity = case entity of {
  ForeignEntityOmitted -> Right (callEntity declaredName);
  ForeignEntityStatic Nothing -> Right (callEntity declaredName);
  ForeignEntityStatic (Just text) -> readEntityText TcForeignCall text;
  ForeignEntityNamed text -> readEntityText TcForeignCall text;
  ForeignEntityAddress Nothing -> Right (addressEntity declaredName);
  ForeignEntityAddress (Just text) -> readEntityText TcForeignAddress text;
  ForeignEntityDynamic -> Left "a dynamic foreign import is not supported";
  ForeignEntityWrapper -> Left "a wrapper foreign import is not supported"} where {
  readEntityText defaultTarget text = let {
  entityWords = T.words text;
  withoutHeader = readEntityWords defaultTarget entityWords;
  withHeader = do {
  header <- listToMaybe entityWords;
  resolved <- readEntityWords defaultTarget (drop 1 entityWords);
  pure resolved {foreignEntityHeader = Just header}}} in case withoutHeader <|> withHeader of {
  Just resolved | Just problem <- entityProblem resolved -> Left (problem <> ": " <> T.unpack text) | otherwise -> Right resolved;
  Nothing -> Left ("unsupported foreign import entity: " <> T.unpack text)};
  readEntityWords defaultTarget entityWords = case entityWords of {
  [] -> Just (ForeignEntity defaultTarget Nothing False declaredName);
  ["&"] -> Just (addressEntity declaredName);
  ["&", name] -> addressEntity <$> cIdentifier name;
  ["value", name] -> valueEntity <$> cIdentifier name;
  [name] | Just addressName <- T.stripPrefix "&" name -> addressEntity <$> cIdentifier addressName | otherwise -> ForeignEntity defaultTarget Nothing False <$> cIdentifier name;
  _ -> Nothing};
  entityProblem resolved | foreignEntityIsValue resolved && not capi = Just "a value entity needs the capi calling convention" | Just header <- foreignEntityHeader resolved, not (validHeaderName header) = Just "unsupported header file name in a foreign import entity" | otherwise = Nothing;
  validHeaderName header = not (T.null header) && T.all (\character -> not (isSpace character) && character `notElem` ['"', '\\', '>', '<']) header;
  cIdentifier name = case T.uncons name of {
  Just (first, rest) | isIdentifierStart first && T.all isIdentifierPart rest -> Just name;
  _ -> Nothing};
  isIdentifierStart character = isAlpha character || character == '_';
  isIdentifierPart character = isAlphaNum character || character == '_'};
data ForeignEntity = ForeignEntity { foreignEntityTarget :: !TcForeignTarget, foreignEntityHeader :: !(Maybe Text), foreignEntityIsValue :: !Bool, foreignEntityName :: !Text } deriving (Eq, Show);
callEntity :: Text -> ForeignEntity;
callEntity = ForeignEntity TcForeignCall Nothing False;
addressEntity :: Text -> ForeignEntity;
addressEntity = ForeignEntity TcForeignAddress Nothing False;
valueEntity :: Text -> ForeignEntity;
valueEntity = ForeignEntity TcForeignCall Nothing True;
checkForeignTarget :: Maybe SourceSpan -> TcForeignImportAnnotation -> TcM TcForeignImportAnnotation;
checkForeignTarget sourceSpan plan = case tcForeignTarget plan of {
  TcForeignDynamic -> pure plan;
  TcForeignWrapper _ -> pure plan;
  TcForeignAddress -> do {
  unless (null (tcForeignArguments plan)) $ emitError sourceSpan (OtherError "an address foreign import must not take arguments");
  unless (tcForeignEffect plan == TcForeignPure) $ emitError sourceSpan (OtherError "an address foreign import must not return IO");
  unless (tcForeignAbiType (tcForeignResult plan) == TcForeignAddr) $ emitError sourceSpan (OtherError "an address foreign import must produce a pointer");
  pure plan};
  TcForeignCall | Just capi <- tcForeignCApi plan, tcForeignCApiKind capi == TcForeignCApiValue -> do {
  unless (null (tcForeignArguments plan)) $ emitError sourceSpan (OtherError "a value foreign import must not take arguments");
  when (tcForeignAbiType (tcForeignResult plan) == TcForeignVoid) $ emitError sourceSpan (OtherError "a value foreign import must produce a value");
  pure plan} | otherwise -> pure plan};
checkForeignImportType :: Maybe SourceSpan -> TcForeignTarget -> Text -> TcType -> TcM TcForeignImportAnnotation;
checkForeignImportType sourceSpan target symbol ty = do {
  let {
  (argumentTypes, resultType) = splitFunctionType ty;
  (effect, valueResultType) = case resultType of {
  TcTyCon (TyCon "IO" 1) [ioResult] -> (TcForeignRealWorld, ioResult);
  _ -> (TcForeignPure, resultType)}};
  arguments <- mapM (checkForeignValueType sourceSpan) argumentTypes;
  result <- checkForeignValueType sourceSpan valueResultType;
  when (any ((== TcForeignVoid) . tcForeignAbiType) arguments) $ emitError sourceSpan (OtherError "a foreign import argument must not have a unit type");
  pure TcForeignImportAnnotation {tcForeignArguments = arguments, tcForeignResult = result, tcForeignEffect = effect, tcForeignSymbol = symbol, tcForeignTarget = target, tcForeignCApi = Nothing}};
checkForeignWrapper :: Maybe SourceSpan -> Text -> TcType -> TcM TcForeignImportAnnotation;
checkForeignWrapper sourceSpan symbol ty = case splitFunctionType ty of {
  ([callback], TcTyCon (TyCon "IO" 1) [pointer@(TcTyCon (TyCon "FunPtr" 1) [pointed])]) -> do {
  unless (equivalentTypeSchemes (typeSchemeFromType callback) (typeSchemeFromType pointed)) $ emitError sourceSpan (OtherError "a wrapper result must point to its callback type");
  let {
  (callbackArguments, callbackResult) = splitFunctionType callback};
  when (null callbackArguments && not (isIO callbackResult)) $ emitError sourceSpan (OtherError "a callback must be a function or an IO action");
  pointerMarshal <- checkForeignValueType sourceSpan pointer;
  plan <- checkForeignImportType sourceSpan TcForeignCall symbol callback;
  when (any (isByteArray . tcForeignSourceType) (tcForeignResult plan : tcForeignArguments plan)) $ emitError sourceSpan (OtherError "a callback cannot use a byte array value");
  pure plan {tcForeignTarget = TcForeignWrapper pointerMarshal}};
  _ -> do {
  emitError sourceSpan (OtherError "a wrapper import must have type f -> IO (FunPtr f)");
  checkForeignImportType sourceSpan TcForeignCall symbol ty}} where {
  isIO (TcTyCon (TyCon "IO" 1) [_]) = True;
  isIO _ = False;
  isByteArray (TcTyCon constructor _) = tyConName constructor `elem` ["ByteArray#", "MutableByteArray#"];
  isByteArray _ = False};
checkForeignDynamic :: Maybe SourceSpan -> TcType -> TcM ();
checkForeignDynamic sourceSpan ty = case ty of {
  TcForAllTy _ body -> checkForeignDynamic sourceSpan body;
  TcFunTy (TcTyCon (TyCon "FunPtr" 1) [pointed]) function | equivalentTypeSchemes (typeSchemeFromType pointed) (typeSchemeFromType function) -> pure ();
  _ -> emitError sourceSpan (OtherError "a dynamic import must have type FunPtr f -> f")};
splitFunctionType :: TcType -> ([TcType], TcType);
splitFunctionType ty = case ty of {
  TcForAllTy _ body -> splitFunctionType body;
  TcFunTy argument result -> let {
  (arguments, finalResult) = splitFunctionType result} in (argument : arguments, finalResult);
  _ -> ([], ty)};
checkForeignValueType :: Maybe SourceSpan -> TcType -> TcM TcForeignMarshal;
checkForeignValueType sourceSpan ty = do {
  resolved <- resolveForeignValueType ty;
  case resolved of {
  Right marshal -> pure marshal;
  Left problem -> do {
  emitError sourceSpan (OtherError ("unsupported foreign import value type: " <> problem));
  primitiveMarshal ty [] "Int32#" TcForeignInt32 Nothing}}};
resolveForeignValueType :: TcType -> TcM (Either String TcForeignMarshal);
resolveForeignValueType sourceType = do {
  cType <- foreignCType sourceType;
  go cType (0 :: Int) [] sourceType} where {
  go cType depth constructors ty | depth > maximumUnwrapDepth = pure (Left ("too many newtype layers in " <> renderTcType sourceType)) | otherwise = case ty of {
  TcTyCon tyCon _ | Just (primitiveName, abiType) <- lookup (tyConName tyCon, tyConArity tyCon) primitiveForeignTypes -> Right <$> primitiveMarshal sourceType (reverse constructors) primitiveName abiType cType | (tyConName tyCon, tyConArity tyCon) `elem` [("ByteArray#", 0), ("MutableByteArray#", 1)] -> pure (Right (byteArrayMarshal ty)) | otherwise -> do {
  mDataType <- lookupDataType tyCon;
  case mDataType of {
  Just dataType | [constructor] <- dtiConstructors dataType, null (dciExTyVars constructor), null (dciTheta constructor) -> case dciFields constructor of {
  [field] | Just substitution <- matchTypes [dciResTy constructor] [ty] -> go cType (depth + 1) (dciName constructor : constructors) (applySubst substitution (dcfiType field));
  [] -> pure (Right (voidMarshal (reverse (dciName constructor : constructors))));
  _ -> unsupported ty};
  _ -> unsupported ty}};
  _ -> unsupported ty};
  unsupported ty | ty == sourceType = pure (Left (renderTcType ty)) | otherwise = pure (Left (renderTcType ty <> " in " <> renderTcType sourceType));
  maximumUnwrapDepth = 64;
  byteArrayMarshal ty = TcForeignMarshal {tcForeignSourceType = sourceType, tcForeignPrimitiveType = ty, tcForeignConstructors = [], tcForeignAbiType = TcForeignAddr, tcForeignCType = Nothing};
  voidMarshal constructors = TcForeignMarshal {tcForeignSourceType = sourceType, tcForeignPrimitiveType = sourceType, tcForeignConstructors = constructors, tcForeignAbiType = TcForeignVoid, tcForeignCType = Nothing}};
foreignCType :: TcType -> TcM (Maybe CType);
foreignCType = go (0 :: Int) where {
  go depth ty | depth > 64 = pure Nothing | otherwise = case ty of {
  TcTyCon tyCon [pointee] | tyConName tyCon `elem` ["Ptr", "FunPtr"] -> fmap (pointer "") <$> go (depth + 1) pointee | tyConName tyCon == "ConstPtr" -> fmap (pointer "const ") <$> go (depth + 1) pointee;
  TcTyCon tyCon _ -> do {
  mDataType <- lookupDataType tyCon;
  case mDataType of {
  Just dataType | Just cType <- dtiCType dataType -> pure (Just cType) | dtiFlavor dataType == NewtypeTyCon, [constructor] <- dtiConstructors dataType, [field] <- dciFields constructor, Just substitution <- matchTypes [dciResTy constructor] [ty] -> go (depth + 1) (applySubst substitution (dcfiType field));
  _ -> pure Nothing}};
  _ -> pure Nothing};
  pointer qualifier cType = cType {cTypeName = qualifier <> cTypeName cType <> " *"}};
primitiveForeignTypes :: [((Text, Int), (Text, TcForeignAbiType))];
primitiveForeignTypes = [(("Int#", 0), ("Int#", TcForeignInt)), (("Int8#", 0), ("Int8#", TcForeignInt8)), (("Int16#", 0), ("Int16#", TcForeignInt16)), (("Int32#", 0), ("Int32#", TcForeignInt32)), (("Int64#", 0), ("Int64#", TcForeignInt64)), (("Word#", 0), ("Word#", TcForeignWord)), (("Word8#", 0), ("Word8#", TcForeignWord8)), (("Word16#", 0), ("Word16#", TcForeignWord16)), (("Word32#", 0), ("Word32#", TcForeignWord32)), (("Word64#", 0), ("Word64#", TcForeignWord64)), (("Float#", 0), ("Float#", TcForeignFloat)), (("Double#", 0), ("Double#", TcForeignDouble)), (("Addr#", 0), ("Addr#", TcForeignAddr)), (("Char#", 0), ("Char#", TcForeignWord32)), (("StablePtr#", 1), ("StablePtr#", TcForeignAddr))];
primitiveMarshal :: TcType -> [Text] -> Text -> TcForeignAbiType -> Maybe CType -> TcM TcForeignMarshal;
primitiveMarshal sourceType constructors primitiveName abiType cType = do {
  wiring <- getWiring;
  kinds <- getKinds;
  primitiveTyCon <- mkWiredTyCon (tcWiringPrimitiveTyCon wiring primitiveName) (typeKind kinds);
  pure TcForeignMarshal {tcForeignSourceType = sourceType, tcForeignPrimitiveType = TcTyCon primitiveTyCon [], tcForeignConstructors = constructors, tcForeignAbiType = abiType, tcForeignCType = cType}};
annotateDeclAt :: Maybe SourceSpan -> TcAnnotation -> Decl -> Decl;
annotateDeclAt Nothing tcAnn decl = annotateDecl tcAnn decl;
annotateDeclAt (Just sp) tcAnn decl = DeclAnn (mkAnnotation sp) (annotateDecl tcAnn decl);
valueDeclSpan :: ValueDecl -> Maybe SourceSpan;
valueDeclSpan valueDecl = case valueDecl of {
  FunctionBind name _ -> unqualifiedNameSpan name;
  PatternBind _ pat _ -> patternSpan pat};
unqualifiedNameSpan :: UnqualifiedName -> Maybe SourceSpan;
unqualifiedNameSpan = sourceSpanFromAnns . unqualifiedNameAnns;
tyConBindingType :: UnqualifiedName -> TcM TcType;
tyConBindingType binder = do {
  mInfo <- lookupDeclaredTyCon binder;
  case mInfo of {
  Just info -> defaultKindMetas (typeSchemeBody (tciKindScheme info));
  Nothing -> missingTypeInfo ("type constructor " <> T.unpack (unqualifiedNameText binder))}};
annotateValueDeclTc :: Map Text TcType -> ValueDecl -> TcM (TcType, ValueDecl);
annotateValueDeclTc checkedValueTypes valueDecl = case valueDecl of {
  FunctionBind name matches -> do {
  bindingTy <- checkedBinderType name;
  pure (bindingTy, FunctionBind name matches)};
  PatternBind anns pat rhs -> case patternBinderSyntaxName pat of {
  Just name -> do {
  bindingTy <- checkedBinderType name;
  pure (bindingTy, PatternBind anns pat rhs)};
  Nothing -> do {
  let {
  name = patternBindingResultName pat};
  ty <- maybe (missingTypeInfo ("pattern binding " <> T.unpack name)) pure (Map.lookup name checkedValueTypes);
  pure (ty, valueDecl)}}} where {
  checkedBinderType name = case Map.lookup (unqualifiedNameText name) checkedValueTypes of {
  Just ty -> pure ty;
  Nothing -> bindingType =<< resolvedUnqualifiedTermKey name}};
annotateInstanceDeclTc :: (Text, Text) -> Bool -> InstanceDecl -> TcM Decl;
annotateInstanceDeclTc origin derived = annotateInstanceDeclWithPlan origin derived Nothing;
annotateInstanceDeclWithPlan :: (Text, Text) -> Bool -> Maybe TcDerivingPlan -> InstanceDecl -> TcM Decl;
annotateInstanceDeclWithPlan origin derived coercedPlan instanceDecl = case (instanceHeadName (instanceDeclHead instanceDecl), instanceHeadTypes (instanceDeclHead instanceDecl)) of {
  (Nothing, _) -> pure (DeclInstance instanceDecl);
  (Just className, headArgTypes) -> do {
  (rawTvIds, tvEnv) <- makeInstanceTyVarEnv instanceDecl headArgTypes;
  let {
  classNameText = nameText className};
  rawHeadTys <- checkInstanceHeadTypes className tvEnv headArgTypes;
  rawContext <- mapM (surfacePredToPred tvEnv) (instanceDeclContext instanceDecl);
  tvIds <- resolveInstanceTyVars origin rawTvIds;
  headTys <- mapM defaultTypeKinds rawHeadTys;
  context <- mapM defaultPredKinds rawContext;
  kinds <- getKinds;
  classInfo <- lookupClassNamed className;
  info <- maybe (missingTypeInfo ("class " <> T.unpack classNameText)) pure classInfo;
  dictName <- lookupInstanceDictName origin (ciTyCon info) headTys;
  headKinds <- mapM tcTypeKind headTys;
  let {
  kindSubstitution = fromMaybe Map.empty (matchTypes (map tvKind (ciTyVars info)) headKinds);
  classSubstitution = Map.fromList [(tvUnique tyVar, ty) | (tyVar, ty) <- zip (ciTyVars info) headTys] <> kindSubstitution;
  superClassTypes = map (applySubst classSubstitution) (ciSuperClassTypes info);
  defaults = ciDefaultMethods info};
  superClasses <- mapM constraintTypePred superClassTypes;
  superClassEvidence <- mapM (solveInstanceSuperClass classNameText context) superClasses;
  let {
  definedMethods = [name | item <- instanceDeclItems instanceDecl, InstanceItemBind valueDecl <- [peelInstanceDeclItemAnn item], name <- valueDeclBinderNames valueDecl]};
  defaultMethodUses <- sequence [do {
  expectedScheme <- methodExpectedScheme info headTys methodName;
  let {
  ForAll _ methodPredicates methodBody = expectedScheme};
  let {
  classVariables = map tvUnique (ciKindTyVars info <> ciTyVars info);
  signatureOwnVariables = [variable | variable <- signatureVariables, tvUnique variable `notElem` classVariables]};
  signatureMetas <- Map.fromList <$> sequence [(tvUnique variable,) <$> freshMetaTvOfKind (tvKind variable) | variable <- signatureOwnVariables, not (typeMentionsTyVar variable signatureBody)];
  let {
  openSubstitution = Map.union signatureMetas classSubstitution;
  signatureSubstitution = fromMaybe Map.empty (matchTypes [applySubst openSubstitution signatureBody] [methodBody]);
  instantiateSignature = applySubst signatureSubstitution . applySubst openSubstitution;
  predicates = filter (not . isPredicateOfClass (ciTyCon info)) (map (applySubstPred signatureSubstitution . applySubstPred openSubstitution) signaturePredicates)};
  evidence <- solveInstanceDefaultSignature classNameText (context <> methodPredicates) predicates;
  signatureTypes <- mapM (zonkType . instantiateSignature . TcTyVar) signatureOwnVariables;
  pure (methodName, signatureTypes, evidence)} | methodName <- defaults, methodName `notElem` definedMethods, isNothing coercedPlan, Just (ForAll signatureVariables signaturePredicates signatureBody) <- [lookup methodName (ciDefaultSignatures info)]];
  let {
  missingMethods = [methodName | not derived, (methodName, _) <- ciMethods info, methodName `notElem` definedMethods, methodName `notElem` defaults]};
  mapM_ (\methodName -> emitWarning (sourceSpanFromAnns (nameAnns className)) (OtherError ("no implementation of " <> T.unpack methodName <> ", and class " <> T.unpack classNameText <> " gives no default; calling it raises at run time"))) missingMethods;
  contextDicts <- mapM predDictBinder context;
  superClassBinders <- mapM predDictBinder superClasses;
  familyInstances <- getTypeFamilyInstances;
  let {
  lookupEquation axiomName = find ((== TcAxiomKey (PackageId (fst origin)) (snd origin) axiomName) . typeFamilyAxiomKey) familyInstances;
  explicitNames = mapMaybe typeFamilyInstName (instanceDeclTypeFamilyInsts instanceDecl);
  explicitEquation familyInst = lookupEquation (sourceTypeFamilyAxiomName origin (typeFamilyInstLhs familyInst));
  defaultEquations = [equation | associated <- ciAssociatedTypes info, tyConName (atiTyCon associated) `notElem` explicitNames, Just _ <- [atiDefault associated], Just equation <- [lookupEquation (associatedDefaultAxiomName kinds associated headTys)]];
  annotateItem item = case item of {
  InstanceItemAnn ann inner -> InstanceItemAnn ann (annotateItem inner);
  InstanceItemTypeFamilyInst familyInst | Just equation <- explicitEquation familyInst -> InstanceItemAnn (mkAnnotation equation) item;
  _ -> item};
  items = map annotateItem (instanceDeclItems instanceDecl);
  associatedEquations = mapMaybe explicitEquation (instanceDeclTypeFamilyInsts instanceDecl) <> defaultEquations};
  let {
  dictTy = foldr TcForAllTy (TcQualTy context (TcTyCon (ciTyCon info) headTys)) tvIds;
  methodOrder = map fst (ciMethods info);
  specializeVariable variable = setTyVarKind (applySubst kindSubstitution (tvKind variable)) variable;
  classTyVars = map specializeVariable (ciTyVars info);
  specializeKinds (name, Scheme inferred specified predicates body) = (name, Scheme (specializeVariables inferred) (specializeVariables specified) (map (applySubstPred kindSubstitution) predicates) (applySubst kindSubstitution body));
  specializeVariables variables = [specializeVariable variable | variable <- variables, Map.notMember (tvUnique variable) kindSubstitution];
  classMethods = zipWith (classMethodFromInfo info {ciTyVars = classTyVars}) [0 :: Int ..] (map specializeKinds (ciMethods info));
  instAnn = TcInstanceAnnotation {tcInstanceDictName = dictName, tcInstanceDictType = dictTy, tcInstanceClassTyCon = ciTyCon info, tcInstanceTyVars = tvIds, tcInstanceHeadTypes = headTys, tcInstanceClassTyVars = classTyVars, tcInstanceClassOrigin = ciOrigin info, tcInstanceClassSuperClasses = map (constraintTypeDictBinder kinds) (ciSuperClassTypes info), tcInstanceClassMethods = classMethods, tcInstanceContextDicts = contextDicts, tcInstanceSuperClasses = zip superClassBinders superClassEvidence, tcInstanceMethodOrder = methodOrder, tcInstanceDefaultMethods = defaults, tcInstanceDefaultMethodEvidence = [(methodName, evidence) | (methodName, _, evidence) <- defaultMethodUses], tcInstanceDefaultMethodTypes = [(methodName, signatureTypes) | (methodName, signatureTypes, _) <- defaultMethodUses], tcInstanceAssociatedTypes = associatedEquations, tcInstanceCoerced = Nothing}};
  checkedAnn <- case coercedPlan of {
  Nothing -> pure instAnn;
  Just plan -> checkCoercedInstance origin solveInstanceSuperClass methodExpectedScheme plan info context instAnn};
  pure (DeclAnn (mkAnnotation checkedAnn) (DeclInstance (instanceDecl {instanceDeclItems = items})))}};
classMethodFromInfo :: ClassInfo -> Int -> (Text, TypeScheme) -> TcClassMethodAnnotation;
classMethodFromInfo info index (methodName, scheme) = let {
  methodType = schemeToType scheme;
  (typeVariables, _) = peelForAlls methodType;
  dictionaryType = TcTyCon (ciTyCon info) (map TcTyVar (ciTyVars info))} in TcClassMethodAnnotation {tcClassMethodName = methodName, tcClassMethodType = methodType, tcClassMethodTyVars = typeVariables, tcClassMethodDictType = dictionaryType, tcClassMethodIndex = index};
isPredicateOfClass :: TyCon -> Pred -> Bool;
isPredicateOfClass classTyCon predicate = case predicate of {
  ClassPred predicateClass _ -> tyConKey predicateClass == tyConKey classTyCon;
  _ -> False};
solveInstanceDefaultSignature :: Text -> [Pred] -> [Pred] -> TcM [EvTerm];
solveInstanceDefaultSignature className givens predicates = do {
  _ <- improveInjectivity givens [predicate | predicate@EqPred {} <- predicates];
  mapM (solveInstanceSuperClass className givens) predicates};
solveInstanceSuperClass :: Text -> [Pred] -> Pred -> TcM EvTerm;
solveInstanceSuperClass className givens predicate = do {
  evidenceVariable <- freshEvVar;
  let {
  constraint = mkWantedCt predicate evidenceVariable (InstOrigin className) Nothing};
  result <- case predicate of {
  EqPred {} -> do {
  equality <- withGivenPredicates givens (solveEquality constraint);
  pure $ case equality of {
  EqSolved -> DictSolved;
  _ -> DictStuck constraint}};
  _ -> solveDictWithGivens givens constraint};
  case result of {
  DictSolved -> do {
  evidence <- lookupEvidence evidenceVariable;
  case evidence of {
  Just term -> pure term;
  Nothing -> missingTypeInfo ("superclass evidence for " <> T.unpack className)}};
  DictStuck stuck -> do {
  emitError (ctLoc stuck) (UnsolvedWanted (ctPred stuck) (ctOrigin stuck));
  pure (EvVarTerm evidenceVariable)}}};
tcClassDeclBodies :: Decl -> TcM Decl;
tcClassDeclBodies (DeclAnn ann inner) = DeclAnn ann <$> tcClassDeclBodies inner;
tcClassDeclBodies (DeclClass classDecl) = do {
  let {
  classBinder = binderHeadName (classDeclHead classDecl)};
  classInfo <- lookupDeclaredClass classBinder;
  case classInfo of {
  Nothing -> missingTypeInfo ("class " <> T.unpack (unqualifiedNameText classBinder));
  Just info -> do {
  items <- mapM (tcClassDefaultBody (ciTyCon info)) (classDeclItems classDecl);
  pure (DeclClass (classDecl {classDeclItems = items}))}}};
tcClassDeclBodies decl = pure decl;
tcClassDefaultBody :: TyCon -> ClassDeclItem -> TcM ClassDeclItem;
tcClassDefaultBody classTyCon item = case item of {
  ClassItemAnn ann inner -> ClassItemAnn ann <$> tcClassDefaultBody classTyCon inner;
  ClassItemDefault valueDecl -> do {
  checked <- tcClassDefaultValue classTyCon valueDecl;
  pure (ClassItemDefault checked)};
  _ -> pure item};
tcClassDefaultValue :: TyCon -> ValueDecl -> TcM ValueDecl;
tcClassDefaultValue classTyCon valueDecl = case valueDeclBinderName valueDecl of {
  Nothing -> pure valueDecl;
  Just (methodName, _) -> do {
  binder <- lookupTermKey (tyConMemberTermKey classTyCon (defaultMethodName methodName));
  case binder of {
  Just (TcIdBinder (ForAll methodTyVars givens methodTy) _) -> case valueDecl of {
  FunctionBind name matches -> do {
  let {
  (argumentTypes, resultType) = splitFunTy methodTy (matchArity matches)};
  results <- withScopedTyVars (tyVarScope methodTyVars) $ mapM (tcMatchEquation Nothing argumentTypes resultType) matches;
  solveInstanceBodyConstraints givens [(constraints, implications) | (_, constraints, implications) <- results];
  pure (FunctionBind name [match | (match, _, _) <- results])};
  PatternBind annotations pattern' rhs -> do {
  results <- withScopedTyVars (tyVarScope methodTyVars) $ mapM (tcMatchEquation Nothing [] methodTy) [zeroArgMatch (patternSpan pattern') rhs];
  solveInstanceBodyConstraints givens [(constraints, implications) | (_, constraints, implications) <- results];
  case results of {
  [(match, _, _)] -> pure (PatternBind annotations pattern' (matchRhs match));
  _ -> pure valueDecl}}};
  _ -> missingTypeInfo ("class default method " <> T.unpack methodName)}}};
tcInstanceDeclBodies :: Decl -> TcM Decl;
tcInstanceDeclBodies (DeclAnn ann inner) | Just annotation <- fromAnnotation @TcInstanceAnnotation ann, DeclInstance instanceDecl <- peelDeclAnn inner = do {
  let {
  classNameText = tyConName (tcInstanceClassTyCon annotation);
  headTys = tcInstanceHeadTypes annotation};
  givens <- mapM (constraintTypePred . tcDictBinderType) (tcInstanceContextDicts annotation);
  classInfo <- lookupClass (tcInstanceClassTyCon annotation) >>= maybe (missingTypeInfo ("class " <> T.unpack classNameText)) pure;
  items <- withScopedTyVars (tyVarScope (tcInstanceTyVars annotation)) $ mapM (tcInstanceItemBody classInfo givens headTys (instanceMethodSignatures (instanceDeclItems instanceDecl))) (instanceDeclItems instanceDecl);
  pure (DeclAnn ann (DeclInstance (instanceDecl {instanceDeclItems = items})))} | otherwise = DeclAnn ann <$> tcInstanceDeclBodies inner;
tcInstanceDeclBodies (DeclInstance instanceDecl) = case (instanceHeadName (instanceDeclHead instanceDecl), instanceHeadTypes (instanceDeclHead instanceDecl)) of {
  (Nothing, _) -> pure (DeclInstance instanceDecl);
  (Just className, headArgTypes) -> do {
  let {
  classNameText = nameText className};
  (_, tvEnv) <- makeInstanceTyVarEnv instanceDecl headArgTypes;
  rawHeadTys <- checkInstanceHeadTypes className tvEnv headArgTypes;
  rawGivens <- mapM (surfacePredToPred tvEnv) (instanceDeclContext instanceDecl);
  headTys <- mapM defaultTypeKinds rawHeadTys;
  givens <- mapM defaultPredKinds rawGivens;
  classInfo <- lookupClassNamed className >>= maybe (missingTypeInfo ("class " <> T.unpack classNameText)) pure;
  items <- withScopedTyVars tvEnv $ mapM (tcInstanceItemBody classInfo givens headTys (instanceMethodSignatures (instanceDeclItems instanceDecl))) (instanceDeclItems instanceDecl);
  pure (DeclInstance (instanceDecl {instanceDeclItems = items}))}};
tcInstanceDeclBodies decl = pure decl;
tyVarScope :: [TyVarId] -> Map Text (TyVarId, TcType);
tyVarScope tyVars = Map.fromList [(tvName tyVar, (tyVar, tvKind tyVar)) | tyVar <- tyVars];
instanceMethodSignatures :: [InstanceDeclItem] -> Map Text Type;
instanceMethodSignatures = Map.fromList . concatMap collect where {
  collect (InstanceItemAnn _ inner) = collect inner;
  collect (InstanceItemTypeSig names ty) = [(unqualifiedNameText name, ty) | name <- names];
  collect _ = []};
instanceMethodScope :: Map Text Type -> Text -> [Pred] -> TypeScheme -> [Match] -> TcM TvKindEnv;
instanceMethodScope signatures name givens (ForAll _ predicates expected) matches = case Map.lookup name signatures of {
  Nothing -> pure Map.empty;
  Just signature -> do {
  scheme <- sigToScheme signature;
  let {
  ForAll variables signaturePredicates signatureType = scheme};
  withScopedTyVars (scopedSigTyVars (explicitForallNames signature) variables) $ do {
  let {
  (arguments, result) = splitFunTy signatureType (matchArity matches)};
  checked <- withGivenPredicates (givens <> signaturePredicates) (mapM (tcMatchEquation Nothing arguments result) matches);
  solveInstanceBodyConstraints (givens <> signaturePredicates) [(cts, impls) | (_, cts, impls) <- checked]};
  instantiated <- instantiateWithArgs scheme;
  evidence <- freshEvVar;
  let {
  span' = surfaceTypeSpan signature;
  origin = SigOrigin span';
  equality = mkWantedCt (EqPred (instType instantiated) expected) evidence origin span'};
  constraints <- forM (instPreds instantiated) $ \predicate -> do {
  ev <- freshEvVar;
  pure (mkWantedCt predicate ev origin span')};
  solveBodyConstraintsWithGivens (givens <> predicates) (equality : constraints) [];
  arguments <- mapM zonkType (instTypeArgs instantiated);
  forM_ (zip variables arguments) $ \(variable, argument) -> when (tvName variable `elem` explicitForallNames signature) $ case argument of {
  TcTyVar _ -> pure ();
  _ -> emitError span' (OtherError "an explicit instance signature variable must remain polymorphic")};
  pure (Map.fromList [(tvName variable, (scoped, tvKind scoped)) | (variable, TcTyVar scoped) <- zip variables arguments, tvName variable `elem` explicitForallNames signature])}};
tcInstanceItemBody :: ClassInfo -> [Pred] -> [TcType] -> Map Text Type -> InstanceDeclItem -> TcM InstanceDeclItem;
tcInstanceItemBody classInfo givens headTys signatures item = case item of {
  InstanceItemAnn ann inner -> InstanceItemAnn ann <$> tcInstanceItemBody classInfo givens headTys signatures inner;
  InstanceItemBind (FunctionBind name matches) -> do {
  scheme <- methodExpectedScheme classInfo headTys (unqualifiedNameText name);
  let {
  ForAll methodTyVars methodGivens methodTy = scheme};
  scope <- instanceMethodScope signatures (unqualifiedNameText name) givens scheme matches;
  let {
  (argTys, resTy) = splitFunTy methodTy (matchArity matches)};
  (results, failed) <- withErrorTracking $ withScopedTyVars scope $ do {
  results <- withGivenPredicates (givens <> methodGivens) (mapM (tcMatchEquation Nothing argTys resTy) matches);
  solveInstanceBodyConstraints (givens <> methodGivens) [(cts, impls) | (_match, cts, impls) <- results];
  pure results};
  if failed then pure item else do {
  let {
  methodName = unqualifiedNameText name;
  checkedType = foldr TcForAllTy (qualifiedType methodGivens methodTy) methodTyVars;
  checkedBind = InstanceItemBind (FunctionBind name [match | (match, _cts, _impls) <- results])};
  pure (InstanceItemAnn (mkAnnotation (TcInstanceMethodAnnotation methodName checkedType)) checkedBind)}};
  InstanceItemBind (PatternBind _ pat rhs) -> case patternBinderName pat of {
  Just (methodName, _) -> do {
  scheme <- methodExpectedScheme classInfo headTys methodName;
  let {
  ForAll methodTyVars methodGivens methodTy = scheme};
  scope <- instanceMethodScope signatures methodName givens scheme [zeroArgMatch (patternSpan pat) rhs];
  (results, failed) <- withErrorTracking $ withScopedTyVars scope $ do {
  results <- withGivenPredicates (givens <> methodGivens) (mapM (tcMatchEquation Nothing [] methodTy) [zeroArgMatch (patternSpan pat) rhs]);
  solveInstanceBodyConstraints (givens <> methodGivens) [(cts, impls) | (_match, cts, impls) <- results];
  pure results};
  case results of {
  [(match, _cts, _impls)] | not failed -> let {
  checkedType = foldr TcForAllTy (qualifiedType methodGivens methodTy) methodTyVars;
  checkedBind = replaceInstancePatternBindRhs (matchRhs match) item} in pure (InstanceItemAnn (mkAnnotation (TcInstanceMethodAnnotation methodName checkedType)) checkedBind);
  _ -> pure item}};
  Nothing -> pure item};
  _ -> pure item} where {
  qualifiedType predicates ty | null predicates = ty | otherwise = TcQualTy predicates ty};
matchArity :: [Match] -> Int;
matchArity (match : _) = length (matchPats match);
matchArity [] = 0;
solveInstanceBodyConstraints :: [Pred] -> [([Ct], [Implication])] -> TcM ();
solveInstanceBodyConstraints givens results = do {
  let {
  (ctsList, implsList) = unzip results;
  cts = concat ctsList;
  impls = concat implsList};
  solveBodyConstraintsWithGivens givens cts impls};
solveBodyConstraintsWithGivens :: [Pred] -> [Ct] -> [Implication] -> TcM ();
solveBodyConstraintsWithGivens givens cts impls = withGivenPredicates givens $ do {
  implications <- mapM addOuterGivens impls;
  residual <- filterM (fmap not . solveGivenEquality givens) cts;
  solveResult <- solveWithImpls residual implications;
  let {
  deferred = filter (\ct -> ctEvVar ct `notElem` map ctEvVar cts) (inertDicts (srInerts solveResult))};
  stuck <- (srResidual solveResult <>) . concat <$> mapM attemptClassCt (cts <> deferred);
  defaulted <- defaultAmbiguousMetas [] stuck;
  remaining <- if defaulted then concat <$> mapM attemptStuckCt stuck else pure stuck;
  mapM_ reportUnsolvedDict remaining} where {
  addOuterGivens implication = do {
  outerGivens <- mapM givenConstraint givens;
  pure (implication {implGivenCts = outerGivens <> implGivenCts implication})};
  givenConstraint predicate = do {
  evidence <- freshEvVar;
  let {
  origin = InstOrigin "class body"};
  pure ((mkWantedCt predicate evidence origin Nothing) {ctFlavor = Given})};
  attemptClassCt ct | isDictionaryPred (ctPred ct) = do {
  result <- solveDictWithGivens givens ct;
  case result of {
  DictSolved -> pure [];
  DictStuck stuck -> pure [stuck]}} | otherwise = pure [];
  attemptStuckCt ct | EqPred {} <- ctPred ct = do {
  result <- solveEquality ct;
  case result of {
  EqSolved -> pure [];
  EqStuck stuck -> pure [stuck];
  EqError errCt -> do {
  case ctPred errCt of {
  EqPred left right -> emitError (ctLoc errCt) (UnificationError left right (ctOrigin errCt) (ctEqProvenance errCt));
  predicate -> emitError (ctLoc errCt) (UnsolvedWanted predicate (ctOrigin errCt))};
  pure []}}} | otherwise = attemptClassCt ct;
  isDictionaryPred predicate = case predicate of {
  ClassPred {} -> True;
  IParamPred {} -> True;
  IrredPred {} -> True;
  EqPred {} -> False;
  QuantifiedPred {} -> False}};
bindingType :: TcTermKey -> TcM TcType;
bindingType key = do {
  mBinder <- lookupTermKey key;
  case mBinder of {
  Just binder -> pure (binderType binder);
  Nothing -> missingTypeInfo ("binding " <> T.unpack (termKeyName key))}};
binderType :: TcBinder -> TcType;
binderType (TcIdBinder scheme _) = schemeToType scheme;
binderType (TcMonoIdBinder ty) = ty;
methodExpectedScheme :: ClassInfo -> [TcType] -> Text -> TcM TypeScheme;
methodExpectedScheme classInfo headTys methodName = case lookup methodName (ciMethods classInfo) of {
  Just (Scheme inferred specified predicates body) -> case splitClassReceiver predicates headTys of {
  Just (receiverSubst, methodPredicates) -> do {
  headKinds <- mapM tcTypeKind headTys;
  let {
  classKinds = map tvKind (ciTyVars classInfo);
  kindSubst = fromMaybe Map.empty (matchTypes classKinds headKinds);
  subst = receiverSubst <> kindSubst;
  unsubstituted = filter (\tyVar -> not (Map.member (tvUnique tyVar) subst))};
  pure (Scheme (unsubstituted inferred) (unsubstituted specified) (map (applySubstPred subst) methodPredicates) (applySubst subst body))};
  Nothing -> missingTypeInfo ("class method receiver for " <> T.unpack methodName)};
  Nothing -> missingTypeInfo ("class method " <> T.unpack methodName)};
splitClassReceiver :: [Pred] -> [TcType] -> Maybe (Map Unique TcType, [Pred]);
splitClassReceiver [] _ = Nothing;
splitClassReceiver (predicate : predicates) headTys = case predicate of {
  ClassPred _ classArgs -> (,predicates) <$> matchTypes classArgs headTys;
  _ -> do {
  (subst, rest) <- splitClassReceiver predicates headTys;
  pure (subst, predicate : rest)}};
missingTypeInfo :: String -> TcM a;
missingTypeInfo msg = abortTc ("internal type annotation error: missing " <> msg);
selectorDictTypeTc :: Text -> TcType -> TcM TcType;
selectorDictTypeTc methodName methodTy = case snd (peelForAlls methodTy) of {
  TcQualTy (pred' : _) _ -> predType pred';
  _ -> missingTypeInfo ("class dictionary type for method selector " <> T.unpack methodName)};
peelForAlls :: TcType -> ([TyVarId], TcType);
peelForAlls (TcForAllTy tv body) = let {
  (tvs, inner) = peelForAlls body} in (tv : tvs, inner);
peelForAlls ty = ([], ty);
predDictBinder :: Pred -> TcM TcDictBinderAnnotation;
predDictBinder pred' = case pred' of {
  ClassPred classTyCon args -> pure (TcDictBinderAnnotation (tyConName classTyCon) args (TcTyCon classTyCon args));
  EqPred {} -> do {
  ty <- predType pred';
  pure (TcDictBinderAnnotation "<constraint>" [] ty)};
  QuantifiedPred {} -> do {
  ty <- predType pred';
  pure (TcDictBinderAnnotation "<quantified>" [] ty)};
  IrredPred constraint -> pure (TcDictBinderAnnotation "<irreducible>" [] constraint);
  IParamPred name payload -> do {
  ty <- predType pred';
  pure (TcDictBinderAnnotation name [payload] ty)}};
constraintTypeDictBinder :: TcKinds -> TcType -> TcDictBinderAnnotation;
constraintTypeDictBinder kinds ty = case constraintTypeToPred kinds ty of {
  Just (ClassPred classTyCon args) -> TcDictBinderAnnotation (tyConName classTyCon) args ty;
  _ -> TcDictBinderAnnotation "<constraint>" [] ty};
constraintTypePred :: TcType -> TcM Pred;
constraintTypePred ty = do {
  kinds <- getKinds;
  case constraintTypeToPred kinds ty of {
  Just predicate -> pure predicate;
  Nothing -> missingTypeInfo ("class predicate for constraint " <> show ty)}};
collectClassMethodNames :: [Decl] -> Map Text [Text];
collectClassMethodNames = Map.fromList . mapMaybe collect where {
  collect decl = case peelDeclAnn decl of {
  DeclClass classDecl -> Just (unqualifiedNameText (binderHeadName (classDeclHead classDecl)), classDeclMethodNames classDecl);
  _ -> Nothing}};
classDeclMethodNames :: ClassDecl -> [Text];
classDeclMethodNames classDecl = concatMap classItemMethodNames (classDeclItems classDecl);
classItemMethodNames :: ClassDeclItem -> [Text];
classItemMethodNames item = case peelClassDeclItemAnn item of {
  ClassItemTypeSig names _ -> map unqualifiedNameText names;
  _ -> []};
classDeclDefaultMethodNames :: ClassDecl -> [Text];
classDeclDefaultMethodNames classDecl = nub (mapMaybe classItemDefaultMethodName (classDeclItems classDecl));
classItemDefaultMethodName :: ClassDeclItem -> Maybe Text;
classItemDefaultMethodName item = case peelClassDeclItemAnn item of {
  ClassItemDefault valueDecl -> fst <$> valueDeclBinderName valueDecl;
  _ -> Nothing};
valueDeclBinderName :: ValueDecl -> Maybe (Text, Text);
valueDeclBinderName valueDecl = case valueDecl of {
  FunctionBind name _ -> Just (binderBindingName name);
  PatternBind _ pat _ -> patternBinderName pat};
defaultMethodName :: Text -> Text;
defaultMethodName methodName = "$dm" <> T.concatMap encodeCharacter methodName where {
  encodeCharacter character | isAlphaNum character || character `elem` ("_$#'" :: String) = T.singleton character | otherwise = "$" <> T.pack (show (ord character)) <> "$"};
matchTypes :: [TcType] -> [TcType] -> Maybe (Map Unique TcType);
matchTypes patterns targets | length patterns /= length targets = Nothing | otherwise = foldM matchOne Map.empty (zip patterns targets);
matchOne :: Map Unique TcType -> (TcType, TcType) -> Maybe (Map Unique TcType);
matchOne subst (TcTyVar tv, target) = case Map.lookup (tvUnique tv) subst of {
  Nothing -> Just (Map.insert (tvUnique tv) target subst);
  Just existing | existing == target -> Just subst | otherwise -> Nothing};
matchOne subst (TcTyCon tc args, TcTyCon targetTc targetArgs) | tc == targetTc, length args == length targetArgs = foldM matchOne subst (zip args targetArgs);
matchOne subst (TcFunTy a b, TcFunTy targetA targetB) = matchOne subst (a, targetA) >>= \subst' -> matchOne subst' (b, targetB);
matchOne subst (TcAppTy f a, TcAppTy targetF targetA) = matchOne subst (f, targetF) >>= \subst' -> matchOne subst' (a, targetA);
matchOne subst (patternTy, targetTy) | patternTy == targetTy = Just subst | otherwise = Nothing;
collectUserSigs :: [Decl] -> TcM (Map TcTermKey UserSig);
collectUserSigs decls = do {
  signatures <- concat <$> mapM (extractSig Nothing) decls;
  foldM insertSignature Map.empty signatures} where {
  insertSignature collected (key, signature) | Map.member key collected = abortTc ("duplicate source signature key: " <> show key) | otherwise = pure (Map.insert key signature collected);
  extractSig ambient (DeclTypeSig names ty) = mapM (\n -> do {
  key <- resolvedUnqualifiedTermKey n;
  let {
  name = unqualifiedNameText n;
  sigSp = ambient <|> unqualifiedNameSpan n <|> typeSpan ty};
  pure (key, UserSig name ty sigSp)}) names;
  extractSig ambient (DeclForeign foreignDecl) | isForeignImport foreignDecl = do {
  key <- resolvedUnqualifiedTermKey (foreignName foreignDecl);
  let {
  name = unqualifiedNameText (foreignName foreignDecl);
  sigSp = ambient <|> unqualifiedNameSpan (foreignName foreignDecl) <|> typeSpan (foreignType foreignDecl)};
  pure [(key, UserSig name (foreignType foreignDecl) sigSp)]};
  extractSig ambient (DeclAnn ann inner) = extractSig (fromAnnotation @SourceSpan ann <|> ambient) inner;
  extractSig ambient (DeclPatSynSig names ty) = extractSig ambient (DeclTypeSig names ty);
  extractSig _ _ = pure []};
checkUserSig :: UserSig -> TcM CheckedSig;
checkUserSig userSig = do {
  scheme <- sigToScheme (userSigType userSig);
  pure CheckedSig {checkedSigName = userSigName userSig, checkedSigScheme = scheme, checkedSigSpan = userSigSpan userSig, checkedSigScopedNames = explicitForallNames (userSigType userSig), checkedSigPartial = hasWildcardType (userSigType userSig)}};
orderTyVarsByKind :: [TyVarId] -> [TyVarId];
orderTyVarsByKind = go [] where {
  go emitted pending = case partition (ready emitted pending) pending of {
  ([], _) -> reverse emitted <> pending;
  (next, rest) -> go (reverse next <> emitted) rest};
  ready emitted pending tyVar = not (any (\other -> other /= tyVar && other `notElem` emitted && kindMentionsUnique (tvUnique other) (tvKind tyVar)) pending)};
resolveInstanceTyVars :: (Text, Text) -> [TyVarId] -> TcM [TyVarId];
resolveInstanceTyVars origin rawTyVars = do {
  polyKinds <- isPolyKindOrigin origin;
  if polyKinds then do {
  generalizeTyVarKinds rawTyVars;
  tyVars <- mapM defaultTyVarKinds rawTyVars;
  pure (orderTyVarsByKind (closeKindVariables tyVars))} else orderTyVarsByKind <$> mapM defaultTyVarKinds rawTyVars};
makeInstanceTyVarEnv :: InstanceDecl -> [Type] -> TcM ([TyVarId], TvKindEnv);
makeInstanceTyVarEnv instanceDecl headArgTypes = do {
  explicitParams <- makeParamEnv (instanceDeclForall instanceDecl);
  let {
  explicitNames = map paramName explicitParams;
  freeVars = nub (explicitNames <> concatMap freeTypeVars (instanceDeclContext instanceDecl <> headArgTypes));
  implicitNames = freeVars \\ explicitNames;
  explicitEnv = Map.fromList [(paramName param, (paramTyVar param, paramKind param)) | param <- explicitParams]};
  rawImplicitTyVars <- mapM freshSkolemTv implicitNames;
  implicitKinds <- mapM (const freshKindMeta) implicitNames;
  let {
  implicitTyVars = zipWith setTyVarKind implicitKinds rawImplicitTyVars;
  implicitEnv = Map.fromList (zip implicitNames (zip implicitTyVars implicitKinds))};
  pure (map paramTyVar explicitParams <> implicitTyVars, explicitEnv <> implicitEnv)};
checkInstanceHeadTypes :: Name -> TvKindEnv -> [Type] -> TcM [TcType];
checkInstanceHeadTypes className tvEnv headArgTypes = do {
  argKinds <- classPredicateArgKinds className (length headArgTypes);
  zipWithM (checkSurfaceType tvEnv) headArgTypes argKinds};
skolemizeQualified :: TypeScheme -> TcM ([TyVarId], [Pred], TcType);
skolemizeQualified (ForAll tvs preds body) = do {
  (skolems, subst) <- foldM extendSubst ([], Map.empty) tvs;
  pure (skolems, map (applySubstPred subst) preds, applySubst subst body)} where {
  extendSubst (skolems, subst) tv = do {
  rawSkolem <- freshSkolemTv (tvName tv);
  let {
  skolem = setTyVarKind (applySubst subst (tvKind tv)) rawSkolem};
  pure (skolems <> [skolem], Map.insert (tvUnique tv) (TcTyVar skolem) subst)}};
splitFunTy :: TcType -> Int -> ([TcType], TcType);
splitFunTy ty 0 = ([], ty);
splitFunTy (TcFunTy a rest) n = let {
  (args, res) = splitFunTy rest (n - 1)} in (a : args, res);
splitFunTy ty _ = ([], ty);
data DeclGroup = SingleDecl Decl | MergedFunctionBind (Maybe SourceSpan) UnqualifiedName [Decl] [Match];
data DeclGraphKey = DeclGraphBinder !TcTermKey | DeclGraphSynthetic !Int deriving (Eq, Ord, Show);
groupValueDecls :: [Decl] -> [DeclGroup];
groupValueDecls [] = [];
groupValueDecls (d : ds) = case extractFunctionBind d of {
  Just (sp, name, matches) -> let {
  (sameNameDecls, rest) = span (hasSameName name) ds;
  groupDecls = d : sameNameDecls;
  allMatches = matches ++ concatMap (maybe [] (\(_, _, ms) -> ms) . extractFunctionBind) sameNameDecls} in MergedFunctionBind sp name groupDecls allMatches : groupValueDecls rest;
  Nothing -> SingleDecl d : groupValueDecls ds};
extractFunctionBind :: Decl -> Maybe (Maybe SourceSpan, UnqualifiedName, [Match]);
extractFunctionBind decl = case peelDeclAnn decl of {
  DeclValue (FunctionBind name matches) -> let {
  sp = peelDeclSpan decl} in Just (sp, name, matches);
  _ -> Nothing};
hasSameName :: UnqualifiedName -> Decl -> Bool;
hasSameName name d = case extractFunctionBind d of {
  Just (_, n, _) -> unqualifiedNameText n == unqualifiedNameText name;
  Nothing -> False};
sortDeclGroups :: [(Int, DeclGroup)] -> TcM [(Int, DeclGroup)];
sortDeclGroups groups = do {
  keyed <- mapM groupNodeKeys groups;
  let {
  owners = Map.fromList [(binder, nodeKey) | (_, nodeKey, binders) <- keyed, binder <- binders]};
  nodes <- mapM (declGraphNode owners) keyed;
  pure (concatMap flattenScc (stronglyConnComp nodes))} where {
  groupNodeKeys numberedGroup@(groupId, group) = do {
  nodeKey <- groupKey groupId group;
  binders <- declGroupBinderKeys group;
  pure (numberedGroup, nodeKey, binders)};
  flattenScc (AcyclicSCC group) = [group];
  flattenScc (CyclicSCC cyclicGroups) = cyclicGroups};
declGraphNode :: Map TcTermKey DeclGraphKey -> ((Int, DeclGroup), DeclGraphKey, [TcTermKey]) -> TcM ((Int, DeclGroup), DeclGraphKey, [DeclGraphKey]);
declGraphNode owners (numberedGroup, nodeKey, _) = do {
  freeVars <- freeVarsGroup (snd numberedGroup);
  let {
  deps = nub (mapMaybe (`Map.lookup` owners) (Set.toList freeVars))};
  pure (numberedGroup, nodeKey, deps)};
groupKey :: Int -> DeclGroup -> TcM DeclGraphKey;
groupKey ix group = do {
  keys <- declGroupBinderKeys group;
  case keys of {
  key : _ -> pure (DeclGraphBinder key);
  [] -> pure (DeclGraphSynthetic ix)}};
declGroupBinderKeys :: DeclGroup -> TcM [TcTermKey];
declGroupBinderKeys group = case group of {
  MergedFunctionBind _sp binder _decls _matches -> (: []) <$> resolvedUnqualifiedTermKey binder;
  SingleDecl decl -> case peelDeclAnn decl of {
  DeclValue (FunctionBind binder _) -> (: []) <$> resolvedUnqualifiedTermKey binder;
  DeclValue (PatternBind _ pat _) -> maybe (pure []) (fmap (: []) . resolvedUnqualifiedTermKey) (patternBinderSyntaxName pat);
  DeclPatSyn patSyn -> do {
  key <- resolvedUnqualifiedTermKey (patSynDeclName patSyn);
  fieldKeys <- mapM (patSynFieldTermKey key) (patSynRecordFields (patSynDeclArgs patSyn));
  pure (key : fieldKeys)};
  _ -> pure []}};
freeVarsGroup :: DeclGroup -> TcM (Set.Set TcTermKey);
freeVarsGroup group = case group of {
  MergedFunctionBind _sp binder _decls matches -> do {
  vars <- Set.unions <$> mapM freeVarsMatch matches;
  binderKey <- resolvedUnqualifiedTermKey binder;
  pure (Set.delete binderKey vars)};
  SingleDecl decl -> freeVarsDecl decl};
renderDeclGroup :: DeclGroup -> [Decl];
renderDeclGroup group = case group of {
  SingleDecl decl -> [decl];
  MergedFunctionBind _ _ decls _ -> decls};
replaceFunctionDeclMatches :: [Match] -> [Decl] -> [Decl];
replaceFunctionDeclMatches matches decls = snd (mapAccumL replace matches decls) where {
  replace remaining decl = let {
  count = functionDeclMatchCount decl;
  (here, rest) = splitAt count remaining} in (rest, replaceDeclFunctionMatches here decl)};
functionDeclMatchCount :: Decl -> Int;
functionDeclMatchCount decl = case peelDeclAnn decl of {
  DeclValue (FunctionBind _ matches) -> length matches;
  _ -> 0};
replaceDeclFunctionMatches :: [Match] -> Decl -> Decl;
replaceDeclFunctionMatches matches decl = case decl of {
  DeclAnn ann inner -> DeclAnn ann (replaceDeclFunctionMatches matches inner);
  DeclValue (FunctionBind name _) -> DeclValue (FunctionBind name matches);
  _ -> decl};
replacePatternBindRhs :: Rhs Expr -> Decl -> Decl;
replacePatternBindRhs rhs decl = case decl of {
  DeclAnn ann inner -> DeclAnn ann (replacePatternBindRhs rhs inner);
  DeclValue (PatternBind mult pat _) -> DeclValue (PatternBind mult pat rhs);
  _ -> decl};
replaceInstancePatternBindRhs :: Rhs Expr -> InstanceDeclItem -> InstanceDeclItem;
replaceInstancePatternBindRhs rhs item = case item of {
  InstanceItemAnn ann inner -> InstanceItemAnn ann (replaceInstancePatternBindRhs rhs inner);
  InstanceItemBind (PatternBind mult pat _) -> InstanceItemBind (PatternBind mult pat rhs);
  _ -> item};
patternBinders :: Pattern -> [Text];
patternBinders = map unqualifiedNameText . patternBinderNames;
tcDeclGroup :: Map TcTermKey CheckedSig -> (Int, DeclGroup) -> TcM TcDeclGroupResult;
tcDeclGroup sigs (groupId, group) = case group of {
  SingleDecl d -> tcSingleDeclGroup sigs groupId d;
  MergedFunctionBind _sp binder decls matches -> tcMergedFunctionGroup sigs groupId binder decls matches};
tcSingleDeclGroup :: Map TcTermKey CheckedSig -> Int -> Decl -> TcM TcDeclGroupResult;
tcSingleDeclGroup sigs groupId d = case peelDeclAnn d of {
  DeclValue (PatternBind _ pat rhs) -> case patternBinderSyntaxName pat of {
  Just binder -> do {
  key <- resolvedUnqualifiedTermKey binder;
  let {
  displayName = renderBinderName binder};
  (maybeMatches, bindings) <- case Map.lookup key sigs of {
  Just sig -> tcTopLevelWithSig key displayName sig [zeroArgMatch (patternSpan pat <|> peelDeclSpan d) rhs];
  Nothing -> tcFunctionInfer key displayName [zeroArgMatch (patternSpan pat) rhs]};
  let {
  annotatedDecls = fmap (\case {
  [match] -> [replacePatternBindRhs (matchRhs match) d];
  _ -> [d]}) maybeMatches};
  pure (TcDeclGroupResult groupId bindings annotatedDecls)};
  Nothing -> tcTopLevelPatternBind sigs groupId d pat rhs};
  DeclPatSyn patSyn -> tcPatSynDecl sigs groupId d patSyn;
  _ -> do {
  bindings <- tcDecl d;
  pure (TcDeclGroupResult groupId bindings Nothing)}};
tcTopLevelPatternBind :: Map TcTermKey CheckedSig -> Int -> Decl -> Pattern -> Rhs Expr -> TcM TcDeclGroupResult;
tcTopLevelPatternBind sigs groupId d pat rhs = do {
  let {
  sp = patternSpan pat <|> peelDeclSpan d;
  binders = patternBinderNames pat};
  placeholders <- forM binders $ \binder -> do {
  key <- resolvedUnqualifiedTermKey binder;
  case Map.lookup key sigs of {
  Just sig -> do {
  ty <- unrestrictedSigType sig;
  pure (binder, key, ty, Just sig)};
  Nothing -> do {
  ty <- freshMetaTv;
  extendTermKeyEnvPermanent key (TcMonoIdBinder ty);
  pure (binder, key, ty, Nothing)}}};
  ((rhs', rhsTy, pat'), failed) <- withErrorTracking $ do {
  (rhs', rhsTy, rhsCts) <- inferRhsWithLocals inferExpr rhs;
  patCheck <- checkPattern sp pat rhsTy;
  tieCts <- forM (pcBindings patCheck) $ \(name, ty) -> case [placeholderTy | (binder, _, placeholderTy, _) <- placeholders, binder == name] of {
  placeholderTy : _ -> do {
  ev <- freshEvVar;
  pure [mkWantedCt (EqPred placeholderTy ty) ev (LetOrigin sp) sp]};
  [] -> pure []};
  solveResult <- solveWithImpls (rhsCts <> pcWantedCts patCheck <> concat tieCts) [];
  residualPreds <- generalizableResidualPreds rhsTy solveResult;
  forM_ residualPreds $ \predicate -> emitError sp (UnsolvedWanted predicate (LetOrigin sp));
  pure (rhs', rhsTy, annotatePatternBindings (pcBindings patCheck) (checkedPattern patCheck))};
  if failed then pure (TcDeclGroupResult groupId [] Nothing) else do {
  ignored <- patternBindEnvironmentKeys placeholders;
  schemes <- generalizeGroupAndCommitIgnoring ignored [] ((rhsTy, []) : [(ty, []) | (_, _, ty, _) <- placeholders]);
  case schemes of {
  [] -> abortTc "pattern binding lost its generalized right-hand side";
  rhsScheme@(ForAll rhsTyVars _ _) : binderSchemes -> do {
  results <- forM (zip placeholders binderSchemes) $ \((binder, key, ty, maybeSig), scheme) -> do {
  let {
  name = unqualifiedNameText binder};
  case maybeSig of {
  Nothing -> finalizeInferredTermEnvPermanent key ty scheme;
  Just sig -> unless (equivalentTypeSchemes scheme (checkedSigScheme sig)) $ emitError (checkedSigSpan sig) $ OtherError ("the signature of " <> T.unpack name <> " does not match the type its pattern binding gives it: " <> renderTcType (schemeToType scheme))};
  typeArgs <- mapM (patternBindTypeArgument sp name scheme) rhsTyVars;
  zonkedTy <- zonkType (schemeToType scheme);
  let {
  pending = PendingTcAnnotation (typeSchemeBody scheme) (typeSchemeTyVars scheme) typeArgs 0 [] [] []};
  pure ((name, pending), TcBindingResult key (renderBinderName binder) zonkedTy)};
  zonkedRhsTy <- zonkType (schemeToType rhsScheme);
  rhsKey <- patternRhsTermKey pat;
  let {
  decl' = replacePatternBind (reannotatePatternBinders (map fst results) pat') rhs' d;
  patternResult = TcBindingResult rhsKey "<pattern>" zonkedRhsTy};
  pure (TcDeclGroupResult groupId (patternResult : map snd results) (Just [decl']))}}}} where {
  patternBindEnvironmentKeys placeholders = pure (Set.fromList [key | (_, key, _, _) <- placeholders]);
  unrestrictedSigType sig = case checkedSigScheme sig of {
  ForAll _ (_ : _) body -> do {
  emitError (checkedSigSpan sig) (OtherError ("the signature of a pattern binding must not have a context: " <> T.unpack (checkedSigName sig)));
  pure body};
  scheme -> fst <$> instantiate scheme};
  patternBindTypeArgument sp' name scheme tyVar | tyVar `elem` typeSchemeTyVars scheme = pure (TcTyVar tyVar) | otherwise = do {
  kinds <- getKinds;
  kind <- zonkType (tvKind tyVar);
  if kind == typeKind kinds then do {
  unitTyCon <- flip mkWiredTyCon (typeKind kinds) =<< wiredTupleTyCon Boxed 0;
  pure (TcTyCon unitTyCon [])} else do {
  emitError sp' $ OtherError ("a pattern binding cannot generalize over the type variable " <> T.unpack (tvName tyVar) <> " of kind " <> renderTcType kind <> ", which " <> T.unpack name <> " does not mention");
  pure (TcTyVar tyVar)}}};
typeSchemeTyVars :: TypeScheme -> [TyVarId];
typeSchemeTyVars (ForAll tyVars _ _) = tyVars;
patternBindingResultName :: Pattern -> Text;
patternBindingResultName pat = "<pattern " <> T.unwords (patternBinders pat) <> ">";
patternRhsTermKey :: Pattern -> TcM TcTermKey;
patternRhsTermKey pat = do {
  keys <- mapM resolvedUnqualifiedTermKey (patternBinderNames pat);
  case [(package, moduleName') | TcTermGlobal package moduleName' _ <- keys] of {
  (package, moduleName') : _ -> pure (TcTermGlobal package moduleName' resultName);
  [] -> abortTc ("pattern binding " <> T.unpack resultName <> " binds no top-level name")}} where {
  resultName = patternBindingResultName pat};
replacePatternBind :: Pattern -> Rhs Expr -> Decl -> Decl;
replacePatternBind pat rhs decl = case decl of {
  DeclAnn ann inner -> DeclAnn ann (replacePatternBind pat rhs inner);
  DeclValue (PatternBind mult _ _) -> DeclValue (PatternBind mult pat rhs);
  _ -> decl};
tcPatSynDecl :: Map TcTermKey CheckedSig -> Int -> Decl -> PatSynDecl -> TcM TcDeclGroupResult;
tcPatSynDecl sigs groupId decl patSyn = do {
  let {
  binder = patSynDeclName patSyn;
  name = unqualifiedNameText binder;
  displayName = renderBinderName binder;
  nameSpan = patSynBinderSpan binder;
  argNames = patSynArgNames (patSynDeclArgs patSyn);
  arity = length argNames;
  pat = patSynDeclPat patSyn;
  failedResult = TcDeclGroupResult groupId [] Nothing};
  key <- resolvedUnqualifiedTermKey binder;
  (package, moduleName') <- case key of {
  TcTermGlobal package moduleName' _ -> pure (package, moduleName');
  TcTermLocal {} -> abortTc ("pattern synonym " <> T.unpack name <> " is not a top-level binder")};
  case mapM (`patternVarBinder` pat) argNames of {
  Nothing -> do {
  emitError nameSpan (OtherError ("pattern synonym " <> T.unpack name <> " has an argument that its pattern does not bind"));
  pure failedResult};
  Just argBinders -> do {
  let {
  matcherName = "$m" <> name;
  builderName = "$b" <> name;
  matcherKey = TcTermGlobal package moduleName' matcherName;
  builderKey = TcTermGlobal package moduleName' builderName;
  matcherMatch = patSynMatcherMatch pat argBinders};
  maybeLayout <- case Map.lookup key sigs of {
  Just sig -> patSynLayoutFromSig name arity sig;
  Nothing -> inferPatSynLayout nameSpan name pat argBinders};
  case maybeLayout of {
  Nothing -> pure failedResult;
  Just layout -> do {
  let {
  scheme = patSynLayoutScheme layout};
  when (Map.notMember key sigs) $ registerCheckedSig key (CheckedSig name scheme nameSpan [] False);
  matcherSig <- patSynMatcherSig matcherName nameSpan layout;
  registerCheckedSig matcherKey matcherSig;
  (maybeMatcherMatches, matcherResults) <- tcFunctionWithSig matcherKey matcherName matcherSig [matcherMatch];
  commitCheckedHelper matcherKey matcherResults;
  case maybeMatcherMatches of {
  Just [matcherMatch'] -> do {
  checkedPat <- maybe (abortTc ("pattern synonym " <> T.unpack name <> " lost its checked pattern")) pure (matcherPattern matcherMatch');
  let {
  builderSig = CheckedSig builderName scheme nameSpan [] False};
  (direction, sourceBuilder) <- case patSynDeclDir patSyn of {
  PatSynUnidirectional -> pure (PatSynUnidirectionalInfo, Just Nothing);
  PatSynExplicitBidirectional matches -> pure (PatSynExplicitBidirectionalInfo, Just (Just matches));
  PatSynBidirectional -> case patternToExpr pat of {
  Just expr -> pure (PatSynImplicitBidirectionalInfo, Just (Just [Match [] MatchHeadPrefix (map PVar argBinders) (UnguardedRhs [] expr Nothing)]));
  Nothing -> do {
  emitError nameSpan (OtherError ("the pattern of the bidirectional pattern synonym " <> T.unpack name <> " is not an expression; give explicit builder equations"));
  pure (PatSynImplicitBidirectionalInfo, Nothing)}}};
  case sourceBuilder of {
  Nothing -> pure failedResult;
  Just builderMatches -> do {
  (maybeBuilderMatches, builderResults) <- case builderMatches of {
  Nothing -> pure (Nothing, []);
  Just matches -> do {
  registerCheckedSig builderKey builderSig;
  checked <- tcFunctionWithSig builderKey builderName builderSig matches;
  commitCheckedHelper builderKey (snd checked);
  pure checked}};
  case (builderMatches, maybeBuilderMatches) of {
  (Just _, Nothing) -> pure failedResult;
  _ -> do {
  (selectorMatches, selectorResults) <- tcPatSynRecordSelectors package moduleName' nameSpan layout (patSynDeclArgs patSyn) pat argBinders;
  addPatSyn PatSynInfo {psiName = name, psiOrigin = (package, moduleName'), psiArity = arity, psiFields = patSynRecordFields (patSynDeclArgs patSyn), psiDirection = direction, psiScheme = scheme, psiReqTheta = patSynLayoutRequired layout, psiProvTheta = patSynLayoutProvided layout};
  zonkedTy <- zonkType (schemeToType scheme);
  let {
  dir' = case (patSynDeclDir patSyn, maybeBuilderMatches) of {
  (PatSynExplicitBidirectional {}, Just matches) -> PatSynExplicitBidirectional matches;
  (dir, _) -> dir};
  spannedPat = case patternSpan checkedPat of {
  Nothing -> checkedPat;
  Just patSpan -> PAnn (mkAnnotation patSpan) checkedPat};
  patSyn' = patSyn {patSynDeclPat = spannedPat, patSynDeclDir = dir'};
  annotation = TcPatSynAnnotation matcherMatch' maybeBuilderMatches selectorMatches;
  decl' = DeclAnn (mkAnnotation annotation) (replacePatSynDecl patSyn' decl);
  results = TcBindingResult key displayName zonkedTy : matcherResults <> builderResults <> selectorResults};
  pure (TcDeclGroupResult groupId results (Just [decl']))}}}}};
  _ -> pure failedResult}}}}}};
checkBundledPatSyns :: Module -> TcM ();
checkBundledPatSyns modu = mapM_ (go Nothing) (fromMaybe [] (moduleExports modu)) where {
  go sp spec = case spec of {
  ExportAnn ann inner -> go (fromAnnotation @SourceSpan ann <|> sp) inner;
  ExportWith _ _ typeName members -> mapM_ (checkMember sp (nameText typeName)) members;
  ExportWithAll _ _ typeName _ members -> mapM_ (checkMember sp (nameText typeName)) members;
  _ -> pure ()};
  checkMember sp typeName member = do {
  let {
  memberName = nameText (ieBundledMemberName member)};
  patSyns <- getPatSyns;
  forM_ [info | info <- patSyns, psiName info == memberName] $ \info -> case patSynResultTyConName info of {
  Just resultName | resultName /= typeName -> emitError sp (OtherError ("pattern synonym " <> T.unpack memberName <> " has the scrutinee type " <> T.unpack resultName <> " and cannot be bundled with " <> T.unpack typeName));
  _ -> pure ()}};
  patSynResultTyConName info = let {
  ForAll _ _ body = psiScheme info} in case resultType body of {
  TcTyCon tyCon _ -> Just (tyConName tyCon);
  _ -> Nothing};
  resultType ty = case ty of {
  TcFunTy _ result -> resultType result;
  _ -> ty}};
data PatSynLayout = PatSynLayout { patSynLayoutUniversals :: ![TyVarId], patSynLayoutExistentials :: ![TyVarId], patSynLayoutRequired :: ![Pred], patSynLayoutProvided :: ![Pred], patSynLayoutArgTypes :: ![TcType], patSynLayoutResultType :: !TcType };
patSynLayoutScheme :: PatSynLayout -> TypeScheme;
patSynLayoutScheme layout = specifiedScheme (patSynLayoutUniversals layout <> patSynLayoutExistentials layout) (patSynLayoutRequired layout <> patSynLayoutProvided layout) (foldr TcFunTy (patSynLayoutResultType layout) (patSynLayoutArgTypes layout));
patSynLayoutFromSig :: Text -> Int -> CheckedSig -> TcM (Maybe PatSynLayout);
patSynLayoutFromSig name arity sig = do {
  let {
  ForAll tyVars required qualifiedBody = checkedSigScheme sig;
  (explicitExistentials, innerBody) = collectForAllTypes qualifiedBody;
  (provided, body) = case innerBody of {
  TcQualTy predicates inner -> (predicates, inner);
  _ -> ([], innerBody)};
  (argTys, resultType) = splitFunTy body arity;
  (universals, implicitExistentials) = partition (`typeMentionsTyVar` resultType) tyVars;
  existentials = implicitExistentials <> explicitExistentials};
  if length argTys /= arity then do {
  emitError (checkedSigSpan sig) (OtherError ("pattern synonym signature for " <> T.unpack name <> " does not have " <> show arity <> " arguments"));
  pure Nothing} else pure (Just PatSynLayout {patSynLayoutUniversals = universals, patSynLayoutExistentials = existentials, patSynLayoutRequired = required, patSynLayoutProvided = provided, patSynLayoutArgTypes = argTys, patSynLayoutResultType = resultType})};
inferPatSynLayout :: Maybe SourceSpan -> Text -> Pattern -> [UnqualifiedName] -> TcM (Maybe PatSynLayout);
inferPatSynLayout sp name pat argBinders = do {
  scrutTy <- freshMetaTv;
  ((patCheck, argTys, residual), failed) <- withErrorTracking $ do {
  patCheck <- checkPattern sp pat scrutTy;
  argTys <- mapM (argumentType patCheck) argBinders;
  residual <- if null (pcGivenCts patCheck) && null (pcSkolems patCheck) then do {
  solveResult <- solveWithImpls (pcWantedCts patCheck) [];
  generalizableResidualPreds (foldr TcFunTy scrutTy argTys) solveResult} else do {
  _ <- solvePatternBranch sp patCheck scrutTy [];
  pure []};
  pure (patCheck, argTys, residual)};
  if failed then pure Nothing else do {
  generalized <- generalizeAndCommit (foldr TcFunTy scrutTy argTys) residual;
  let {
  ForAll universals required body = generalized};
  provided <- mapM zonkPred [ctPred ct | ct <- pcGivenCts patCheck, isClassPredicate (ctPred ct)];
  let {
  (argTys', resultType) = splitFunTy body (length argTys)};
  pure (Just PatSynLayout {patSynLayoutUniversals = universals, patSynLayoutExistentials = pcSkolems patCheck, patSynLayoutRequired = required, patSynLayoutProvided = provided, patSynLayoutArgTypes = argTys', patSynLayoutResultType = resultType})}} where {
  argumentType patCheck binder = case [ty | (bound, ty) <- pcBindings patCheck, unqualifiedNameText bound == unqualifiedNameText binder] of {
  ty : _ -> pure ty;
  [] -> abortTc ("pattern synonym " <> T.unpack name <> " does not bind " <> T.unpack (unqualifiedNameText binder))};
  isClassPredicate predicate = case predicate of {
  ClassPred {} -> True;
  _ -> False}};
patSynMatcherSig :: Text -> Maybe SourceSpan -> PatSynLayout -> TcM CheckedSig;
patSynMatcherSig matcherName sp layout = do {
  kinds <- getKinds;
  unitTyCon <- flip mkWiredTyCon (typeKind kinds) =<< wiredTupleTyCon Boxed 0;
  representation <- freshSkolemTvOfKind "rep" (runtimeRepKind kinds);
  result <- freshSkolemTvOfKind "r" (mkTYPEKind kinds (TcTyVar representation));
  let {
  resultTy = TcTyVar result;
  unitTy = TcTyCon unitTyCon [];
  continuationArgs = case patSynLayoutArgTypes layout of {
  [] -> [unitTy];
  argTypes -> argTypes};
  continuationBody = foldr TcFunTy resultTy continuationArgs;
  qualifiedContinuation = case patSynLayoutProvided layout of {
  [] -> continuationBody;
  provided -> TcQualTy provided continuationBody};
  continuation = foldr TcForAllTy qualifiedContinuation (patSynLayoutExistentials layout);
  failure = TcFunTy unitTy resultTy;
  matcherTy = TcFunTy (patSynLayoutResultType layout) (TcFunTy continuation (TcFunTy failure resultTy))};
  pure (CheckedSig matcherName (specifiedScheme (patSynLayoutUniversals layout <> [representation, result]) (patSynLayoutRequired layout) matcherTy) sp [] False)};
commitCheckedHelper :: TcTermKey -> [TcBindingResult] -> TcM ();
commitCheckedHelper key results = case [tbType result | result <- results, tbKey result == key] of {
  ty : _ -> do {
  let {
  binder = TcIdBinder (typeToScheme ty) Closed};
  replaceTermKeyEnvPermanent key binder};
  [] -> pure ()};
registerDeclaredRecordPatSyn :: Decl -> TcM ();
registerDeclaredRecordPatSyn decl = case peelDeclAnn decl of {
  DeclPatSyn patSyn | fields@(_ : _) <- patSynRecordFields (patSynDeclArgs patSyn) -> do {
  key <- resolvedUnqualifiedTermKey (patSynDeclName patSyn);
  case key of {
  TcTermGlobal package moduleName' name -> addDeclaredRecordPatSyn key RecordHead {rhName = name, rhOrigin = (package, moduleName'), rhFields = map Just fields};
  TcTermLocal {} -> pure ()}};
  _ -> pure ()};
patSynRecordFields :: PatSynArgs -> [Text];
patSynRecordFields args = case args of {
  PatSynRecordArgs fields -> fields;
  _ -> []};
patSynFieldTermKey :: TcTermKey -> Text -> TcM TcTermKey;
patSynFieldTermKey key field = case key of {
  TcTermGlobal package moduleName' _ -> pure (TcTermGlobal package moduleName' field);
  TcTermLocal {} -> abortTc ("record pattern synonym field " <> T.unpack field <> " is not a top-level binder")};
tcPatSynRecordSelectors :: PackageId -> Text -> Maybe SourceSpan -> PatSynLayout -> PatSynArgs -> Pattern -> [UnqualifiedName] -> TcM ([(Text, Match)], [TcBindingResult]);
tcPatSynRecordSelectors package moduleName' nameSpan layout args pat argBinders = do {
  checked <- sequence (zipWith3 selector (patSynRecordFields args) argBinders (patSynLayoutArgTypes layout));
  pure (concatMap fst checked, concatMap snd checked)} where {
  selector field argBinder argType | any (`typeMentionsTyVar` argType) (patSynLayoutExistentials layout) = do {
  emitError nameSpan (OtherError ("the field " <> T.unpack field <> " of a record pattern synonym has an existential type, so it has no selector"));
  pure ([], [])} | otherwise = do {
  let {
  key = TcTermGlobal package moduleName' field;
  scheme = specifiedScheme (patSynLayoutUniversals layout) (patSynLayoutRequired layout) (TcFunTy (patSynLayoutResultType layout) argType);
  sig = CheckedSig field scheme nameSpan [] False};
  registerCheckedSig key sig;
  (maybeMatches, results) <- tcFunctionWithSig key field sig [patSynSelectorMatch pat argBinder];
  commitCheckedHelper key results;
  case maybeMatches of {
  Just [match] -> pure ([(field, match)], results);
  _ -> pure ([], results)}}};
patSynSelectorMatch :: Pattern -> UnqualifiedName -> Match;
patSynSelectorMatch pat argBinder = Match {matchAnns = [], matchHeadForm = MatchHeadPrefix, matchPats = [PVar scrutinee], matchRhs = UnguardedRhs [] (ECase (localVar scrutinee) [CaseAlt [] pat (UnguardedRhs [] (localVar argBinder) Nothing)]) Nothing} where {
  scrutinee = synthesizedLocal (-1) "$scrutinee"};
patSynArgNames :: PatSynArgs -> [Text];
patSynArgNames args = case args of {
  PatSynPrefixArgs names -> names;
  PatSynInfixArgs left right -> [left, right];
  PatSynRecordArgs fields -> fields};
patSynBinderSpan :: UnqualifiedName -> Maybe SourceSpan;
patSynBinderSpan binder = unqualifiedNameSpan binder <|> case [resolutionSpan resolution | Just resolution <- map fromAnnotation (unqualifiedNameAnns binder)] of {
  sp : _ -> sp;
  [] -> Nothing};
patternVarBinder :: Text -> Pattern -> Maybe UnqualifiedName;
patternVarBinder target = go where {
  go pat = case pat of {
  PVar name | unqualifiedNameText name == target -> Just name | otherwise -> Nothing;
  PAs name inner | unqualifiedNameText name == target -> Just name | otherwise -> go inner;
  PAnn _ inner -> go inner;
  PParen inner -> go inner;
  PStrict inner -> go inner;
  PIrrefutable inner -> go inner;
  PTypeSig inner _ -> go inner;
  PView _ inner -> go inner;
  PUnboxedSum _ _ inner -> go inner;
  PList items -> firstJust items;
  PTuple _ items -> firstJust items;
  PCon _ _ items -> firstJust items;
  PBuiltinCon _ _ items -> firstJust items;
  PInfix left _ right -> firstJust [left, right];
  PRecord _ fields _ -> firstJust (map recordFieldValue fields);
  _ -> Nothing};
  firstJust = listToMaybe . mapMaybe go};
synthesizedLocal :: Int -> Text -> UnqualifiedName;
synthesizedLocal unique text = UnqualifiedName NameVarId text [mkAnnotation (ResolutionAnnotation Nothing (IdentifierNamed text) ResolutionNamespaceTerm (ResolvedLocal unique (mkUnqualifiedName NameVarId text)))];
localVar :: UnqualifiedName -> Expr;
localVar = EVar . qualifyName Nothing;
patSynMatcherMatch :: Pattern -> [UnqualifiedName] -> Match;
patSynMatcherMatch pat argBinders = Match {matchAnns = [], matchHeadForm = MatchHeadPrefix, matchPats = map PVar [scrutinee, continue, failure], matchRhs = UnguardedRhs [] (ECase (localVar scrutinee) [CaseAlt [] pat (UnguardedRhs [] success Nothing), CaseAlt [] PWildcard (UnguardedRhs [] failed Nothing)]) Nothing} where {
  scrutinee = synthesizedLocal (-1) "$scrutinee";
  continue = synthesizedLocal (-2) "$continue";
  failure = synthesizedLocal (-3) "$failure";
  success = foldl EApp (localVar continue) (if null argBinders then [unit] else map localVar argBinders);
  failed = EApp (localVar failure) unit;
  unit = ETuple Boxed []};
matcherPattern :: Match -> Maybe Pattern;
matcherPattern match = case matchRhs match of {
  UnguardedRhs _ expr _ -> go expr;
  GuardedRhss {} -> Nothing} where {
  go expr = case expr of {
  EAnn _ inner -> go inner;
  EParen inner -> go inner;
  EPragma _ inner -> go inner;
  ECase _ (alt : _) -> Just (caseAltPattern alt);
  _ -> Nothing}};
patternToExpr :: Pattern -> Maybe Expr;
patternToExpr pat | Just expr <- literalPatternToExpr pat = Just expr;
patternToExpr pat = case pat of {
  PAnn ann inner -> EAnn ann <$> patternToExpr inner;
  PVar name -> Just (localVar name);
  PLit literal -> Just (literalToExpr literal);
  PTuple flavor items -> ETuple flavor <$> mapM (fmap Just . patternToExpr) items;
  PList items -> EList <$> mapM patternToExpr items;
  PCon name _ items -> foldl EApp (EVar name) <$> mapM patternToExpr items;
  PBuiltinCon (BuiltinTuple flavor arity) _ items | length items == arity -> ETuple flavor <$> mapM (fmap Just . patternToExpr) items;
  PInfix left name right -> EApp . EApp (EVar name) <$> patternToExpr left <*> patternToExpr right;
  PParen inner -> EParen <$> patternToExpr inner;
  PStrict inner -> patternToExpr inner;
  PTypeSig inner ty -> (`ETypeSig` ty) <$> patternToExpr inner;
  _ -> Nothing};
literalPatternToExpr :: Pattern -> Maybe Expr;
literalPatternToExpr = go [] where {
  go anns pattern' = case pattern' of {
  PAnn ann inner | isMatchOnlyAnnotation ann -> go anns inner | otherwise -> go (ann : anns) inner;
  PLit literal -> let {
  (literalAnns, bare) = peelLiteralAnns literal} in Just (foldl (flip EAnn) (literalToExpr bare) (anns <> literalAnns));
  _ -> Nothing}};
isMatchOnlyAnnotation :: Annotation -> Bool;
isMatchOnlyAnnotation ann = case fromAnnotation @ResolutionAnnotation ann of {
  Just resolution -> resolutionNamespace resolution == ResolutionNamespaceTerm && resolutionIdentifier resolution == IdentifierNamed "==";
  Nothing -> False};
peelLiteralAnns :: Literal -> ([Annotation], Literal);
peelLiteralAnns literal = case literal of {
  LitAnn ann inner -> let {
  (anns, bare) = peelLiteralAnns inner} in (anns <> [ann], bare);
  _ -> ([], literal)};
literalToExpr :: Literal -> Expr;
literalToExpr literal = case literal of {
  LitAnn ann inner -> EAnn ann (literalToExpr inner);
  LitInt value numericType source -> EInt value numericType source;
  LitFloat value floatType source -> EFloat value floatType source;
  LitChar value source -> EChar value source;
  LitCharHash value source -> ECharHash value source;
  LitString value source -> EString value source;
  LitStringHash value source -> EStringHash value source};
replacePatSynDecl :: PatSynDecl -> Decl -> Decl;
replacePatSynDecl patSyn decl = case decl of {
  DeclAnn ann inner -> DeclAnn ann (replacePatSynDecl patSyn inner);
  DeclPatSyn {} -> DeclPatSyn patSyn;
  _ -> decl};
tcMergedFunctionGroup :: Map TcTermKey CheckedSig -> Int -> UnqualifiedName -> [Decl] -> [Match] -> TcM TcDeclGroupResult;
tcMergedFunctionGroup sigs groupId binder decls matches = do {
  let {
  displayName = renderBinderName binder};
  key <- resolvedUnqualifiedTermKey binder;
  (maybeMatches, bindings) <- case Map.lookup key sigs of {
  Just sig -> tcTopLevelWithSig key displayName sig matches;
  Nothing -> do {
  tcFunctionInfer key displayName matches}};
  let {
  annotatedDecls = fmap (`replaceFunctionDeclMatches` decls) maybeMatches};
  pure (TcDeclGroupResult groupId bindings annotatedDecls)};
tcTopLevelWithSig :: TcTermKey -> Text -> CheckedSig -> [Match] -> TcM (Maybe [Match], [TcBindingResult]);
tcTopLevelWithSig key displayName sig matches = do {
  (maybeMatches, bindings) <- tcFunctionWithSig key displayName sig matches;
  bindings' <- if checkedSigPartial sig then mapM (generalizePartialSigBinding key) bindings else pure bindings;
  pure (maybeMatches, bindings')};
generalizePartialSigBinding :: TcTermKey -> TcBindingResult -> TcM TcBindingResult;
generalizePartialSigBinding key (TcBindingResult resultKey displayName ty) = do {
  let {
  ForAll sigTyVars sigPreds body = typeSchemeFromType ty};
  generalized <- generalizeAndCommitIgnoring (Set.singleton key) body sigPreds;
  let {
  ForAll extraTyVars preds body' = generalized};
  let {
  scheme = Scheme extraTyVars sigTyVars preds body';
  binder = TcIdBinder scheme Closed};
  replaceTermKeyEnvPermanent key binder;
  zonkedTy <- zonkType (schemeToType scheme);
  pure (TcBindingResult resultKey displayName zonkedTy)};
tcFunctionWithSig :: TcTermKey -> Text -> CheckedSig -> [Match] -> TcM (Maybe [Match], [TcBindingResult]);
tcFunctionWithSig key displayName sig matches = do {
  let {
  scheme = checkedSigScheme sig};
  ((skolems, sigPreds, sigTy, matches'), failed) <- withErrorTracking $ do {
  (skolems, sigPreds, sigTy) <- skolemizeQualified scheme;
  let {
  nArgs = case matches of {
  (m : _) -> length (matchPats m);
  [] -> 0};
  (argTys, resTy) = splitFunTy sigTy nArgs};
  results <- withGivenPredicates sigPreds $ withScopedTyVars (scopedSigTyVars (checkedSigScopedNames sig) skolems) $ mapM (tcMatchEquation (Just (TypeSignatureOrigin (checkedSigName sig) (checkedSigSpan sig))) argTys resTy) matches;
  let {
  (_matches', ctsList, implsList) = unzip3 results;
  allCts = concat ctsList;
  allImpls = concat implsList};
  solveBodyConstraintsWithGivens sigPreds allCts allImpls;
  rejectEscapingExistentials sigTy allImpls;
  pure (skolems, sigPreds, sigTy, _matches')};
  if failed then pure (Nothing, []) else do {
  let {
  qualifiedTy | null sigPreds = sigTy | otherwise = TcQualTy sigPreds sigTy;
  checkedTy = foldr TcForAllTy qualifiedTy skolems};
  zonkedTy <- zonkType checkedTy;
  pure (Just matches', [TcBindingResult key displayName zonkedTy])}};
tcFunctionInfer :: TcTermKey -> Text -> [Match] -> TcM (Maybe [Match], [TcBindingResult]);
tcFunctionInfer key displayName matches = do {
  placeholderTy <- freshMetaTv;
  ((matches', ty, residualPreds), failed) <- withErrorTracking $ do {
  extendTermKeyEnvPermanent key (TcMonoIdBinder placeholderTy);
  (matches', ty, cts', impls') <- tcMatches matches;
  solveResult <- solveWithImpls cts' impls';
  rejectEscapingExistentials ty impls';
  residualPreds <- generalizableResidualPreds ty solveResult;
  pure (matches', ty, residualPreds)};
  if failed then pure (Nothing, []) else do {
  scheme <- generalizeAndCommitIgnoring (Set.singleton key) ty residualPreds;
  let {
  schemeTy = schemeToType scheme};
  zonkedTy <- zonkType schemeTy;
  finalizeInferredTermEnvPermanent key placeholderTy scheme;
  pure (Just matches', [TcBindingResult key displayName zonkedTy])}};
generalizableResidualPreds :: TcType -> SolveResult -> TcM [Pred];
generalizableResidualPreds inferredType solveResult = do {
  initialCts <- mapM zonkCtPred (srResidual solveResult <> inertDicts (srInerts solveResult));
  keep <- generalizedMetaVars inferredType;
  defaulted <- defaultAmbiguousMetas keep initialCts;
  allResidualCts <- mapM zonkCtPred initialCts;
  let {
  (callStackCts, residualCts) = partition (isCallStackPred . ctPred) allResidualCts};
  mapM_ reportUnsolvedDict callStackCts;
  let {
  uniqueResidualCts = nubBy sameCtPred residualCts;
  (polymorphicCts, defaultedCts) = partition (predicateCanGeneralize . ctPred) uniqueResidualCts};
  concreteCts <- if defaulted then concat <$> mapM attemptDefaultedCt defaultedCts else pure defaultedCts;
  forM_ residualCts $ \ct -> when (predicateCanGeneralize (ctPred ct)) $ bindEvidence (ctEvVar ct) (EvGiven (ctPred ct));
  forM_ concreteCts $ \ct -> emitError (ctLoc ct) (UnsolvedWanted (ctPred ct) (ctOrigin ct));
  pure (map ctPred polymorphicCts)} where {
  zonkCtPred ct = do {
  pred' <- zonkPred (ctPred ct);
  pure (ct {ctPred = pred'})};
  sameCtPred left right = ctPred left == ctPred right;
  attemptDefaultedCt ct = case ctPred ct of {
  ClassPred {} -> do {
  result <- solveDict ct;
  case result of {
  DictSolved -> pure [];
  DictStuck stuck -> pure [stuck]}};
  _ -> pure [ct]}};
generalizedMetaVars :: TcType -> TcM [Unique];
generalizedMetaVars inferredType = do {
  zonked <- zonkType inferredType;
  envMetaVars <- environmentMetaVars Set.empty;
  pure (collectMetaVars zonked <> envMetaVars)};
predicateCanGeneralize :: Pred -> Bool;
predicateCanGeneralize predicate = case predicate of {
  IParamPred {} -> True;
  EqPred {} -> False;
  _ -> not (null (predMetaVars predicate))};
rejectEscapingExistentials :: TcType -> [Implication] -> TcM ();
rejectEscapingExistentials outerType implications = do {
  zonkedOuterType <- zonkType outerType;
  let {
  skolems = concatMap implSkols implications;
  escaping = filter (`typeMentionsTyVar` zonkedOuterType) skolems};
  unless (null escaping) $ emitError Nothing (OtherError ("existential type variable escapes its pattern-match branch: " <> T.unpack (T.intercalate ", " (map tvName escaping))))};
zonkPred :: Pred -> TcM Pred;
zonkPred pred' = case pred' of {
  ClassPred className args -> ClassPred className <$> mapM zonkType args;
  EqPred left right -> EqPred <$> zonkType left <*> zonkType right;
  IParamPred name payload -> IParamPred name <$> zonkType payload;
  IrredPred constraint -> IrredPred <$> zonkType constraint;
  QuantifiedPred variables antecedents consequent -> QuantifiedPred <$> mapM defaultTyVarKinds variables <*> mapM zonkPred antecedents <*> zonkPred consequent};
collectStandaloneKindSignatures :: [Decl] -> Map TcTypeKey Type;
collectStandaloneKindSignatures = Map.fromList . mapMaybe collect where {
  collect declaration = case declaration of {
  DeclAnn _ inner -> collect inner;
  DeclStandaloneKindSig name kind -> (,kind) <$> resolvedTypeKey name;
  _ -> Nothing}};
resolvedTypeKey :: UnqualifiedName -> Maybe TcTypeKey;
resolvedTypeKey name = do {
  ResolutionAnnotation {resolutionNamespace = namespace, resolutionTarget = ResolvedTopLevel packageId moduleName' resolvedName} <- nameResolution name;
  pure (TcTypeKey (nameText resolvedName) packageId moduleName' namespace)};
registerTypeDeclHeader :: Map TcTypeKey TypeScheme -> Decl -> TcM ();
registerTypeDeclHeader kindSchemes (DeclData dataDecl) = registerDataDeclHeader (resolvedTypeKey (binderHeadName (dataDeclHead dataDecl)) >>= (`Map.lookup` kindSchemes)) dataDecl;
registerTypeDeclHeader kindSchemes (DeclNewtype newtypeDecl) = registerNewtypeDeclHeader (resolvedTypeKey (binderHeadName (newtypeDeclHead newtypeDecl)) >>= (`Map.lookup` kindSchemes)) newtypeDecl;
registerTypeDeclHeader kindSchemes (DeclDataFamilyDecl familyDecl) = registerDataFamilyDeclHeader (resolvedTypeKey (binderHeadName (dataFamilyDeclHead familyDecl)) >>= (`Map.lookup` kindSchemes)) familyDecl;
registerTypeDeclHeader kindSchemes (DeclTypeFamilyDecl familyDecl) = registerTypeFamilyDeclHeader (typeFamilyHeadName (typeFamilyDeclHead familyDecl) >>= resolvedTypeKey >>= (`Map.lookup` kindSchemes)) familyDecl;
registerTypeDeclHeader kindSchemes (DeclTypeSyn typeSynDecl) = registerTypeSynonymHeader (resolvedTypeKey (binderHeadName (typeSynHead typeSynDecl)) >>= (`Map.lookup` kindSchemes)) typeSynDecl;
registerTypeDeclHeader kindSchemes (DeclClass classDecl) = do {
  let {
  classHead = classDeclHead classDecl;
  classBinder = binderHeadName classHead;
  classParamNames = map tyVarBinderName (binderHeadParams classHead)};
  classTyCon <- mkDeclaredTyCon classBinder (unqualifiedNameText classBinder) (length classParamNames);
  predeclared <- lookupTyConByIdentity classTyCon;
  let {
  classParamKinds = maybe [] (takeVisibleArgumentKinds (length classParamNames) . typeSchemeBody . tciKindScheme) predeclared;
  sharedKinds = Map.fromList (zip classParamNames classParamKinds)};
  case resolvedTypeKey classBinder >>= (`Map.lookup` kindSchemes) of {
  Just scheme -> do {
  (declaredKind, _) <- instantiate scheme;
  zipWithM_ unifyKinds classParamKinds (takeVisibleArgumentKinds (length classParamNames) declaredKind)};
  Nothing -> pure ()};
  mapM_ (\familyDecl -> registerTypeFamilyDeclHeaderWith sharedKinds (typeFamilyHeadName (typeFamilyDeclHead familyDecl) >>= resolvedTypeKey >>= (`Map.lookup` kindSchemes)) familyDecl) (classDeclTypeFamilies classDecl)};
registerTypeDeclHeader kindSchemes (DeclAnn _ inner) = registerTypeDeclHeader kindSchemes inner;
registerTypeDeclHeader _ _ = pure ();
predeclareTypeConstructor :: Decl -> TcM ();
predeclareTypeConstructor declaration = case declaration of {
  DeclAnn _ inner -> predeclareTypeConstructor inner;
  DeclData dataDeclaration -> let {
  binder = binderHeadName (dataDeclHead dataDeclaration);
  name = unqualifiedNameText binder} in predeclare binder name (length (binderHeadParams (dataDeclHead dataDeclaration))) DataTyCon;
  DeclNewtype newtypeDeclaration -> let {
  binder = binderHeadName (newtypeDeclHead newtypeDeclaration)} in predeclare binder (unqualifiedNameText binder) (length (binderHeadParams (newtypeDeclHead newtypeDeclaration))) NewtypeTyCon;
  DeclTypeSyn synonymDeclaration -> let {
  binder = binderHeadName (typeSynHead synonymDeclaration)} in predeclare binder (unqualifiedNameText binder) (length (binderHeadParams (typeSynHead synonymDeclaration))) SynonymTyCon;
  DeclDataFamilyDecl familyDeclaration -> let {
  binder = binderHeadName (dataFamilyDeclHead familyDeclaration)} in predeclare binder (unqualifiedNameText binder) (length (binderHeadParams (dataFamilyDeclHead familyDeclaration))) DataFamilyTyCon;
  DeclClass classDeclaration -> do {
  let {
  binder = binderHeadName (classDeclHead classDeclaration)};
  predeclare binder (unqualifiedNameText binder) (length (binderHeadParams (classDeclHead classDeclaration))) ClassTyCon;
  mapM_ (predeclareTypeConstructor . DeclTypeFamilyDecl) (classDeclTypeFamilies classDeclaration)};
  DeclTypeFamilyDecl familyDeclaration -> case typeFamilyHeadName (typeFamilyDeclHead familyDeclaration) of {
  Just binder -> predeclare binder (unqualifiedNameText binder) (length (typeFamilyDeclParams familyDeclaration)) TypeFamilyTyCon;
  Nothing -> pure ()};
  _ -> pure ()} where {
  predeclare binder name arity flavor = do {
  kinds <- getKinds;
  provisionalKind <- case flavor of {
  ClassTyCon -> foldr KFun (constraintKind kinds) <$> replicateM arity freshKindMeta;
  _ -> freshKindMeta};
  tyCon <- mkDeclaredTyCon binder name arity;
  storeTyConInfo TyConInfo {tciName = name, tciArity = arity, tciTyCon = tyCon, tciKindScheme = Scheme [] [] [] provisionalKind, tciFlavor = flavor, tciTypeSynonym = Nothing, tciInjectivity = Nothing}}};
storeTyConInfo :: TyConInfo -> TcM ();
storeTyConInfo info = do {
  existing <- lookupTyConByIdentity (tciTyCon info);
  case existing of {
  Just provisional -> do {
  unifyKinds (typeSchemeBody (tciKindScheme provisional)) (typeSchemeBody (tciKindScheme info));
  replaceTyConEnvPermanent info};
  Nothing -> extendTyConEnvPermanent info}};
registerStructuralDecl :: (Text, Text) -> Decl -> TcM [TcBindingResult];
registerStructuralDecl origin (DeclData dataDecl) = registerDataConstructors origin dataDecl;
registerStructuralDecl origin (DeclNewtype newtypeDecl) = registerNewtypeConstructor origin newtypeDecl;
registerStructuralDecl origin (DeclDataFamilyInst familyInst) = registerDataFamilyInstance origin familyInst;
registerStructuralDecl origin (DeclTypeFamilyDecl familyDecl) = registerClosedTypeFamilyEquations origin familyDecl;
registerStructuralDecl origin (DeclTypeFamilyInst familyInst) = registerTypeFamilyInstance origin familyInst;
registerStructuralDecl origin (DeclClass classDecl) = registerClassDecl origin classDecl;
registerStructuralDecl origin (DeclInstance instanceDecl) = registerInstanceDecl origin instanceDecl;
registerStructuralDecl origin (DeclAnn _ inner) = registerStructuralDecl origin inner;
registerStructuralDecl _ _ = pure [];
isInstanceDecl :: Decl -> Bool;
isInstanceDecl (DeclAnn _ inner) = isInstanceDecl inner;
isInstanceDecl DeclInstance {} = True;
isInstanceDecl _ = False;
predeclareTypeLevelDataConstructors :: Decl -> TcM ();
predeclareTypeLevelDataConstructors declaration = case declaration of {
  DeclAnn _ inner -> predeclareTypeLevelDataConstructors inner;
  DeclData dataDeclaration -> do {
  let {
  parentBinder = binderHeadName (dataDeclHead dataDeclaration);
  parentName = unqualifiedNameText parentBinder;
  parentArity = length (binderHeadParams (dataDeclHead dataDeclaration))};
  parent <- mkDeclaredTyCon parentBinder parentName parentArity;
  mapM_ (predeclareConstructor parent) (dataDeclConstructors dataDeclaration)};
  DeclNewtype newtypeDeclaration -> do {
  let {
  parentBinder = binderHeadName (newtypeDeclHead newtypeDeclaration);
  parentName = unqualifiedNameText parentBinder;
  parentArity = length (binderHeadParams (newtypeDeclHead newtypeDeclaration))};
  parent <- mkDeclaredTyCon parentBinder parentName parentArity;
  maybe (pure ()) (predeclareConstructor parent) (newtypeDeclConstructor newtypeDeclaration)};
  _ -> pure ()} where {
  predeclareConstructor parent constructor = do {
  let {
  (_, fields, _) = dataConSourceLayout constructor;
  arity = length fields};
  names <- dataConNames constructor;
  mapM_ (predeclareName parent arity) names};
  predeclareName parent arity name = do {
  let {
  dataConTyCon = mkTyConWithNamespace ResolutionNamespaceTerm (tyConPackageId parent) (tyConModuleName parent) name arity};
  kindScheme <- Scheme [] [] [] <$> freshKindMeta;
  storeTyConInfo TyConInfo {tciName = name, tciArity = arity, tciTyCon = dataConTyCon, tciKindScheme = kindScheme, tciFlavor = DataTyCon, tciTypeSynonym = Nothing, tciInjectivity = Nothing}}};
isForeignImport :: ForeignDecl -> Bool;
isForeignImport foreignDecl = foreignDirection foreignDecl == ForeignImport;
checkClassFunDep :: Text -> [Text] -> FunctionalDependency -> TcM (Maybe FunDep);
checkClassFunDep className paramNames dependency = do {
  forM_ unknown (emitError loc . FunDepUnknownTyVar className);
  pure (classFunDep paramNames dependency)} where {
  loc = sourceSpanFromAnns (functionalDependencyAnns dependency);
  unknown = filter (`notElem` paramNames) (functionalDependencyDeterminers dependency <> functionalDependencyDetermined dependency)};
classFunDep :: [Text] -> FunctionalDependency -> Maybe FunDep;
classFunDep paramNames dependency = FunDep <$> traverse position (functionalDependencyDeterminers dependency) <*> traverse position (functionalDependencyDetermined dependency) where {
  position name = elemIndex name paramNames};
registerClassDecl :: (Text, Text) -> ClassDecl -> TcM [TcBindingResult];
registerClassDecl origin classDecl = do {
  let {
  classBinder = binderHeadName (classDeclHead classDecl);
  className = unqualifiedNameText classBinder;
  params = binderHeadParams (classDeclHead classDecl)};
  poly <- isImplicitlyKindPolymorphicClass origin className;
  kindParams <- if poly then implicitBinderKindParams params else pure [];
  let {
  kindEnv = Map.fromList [(paramName param, (paramTyVar param, paramKind param)) | param <- kindParams]};
  paramInfos <- makeParamEnvWith kindEnv params;
  let {
  paramTyVars = map paramTyVar paramInfos;
  allClassTyVars = map paramTyVar kindParams <> paramTyVars;
  paramKinds = map paramKind paramInfos;
  paramTvEnv = kindEnv <> Map.fromList [(paramName param, (paramTyVar param, paramKind param)) | param <- paramInfos]};
  kinds <- getKinds;
  superClassTypes <- mapM (\ty -> checkSurfaceType paramTvEnv ty (constraintKind kinds)) (fromMaybe [] (classDeclContext classDecl));
  let {
  classKind = foldr KFun (constraintKind kinds) paramKinds};
  classTyCon <- mkDeclaredTyCon classBinder className (length params);
  let {
  classPred = ClassPred classTyCon (map TcTyVar paramTyVars)};
  storeTyConInfo TyConInfo {tciName = className, tciArity = length params, tciTyCon = classTyCon, tciKindScheme = specifiedScheme (map paramTyVar kindParams) [] classKind, tciFlavor = ClassTyCon, tciTypeSynonym = Nothing, tciInjectivity = Nothing};
  methodResults <- concat <$> mapM (registerClassItem classPred paramTvEnv allClassTyVars) (classDeclItems classDecl);
  methods <- mapM (registeredMethod classTyCon) (classDeclMethodNames classDecl);
  defaultSignatures <- catMaybes <$> mapM (registerClassDefaultSignature paramTvEnv allClassTyVars) (classDeclItems classDecl);
  let {
  defaults = classDeclDefaultMethodNames classDecl};
  defaultResults <- mapM (registerDefaultMethod classTyCon defaults defaultSignatures) methods;
  associatedTypes <- catMaybes <$> mapM (registerAssociatedTypeFamily origin (map tyVarBinderName params) (classDeclTypeFamilyDefaults classDecl)) (classDeclTypeFamilies classDecl);
  funDeps <- catMaybes <$> mapM (checkClassFunDep className (map tyVarBinderName params)) (classDeclFundeps classDecl);
  addClass ClassInfo {ciName = className, ciTyCon = classTyCon, ciOrigin = Just origin, ciKindTyVars = map paramTyVar kindParams, ciTyVars = paramTyVars, ciSuperClassTypes = superClassTypes, ciMethods = methods, ciDefaultMethods = defaults, ciDefaultSignatures = defaultSignatures, ciAssociatedTypes = associatedTypes, ciFunDeps = funDeps};
  pure (methodResults <> catMaybes defaultResults)} where {
  registeredMethod classTyCon methodName = do {
  binder <- lookupTermKey (tyConMemberTermKey classTyCon methodName);
  case binder of {
  Just (TcIdBinder scheme _) -> pure (methodName, scheme);
  _ -> missingTypeInfo ("class method " <> T.unpack methodName)}};
  registerDefaultMethod classTyCon defaults defaultSignatures (methodName, scheme) | methodName `elem` defaults = do {
  let {
  workerName = defaultMethodName methodName;
  workerScheme = maybe scheme (defaultMethodWorkerScheme scheme) (lookup methodName defaultSignatures);
  workerType = schemeToType workerScheme};
  extendTyConTermEnvPermanent classTyCon workerName (TcIdBinder workerScheme Closed);
  pure (Just (TcBindingResult (tyConMemberTermKey classTyCon workerName) workerName workerType))} | otherwise = pure Nothing};
classDeclTypeFamilies :: ClassDecl -> [TypeFamilyDecl];
classDeclTypeFamilies classDecl = mapMaybe familyItem (classDeclItems classDecl) where {
  familyItem item = case peelClassDeclItemAnn item of {
  ClassItemTypeFamilyDecl familyDecl -> Just familyDecl;
  _ -> Nothing}};
classDeclTypeFamilyDefaults :: ClassDecl -> [TypeFamilyInst];
classDeclTypeFamilyDefaults classDecl = mapMaybe defaultItem (classDeclItems classDecl) where {
  defaultItem item = case peelClassDeclItemAnn item of {
  ClassItemDefaultTypeInst familyInst -> Just familyInst;
  _ -> Nothing}};
instanceDeclTypeFamilyInsts :: InstanceDecl -> [TypeFamilyInst];
instanceDeclTypeFamilyInsts instanceDecl = mapMaybe familyItem (instanceDeclItems instanceDecl) where {
  familyItem item = case item of {
  InstanceItemAnn _ inner -> familyItem inner;
  InstanceItemTypeFamilyInst familyInst -> Just familyInst;
  _ -> Nothing}};
typeFamilyInstName :: TypeFamilyInst -> Maybe Text;
typeFamilyInstName familyInst = unqualifiedNameText <$> typeFamilyHeadName (typeFamilyInstLhs familyInst);
registerAssociatedTypeFamily :: (Text, Text) -> [Text] -> [TypeFamilyInst] -> TypeFamilyDecl -> TcM (Maybe AssociatedTypeInfo);
registerAssociatedTypeFamily origin classParamNames defaults familyDecl = case typeFamilyHeadName (typeFamilyDeclHead familyDecl) of {
  Nothing -> pure Nothing;
  Just familyBinder -> do {
  let {
  familyName = unqualifiedNameText familyBinder;
  params = typeFamilyDeclParams familyDecl;
  familyDefaults = [familyInst | familyInst <- defaults, typeFamilyInstName familyInst == Just familyName]};
  familyTyCon <- mkDeclaredTyCon familyBinder familyName (length params);
  defaultEquation <- case familyDefaults of {
  [] -> pure Nothing;
  [familyInst] -> checkTypeFamilyEquation origin False (typeFamilyInstForall familyInst) (typeFamilyInstEquation familyInst);
  _ -> do {
  emitError Nothing (OtherError ("more than one default equation for associated type " <> T.unpack familyName));
  pure Nothing}};
  pure (Just AssociatedTypeInfo {atiTyCon = familyTyCon, atiClassParams = [elemIndex (tyVarBinderName param) classParamNames | param <- params], atiDefault = defaultEquation})}};
registerInstanceAssociatedTypes :: (Text, Text) -> ClassInfo -> [TyVarId] -> [TcType] -> InstanceDecl -> TcM ();
registerInstanceAssociatedTypes origin classInfo instanceTyVars headTys instanceDecl = do {
  let {
  explicit = instanceDeclTypeFamilyInsts instanceDecl;
  explicitNames = mapMaybe typeFamilyInstName explicit};
  mapM_ (registerTypeFamilyInstance origin) explicit;
  mapM_ (\(info, defaultEquation) -> instantiateAssociatedDefault origin instanceTyVars headTys info defaultEquation >>= addTypeFamilyInstance) [(info, defaultEquation) | info <- ciAssociatedTypes classInfo, tyConName (atiTyCon info) `notElem` explicitNames, Just defaultEquation <- [atiDefault info]]};
instantiateAssociatedDefault :: (Text, Text) -> [TyVarId] -> [TcType] -> AssociatedTypeInfo -> TypeFamilyInstanceInfo -> TcM TypeFamilyInstanceInfo;
instantiateAssociatedDefault (packageName, moduleName') instanceTyVars headTys info defaultEquation = do {
  kinds <- getKinds;
  args <- mapM argumentType (atiClassParams info);
  let {
  substitution = Map.fromList [(tvUnique tyVar, arg) | (TcTyVar tyVar, arg) <- zip (typeArguments (tfiiLeft defaultEquation)) args];
  freshTyVars = [tyVar | TcTyVar tyVar <- args, tyVar `notElem` instanceTyVars]};
  pure TypeFamilyInstanceInfo {tfiiFamilyName = tyConName (atiTyCon info), tfiiAxiomName = associatedDefaultAxiomName kinds info headTys, tfiiOrigin = (PackageId packageName, moduleName'), tfiiTyVars = instanceTyVars <> freshTyVars, tfiiLeft = TcTyCon (atiTyCon info) args, tfiiRight = applySubst substitution (tfiiRight defaultEquation), tfiiClosed = False}} where {
  argumentType maybeIndex = case associatedClassArgument headTys maybeIndex of {
  Just ty -> pure ty;
  Nothing -> do {
  rawTyVar <- freshSkolemTv "a";
  kind <- freshKindMeta;
  pure (TcTyVar (setTyVarKind kind rawTyVar))}}};
associatedClassArgument :: [TcType] -> Maybe Int -> Maybe TcType;
associatedClassArgument headTys maybeIndex = maybeIndex >>= \index -> listToMaybe (drop index headTys);
associatedDefaultAxiomName :: TcKinds -> AssociatedTypeInfo -> [TcType] -> Text;
associatedDefaultAxiomName kinds info headTys = "$ax$" <> tyConName (atiTyCon info) <> T.concat ["$" <> maybe "a" (typeSuffix kinds) (associatedClassArgument headTys maybeIndex) | maybeIndex <- atiClassParams info];
typeArguments :: TcType -> [TcType];
typeArguments ty = case ty of {
  TcTyCon _ args -> args;
  TcAppTy function argument -> typeArguments function <> [argument];
  _ -> []};
isImplicitlyKindPolymorphicClass :: (Text, Text) -> Text -> TcM Bool;
isImplicitlyKindPolymorphicClass (_, moduleName') className = do {
  wiring <- getWiring;
  let {
  equality = tcWiringEqualityTyCon wiring};
  pure ((moduleName', className) == tcWiringLiftClass wiring || (moduleName', className) `elem` [("Type.Reflection", "Typeable"), ("Type.Reflection.Internal", "Typeable")] || (moduleName' == tyConModuleName equality && className `elem` [tyConName equality, "~~"]))};
registerClassItem :: Pred -> TvKindEnv -> [TyVarId] -> ClassDeclItem -> TcM [TcBindingResult];
registerClassItem classPred classTvEnv classTyVars item = case peelClassDeclItemAnn item of {
  ClassItemTypeSig names ty -> do {
  Scheme inferred specified contextPreds methodBody <- classSignatureScheme classTvEnv classTyVars ty;
  let {
  preds = classPred : contextPreds;
  scheme = Scheme inferred specified preds methodBody;
  declaredTy = schemeToType scheme};
  mapM (\methodName -> do {
  let {
  displayName = renderBinderName methodName};
  methodKey <- resolvedUnqualifiedTermKey methodName;
  extendResolvedTermEnvPermanent methodName (TcIdBinder scheme Closed);
  zonkedTy <- zonkType declaredTy;
  pure (TcBindingResult methodKey displayName zonkedTy)}) names};
  _ -> pure []};
registerClassDefaultSignature :: TvKindEnv -> [TyVarId] -> ClassDeclItem -> TcM (Maybe (Text, TypeScheme));
registerClassDefaultSignature classTvEnv classTyVars item = case peelClassDeclItemAnn item of {
  ClassItemDefaultSig methodName ty -> do {
  scheme <- classSignatureScheme classTvEnv classTyVars ty;
  pure (Just (unqualifiedNameText methodName, scheme))};
  _ -> pure Nothing};
classSignatureScheme :: TvKindEnv -> [TyVarId] -> Type -> TcM TypeScheme;
classSignatureScheme classTvEnv classTyVars ty = do {
  let {
  (explicitBinders, context, body) = splitSigma ty;
  classVarNames = Map.keys classTvEnv;
  freeVars = freeTypeVars ty \\ classVarNames};
  rawExtraTyVars <- mapM freshSkolemTv freeVars;
  extraKinds <- mapM (const freshKindMeta) freeVars;
  let {
  extraTyVars = zipWith setTyVarKind extraKinds rawExtraTyVars;
  implicitEnv = classTvEnv <> Map.fromList (zip freeVars (zip extraTyVars extraKinds))};
  explicitParams <- makeParamEnvWith implicitEnv explicitBinders;
  let {
  explicitTyVars = map paramTyVar explicitParams;
  tvEnv = implicitEnv <> Map.fromList [(paramName param, (paramTyVar param, paramKind param)) | param <- explicitParams]};
  kinds <- getKinds;
  methodBody <- checkSurfaceType tvEnv body (typeKind kinds);
  contextPreds <- mapM (surfacePredToPred tvEnv) (filter (not . isEmptyContext) context);
  pure (specifiedScheme (classTyVars <> extraTyVars <> explicitTyVars) contextPreds methodBody)};
registerInstanceDecl :: (Text, Text) -> InstanceDecl -> TcM [TcBindingResult];
registerInstanceDecl origin instanceDecl = case instanceHeadName (instanceDeclHead instanceDecl) of {
  Nothing -> pure [];
  Just className -> do {
  let {
  headArgs = instanceHeadTypes (instanceDeclHead instanceDecl)};
  (rawTvIds, tvEnv) <- makeInstanceTyVarEnv instanceDecl headArgs;
  let {
  classNameText = nameText className};
  headTys <- checkInstanceHeadTypes className tvEnv headArgs;
  dictName <- allocateInstanceDictName origin classNameText headTys;
  context <- mapM (surfacePredToPred tvEnv) (instanceDeclContext instanceDecl);
  tvIds <- resolveInstanceTyVars origin rawTvIds;
  classInfo <- lookupClassNamed className >>= maybe (missingTypeInfo ("class " <> T.unpack classNameText)) pure;
  registerInstanceAssociatedTypes origin classInfo tvIds headTys instanceDecl;
  checkInstanceFunDeps (sourceSpanFromAnns (nameAnns className)) classInfo tvIds headTys context;
  let {
  dictTy = foldr TcForAllTy (TcQualTy context (TcTyCon (ciTyCon classInfo) headTys)) tvIds};
  addInstance InstanceInfo {iiClassName = classNameText, iiDictName = dictName, iiDictOrigin = origin, iiDictType = dictTy, iiTyVars = tvIds, iiContext = context, iiHead = headTys};
  pure [TcBindingResult (originTermKey origin dictName) dictName dictTy]}};
predType :: Pred -> TcM TcType;
predType (ClassPred classTyCon args) = pure (TcTyCon classTyCon args);
predType (EqPred left right) = do {
  kinds <- getKinds;
  equalityTyCon <- wiredTyCon tcWiringEqualityTyCon (KFun (typeKind kinds) (KFun (typeKind kinds) (constraintKind kinds)));
  pure (TcTyCon equalityTyCon [left, right])};
predType (IParamPred name payload) = implicitParamType name payload;
predType (IrredPred constraint) = pure constraint;
predType (QuantifiedPred variables antecedents consequent) = do {
  consequentType <- predType consequent;
  let {
  qualifiedType | null antecedents = consequentType | otherwise = TcQualTy antecedents consequentType};
  pure (foldr TcForAllTy qualifiedType variables)};
instanceDictName :: TcKinds -> Text -> [TcType] -> Text;
instanceDictName kinds className tys = "$f" <> className <> T.concat (map (typeSuffix kinds) tys);
typeSuffix :: TcKinds -> TcType -> Text;
typeSuffix kinds ty = case ty of {
  TcTyVar tv -> tvName tv;
  TcArrowTy -> tyConName (kindsArrowTyCon kinds);
  TcAppTy TcArrowTy argument -> tyConName (kindsArrowTyCon kinds) <> typeSuffix kinds argument;
  TcFunTy argument result -> tyConName (kindsArrowTyCon kinds) <> typeSuffix kinds argument <> typeSuffix kinds result;
  TcTyCon tc [] -> tyConName tc;
  TcTyCon tc [_] | tyConKey tc == tyConKey (kindsListTyCon kinds) -> tyConName (kindsListDeclaration kinds);
  TcTyCon tc args -> tyConName tc <> T.concat (map (typeSuffix kinds) args);
  _ -> "T"};
allocateInstanceDictName :: (Text, Text) -> Text -> [TcType] -> TcM Text;
allocateInstanceDictName origin className headTys = do {
  kinds <- getKinds;
  instances <- getInstances;
  let {
  taken = Set.fromList [iiDictName info | info <- instances, iiDictOrigin info == origin];
  shortName = instanceDictName kinds className headTys;
  modules = nub (mapMaybe typeConstructorModule headTys);
  qualifiedName = shortName <> T.concat (map ("$" <>) modules)};
  pure (if shortName `Set.notMember` taken then shortName else if qualifiedName `Set.notMember` taken && qualifiedName /= shortName then qualifiedName else shortName <> "$" <> T.pack (show (Set.size taken)))};
lookupInstanceDictName :: (Text, Text) -> TyCon -> [TcType] -> TcM Text;
lookupInstanceDictName origin classTyCon headTys = do {
  instances <- getInstances;
  let {
  matches info = iiDictOrigin info == origin && fmap tyConKey (instanceClassTyCon info) == Just (tyConKey classTyCon) && isJust (matchTypes (iiHead info) headTys) && isJust (matchTypes headTys (iiHead info))};
  case find matches instances of {
  Just info -> pure (iiDictName info);
  Nothing -> allocateInstanceDictName origin (tyConName classTyCon) headTys}};
typeConstructorModule :: TcType -> Maybe Text;
typeConstructorModule ty = case ty of {
  TcTyCon tyCon _ -> Just (tyConModuleName tyCon);
  _ -> Nothing};
registerDataFamilyDeclHeader :: Maybe TypeScheme -> DataFamilyDecl -> TcM ();
registerDataFamilyDeclHeader maybeKindScheme familyDecl = do {
  let {
  familyBinder = binderHeadName (dataFamilyDeclHead familyDecl);
  familyName = unqualifiedNameText familyBinder;
  params = binderHeadParams (dataFamilyDeclHead familyDecl);
  arity = length params};
  (kindParams, paramInfos) <- typeDeclParamInfos maybeKindScheme params;
  inferredKind <- tyConKindFromParams paramInfos (dataFamilyDeclKind familyDecl);
  familyTyCon <- mkDeclaredTyCon familyBinder familyName arity;
  let {
  declaredKind = maybe inferredKind typeSchemeBody maybeKindScheme};
  storeTyConInfo TyConInfo {tciName = familyName, tciArity = arity, tciTyCon = familyTyCon, tciKindScheme = specifiedScheme (map paramTyVar kindParams) [] declaredKind, tciFlavor = DataFamilyTyCon, tciTypeSynonym = Nothing, tciInjectivity = Nothing};
  void (defaultKindMetas declaredKind)};
registerDataFamilyInstance :: (Text, Text) -> DataFamilyInst -> TcM [TcBindingResult];
registerDataFamilyInstance (packageName, moduleName') familyInst = do {
  paramInfos <- dataFamilyInstanceParams familyInst;
  let {
  tvEnv = Map.fromList [(paramName param, (paramTyVar param, paramKind param)) | param <- paramInfos]};
  constructorNames <- concat <$> mapM dataConNames (dataFamilyInstConstructors familyInst);
  kinds <- getKinds;
  familyType <- checkSurfaceType tvEnv (dataFamilyInstHead familyInst) (typeKind kinds);
  case (familyType, constructorNames) of {
  (_, []) -> do {
  emitError Nothing (OtherError "data-family instances without constructors are not supported");
  pure []};
  (TcTyCon familyTyCon _, firstConstructor : _) -> do {
  maybeFamilyInfo <- lookupTyConByIdentity familyTyCon;
  case maybeFamilyInfo of {
  Just familyInfo | tciFlavor familyInfo == DataFamilyTyCon -> do {
  representationKind <- tyConKindFromParams paramInfos (dataFamilyInstKind familyInst);
  let {
  familyName = tciName familyInfo;
  representationName = dataFamilyRepresentationName familyName firstConstructor;
  representationTyCon = mkTyConWithOrigin (PackageId packageName) moduleName' representationName (length paramInfos);
  axiomName = dataFamilyAxiomName familyName firstConstructor;
  representationInfo = TyConInfo {tciName = representationName, tciArity = length paramInfos, tciTyCon = representationTyCon, tciKindScheme = Scheme [] [] [] representationKind, tciFlavor = DataTyCon, tciTypeSynonym = Nothing, tciInjectivity = Nothing}};
  extendTyConEnvPermanent representationInfo;
  bindings <- mapM (registerDataConWithResult paramInfos familyType) (dataFamilyInstConstructors familyInst);
  constructors <- concat <$> mapM (checkedDataConInfos representationTyCon) (dataFamilyInstConstructors familyInst);
  addDataFamilyInstance DataFamilyInstanceInfo {dfiiFamilyName = familyName, dfiiFamilyType = familyType, dfiiTyVars = map paramTyVar paramInfos, dfiiRepresentationTyCon = representationTyCon, dfiiAxiomName = axiomName, dfiiConstructorNames = constructorNames, dfiiConstructors = constructors, dfiiIsNewtype = dataFamilyInstIsNewtype familyInst};
  selectorBindings <- registerRecordSelectors (packageName, moduleName') constructors;
  pure (bindings <> selectorBindings)};
  _ -> do {
  emitError Nothing (OtherError ("data-family instance head does not name a data family: " <> T.unpack (tyConName familyTyCon)));
  pure []}}};
  _ -> do {
  emitError Nothing (OtherError ("invalid data-family instance head: " <> show familyType));
  pure []}}};
dataFamilyInstHeadTyCon :: DataFamilyInst -> TcM TyCon;
dataFamilyInstHeadTyCon familyInst = go (dataFamilyInstHead familyInst) where {
  go ty = case peelTypeHead ty of {
  TCon name _ -> resolvedHeadTyCon name;
  TInfix _ name _ _ -> resolvedHeadTyCon name;
  TApp function _ -> go function;
  TTypeApp function _ -> go function;
  head' -> abortTc ("data-family instance head does not name a data family: " <> show head')};
  resolvedHeadTyCon name = do {
  info <- lookupResolvedTyCon name;
  maybe (missingTypeInfo ("data family " <> T.unpack (nameText name))) (pure . tciTyCon) info}};
dataFamilyInstanceParams :: DataFamilyInst -> TcM [ParamInfo];
dataFamilyInstanceParams familyInst = do {
  explicitParams <- makeParamEnv (dataFamilyInstForall familyInst);
  let {
  explicitNames = map paramName explicitParams;
  implicitNames = freeTypeVars (dataFamilyInstHead familyInst) \\ explicitNames};
  implicitParams <- mapM makeImplicitParam implicitNames;
  pure (explicitParams <> implicitParams)} where {
  makeImplicitParam name = do {
  rawTyVar <- freshSkolemTv name;
  kind <- freshKindMeta;
  pure ParamInfo {paramName = name, paramTyVar = setTyVarKind kind rawTyVar, paramKind = kind}}};
typeFamilyHeadName :: Type -> Maybe UnqualifiedName;
typeFamilyHeadName ty = case peelTypeHead ty of {
  TCon name _ -> Just (unqualifiedFromResolvedName name);
  TInfix _ name _ _ -> Just (unqualifiedFromResolvedName name);
  TApp function _ -> typeFamilyHeadName function;
  TTypeApp function _ -> typeFamilyHeadName function;
  _ -> Nothing};
unqualifiedFromResolvedName :: Name -> UnqualifiedName;
unqualifiedFromResolvedName name = UnqualifiedName {unqualifiedNameType = nameType name, unqualifiedNameText = nameText name, unqualifiedNameAnns = nameAnns name};
sourceTypeFamilyAxiomName :: (Text, Text) -> Type -> Text;
sourceTypeFamilyAxiomName home ty = "$ax$" <> sourceTypeKey home ty;
sourceTypeKey :: (Text, Text) -> Type -> Text;
sourceTypeKey home ty = case peelTypeHead ty of {
  TCon name _ -> typeConKey home name;
  TVar name -> unqualifiedNameText name;
  TApp function argument -> sourceTypeKey home function <> "$" <> sourceTypeKey home argument;
  TTypeApp function argument -> sourceTypeKey home function <> "$" <> sourceTypeKey home argument;
  TInfix left name _ right -> sourceTypeKey home left <> "$" <> typeConKey home name <> "$" <> sourceTypeKey home right;
  TFun _ argument result -> "Fun$" <> sourceTypeKey home argument <> "$" <> sourceTypeKey home result;
  TList _ arguments -> arguments `keyedUnder` "List";
  TTuple flavor _ arguments -> arguments `keyedUnder` (tupleFlavorKey flavor <> intKey (length arguments));
  TUnboxedSum arguments -> arguments `keyedUnder` ("Sum" <> intKey (length arguments));
  TStar {} -> "Star";
  TKindSig inner _ -> sourceTypeKey home inner;
  _ -> "T"} where {
  arguments `keyedUnder` tag = T.concat (tag : [T.cons '$' (sourceTypeKey home argument) | argument <- arguments]);
  tupleFlavorKey flavor = case flavor of {
  Boxed -> "Tuple";
  Unboxed -> "UnboxedTuple"};
  intKey = T.pack . show};
typeConKey :: (Text, Text) -> Name -> Text;
typeConKey home name = case nameResolution (unqualifiedFromResolvedName name) of {
  Just ResolutionAnnotation {resolutionTarget = ResolvedTopLevel packageId definingModuleName resolvedName} | (packageIdText packageId, definingModuleName) /= home -> packageIdText packageId <> ":" <> definingModuleName <> "." <> nameText resolvedName;
  _ -> nameText name};
registerTypeFamilyDeclHeader :: Maybe TypeScheme -> TypeFamilyDecl -> TcM ();
registerTypeFamilyDeclHeader = registerTypeFamilyDeclHeaderWith Map.empty;
registerTypeFamilyDeclHeaderWith :: Map Text TcType -> Maybe TypeScheme -> TypeFamilyDecl -> TcM ();
registerTypeFamilyDeclHeaderWith sharedKinds maybeKindScheme familyDecl = case typeFamilyHeadName (typeFamilyDeclHead familyDecl) of {
  Nothing -> emitError Nothing (OtherError "type family head does not name a type family");
  Just familyBinder -> do {
  let {
  familyName = unqualifiedNameText familyBinder;
  params = typeFamilyDeclParams familyDecl;
  arity = length params};
  (kindParams, paramInfos) <- typeDeclParamInfos maybeKindScheme params;
  forM_ paramInfos $ \param -> forM_ (Map.lookup (paramName param) sharedKinds) (`unifyKinds` paramKind param);
  inferredKind <- tyConKindFromParams paramInfos (typeFamilyResultKindType familyDecl);
  familyTyCon <- mkDeclaredTyCon familyBinder familyName arity;
  let {
  declaredKind = maybe inferredKind typeSchemeBody maybeKindScheme};
  storeTyConInfo TyConInfo {tciName = familyName, tciArity = arity, tciTyCon = familyTyCon, tciKindScheme = specifiedScheme (map paramTyVar kindParams) [] declaredKind, tciFlavor = TypeFamilyTyCon, tciTypeSynonym = Nothing, tciInjectivity = typeFamilyInjectivePositions familyDecl};
  if Map.null sharedKinds then void (defaultKindMetas declaredKind) else do {
  forM_ paramInfos $ \param -> unless (Map.member (paramName param) sharedKinds) (void (defaultKindMetas (paramKind param)));
  void (defaultKindMetas (typeResultKind arity declaredKind))}}};
typeFamilyInjectivePositions :: TypeFamilyDecl -> Maybe [Int];
typeFamilyInjectivePositions familyDecl = case typeFamilyDeclResultSig familyDecl of {
  Just (TypeFamilyInjectiveSig _ injectivity) -> Just [position | (position, param) <- zip [0 ..] (typeFamilyDeclParams familyDecl), tyVarBinderName param `elem` typeFamilyInjectivityDetermined injectivity];
  _ -> Nothing};
typeFamilyResultKindType :: TypeFamilyDecl -> Maybe Type;
typeFamilyResultKindType familyDecl = case typeFamilyDeclResultSig familyDecl of {
  Just (TypeFamilyKindSig ty) -> Just ty;
  _ -> Nothing};
registerClosedTypeFamilyEquations :: (Text, Text) -> TypeFamilyDecl -> TcM [TcBindingResult];
registerClosedTypeFamilyEquations origin familyDecl = case typeFamilyDeclEquations familyDecl of {
  Nothing -> pure [];
  Just equations -> do {
  mapM_ (registerTypeFamilyEquation origin True (typeFamilyDeclParams familyDecl)) equations;
  pure []}};
registerTypeFamilyInstance :: (Text, Text) -> TypeFamilyInst -> TcM [TcBindingResult];
registerTypeFamilyInstance origin familyInst = do {
  registerTypeFamilyEquation origin False (typeFamilyInstForall familyInst) (typeFamilyInstEquation familyInst);
  pure []};
typeFamilyInstEquation :: TypeFamilyInst -> TypeFamilyEq;
typeFamilyInstEquation familyInst = TypeFamilyEq {typeFamilyEqAnns = [], typeFamilyEqForall = typeFamilyInstForall familyInst, typeFamilyEqHeadForm = typeFamilyInstHeadForm familyInst, typeFamilyEqLhs = typeFamilyInstLhs familyInst, typeFamilyEqRhs = typeFamilyInstRhs familyInst};
registerTypeFamilyEquation :: (Text, Text) -> Bool -> [TyVarBinder] -> TypeFamilyEq -> TcM ();
registerTypeFamilyEquation origin isClosed extraBinders equation = checkTypeFamilyEquation origin isClosed extraBinders equation >>= mapM_ addTypeFamilyInstance;
checkTypeFamilyEquation :: (Text, Text) -> Bool -> [TyVarBinder] -> TypeFamilyEq -> TcM (Maybe TypeFamilyInstanceInfo);
checkTypeFamilyEquation (packageName, moduleName') isClosed extraBinders equation = do {
  paramInfos <- typeFamilyEquationParams extraBinders equation;
  let {
  tvEnv = Map.fromList [(paramName param, (paramTyVar param, paramKind param)) | param <- paramInfos]};
  (rawLhs, lhsKind) <- convertSurfaceTypeWithKinds tvEnv (typeFamilyEqLhs equation);
  rawRhs <- checkSurfaceType tvEnv (typeFamilyEqRhs equation) =<< zonkKind lhsKind;
  wildcardParams <- bindWildcardParams rawLhs;
  lhs <- zonkType rawLhs;
  rhs <- zonkType rawRhs;
  let {
  mentioned param = typeMentionsTyVar (paramTyVar param) lhs || typeMentionsTyVar (paramTyVar param) rhs;
  allParamInfos = filter mentioned (paramInfos <> wildcardParams)};
  case typeFamilyApplicationHead lhs of {
  Just familyTyCon -> do {
  maybeFamilyInfo <- lookupTyConByIdentity familyTyCon;
  case maybeFamilyInfo of {
  Just familyInfo | tciFlavor familyInfo == TypeFamilyTyCon -> do {
  existing <- getTypeFamilyInstances;
  let {
  familyName = tciName familyInfo;
  axiomName = if isClosed then typeFamilyAxiomName familyName (length existing) else sourceTypeFamilyAxiomName (packageName, moduleName') (typeFamilyEqLhs equation);
  instanceInfo = TypeFamilyInstanceInfo {tfiiFamilyName = familyName, tfiiAxiomName = axiomName, tfiiOrigin = (PackageId packageName, moduleName'), tfiiTyVars = map paramTyVar allParamInfos, tfiiLeft = lhs, tfiiRight = rhs, tfiiClosed = isClosed}};
  pure (Just instanceInfo)};
  _ -> do {
  emitError Nothing (OtherError ("type-family instance head does not name a type family: " <> T.unpack (tyConName familyTyCon)));
  pure Nothing}}};
  Nothing -> do {
  emitError Nothing (OtherError ("invalid type-family instance head: " <> show lhs));
  pure Nothing}}};
bindWildcardParams :: TcType -> TcM [ParamInfo];
bindWildcardParams lhs = do {
  zonked <- zonkType lhs;
  mapM bindOne (nub (typeMetas zonked))} where {
  typeMetas ty = case ty of {
  TcMetaTv unique -> [unique];
  TcTyVar {} -> [];
  TcArrowTy -> [];
  TcTyLit {} -> [];
  TcTyCon _ arguments -> concatMap typeMetas arguments;
  TcFunTy argument result -> typeMetas argument <> typeMetas result;
  TcForAllTy _ body -> typeMetas body;
  TcQualTy _ body -> typeMetas body;
  TcAppTy function argument -> typeMetas function <> typeMetas argument};
  bindOne unique = do {
  kind <- zonkKind =<< readMetaTvKind unique;
  tyVar <- freshSkolemTvOfKind "_" kind;
  writeMetaTv unique (TcTyVar tyVar);
  pure ParamInfo {paramName = "_", paramTyVar = tyVar, paramKind = kind}}};
typeFamilyApplicationHead :: TcType -> Maybe TyCon;
typeFamilyApplicationHead ty = case ty of {
  TcTyCon tyCon _ -> Just tyCon;
  TcAppTy function _ -> typeFamilyApplicationHead function;
  _ -> Nothing};
typeFamilyEquationParams :: [TyVarBinder] -> TypeFamilyEq -> TcM [ParamInfo];
typeFamilyEquationParams extraBinders equation = do {
  explicitParams <- makeParamEnv (extraBinders <> typeFamilyEqForall equation);
  let {
  explicitNames = map paramName explicitParams;
  implicitNames = nub (freeTypeVars (typeFamilyEqLhs equation) <> freeTypeVars (typeFamilyEqRhs equation)) \\ explicitNames};
  implicitParams <- mapM makeImplicitParam implicitNames;
  pure (explicitParams <> implicitParams)} where {
  makeImplicitParam name = do {
  rawTyVar <- freshSkolemTv name;
  kind <- freshKindMeta;
  pure ParamInfo {paramName = name, paramTyVar = setTyVarKind kind rawTyVar, paramKind = kind}}};
typeDeclParamInfos :: Maybe TypeScheme -> [TyVarBinder] -> TcM ([ParamInfo], [ParamInfo]);
typeDeclParamInfos maybeKindScheme params = case maybeKindScheme of {
  Nothing -> ([],) <$> makeParamEnv params;
  Just scheme@(ForAll kindTyVars _ _) -> do {
  let {
  kindParams = map kindParam kindTyVars;
  kindEnv = Map.fromList [(paramName param, (paramTyVar param, paramKind param)) | param <- kindParams];
  expectedKinds = takeVisibleArgumentKinds (length params) (typeSchemeBody scheme)};
  paramInfos <- makeParamEnvWith kindEnv params;
  if length expectedKinds == length params then do {
  zipWithM_ (unifyKinds . paramKind) paramInfos expectedKinds;
  let {
  checkedParams = zipWith setParamKind expectedKinds paramInfos};
  pure (kindParams, checkedParams)} else do {
  emitError Nothing (OtherError "standalone kind signature arity does not match its type declaration");
  pure (kindParams, paramInfos)}}} where {
  kindParam tyVar = ParamInfo (tvName tyVar) tyVar (tvKind tyVar);
  setParamKind kind param = param {paramTyVar = setTyVarKind kind (paramTyVar param), paramKind = kind}};
implicitBinderKindParams :: [TyVarBinder] -> TcM [ParamInfo];
implicitBinderKindParams binders = mapM makeImplicitParam implicitNames where {
  explicitNames = map tyVarBinderName binders;
  implicitNames = nub (concatMap (maybe [] freeTypeVars . tyVarBinderKind) binders) \\ explicitNames;
  makeImplicitParam name = do {
  rawTyVar <- freshSkolemTv name;
  kind <- freshKindMeta;
  pure ParamInfo {paramName = name, paramTyVar = setTyVarKind kind rawTyVar, paramKind = kind}}};
dataDeclParamInfos :: Maybe TypeScheme -> DataDecl -> TcM ([ParamInfo], [ParamInfo]);
dataDeclParamInfos maybeKindScheme declaration = case maybeKindScheme of {
  Just {} -> typeDeclParamInfos maybeKindScheme binders;
  Nothing -> do {
  kindParams <- implicitBinderKindParams binders;
  let {
  kindEnv = Map.fromList [(paramName param, (paramTyVar param, paramKind param)) | param <- kindParams]};
  params <- makeParamEnvWith kindEnv binders;
  pure (kindParams, params)}} where {
  binders = binderHeadParams (dataDeclHead declaration)};
registerDataDeclHeader :: Maybe TypeScheme -> DataDecl -> TcM ();
registerDataDeclHeader maybeKindScheme dd = do {
  let {
  tyBinder = binderHeadName (dataDeclHead dd);
  tyName = unqualifiedNameText tyBinder;
  params = binderHeadParams (dataDeclHead dd);
  arity = length params};
  (kindParams, paramInfos) <- dataDeclParamInfos maybeKindScheme dd;
  let {
  kindEnv = Map.fromList [(paramName param, (paramTyVar param, paramKind param)) | param <- kindParams]};
  inferredKind <- tyConKindFromParamsWith kindEnv paramInfos (dataDeclKind dd);
  tc <- mkDeclaredTyCon tyBinder tyName arity;
  let {
  declaredKind = maybe inferredKind typeSchemeBody maybeKindScheme};
  storeTyConInfo TyConInfo {tciName = tyName, tciArity = arity, tciTyCon = tc, tciKindScheme = specifiedScheme (map paramTyVar kindParams) [] declaredKind, tciFlavor = DataTyCon, tciTypeSynonym = Nothing, tciInjectivity = Nothing};
  pure ()};
registerDataConstructors :: (Text, Text) -> DataDecl -> TcM [TcBindingResult];
registerDataConstructors origin dataDecl = do {
  let {
  tyBinder = binderHeadName (dataDeclHead dataDecl);
  tyName = unqualifiedNameText tyBinder};
  maybeInfo <- lookupDeclaredTyCon tyBinder;
  case maybeInfo of {
  Nothing -> missingTypeInfo ("data type " <> T.unpack tyName);
  Just info -> do {
  (kindParams, paramInfos) <- dataDeclParamInfos (Just (tciKindScheme info)) dataDecl;
  bindings <- mapM (registerDataCon (tciTyCon info) kindParams paramInfos) (dataDeclConstructors dataDecl);
  constructors <- concat <$> mapM (checkedDataConInfos (tciTyCon info)) (dataDeclConstructors dataDecl);
  mapM_ registerTypeLevelDataCon constructors;
  selectorBindings <- registerRecordSelectors origin constructors;
  let {
  tyVars = map paramTyVar paramInfos};
  resultKind <- tcTypeKind (TcTyCon (tciTyCon info) (map TcTyVar tyVars));
  addDataType DataTypeInfo {dtiName = tyName, dtiTyCon = tciTyCon info, dtiTyVars = tyVars, dtiResultKind = resultKind, dtiFlavor = DataTyCon, dtiConstructors = constructors, dtiNominalRoles = replicate (length tyVars) False, dtiCType = cTypePragma (dataDeclCTypePragma dataDecl)};
  pure (bindings <> selectorBindings)}}};
registerNewtypeDeclHeader :: Maybe TypeScheme -> NewtypeDecl -> TcM ();
registerNewtypeDeclHeader maybeKindScheme nd = do {
  let {
  tyBinder = binderHeadName (newtypeDeclHead nd);
  tyName = unqualifiedNameText tyBinder;
  params = binderHeadParams (newtypeDeclHead nd);
  arity = length params};
  (kindParams, paramInfos) <- typeDeclParamInfos maybeKindScheme params;
  inferredKind <- tyConKindFromParams paramInfos (newtypeDeclKind nd);
  tc <- mkDeclaredTyCon tyBinder tyName arity;
  let {
  declaredKind = maybe inferredKind typeSchemeBody maybeKindScheme};
  storeTyConInfo TyConInfo {tciName = tyName, tciArity = arity, tciTyCon = tc, tciKindScheme = specifiedScheme (map paramTyVar kindParams) [] declaredKind, tciFlavor = NewtypeTyCon, tciTypeSynonym = Nothing, tciInjectivity = Nothing};
  pure ()};
registerNewtypeConstructor :: (Text, Text) -> NewtypeDecl -> TcM [TcBindingResult];
registerNewtypeConstructor origin newtypeDecl = do {
  let {
  tyBinder = binderHeadName (newtypeDeclHead newtypeDecl);
  tyName = unqualifiedNameText tyBinder};
  maybeInfo <- lookupDeclaredTyCon tyBinder;
  case maybeInfo of {
  Nothing -> missingTypeInfo ("newtype " <> T.unpack tyName);
  Just info -> do {
  (kindParams, paramInfos) <- typeDeclParamInfos (Just (tciKindScheme info)) (binderHeadParams (newtypeDeclHead newtypeDecl));
  constructor <- mapM (registerDataCon (tciTyCon info) kindParams paramInfos) (newtypeDeclConstructor newtypeDecl);
  constructors <- maybe (pure []) (checkedDataConInfos (tciTyCon info)) (newtypeDeclConstructor newtypeDecl);
  mapM_ registerTypeLevelDataCon constructors;
  selectorBindings <- registerRecordSelectors origin constructors;
  let {
  tyVars = map paramTyVar paramInfos};
  resultKind <- tcTypeKind (TcTyCon (tciTyCon info) (map TcTyVar tyVars));
  addDataType DataTypeInfo {dtiName = tyName, dtiTyCon = tciTyCon info, dtiTyVars = tyVars, dtiResultKind = resultKind, dtiFlavor = NewtypeTyCon, dtiConstructors = constructors, dtiNominalRoles = replicate (length tyVars) False, dtiCType = cTypePragma (newtypeDeclCTypePragma newtypeDecl)};
  pure (maybeToList constructor <> selectorBindings)}}};
cTypePragma :: Maybe Pragma -> Maybe CType;
cTypePragma maybePragma = do {
  pragma <- maybePragma;
  let {
  text = case pragmaType pragma of {
  PragmaUnknown raw -> raw;
  _ -> pragmaRawText pragma}};
  case quotedStrings text of {
  [name] -> Just CType {cTypeHeader = Nothing, cTypeName = name};
  [header, name] -> Just CType {cTypeHeader = Just header, cTypeName = name};
  _ -> Nothing}} where {
  quotedStrings text = case T.breakOn "\"" text of {
  (_, rest) | T.null rest -> [] | otherwise -> let {
  (literal, remaining) = T.breakOn "\"" (T.drop 1 rest)} in if T.null remaining then [] else literal : quotedStrings (T.drop 1 remaining)}};
registerTypeLevelDataCon :: DataConInfo -> TcM ();
registerTypeLevelDataCon constructor = do {
  let {
  name = dciName constructor;
  fieldTypes = dataConArgTypes constructor;
  arity = length fieldTypes;
  (packageId, moduleName') = dciOrigin constructor;
  dataConTyCon = mkTyConWithNamespace ResolutionNamespaceTerm packageId moduleName' name arity};
  let {
  kindScheme = specifiedScheme (dciUnivTyVars constructor <> dciExTyVars constructor) (dciTheta constructor) (foldr TcFunTy (dciResTy constructor) fieldTypes);
  info = TyConInfo {tciName = name, tciArity = arity, tciTyCon = dataConTyCon, tciKindScheme = kindScheme, tciFlavor = DataTyCon, tciTypeSynonym = Nothing, tciInjectivity = Nothing}};
  storeTyConInfo info};
registerRecordSelectors :: (Text, Text) -> [DataConInfo] -> TcM [TcBindingResult];
registerRecordSelectors origin constructors = mapM registerSelector (Map.toList selectors) where {
  selectors = Map.fromListWith (++) [(label, [(constructor, field)]) | constructor <- constructors, field <- dciFields constructor, not (any (`elem` dciExTyVars constructor) (typeTyVars (dcfiType field))), Just label <- [dcfiLabel field]];
  registerSelector (label, (constructor, field) : _) = do {
  let {
  scheme = specifiedScheme (dciUnivTyVars constructor) [] (TcFunTy (dciResTy constructor) (dcfiType field))};
  let {
  binder = TcIdBinder scheme Closed;
  selectorKey = originTermKey origin label};
  extendTermKeyEnvPermanent selectorKey binder;
  zonkedType <- zonkType (schemeToType scheme);
  pure (TcBindingResult selectorKey label zonkedType)};
  registerSelector (label, []) = abortTc ("record selector has no fields: " <> T.unpack label)};
registerTypeSynonymHeader :: Maybe TypeScheme -> TypeSynDecl -> TcM ();
registerTypeSynonymHeader maybeKindScheme typeSynDecl = do {
  let {
  tyBinder = binderHeadName (typeSynHead typeSynDecl);
  tyName = unqualifiedNameText tyBinder;
  params = binderHeadParams (typeSynHead typeSynDecl);
  arity = length params};
  (_, paramInfos) <- typeDeclParamInfos maybeKindScheme params;
  inferredResultKind <- freshKindMeta;
  let {
  inferredKind = foldr (KFun . paramKind) inferredResultKind paramInfos};
  tyCon <- mkDeclaredTyCon tyBinder tyName arity;
  let {
  declaredKindScheme = fromMaybe (Scheme [] [] [] inferredKind) maybeKindScheme};
  let {
  synonym = TypeSynonymInfo (map paramTyVar paramInfos) Nothing};
  storeTyConInfo TyConInfo {tciName = tyName, tciArity = arity, tciTyCon = tyCon, tciKindScheme = declaredKindScheme, tciFlavor = SynonymTyCon, tciTypeSynonym = Just synonym, tciInjectivity = Nothing}};
registerTypeSynonymBody :: Decl -> TcM ();
registerTypeSynonymBody (DeclAnn _ inner) = registerTypeSynonymBody inner;
registerTypeSynonymBody (DeclTypeSyn typeSynDecl) = do {
  let {
  tyBinder = binderHeadName (typeSynHead typeSynDecl);
  tyName = unqualifiedNameText tyBinder};
  maybeInfo <- lookupDeclaredTyCon tyBinder;
  case maybeInfo of {
  Just info | Just synonym <- tciTypeSynonym info -> do {
  let {
  params = tsiParams synonym;
  tvEnv = Map.fromList [(tvName param, (param, tvKind param)) | param <- params]};
  (body, _) <- convertSurfaceTypeWithKinds tvEnv (typeSynBody typeSynDecl);
  replaceTyConEnvPermanent (info {tciTypeSynonym = Just (synonym {tsiBody = Just body})})};
  _ -> missingTypeInfo ("type synonym " <> T.unpack tyName)}};
registerTypeSynonymBody _ = pure ();
checkTypeSynonymBody :: Decl -> TcM ();
checkTypeSynonymBody (DeclAnn _ inner) = checkTypeSynonymBody inner;
checkTypeSynonymBody (DeclTypeSyn typeSynDecl) = do {
  let {
  tyBinder = binderHeadName (typeSynHead typeSynDecl);
  tyName = unqualifiedNameText tyBinder};
  maybeInfo <- lookupDeclaredTyCon tyBinder;
  case maybeInfo of {
  Just info | Just synonym <- tciTypeSynonym info -> do {
  let {
  params = tsiParams synonym;
  tvEnv = Map.fromList [(tvName param, (param, tvKind param)) | param <- params];
  resultKind = typeResultKind (length params) (typeSchemeBody (tciKindScheme info))};
  (_, bodyKind) <- convertSurfaceTypeWithKinds tvEnv (typeSynBody typeSynDecl);
  unifyKindsAt (surfaceTypeSpan (typeSynBody typeSynDecl)) resultKind bodyKind};
  _ -> missingTypeInfo ("type synonym " <> T.unpack tyName)}};
checkTypeSynonymBody _ = pure ();
typeResultKind :: Int -> TcType -> TcType;
typeResultKind remaining kind | remaining <= 0 = kind;
typeResultKind remaining (KFun _ result) = typeResultKind (remaining - 1) result;
typeResultKind _ kind = kind;
mkDeclaredTyCon :: UnqualifiedName -> Text -> Int -> TcM TyCon;
mkDeclaredTyCon binder name arity = case nameResolution binder of {
  Just ResolutionAnnotation {resolutionTarget = ResolvedTopLevel packageId definingModuleName _} -> wiredDeclarationIdentity (mkTyConWithOrigin packageId definingModuleName name arity);
  _ -> abortTc ("type declaration has no package or module identity: " <> T.unpack name)};
registerDataCon :: TyCon -> [ParamInfo] -> [ParamInfo] -> DataConDecl -> TcM TcBindingResult;
registerDataCon tc kindParams paramInfos = registerDataConWithResult (kindParams <> paramInfos) (TcTyCon tc (map (TcTyVar . paramTyVar) paramInfos));
registerDataConWithResult :: [ParamInfo] -> TcType -> DataConDecl -> TcM TcBindingResult;
registerDataConWithResult paramInfos resTy con = case con of {
  DataConAnn _ inner -> registerDataConWithResult paramInfos resTy inner;
  PrefixCon forallVars context conName args -> registerH98DataCon forallVars context (Just conName) (unqualifiedNameText conName) (map bangType args);
  InfixCon forallVars context lhs conName rhs -> registerH98DataCon forallVars context (Just conName) (unqualifiedNameText conName) (map bangType [lhs, rhs]);
  RecordCon forallVars context conName fields -> registerH98DataCon forallVars context (Just conName) (unqualifiedNameText conName) (map bangType (recordBangFields fields));
  TupleCon forallVars context flavor fields -> registerBuiltinDataCon forallVars context (BuiltinTupleCon flavor (length fields)) (map bangType fields);
  UnboxedSumCon forallVars context alternative arity field -> registerBuiltinDataCon forallVars context (BuiltinUnboxedSumCon alternative arity) [bangType field];
  ListCon forallVars context -> registerBuiltinDataCon forallVars context BuiltinNilCon [];
  GadtCon forallBinders context names body -> do {
  explicitParams <- makeParamEnv (concatMap forallTelescopeBinders forallBinders);
  let {
  explicitNames = map paramName (explicitParams <> paramInfos);
  implicitNames = filter (`notElem` explicitNames) (nub (concatMap freeTypeVars (gadtBodyResultType body : gadtBodyArgTypes body <> context)))};
  implicitParams <- forM implicitNames $ \name -> do {
  variable <- freshSkolemTv name;
  kind <- freshKindMeta;
  pure (ParamInfo name (setTyVarKind kind variable) kind)};
  let {
  constructorParams = explicitParams <> implicitParams;
  constructorEnv = Map.fromList [(paramName param, (paramTyVar param, paramKind param)) | param <- constructorParams] <> paramEnv;
  constructorTyVars = map paramTyVar constructorParams};
  let {
  resultSurfTy = gadtBodyResultType body;
  argSurfTys = gadtBodyArgTypes body};
  kinds <- getKinds;
  gadtResTy <- checkSurfaceType constructorEnv resultSurfTy (typeKind kinds);
  gadtArgTys <- mapM (checkRuntimeType constructorEnv) argSurfTys;
  writtenPredicates <- mapM (surfacePredToPred constructorEnv) context;
  (universalTyVars, refinementPredicates, universalResTy) <- rejigGadtResult resTy gadtResTy;
  let {
  predicates = refinementPredicates <> writtenPredicates;
  conTy = foldr TcFunTy universalResTy gadtArgTys;
  candidateTyVars = filter (`notElem` universalTyVars) (paramVarIds <> constructorTyVars);
  quantifiedTyVars = universalTyVars <> filter (\tyVar -> typeMentionsTyVar tyVar conTy || any (predicateMentionsTyVar tyVar) predicates) candidateTyVars;
  gadtScheme = specifiedScheme quantifiedTyVars predicates conTy};
  mapM_ (\n -> do {
  constructorKey <- resolvedUnqualifiedTermKey n;
  extendResolvedTermEnvPermanent n (TcIdBinder gadtScheme Closed);
  markGadtCon constructorKey}) names;
  case names of {
  (n : _) -> do {
  constructorKey <- resolvedUnqualifiedTermKey n;
  zonkedTy <- zonkType conTy;
  pure (TcBindingResult constructorKey (unqualifiedNameText n) zonkedTy)};
  [] -> abortTc "a GADT constructor form declares no constructor name"}}} where {
  paramEnv = Map.fromList [(paramName param, (paramTyVar param, paramKind param)) | param <- paramInfos];
  paramVarIds = map paramTyVar paramInfos;
  registerBuiltinDataCon forallVars context builtin fieldTypes = do {
  wired <- wiredBuiltinDataCon builtin;
  registerH98DataCon forallVars context Nothing (tyConName wired) fieldTypes};
  registerH98DataCon forallVars context maybeName name fieldTypes = do {
  constructorParams <- makeParamEnv forallVars;
  let {
  constructorEnv = Map.fromList [(paramName param, (paramTyVar param, paramKind param)) | param <- constructorParams] <> paramEnv;
  constructorTyVars = map paramTyVar constructorParams};
  argTys <- mapM (checkRuntimeType constructorEnv) fieldTypes;
  predicates <- mapM (surfacePredToPred constructorEnv) context;
  let {
  conTy = foldr TcFunTy resTy argTys;
  scheme = specifiedScheme (paramVarIds <> constructorTyVars) predicates conTy};
  constructorKey <- case maybeName of {
  Just sourceName -> do {
  extendResolvedTermEnvPermanent sourceName (TcIdBinder scheme Closed);
  resolvedUnqualifiedTermKey sourceName};
  Nothing -> case resTy of {
  TcTyCon resultTyCon _ -> do {
  extendTyConTermEnvPermanent resultTyCon name (TcIdBinder scheme Closed);
  pure (tyConMemberTermKey resultTyCon name)};
  _ -> abortTc ("a built-in constructor form declares no type constructor: " <> T.unpack name)}};
  zonkedTy <- zonkType (schemeToType scheme);
  pure (TcBindingResult constructorKey name zonkedTy)}};
rejigGadtResult :: TcType -> TcType -> TcM ([TyVarId], [Pred], TcType);
rejigGadtResult declaredResTy writtenResTy = case (declaredResTy, writtenResTy) of {
  (TcTyCon declaredTyCon declaredArgs, TcTyCon writtenTyCon writtenArgs) | declaredTyCon == writtenTyCon, length declaredArgs == length writtenArgs, Just params <- mapM declaredParam declaredArgs, distinctTyVars params -> do {
  (universals, predicates) <- rejigIndices [] (zip params writtenArgs);
  pure (universals, predicates, TcTyCon writtenTyCon (map TcTyVar universals))};
  _ -> pure ([], [], writtenResTy)} where {
  declaredParam ty = case ty of {
  TcTyVar tyVar -> Just tyVar;
  _ -> Nothing};
  distinctTyVars tyVars = length (nub (map tvUnique tyVars)) == length tyVars};
rejigIndices :: [TyVarId] -> [(TyVarId, TcType)] -> TcM ([TyVarId], [Pred]);
rejigIndices = go Map.empty where {
  go _ _ [] = pure ([], []);
  go kindParams claimed ((param, writtenArg) : rest) = do {
  (universal, predicates, kindParams') <- case writtenArg of {
  TcTyVar tyVar | tvUnique tyVar `notElem` map tvUnique claimed -> pure (tyVar, [], claimKindParams kindParams (tvKind param) (tvKind tyVar));
  _ -> do {
  writtenKind <- tcTypeKind writtenArg >>= zonkKind;
  declaredKind <- zonkKind (substituteKindParams kindParams (tvKind param));
  fresh <- setTyVarKind declaredKind <$> freshSkolemTv (tvName param);
  let {
  kindPredicates = [EqPred declaredKind writtenKind | declaredKind /= writtenKind]};
  pure (fresh, kindPredicates <> [EqPred (TcTyVar fresh) writtenArg], kindParams)}};
  (universals, restPredicates) <- go kindParams' (universal : claimed) rest;
  pure (universal : universals, predicates <> restPredicates)};
  claimKindParams kindParams declaredKind writtenKind = case declaredKind of {
  TcTyVar kindParam | tvUnique kindParam `Map.notMember` kindParams -> Map.insert (tvUnique kindParam) writtenKind kindParams;
  _ -> kindParams};
  substituteKindParams kindParams kind = case kind of {
  TcTyVar tyVar -> fromMaybe kind (Map.lookup (tvUnique tyVar) kindParams);
  TcTyCon tyCon arguments -> TcTyCon tyCon (map (substituteKindParams kindParams) arguments);
  TcFunTy argument result -> TcFunTy (substituteKindParams kindParams argument) (substituteKindParams kindParams result);
  TcAppTy function argument -> TcAppTy (substituteKindParams kindParams function) (substituteKindParams kindParams argument);
  _ -> kind}};
gadtBodyArgTypes :: GadtBody -> [Type];
gadtBodyArgTypes (GadtPrefixBody argsWithKinds _) = map (bangType . fst) argsWithKinds;
gadtBodyArgTypes (GadtRecordBody fields _) = map bangType (recordBangFields fields);
recordBangFields :: [FieldDecl] -> [BangType];
recordBangFields = concatMap $ \field -> replicate (length (fieldNames field)) (fieldType field);
checkedDataConInfos :: TyCon -> DataConDecl -> TcM [DataConInfo];
checkedDataConInfos tyCon declaration = do {
  let {
  (sourceForm, sourceFields, _) = dataConSourceLayout declaration;
  origin = (tyConPackageId tyCon, tyConModuleName tyCon)};
  constructorNames <- dataConNames declaration;
  mapM (checkedDataConInfo origin sourceForm sourceFields) constructorNames};
checkedDataConInfo :: (PackageId, Text) -> DataConSourceForm -> [(Maybe Text, BangType)] -> Text -> TcM DataConInfo;
checkedDataConInfo origin@(originPackage, originModule) sourceForm sourceFields constructorName = do {
  maybeBinder <- lookupTermKey (TcTermGlobal originPackage originModule constructorName);
  case maybeBinder of {
  Just (TcIdBinder (ForAll tyVars predicates constructorType) _) -> do {
  let {
  (argumentTypes, resultType) = splitFunctionType constructorType};
  if length sourceFields /= length argumentTypes then abortTc ("constructor metadata arity disagrees with checked type for " <> T.unpack constructorName) else do {
  let {
  (universalTyVars, existentialTyVars) = partition (`typeMentionsTyVar` resultType) tyVars};
  pure DataConInfo {dciName = constructorName, dciOrigin = origin, dciUnivTyVars = universalTyVars, dciExTyVars = existentialTyVars, dciTheta = predicates, dciFields = zipWith checkedFieldInfo sourceFields argumentTypes, dciResTy = resultType, dciSourceForm = sourceForm}}};
  Just TcMonoIdBinder {} -> abortTc ("data constructor has a monomorphic binder: " <> T.unpack constructorName);
  Nothing -> missingTypeInfo ("data constructor " <> T.unpack constructorName)}};
checkedFieldInfo :: (Maybe Text, BangType) -> TcType -> DataConFieldInfo;
checkedFieldInfo (label, bang) fieldType' = DataConFieldInfo {dcfiLabel = label, dcfiType = fieldType', dcfiStrict = bangStrict bang, dcfiLazy = bangLazy bang, dcfiUnpack = fieldUnpack bang};
fieldUnpack :: BangType -> DataConFieldUnpack;
fieldUnpack bang = case [unpack | Pragma (PragmaUnpack unpack) _ <- bangPragmas bang] of {
  UnpackPragma : _ -> UnpackField;
  NoUnpackPragma : _ -> NoUnpackField;
  [] -> NoFieldUnpack};
dataConSourceLayout :: DataConDecl -> (DataConSourceForm, [(Maybe Text, BangType)], [DataConIdentity]);
dataConSourceLayout declaration = case declaration of {
  DataConAnn _ inner -> dataConSourceLayout inner;
  PrefixCon _ _ _ fields -> (PrefixDataCon, map (Nothing,) fields, identities);
  InfixCon _ _ left _ right -> (InfixDataCon, map (Nothing,) [left, right], identities);
  RecordCon _ _ _ fields -> (RecordDataCon, recordSourceFields fields, identities);
  TupleCon _ _ Boxed fields -> (SyntaxDataCon, map (Nothing,) fields, identities);
  TupleCon _ _ Unboxed fields -> (UnboxedTupleDataCon, map (Nothing,) fields, identities);
  UnboxedSumCon _ _ alternative arity field -> (UnboxedSumDataCon alternative arity, [(Nothing, field)], identities);
  ListCon {} -> (SyntaxDataCon, [], identities);
  GadtCon _ _ _ body -> case body of {
  GadtPrefixBody fields _ -> (PrefixDataCon, map ((Nothing,) . fst) fields, identities);
  GadtRecordBody fields _ -> (RecordDataCon, recordSourceFields fields, identities)}} where {
  identities = dataConIdentities declaration};
recordSourceFields :: [FieldDecl] -> [(Maybe Text, BangType)];
recordSourceFields = concatMap $ \field -> [(Just (unqualifiedNameText label), fieldType field) | label <- fieldNames field];
tcDecl :: Decl -> TcM [TcBindingResult];
tcDecl (DeclValue vd) = tcValueDecl vd;
tcDecl (DeclAnn _ inner) = tcDecl inner;
tcDecl _ = pure [];
tcValueDecl :: ValueDecl -> TcM [TcBindingResult];
tcValueDecl (FunctionBind binder matches) = do {
  let {
  displayName = renderBinderName binder};
  key <- resolvedUnqualifiedTermKey binder;
  snd <$> tcFunctionInfer key displayName matches};
tcValueDecl (PatternBind _ pat rhs) = case patternBinderName pat of {
  Just (_, displayName) -> do {
  case patternBinderSyntaxName pat of {
  Just binder -> do {
  key <- resolvedUnqualifiedTermKey binder;
  snd <$> tcFunctionInfer key displayName [zeroArgMatch (patternSpan pat) rhs]};
  Nothing -> abortTc "a named pattern binding does not have binder syntax"}};
  Nothing -> do {
  key <- patternRhsTermKey pat;
  (_rhs', ty) <- tcRhs rhs;
  zonkedTy <- zonkType ty;
  pure [TcBindingResult key "<pattern>" zonkedTy]}};
patternBinderSyntaxName :: Pattern -> Maybe UnqualifiedName;
patternBinderSyntaxName (PVar n) = Just n;
patternBinderSyntaxName (PParen inner) = patternBinderSyntaxName inner;
patternBinderSyntaxName (PAnn _ inner) = patternBinderSyntaxName inner;
patternBinderSyntaxName _ = Nothing;
patternBinderName :: Pattern -> Maybe (Text, Text);
patternBinderName pat = binderBindingName <$> patternBinderSyntaxName pat;
zeroArgMatch :: Maybe SourceSpan -> Rhs Expr -> Match;
zeroArgMatch sp rhs = Match {matchAnns = sourceSpanAnn sp, matchHeadForm = MatchHeadPrefix, matchPats = [], matchRhs = rhs};
sourceSpanAnn :: Maybe SourceSpan -> [Annotation];
sourceSpanAnn = map mkAnnotation . maybeToList;
patternSpan :: Pattern -> Maybe SourceSpan;
patternSpan pat = case pat of {
  PAnn ann inner -> fromAnnotation @SourceSpan ann <|> patternSpan inner;
  PVar name -> sourceSpanFromAnns (unqualifiedNameAnns name);
  PParen inner -> patternSpan inner;
  PAs name _ -> sourceSpanFromAnns (unqualifiedNameAnns name);
  PStrict inner -> patternSpan inner;
  PIrrefutable inner -> patternSpan inner;
  PCon name _ _ -> nameSpan name;
  PInfix _ name _ -> nameSpan name;
  _ -> Nothing} where {
  nameSpan name = sourceSpanFromAnns (nameAnns name) <|> case [resolutionSpan resolution | Just resolution <- map fromAnnotation (nameAnns name)] of {
  sp : _ -> sp;
  [] -> Nothing}};
typeSpan :: Type -> Maybe SourceSpan;
typeSpan ty = case ty of {
  TAnn ann inner -> fromAnnotation @SourceSpan ann <|> typeSpan inner;
  TParen inner -> typeSpan inner;
  TForall _ inner -> typeSpan inner;
  TContext _ inner -> typeSpan inner;
  TKindSig inner _ -> typeSpan inner;
  _ -> Nothing};
rhsExprSpan :: Rhs Expr -> Maybe SourceSpan;
rhsExprSpan rhs = case rhs of {
  UnguardedRhs anns expr _ -> exprSpan expr <|> sourceSpanFromAnns anns;
  GuardedRhss anns _ _ -> sourceSpanFromAnns anns};
exprSpan :: Expr -> Maybe SourceSpan;
exprSpan expr = case expr of {
  EAnn ann inner -> fromAnnotation @SourceSpan ann <|> exprSpan inner;
  EParen inner -> exprSpan inner;
  EPragma _ inner -> exprSpan inner;
  ETypeSig inner _ -> exprSpan inner;
  _ -> Nothing};
tcMatches :: [Match] -> TcM ([Match], TcType, [Ct], [Implication]);
tcMatches [] = do {
  ty <- freshMetaTv;
  pure ([], ty, [], [])};
tcMatches matches@(m0 : _) = do {
  let {
  nArgs = length (matchPats m0)};
  if nArgs == 0 then do {
  (rhs0, ty0, cts0) <- inferRhsExpr (matchRhs m0);
  restResults <- mapM (unifyMatchRhs ty0) (drop 1 matches);
  let {
  firstMatch = m0 {matchRhs = rhs0};
  restMatches = map fst restResults;
  restCts = concatMap snd restResults};
  pure (firstMatch : restMatches, ty0, cts0 ++ restCts, [])} else do {
  argTys <- mapM (const freshMetaTv) [1 .. nArgs];
  resTy <- freshMetaTv;
  results <- mapM (tcMatchEquation Nothing argTys resTy) matches;
  let {
  (matches', ctsList, implsList) = unzip3 results;
  allCts = concat ctsList;
  allImpls = concat implsList;
  funTy = foldr TcFunTy resTy argTys};
  pure (matches', funTy, allCts, allImpls)}};
tcMatchEquation :: Maybe TypeOrigin -> [TcType] -> TcType -> Match -> TcM (Match, [Ct], [Implication]);
tcMatchEquation expectedOrigin argTys resTy match = do {
  let {
  pats = matchPats match;
  sp = sourceSpanFromAnns (matchAnns match)};
  patCheck <- checkFunctionPatternsWithGivens sp (zip pats argTys);
  (rhs', rhsTy, rhsCts) <- withGivenPredicates (map ctPred (pcGivenCts patCheck)) (withPatternBindings (pcBindings patCheck) (checkRhs resTy (matchRhs match)));
  ev <- freshEvVar;
  let {
  rhsSp = rhsExprSpan (matchRhs match) <|> sp;
  resCt = mkWantedEqCt TypeTrace {typeTraceType = rhsTy, typeTraceRole = ActualType, typeTraceOrigin = ExpressionTypeOrigin rhsSp} TypeTrace {typeTraceType = resTy, typeTraceRole = ExpectedType, typeTraceOrigin = fromMaybe (ConstraintTypeOrigin (AppOrigin sp)) expectedOrigin} ev (AppOrigin rhsSp) rhsSp};
  let {
  pats' = map (annotatePatternBindings (pcBindings patCheck)) (pcPatterns patCheck);
  givenCts = pcGivenCts patCheck;
  bodyWanteds = pcWantedCts patCheck ++ rhsCts ++ [resCt]};
  if null givenCts && null (pcSkolems patCheck) then pure (match {matchPats = pats', matchRhs = annotateRhsCast resTy ev rhs'}, bodyWanteds, []) else do {
  let {
  impl = Implication {implSkols = pcSkolems patCheck, implGivenCts = givenCts, implWantedCts = bodyWanteds}};
  pure (match {matchPats = pats', matchRhs = annotateRhsCast resTy ev rhs'}, [], [impl])}};
unifyMatchRhs :: TcType -> Match -> TcM (Match, [Ct]);
unifyMatchRhs expectedTy match = do {
  (rhs', rhsTy, rhsCts) <- inferRhsExpr (matchRhs match);
  ev <- freshEvVar;
  let {
  sp = sourceSpanFromAnns (matchAnns match);
  rhsSp = rhsExprSpan (matchRhs match) <|> sp;
  eqCt = mkWantedEqCt TypeTrace {typeTraceType = rhsTy, typeTraceRole = ActualType, typeTraceOrigin = ExpressionTypeOrigin rhsSp} TypeTrace {typeTraceType = expectedTy, typeTraceRole = ExpectedType, typeTraceOrigin = ConstraintTypeOrigin (AppOrigin sp)} ev (AppOrigin rhsSp) rhsSp};
  pure (match {matchRhs = rhs'}, rhsCts ++ [eqCt])};
inferRhsExpr :: Rhs Expr -> TcM (Rhs Expr, TcType, [Ct]);
inferRhsExpr = inferRhsWithLocals inferExpr;
tcRhs :: Rhs Expr -> TcM (Rhs Expr, TcType);
tcRhs rhs = do {
  (rhs', ty, cts) <- inferRhsWithLocals inferExpr rhs;
  _ <- solveConstraints cts;
  pure (rhs', ty)};
renderBinderName :: UnqualifiedName -> Text;
renderBinderName uname = case unqualifiedNameType uname of {
  NameVarSym -> "(" <> unqualifiedNameText uname <> ")";
  NameConSym -> "(" <> unqualifiedNameText uname <> ")";
  _ -> unqualifiedNameText uname}
}
