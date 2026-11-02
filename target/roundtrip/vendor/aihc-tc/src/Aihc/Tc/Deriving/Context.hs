{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Aihc.Tc.Deriving.Context (inferDerivingContexts, isContextFreeStockPlan, settleContextFreePlans, derivingObligations, newtypeRepresentation, stockFieldTypes, stockFunctorialFields, typeTyVars, moduleDerivingPlans, replaceModulePlans, planKey) where {
import Aihc.Parser.Syntax (Decl(..), Module(..), fromAnnotation, mkAnnotation);
import Aihc.Tc.Annotations (TcDerivingAnnotation(..), TcDerivingContext(..), TcDerivingPlan(..), TcDerivingStrategy(..), TcDictBinderAnnotation(..));
import Aihc.Tc.Constraint (CtOrigin(..));
import Aihc.Tc.Deriving.Functorial (fieldUse, fieldUseObligations);
import Aihc.Tc.Deriving.StockClass (StockObligations(..), generatesStockMethods, stockClassObligationsOf);
import Aihc.Tc.Env (DataConFieldInfo(..), DataConInfo(..), DataTypeInfo(..), InstanceInfo(..), TyConFlavor(..), instanceIsForClass);
import Aihc.Tc.Error (TcErrorKind(..));
import Aihc.Tc.Match (matchTypes);
import Aihc.Tc.Monad;
import Aihc.Tc.Solve.Family (reducePredFamilies);
import Aihc.Tc.Types;
import Control.Monad (foldM);
import Data.List (find, nub);
import Data.Map.Strict (Map);
import Data.Map.Strict qualified as Map;
import Data.Maybe (fromMaybe, mapMaybe);
import Data.Text (Text);
import Data.Text qualified as T;
inferDerivingContexts :: [Module] -> TcM [Module];
inferDerivingContexts modules = do {
  kinds <- getKinds;
  existingInstances <- getInstances;
  let {
  originalPlans = concatMap moduleDerivingPlans modules};
  trialEnvironment <- derivingEnv kinds existingInstances originalPlans;
  let {
  selectPlan plan | tcDerivingStockFallback plan, TcDerivingInferContext <- tcDerivingContext plan, Left _ <- inferredContext trialEnvironment plan = plan {tcDerivingStrategy = TcDerivingStock} | otherwise = plan;
  selectedPlans = map selectPlan originalPlans};
  environment <- derivingEnv kinds existingInstances selectedPlans;
  contextPlans <- mapM (inferPlanContext kinds environment) selectedPlans;
  pure (map (replaceModulePlans contextPlans) modules)};
isContextFreeStockPlan :: TcDerivingPlan -> Bool;
isContextFreeStockPlan plan = tcDerivingStrategy plan == TcDerivingStock && stockClassObligationsOf (tcDerivingClassName plan) == Just NoObligations;
settleContextFreePlans :: Module -> Module;
settleContextFreePlans modu = replaceModulePlans (map settle (moduleDerivingPlans modu)) modu where {
  settle plan | isContextFreeStockPlan plan, TcDerivingInferContext <- tcDerivingContext plan = plan {tcDerivingContext = TcDerivingExplicitContext []} | otherwise = plan};
data DerivingEnv = DerivingEnv { derivingEnvInstances :: !(Map Text [InstanceInfo]), derivingEnvPlans :: !(Map Text [TcDerivingPlan]), derivingEnvContexts :: !(Map PlanKey (Either Pred [Pred])) };
derivingEnv :: TcKinds -> [InstanceInfo] -> [TcDerivingPlan] -> TcM DerivingEnv;
derivingEnv kinds existingInstances plans = do {
  contexts <- solveContexts (length inferable + 2) (Map.fromList (map initialContext inferable));
  pure base {derivingEnvContexts = contexts}} where {
  base = DerivingEnv {derivingEnvInstances = groupByClass iiClassName existingInstances, derivingEnvPlans = groupByClass tcDerivingClassName plans, derivingEnvContexts = Map.empty};
  groupByClass className = Map.fromListWith (flip (<>)) . map (\value -> (className value, [value]));
  inferable = [(plan, obligations) | plan <- plans, Just (Right obligations) <- [inferableObligations kinds plan]];
  initialContext (plan, _) | reusesInstance (tcDerivingStrategy plan) = (planKey plan, Left (planPredicate plan)) | otherwise = (planKey plan, Right []);
  solveContexts :: Int -> Map PlanKey (Either Pred [Pred]) -> TcM (Map PlanKey (Either Pred [Pred]));
  solveContexts fuel contexts | fuel <= 0 = pure contexts | otherwise = do {
  let {
  environment = base {derivingEnvContexts = contexts}};
  next <- Map.fromList <$> mapM (\(plan, obligations) -> (,) (planKey plan) . fmap nub <$> simplifyPredicates kinds environment plan obligations) inferable;
  if next == contexts then pure contexts else solveContexts (fuel - 1) next}};
inferableObligations :: TcKinds -> TcDerivingPlan -> Maybe (Either String [Pred]);
inferableObligations kinds plan = case tcDerivingContext plan of {
  TcDerivingInferContext -> derivingObligations kinds plan;
  TcDerivingExplicitContext {} -> Nothing};
derivingObligations :: TcKinds -> TcDerivingPlan -> Maybe (Either String [Pred]);
derivingObligations kinds plan = case tcDerivingStrategy plan of {
  TcDerivingAnyclass -> Just (Right (anyClassObligations kinds plan));
  TcDerivingStock | generatesStockMethods (tcDerivingClassName plan), Just shape <- stockClassObligationsOf (tcDerivingClassName plan) -> Just $ case shape of {
  FieldObligations -> map (ClassPred (tcDerivingClassTyCon plan) . (: [])) . concat <$> stockFieldTypes plan;
  FunctorialObligations -> functorialObligations plan;
  NoObligations -> Right []} | otherwise -> Nothing;
  TcDerivingNewtype -> Just (coercedObligations kinds plan <$> newtypeRepresentation plan);
  TcDerivingVia viaType -> Just (Right (coercedObligations kinds plan viaType))};
reusesInstance :: TcDerivingStrategy -> Bool;
reusesInstance strategy = case strategy of {
  TcDerivingAnyclass -> False;
  TcDerivingNewtype -> True;
  TcDerivingVia {} -> True;
  TcDerivingStock -> False};
coercedObligations :: TcKinds -> TcDerivingPlan -> TcType -> [Pred];
coercedObligations kinds plan source = supers <> methods where {
  substitution = Map.fromList (zip (map tvUnique (tcDerivingClassTyVars plan)) (tcDerivingHeadTypes plan));
  supers = mapMaybe (constraintTypeToPred kinds . applySubst substitution . tcDictBinderType) (tcDerivingClassSuperClasses plan);
  methods = [ClassPred (tcDerivingClassTyCon plan) (init (tcDerivingHeadTypes plan) <> [source]) | not (null (tcDerivingClassMethods plan))]};
inferPlanContext :: TcKinds -> DerivingEnv -> TcDerivingPlan -> TcM TcDerivingPlan;
inferPlanContext kinds environment plan = case inferableObligations kinds plan of {
  Nothing -> pure plan;
  Just (Left message) -> do {
  emitError (tcDerivingSourceSpan plan) (OtherError message);
  pure plan};
  Just (Right _) -> case inferredContext environment plan of {
  Left predicate -> do {
  emitError (tcDerivingSourceSpan plan) (UnsolvedWanted predicate (InstOrigin (tcDerivingClassName plan)));
  pure plan};
  Right context -> pure plan {tcDerivingContext = TcDerivingExplicitContext context}}};
inferredContext :: DerivingEnv -> TcDerivingPlan -> Either Pred [Pred];
inferredContext environment plan = Map.findWithDefault (Left (planPredicate plan)) (planKey plan) (derivingEnvContexts environment);
simplifyPredicates :: TcKinds -> DerivingEnv -> TcDerivingPlan -> [Pred] -> TcM (Either Pred [Pred]);
simplifyPredicates kinds environment owner = foldM step (Right []) where {
  step (Left blocked) _ = pure (Left blocked);
  step (Right context) predicate = fmap (context <>) <$> simplifyPredicate kinds environment owner predicate};
simplifyPredicate :: TcKinds -> DerivingEnv -> TcDerivingPlan -> Pred -> TcM (Either Pred [Pred]);
simplifyPredicate kinds environment owner rawPredicate = do {
  predicate <- reducePredFamilies rawPredicate;
  simplifyReducedPredicate kinds environment owner predicate};
simplifyReducedPredicate :: TcKinds -> DerivingEnv -> TcDerivingPlan -> Pred -> TcM (Either Pred [Pred]);
simplifyReducedPredicate kinds environment owner predicate | isBareVariablePredicate (tcDerivingTyVars owner) predicate = pure (Right [predicate]) | ClassPred typeableTyCon _ <- predicate, Just arguments <- typeableArguments predicate = simplifyPredicates kinds environment owner (map (ClassPred typeableTyCon . (: [])) arguments) | otherwise = do {
  matched <- firstSuccessful (map simplifyExisting matchingExisting <> map simplifyDerived matchingDerived);
  pure $ case matched of {
  Just context -> Right context;
  Nothing | isAdmissibleContextPredicate owner predicate -> Right [predicate] | otherwise -> Left predicate}} where {
  className = predClassName predicate;
  matchingExisting = [(instanceInfo, substitution) | instanceInfo <- Map.findWithDefault [] className (derivingEnvInstances environment), predIsForInstance predicate instanceInfo, Just substitution <- [matchTypes (iiHead instanceInfo) (predArguments predicate)]];
  matchingDerived = [(candidate, substitution) | candidate <- Map.findWithDefault [] className (derivingEnvPlans environment), predIsForPlan predicate candidate, Just substitution <- [matchTypes (tcDerivingHeadTypes candidate) (predArguments predicate)]];
  simplifyExisting (instanceInfo, substitution) = simplifyPredicates kinds environment owner (map (applySubstPred substitution) (iiContext instanceInfo));
  simplifyDerived (candidate, substitution) = case candidateContext candidate of {
  Left blocked -> pure (Left blocked);
  Right context -> simplifyPredicates kinds environment owner (map (applySubstPred substitution) context)};
  candidateContext candidate = case tcDerivingContext candidate of {
  TcDerivingExplicitContext context -> Right context;
  TcDerivingInferContext -> case Map.lookup (planKey candidate) (derivingEnvContexts environment) of {
  Just context -> context;
  Nothing -> Left predicate}}};
firstSuccessful :: (Monad m) => [m (Either error value)] -> m (Maybe value);
firstSuccessful alternatives = case alternatives of {
  [] -> pure Nothing;
  alternative : rest -> do {
  result <- alternative;
  case result of {
  Left _ -> firstSuccessful rest;
  Right value -> pure (Just value)}}};
anyClassObligations :: TcKinds -> TcDerivingPlan -> [Pred];
anyClassObligations kinds plan = mapMaybe (constraintTypeToPred kinds . applySubst substitution . tcDictBinderType) (tcDerivingClassSuperClasses plan) <> concat [map (applySubstPred substitution) predicates | (methodName, predicates) <- tcDerivingDefaultSignatures plan, methodName `elem` tcDerivingDefaultMethods plan] where {
  substitution = Map.fromList [(tvUnique tyVar, headType) | (tyVar, headType) <- zip (tcDerivingClassTyVars plan) (tcDerivingHeadTypes plan)]};
functorialObligations :: TcDerivingPlan -> Either String [Pred];
functorialObligations plan = do {
  (parameter, fields) <- stockFunctorialFields plan;
  uses <- mapM (fieldUse mechanism parameter) (concat fields);
  pure (nub (concatMap (fieldUseObligations (tcDerivingClassTyCon plan)) uses))} where {
  mechanism = "stock " <> T.unpack (tcDerivingClassName plan) <> " deriving"};
stockFunctorialFields :: TcDerivingPlan -> Either String (TyVarId, [[TcType]]);
stockFunctorialFields plan = do {
  dataType <- maybe (Left (mechanism <> " requires checked datatype metadata")) Right (tcDerivingDataType plan);
  targetArguments <- targetTypeArguments mechanism dataType plan;
  validateStockDataType mechanism dataType;
  parameter <- case drop (length targetArguments) (dtiTyVars dataType) of {
  [parameter] -> Right parameter;
  _ -> Left (mechanism <> " requires a datatype whose last parameter the instance head leaves out")};
  let {
  substitution = Map.fromList [(tvUnique tyVar, argument) | (tyVar, argument) <- zip (dtiTyVars dataType) targetArguments]};
  pure (parameter, [[applySubst substitution (dcfiType field) | field <- dciFields constructor] | constructor <- dtiConstructors dataType])} where {
  mechanism = "stock " <> T.unpack (tcDerivingClassName plan) <> " deriving"};
stockFieldTypes :: TcDerivingPlan -> Either String [[TcType]];
stockFieldTypes plan = do {
  dataType <- maybe (Left (mechanism <> " requires checked datatype metadata")) Right (tcDerivingDataType plan);
  targetArguments <- targetTypeArguments mechanism dataType plan;
  validateStockDataType mechanism dataType;
  let {
  substitution = Map.fromList [(tvUnique tyVar, argument) | (tyVar, argument) <- zip (dtiTyVars dataType) targetArguments]};
  pure [[applySubst substitution (dcfiType field) | field <- dciFields constructor] | constructor <- dtiConstructors dataType]} where {
  mechanism = "stock " <> T.unpack (tcDerivingClassName plan) <> " deriving"};
newtypeRepresentation :: TcDerivingPlan -> Either String TcType;
newtypeRepresentation plan = do {
  dataType <- maybe (Left "newtype deriving requires checked datatype metadata") Right (tcDerivingDataType plan);
  constructor <- case (dtiFlavor dataType, dtiConstructors dataType) of {
  (NewtypeTyCon, [constructor]) -> Right constructor;
  _ -> Left "newtype deriving requires a newtype declaration"};
  field <- case dciFields constructor of {
  [field] -> Right (dcfiType field);
  _ -> Left "newtype deriving requires a newtype with one field"};
  targetArguments <- targetTypeArguments "newtype deriving" dataType plan;
  let {
  supplied = length targetArguments;
  substitution = Map.fromList [(tvUnique tyVar, argument) | (tyVar, argument) <- zip (dtiTyVars dataType) targetArguments];
  dropped = drop supplied (dtiTyVars dataType)};
  case etaReduce dropped (applySubst substitution field) of {
  Just representation | not (any (`elem` typeTyVars representation) dropped) -> Right representation;
  _ -> Left "newtype deriving cannot eta-reduce the representation type to the instance head"}};
etaReduce :: [TyVarId] -> TcType -> Maybe TcType;
etaReduce [] ty = Just ty;
etaReduce tyVars ty = case (ty, last tyVars) of {
  (TcAppTy function (TcTyVar argument), expected) | argument == expected -> etaReduce (init tyVars) function;
  (TcTyCon tyCon arguments, expected) | not (null arguments), TcTyVar argument <- last arguments, argument == expected -> etaReduce (init tyVars) (TcTyCon tyCon (init arguments));
  _ -> Nothing};
targetTypeArguments :: String -> DataTypeInfo -> TcDerivingPlan -> Either String [TcType];
targetTypeArguments mechanism dataType plan = case reverse (tcDerivingHeadTypes plan) of {
  TcTyCon tyCon arguments : _ | tyConKey tyCon == tyConKey (dtiTyCon dataType), length arguments <= length (dtiTyVars dataType) -> Right arguments;
  _ -> Left (mechanism <> " target does not match its checked datatype metadata")};
validateStockDataType :: String -> DataTypeInfo -> Either String ();
validateStockDataType mechanism dataType | dtiFlavor dataType `notElem` [DataTyCon, NewtypeTyCon] = Left (mechanism <> " requires a data or newtype declaration") | null constructors = Left (mechanism <> " does not yet support empty data declarations") | not (all (null . dciExTyVars) constructors) = Left (mechanism <> " does not yet support existential constructors") | not (all (null . dciTheta) constructors) = Left (mechanism <> " does not yet support constrained constructors") | any ((/= expectedResult) . dciResTy) constructors = Left (mechanism <> " does not yet support refined GADT result types") | otherwise = Right () where {
  constructors = dtiConstructors dataType;
  expectedResult = TcTyCon (dtiTyCon dataType) (map TcTyVar (dtiTyVars dataType))};
moduleDerivingPlans :: Module -> [TcDerivingPlan];
moduleDerivingPlans = concatMap declDerivingPlans . moduleDecls;
declDerivingPlans :: Decl -> [TcDerivingPlan];
declDerivingPlans decl = case decl of {
  DeclAnn annotation inner -> maybe [] tcDerivingPlans (fromAnnotation @TcDerivingAnnotation annotation) <> declDerivingPlans inner;
  _ -> []};
replaceModulePlans :: [TcDerivingPlan] -> Module -> Module;
replaceModulePlans plans modu = modu {moduleDecls = map replaceDecl (moduleDecls modu)} where {
  replaceDecl decl = case decl of {
  DeclAnn annotation inner | Just derivingAnnotation <- fromAnnotation @TcDerivingAnnotation annotation -> DeclAnn (mkAnnotation (derivingAnnotation {tcDerivingPlans = map replacePlan (tcDerivingPlans derivingAnnotation)})) (replaceDecl inner) | otherwise -> DeclAnn annotation (replaceDecl inner);
  _ -> decl};
  replacePlan original = fromMaybe original (find ((== planKey original) . planKey) plans)};
type PlanKey = (TyCon, [TcType]);
planKey :: TcDerivingPlan -> PlanKey;
planKey plan = (tcDerivingClassTyCon plan, tcDerivingHeadTypes plan);
planPredicate :: TcDerivingPlan -> Pred;
planPredicate plan = ClassPred (tcDerivingClassTyCon plan) (tcDerivingHeadTypes plan);
predIsForInstance :: Pred -> InstanceInfo -> Bool;
predIsForInstance predicate instanceInfo = case predicate of {
  ClassPred classTyCon _ -> instanceIsForClass classTyCon instanceInfo;
  _ -> iiClassName instanceInfo == predClassName predicate};
predIsForPlan :: Pred -> TcDerivingPlan -> Bool;
predIsForPlan predicate plan = case predicate of {
  ClassPred classTyCon _ -> tyConKey classTyCon == tyConKey (tcDerivingClassTyCon plan);
  _ -> tcDerivingClassName plan == predClassName predicate};
predClassName :: Pred -> Text;
predClassName predicate = case predicate of {
  ClassPred className _ -> tyConName className;
  EqPred {} -> "~";
  QuantifiedPred {} -> "quantified";
  IParamPred name _ -> name;
  IrredPred {} -> "irreducible"};
predArguments :: Pred -> [TcType];
predArguments predicate = case predicate of {
  ClassPred _ arguments -> arguments;
  EqPred left right -> [left, right];
  QuantifiedPred _ antecedents consequent -> concatMap predArguments antecedents <> predArguments consequent;
  IParamPred _ payload -> [payload];
  IrredPred constraint -> [constraint]};
typeableArguments :: Pred -> Maybe [TcType];
typeableArguments predicate = case predicate of {
  ClassPred classTyCon [ty] | tyConName classTyCon == "Typeable" -> case ty of {
  TcTyCon _ arguments -> Just arguments;
  TcFunTy argument result -> Just [argument, result];
  TcTyVar {} -> Nothing;
  TcMetaTv {} -> Nothing;
  TcArrowTy -> Nothing;
  TcTyLit {} -> Nothing;
  TcForAllTy {} -> Nothing;
  TcQualTy {} -> Nothing;
  TcAppTy {} -> Nothing};
  _ -> Nothing};
isBareVariablePredicate :: [TyVarId] -> Pred -> Bool;
isBareVariablePredicate tyVars predicate = case predicate of {
  ClassPred _ arguments -> not (null arguments) && all isPlanTyVar arguments;
  EqPred {} -> False;
  QuantifiedPred {} -> False;
  IParamPred {} -> False;
  IrredPred {} -> False} where {
  isPlanTyVar (TcTyVar tyVar) = tyVar `elem` tyVars;
  isPlanTyVar _ = False};
isAdmissibleContextPredicate :: TcDerivingPlan -> Pred -> Bool;
isAdmissibleContextPredicate plan predicate = not (null mentionedVariables) && all (`elem` tcDerivingTyVars plan) mentionedVariables && maybe True (not . predicateMentionsTyCon predicate) (derivedTargetTyCon plan) where {
  mentionedVariables = predTyVars predicate};
derivedTargetTyCon :: TcDerivingPlan -> Maybe Text;
derivedTargetTyCon plan = case reverse (tcDerivingHeadTypes plan) of {
  target : _ -> typeHeadTyCon target;
  [] -> Nothing};
typeHeadTyCon :: TcType -> Maybe Text;
typeHeadTyCon ty = case ty of {
  TcTyCon tyCon _ -> Just (tyConName tyCon);
  TcAppTy function _ -> typeHeadTyCon function;
  _ -> Nothing};
predicateMentionsTyCon :: Pred -> Text -> Bool;
predicateMentionsTyCon predicate name = any (typeMentionsTyCon name) (predArguments predicate);
typeMentionsTyCon :: Text -> TcType -> Bool;
typeMentionsTyCon name ty = case ty of {
  TcTyVar {} -> False;
  TcMetaTv {} -> False;
  TcArrowTy -> False;
  TcTyLit {} -> False;
  TcTyCon tyCon arguments -> tyConName tyCon == name || any (typeMentionsTyCon name) arguments;
  TcFunTy argument result -> typeMentionsTyCon name argument || typeMentionsTyCon name result;
  TcForAllTy _ body -> typeMentionsTyCon name body;
  TcQualTy predicates body -> any (`predicateMentionsTyCon` name) predicates || typeMentionsTyCon name body;
  TcAppTy function argument -> typeMentionsTyCon name function || typeMentionsTyCon name argument};
predTyVars :: Pred -> [TyVarId];
predTyVars predicate = case predicate of {
  QuantifiedPred variables antecedents consequent -> filter (`notElem` variables) (nub (concatMap predTyVars antecedents <> predTyVars consequent));
  _ -> nub (concatMap typeTyVars (predArguments predicate))};
typeTyVars :: TcType -> [TyVarId];
typeTyVars ty = case ty of {
  TcTyVar tyVar -> [tyVar];
  TcMetaTv {} -> [];
  TcArrowTy -> [];
  TcTyLit {} -> [];
  TcTyCon _ arguments -> concatMap typeTyVars arguments;
  TcFunTy argument result -> typeTyVars argument <> typeTyVars result;
  TcForAllTy tyVar body -> filter (/= tyVar) (typeTyVars body);
  TcQualTy predicates body -> concatMap predTyVars predicates <> typeTyVars body;
  TcAppTy function argument -> typeTyVars function <> typeTyVars argument}
}
