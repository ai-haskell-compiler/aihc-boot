{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module Aihc.Tc.Types (TcTermKey(..), tyConTermKey, tyConMemberTermKey, termKeyName, Unique(..), TyVarId(TyVarId, tvName, tvUnique), mkTyVarId, tvKind, sameTyVar, TypeShape, PredShape, typeShape, predShape, sameType, samePred, setTyVarKind, TcType(..), isPolyType, TcTypeKey(..), TcAxiomKey(..), TcKindEnv, TyCon(TyCon, tyConName, tyConArity), tyConKey, tyConPackageId, tyConModuleName, TcKinds(..), isEqualityTyCon, mkAppTy, tyConNamespace, mkTyConWithOrigin, mkTyConWithNamespace, TypeScheme(.., ForAll), specifiedScheme, traverseScheme, defaultMethodWorkerScheme, typeSchemeInferred, typeSchemeSpecified, TyLit(..), tyLitKind, tyLitKindTyCon, typeKindInEnv, TcTypeApplicationKinds(..), typeApplicationKinds, typeKind, constraintKind, runtimeRepKind, mkTYPEKind, liftedRep, tupleRep, sumRep, intRep, int8Rep, int16Rep, int32Rep, int64Rep, wordRep, word8Rep, word16Rep, word32Rep, word64Rep, addrRep, runtimeRepFromKind, isFixedRuntimeRep, runtimeRepOfTypeInEnv, isUnliftedTypeInEnv, pattern KTYPE, pattern KConstraint, pattern KRuntimeRep, pattern KLevity, pattern KVecCount, pattern KVecElem, pattern KFun, pattern KMeta, pattern KType, pattern BoxedRep, pattern TupleRep, pattern SumRep, pattern VecRep, pattern Lifted, pattern Unlifted, pattern IntRep, pattern Int8Rep, pattern Int16Rep, pattern Int32Rep, pattern Int64Rep, pattern WordRep, pattern Word8Rep, pattern Word16Rep, pattern Word32Rep, pattern Word64Rep, pattern AddrRep, pattern FloatRep, pattern DoubleRep, typeSchemeBody, applySubst, applySubstPred, typeMentionsTyVar, predicateMentionsTyVar, typeMentionsMeta, predicateMentionsMeta, kindMentionsUnique, Pred(..), constraintTypeToPred, collectForAllTypes, collectTypeApplications, isImplicitParamTyConName) where {
import Aihc.Resolve (PackageId(..), ResolutionNamespace(..));
import Control.DeepSeq (NFData(..));
import Control.Monad (zipWithM);
import Data.Map.Strict (Map);
import Data.Map.Strict qualified as Map;
import Data.Text (Text);
import Data.Text qualified as T;
import GHC.Generics (Generic);
data TcTermKey = TcTermLocal !Int | TcTermGlobal !PackageId !Text !Text deriving (Eq, Ord, Show, Read, Generic);
instance NFData TcTermKey where {};
tyConTermKey :: TyCon -> TcTermKey;
tyConTermKey tyCon = tyConMemberTermKey tyCon (tyConName tyCon);
tyConMemberTermKey :: TyCon -> Text -> TcTermKey;
tyConMemberTermKey tyCon = TcTermGlobal (tyConPackageId tyCon) (tyConModuleName tyCon);
termKeyName :: TcTermKey -> Text;
termKeyName key = case key of {
  TcTermGlobal _ _ name -> name;
  TcTermLocal unique -> T.pack ("<local " <> show unique <> ">")};
newtype Unique = Unique Int deriving (Eq, Ord, Show, Read, Generic);
instance NFData Unique where {
  rnf unique = unique `seq` ()};
data TyVarId = TyVarIdInternal !Text !Unique !TcType deriving (Eq, Ord, Show, Read, Generic);
instance NFData TyVarId where {
  rnf (TyVarIdInternal _ _ kind) = rnf kind};
tyVarIdentity :: TyVarId -> (Unique, Text);
tyVarIdentity (TyVarIdInternal name unique _) = (unique, name);
pattern TyVarId :: Text -> Unique -> TyVarId;
pattern TyVarId {tvName, tvUnique} <- TyVarIdInternal tvName tvUnique _;
mkTyVarId :: Text -> Unique -> TcType -> TyVarId;
mkTyVarId = TyVarIdInternal;
tvKind :: TyVarId -> TcType;
tvKind (TyVarIdInternal _ _ kind) = kind;
setTyVarKind :: TcType -> TyVarId -> TyVarId;
setTyVarKind kind (TyVarIdInternal name unique _) = TyVarIdInternal name unique kind;
data TyCon = TyConInternal !Text !PackageId !Text !ResolutionNamespace !Int deriving (Eq, Ord, Show, Read, Generic);
instance NFData TyCon where {
  rnf tyCon = tyCon `seq` ()};
pattern TyCon :: Text -> Int -> TyCon;
pattern TyCon {tyConName, tyConArity} <- TyConInternal tyConName _ _ _ tyConArity;
data TcTypeKey = TcTypeKey { typeKeyName :: !Text, typeKeyPackage :: !PackageId, typeKeyModule :: !Text, typeKeyNamespace :: !ResolutionNamespace } deriving (Eq, Ord, Show, Read, Generic);
instance NFData TcTypeKey where {
  rnf key = key `seq` ()};
data TcAxiomKey = TcAxiomKey { axiomKeyPackage :: !PackageId, axiomKeyModule :: !Text, axiomKeyName :: !Text } deriving (Eq, Ord, Show, Read, Generic);
instance NFData TcAxiomKey where {
  rnf key = key `seq` ()};
type TcKindEnv = Map TcTypeKey TypeScheme;
tyConPackageId :: TyCon -> PackageId;
tyConPackageId (TyConInternal _ packageId _ _ _) = packageId;
tyConModuleName :: TyCon -> Text;
tyConModuleName (TyConInternal _ _ moduleName _ _) = moduleName;
mkAppTy :: TcType -> TcType -> TcType;
mkAppTy function argument = case function of {
  TcTyCon tyCon arguments -> TcTyCon tyCon (arguments <> [argument]);
  TcAppTy TcArrowTy domain -> TcFunTy domain argument;
  _ -> TcAppTy function argument};
tyConNamespace :: TyCon -> ResolutionNamespace;
tyConNamespace (TyConInternal _ _ _ namespace _) = namespace;
tyConKey :: TyCon -> TcTypeKey;
tyConKey tyCon = TcTypeKey (tyConName tyCon) (tyConPackageId tyCon) (tyConModuleName tyCon) (tyConNamespace tyCon);
mkTyConWithOrigin :: PackageId -> Text -> Text -> Int -> TyCon;
mkTyConWithOrigin packageId moduleName name = TyConInternal name packageId moduleName ResolutionNamespaceType;
mkTyConWithNamespace :: ResolutionNamespace -> PackageId -> Text -> Text -> Int -> TyCon;
mkTyConWithNamespace namespace packageId moduleName name = TyConInternal name packageId moduleName namespace;
data TcType = TcTyVar !TyVarId | TcMetaTv !Unique | TcTyCon !TyCon ![TcType] | TcArrowTy | TcFunTy !TcType !TcType | TcForAllTy !TyVarId !TcType | TcQualTy ![Pred] !TcType | TcAppTy !TcType !TcType | TcTyLit !TyLit deriving (Eq, Ord, Show, Read, Generic);
instance NFData TcType where {
  rnf ty = case ty of {
  TcTyVar tyVar -> rnf tyVar;
  TcMetaTv {} -> ();
  TcTyCon _ arguments -> rnf arguments;
  TcArrowTy -> ();
  TcFunTy argument result -> rnf argument `seq` rnf result;
  TcForAllTy tyVar body -> rnf tyVar `seq` rnf body;
  TcQualTy predicates body -> rnf predicates `seq` rnf body;
  TcAppTy function argument -> rnf function `seq` rnf argument;
  TcTyLit {} -> ()}};
data TyLit = TyLitNat !Integer | TyLitSymbol !Text | TyLitChar !Char deriving (Eq, Ord, Show, Read, Generic);
instance NFData TyLit where {
  rnf literal = literal `seq` ()};
data TypeScheme = Scheme ![TyVarId] ![TyVarId] ![Pred] !TcType deriving (Eq, Ord, Show, Read, Generic);
instance NFData TypeScheme where {
  rnf (Scheme inferred specified predicates body) = rnf inferred `seq` rnf specified `seq` rnf predicates `seq` rnf body};
pattern ForAll :: [TyVarId] -> [Pred] -> TcType -> TypeScheme;
pattern ForAll tyVars predicates body <- (schemeBinders -> (tyVars, predicates, body));
schemeBinders :: TypeScheme -> ([TyVarId], [Pred], TcType);
schemeBinders (Scheme inferred specified predicates body) = (inferred <> specified, predicates, body);
specifiedScheme :: [TyVarId] -> [Pred] -> TcType -> TypeScheme;
specifiedScheme = Scheme [];
traverseScheme :: (Applicative f) => (TyVarId -> f TyVarId) -> (Pred -> f Pred) -> (TcType -> f TcType) -> TypeScheme -> f TypeScheme;
traverseScheme onTyVar onPred onBody (Scheme inferred specified predicates body) = Scheme <$> traverse onTyVar inferred <*> traverse onTyVar specified <*> traverse onPred predicates <*> onBody body;
defaultMethodWorkerScheme :: TypeScheme -> TypeScheme -> TypeScheme;
defaultMethodWorkerScheme ordinaryScheme scheme@(Scheme inferred specified predicates body) = case ordinaryScheme of {
  ForAll _ (classPredicate : _) _ -> Scheme inferred specified (classPredicate : predicates) body;
  _ -> scheme};
typeSchemeInferred :: TypeScheme -> [TyVarId];
typeSchemeInferred (Scheme inferred _ _ _) = inferred;
typeSchemeSpecified :: TypeScheme -> [TyVarId];
typeSchemeSpecified (Scheme _ specified _ _) = specified;
isPolyType :: TcType -> Bool;
isPolyType TcForAllTy {} = True;
isPolyType TcQualTy {} = True;
isPolyType _ = False;
typeSchemeBody :: TypeScheme -> TcType;
typeSchemeBody (ForAll _ _ body) = body;
data Pred = ClassPred !TyCon ![TcType] | EqPred !TcType !TcType | QuantifiedPred ![TyVarId] ![Pred] !Pred | IParamPred !Text !TcType | IrredPred !TcType deriving (Eq, Ord, Show, Read, Generic);
instance NFData Pred where {
  rnf predicate = case predicate of {
  ClassPred _ arguments -> rnf arguments;
  EqPred left right -> rnf left `seq` rnf right;
  QuantifiedPred variables antecedents consequent -> rnf variables `seq` rnf antecedents `seq` rnf consequent;
  IParamPred _ payload -> rnf payload;
  IrredPred constraint -> rnf constraint}};
constraintTypeToPred :: TcKinds -> TcType -> Maybe Pred;
constraintTypeToPred kinds ty = case collectForAllTypes ty of {
  (variables@(_ : _), qualified) -> do {
  let {
  (antecedents, consequentType) = case qualified of {
  TcQualTy predicates body -> (predicates, body);
  body -> ([], body)}};
  consequent <- atomicConstraintTypeToPred kinds consequentType;
  pure (QuantifiedPred variables antecedents consequent)};
  ([], body) -> atomicConstraintTypeToPred kinds body};
atomicConstraintTypeToPred :: TcKinds -> TcType -> Maybe Pred;
atomicConstraintTypeToPred kinds ty = case collectTypeApplications ty of {
  (TcTyCon tyCon headArgs, arguments) | isEqualityTyCon kinds tyCon, [left, right] <- headArgs <> arguments -> Just (EqPred left right);
  (TcTyCon tyCon [payload], []) | isImplicitParamTyConName (tyConName tyCon) -> Just (IParamPred (tyConName tyCon) payload);
  (TcTyCon tyCon headArgs, arguments) -> Just (ClassPred tyCon (headArgs <> arguments));
  _ -> Nothing};
isEqualityTyCon :: TcKinds -> TyCon -> Bool;
isEqualityTyCon kinds tyCon = tyCon == kindsEqualityTyCon kinds || tyCon == mkTyConWithOrigin (tyConPackageId equality) (tyConModuleName equality) "~~" 2 where {
  equality = kindsEqualityTyCon kinds};
isImplicitParamTyConName :: Text -> Bool;
isImplicitParamTyConName = T.isPrefixOf "?";
collectForAllTypes :: TcType -> ([TyVarId], TcType);
collectForAllTypes (TcForAllTy variable body) = let {
  (variables, result) = collectForAllTypes body} in (variable : variables, result);
collectForAllTypes ty = ([], ty);
collectTypeApplications :: TcType -> (TcType, [TcType]);
collectTypeApplications ty = case ty of {
  TcAppTy function argument -> let {
  (headType, arguments) = collectTypeApplications function} in (headType, arguments <> [argument]);
  _ -> (ty, [])};
data TcKinds = TcKinds { kindsTyCon :: Text -> Int -> TyCon, kindsDataCon :: Text -> Int -> TyCon, kindsEqualityTyCon :: TyCon, kindsArrowTyCon :: TyCon, kindsListTyCon :: TyCon, kindsListDeclaration :: TyCon, kindsNilDataCon :: TyCon, kindsConsDataCon :: TyCon, kindsNaturalTyCon :: TyCon, kindsSymbolTyCon :: TyCon, kindsCharTyCon :: TyCon };
tyLitKind :: TcKinds -> TyLit -> TcType;
tyLitKind kinds literal = TcTyCon (tyLitKindTyCon kinds literal) [];
tyLitKindTyCon :: TcKinds -> TyLit -> TyCon;
tyLitKindTyCon kinds literal = case literal of {
  TyLitNat {} -> kindsNaturalTyCon kinds;
  TyLitSymbol {} -> kindsSymbolTyCon kinds;
  TyLitChar {} -> kindsCharTyCon kinds};
instance Show TcKinds where {
  show _ = "TcKinds"};
typeKind :: TcKinds -> TcType;
typeKind kinds = mkTYPEKind kinds (liftedRep kinds);
constraintKind :: TcKinds -> TcType;
constraintKind kinds = TcTyCon (kindsTyCon kinds "Constraint" 0) [];
runtimeRepKind :: TcKinds -> TcType;
runtimeRepKind kinds = TcTyCon (kindsTyCon kinds "RuntimeRep" 0) [];
mkTYPEKind :: TcKinds -> TcType -> TcType;
mkTYPEKind kinds representation = TcTyCon (kindsTyCon kinds "TYPE" 1) [representation];
nullaryRep :: TcKinds -> Text -> TcType;
nullaryRep kinds name = TcTyCon (kindsDataCon kinds name 0) [];
liftedRep :: TcKinds -> TcType;
liftedRep kinds = boxedRep kinds (nullaryRep kinds "Lifted");
intRep, int8Rep, int16Rep, int32Rep, int64Rep :: TcKinds -> TcType;
wordRep, word8Rep, word16Rep, word32Rep, word64Rep, addrRep :: TcKinds -> TcType;
intRep kinds = nullaryRep kinds "IntRep";
int8Rep kinds = nullaryRep kinds "Int8Rep";
int16Rep kinds = nullaryRep kinds "Int16Rep";
int32Rep kinds = nullaryRep kinds "Int32Rep";
int64Rep kinds = nullaryRep kinds "Int64Rep";
wordRep kinds = nullaryRep kinds "WordRep";
word8Rep kinds = nullaryRep kinds "Word8Rep";
word16Rep kinds = nullaryRep kinds "Word16Rep";
word32Rep kinds = nullaryRep kinds "Word32Rep";
word64Rep kinds = nullaryRep kinds "Word64Rep";
addrRep kinds = nullaryRep kinds "AddrRep";
boxedRep :: TcKinds -> TcType -> TcType;
boxedRep kinds levity = TcTyCon (kindsDataCon kinds "BoxedRep" 1) [levity];
tupleRep :: TcKinds -> [TcType] -> TcType;
tupleRep kinds fields = TcTyCon (kindsDataCon kinds "TupleRep" 1) [dataConstructorList kinds fields];
sumRep :: TcKinds -> [TcType] -> TcType;
sumRep kinds fields = TcTyCon (kindsDataCon kinds "SumRep" 1) [dataConstructorList kinds fields];
dataConstructorList :: TcKinds -> [TcType] -> TcType;
dataConstructorList kinds = foldr cons nil where {
  nil = TcTyCon (kindsNilDataCon kinds) [];
  cons field rest = TcTyCon (kindsConsDataCon kinds) [field, rest]};
typeKindInEnv :: TcKinds -> TcKindEnv -> TcType -> Either String TcType;
typeKindInEnv kinds kindEnv = go where {
  go rawType = case rawType of {
  TcTyVar tyVar -> Right (tvKind tyVar);
  TcMetaTv {} -> Left "type still has a meta variable";
  TcTyCon tyCon arguments -> do {
  scheme <- maybe (Left ("missing kind scheme for type constructor: " <> T.unpack (tyConName tyCon))) Right (Map.lookup (tyConKey tyCon) kindEnv);
  applyArguments scheme arguments};
  TcArrowTy -> Right (KFun (typeKind kinds) (KFun (typeKind kinds) (typeKind kinds)));
  TcFunTy {} -> Right (typeKind kinds);
  TcForAllTy _ body -> go body;
  TcQualTy _ body -> go body;
  TcAppTy function argument -> do {
  functionKind <- go function;
  applyKind functionKind argument};
  TcTyLit literal -> Right (tyLitKind kinds literal)};
  applyArguments (ForAll quantified _ body) = applyMany (map tvUnique quantified) body;
  applyMany _ kind [] = Right kind;
  applyMany quantified kind (argument : rest) = do {
  kind' <- applyKindWith quantified kind argument;
  applyMany quantified kind' rest};
  applyKind = applyKindWith [];
  applyKindWith quantified (TcFunTy formal result) argument = do {
  actual <- go argument;
  substitution <- matchKinds quantified (argumentKindVariables argument) formal actual;
  Right (applySubst substitution result)};
  applyKindWith _ kind _ = Left ("type application uses a non-function kind: " <> show kind);
  argumentKindVariables argument = case argument of {
  TcTyCon tyCon _ -> case Map.lookup (tyConKey tyCon) kindEnv of {
  Just (ForAll variables _ _) -> map tvUnique variables;
  Nothing -> []};
  TcAppTy function _ -> argumentKindVariables function;
  _ -> []};
  matchKinds quantified argumentQuantified formal actual = case (formal, actual) of {
  (TcTyVar tyVar, _) | tvUnique tyVar `elem` quantified -> Right (Map.singleton (tvUnique tyVar) actual);
  (_, TcTyVar tyVar) | tvUnique tyVar `elem` argumentQuantified -> Right Map.empty;
  (KTYPE formalRep, KTYPE actualRep) -> recur formalRep actualRep;
  (TcFunTy left right, TcFunTy left' right') -> Map.union <$> recur left left' <*> recur right right';
  (TcTyCon left leftArguments, TcTyCon right rightArguments) | left == right, length leftArguments == length rightArguments -> Map.unions <$> zipWithM recur leftArguments rightArguments;
  _ | formal == actual -> Right Map.empty | otherwise -> Left ("kind mismatch: expected " <> show formal <> ", got " <> show actual)} where {
  recur = matchKinds quantified argumentQuantified}};
runtimeRepOfTypeInEnv :: TcKinds -> TcKindEnv -> TcType -> Either String TcType;
runtimeRepOfTypeInEnv kinds kindEnv ty = typeKindInEnv kinds kindEnv ty >>= runtimeRepFromKind;
isUnliftedTypeInEnv :: TcKinds -> TcKindEnv -> TcType -> Bool;
isUnliftedTypeInEnv kinds kindEnv ty = case runtimeRepOfTypeInEnv kinds kindEnv ty of {
  Right representation -> not (matchesLiftedRuntimeRep representation);
  Left _ -> False};
applySubst :: Map Unique TcType -> TcType -> TcType;
applySubst substitution = go where {
  go ty = case ty of {
  TcTyVar tyVar -> Map.findWithDefault (TcTyVar (setTyVarKind (go (tvKind tyVar)) tyVar)) (tvUnique tyVar) substitution;
  TcMetaTv {} -> ty;
  TcArrowTy -> ty;
  TcTyLit {} -> ty;
  TcTyCon tyCon arguments -> TcTyCon tyCon (map go arguments);
  TcFunTy argument result -> TcFunTy (go argument) (go result);
  TcForAllTy tyVar body -> TcForAllTy (setTyVarKind (go (tvKind tyVar)) tyVar) (applySubst (Map.delete (tvUnique tyVar) substitution) body);
  TcQualTy predicates body -> TcQualTy (map (applySubstPred substitution) predicates) (go body);
  TcAppTy function argument -> mkAppTy (go function) (go argument)}};
applySubstPred :: Map Unique TcType -> Pred -> Pred;
applySubstPred substitution predicate = case predicate of {
  ClassPred className arguments -> ClassPred className (map (applySubst substitution) arguments);
  EqPred left right -> EqPred (applySubst substitution left) (applySubst substitution right);
  IParamPred name payload -> IParamPred name (applySubst substitution payload);
  IrredPred constraint -> IrredPred (applySubst substitution constraint);
  QuantifiedPred variables antecedents consequent -> let {
  scopedSubstitution = foldr (Map.delete . tvUnique) substitution variables} in QuantifiedPred [setTyVarKind (applySubst scopedSubstitution (tvKind variable)) variable | variable <- variables] (map (applySubstPred scopedSubstitution) antecedents) (applySubstPred scopedSubstitution consequent)};
typeMentionsMeta :: TcType -> Bool;
typeMentionsMeta ty = case ty of {
  TcMetaTv {} -> True;
  TcTyVar {} -> False;
  TcArrowTy -> False;
  TcTyLit {} -> False;
  TcTyCon _ arguments -> any typeMentionsMeta arguments;
  TcFunTy argument result -> typeMentionsMeta argument || typeMentionsMeta result;
  TcForAllTy _ body -> typeMentionsMeta body;
  TcQualTy predicates body -> any predicateMentionsMeta predicates || typeMentionsMeta body;
  TcAppTy function argument -> typeMentionsMeta function || typeMentionsMeta argument};
predicateMentionsMeta :: Pred -> Bool;
predicateMentionsMeta predicate = case predicate of {
  ClassPred _ arguments -> any typeMentionsMeta arguments;
  EqPred left right -> typeMentionsMeta left || typeMentionsMeta right;
  IParamPred _ payload -> typeMentionsMeta payload;
  IrredPred constraint -> typeMentionsMeta constraint;
  QuantifiedPred variables antecedents consequent -> any (typeMentionsMeta . tvKind) variables || any predicateMentionsMeta antecedents || predicateMentionsMeta consequent};
typeMentionsTyVar :: TyVarId -> TcType -> Bool;
typeMentionsTyVar target ty = case ty of {
  TcTyVar tyVar -> sameTyVar tyVar target || kindMentionsUnique (tvUnique target) (tvKind tyVar);
  TcMetaTv {} -> False;
  TcArrowTy -> False;
  TcTyLit {} -> False;
  TcTyCon _ arguments -> any (typeMentionsTyVar target) arguments;
  TcFunTy argument result -> typeMentionsTyVar target argument || typeMentionsTyVar target result;
  TcForAllTy tyVar body -> not (sameTyVar tyVar target) && typeMentionsTyVar target body;
  TcQualTy predicates body -> any (predicateMentionsTyVar target) predicates || typeMentionsTyVar target body;
  TcAppTy function argument -> typeMentionsTyVar target function || typeMentionsTyVar target argument};
predicateMentionsTyVar :: TyVarId -> Pred -> Bool;
predicateMentionsTyVar target predicate = case predicate of {
  ClassPred _ arguments -> any (typeMentionsTyVar target) arguments;
  EqPred left right -> typeMentionsTyVar target left || typeMentionsTyVar target right;
  IParamPred _ payload -> typeMentionsTyVar target payload;
  IrredPred constraint -> typeMentionsTyVar target constraint;
  QuantifiedPred variables antecedents consequent -> not (any (sameTyVar target) variables) && (any (predicateMentionsTyVar target) antecedents || predicateMentionsTyVar target consequent)};
sameTyVar :: TyVarId -> TyVarId -> Bool;
sameTyVar left right = tyVarIdentity left == tyVarIdentity right;
data TypeShape = ShapeTyVar !Unique !Text | ShapeMetaTv !Unique | ShapeTyCon !TyCon ![TypeShape] | ShapeArrowTy | ShapeFunTy !TypeShape !TypeShape | ShapeForAllTy !Unique !Text !TypeShape | ShapeQualTy ![PredShape] !TypeShape | ShapeAppTy !TypeShape !TypeShape | ShapeTyLit !TyLit deriving (Eq, Ord, Show);
data PredShape = ShapeClassPred !TyCon ![TypeShape] | ShapeEqPred !TypeShape !TypeShape | ShapeQuantifiedPred ![(Unique, Text)] ![PredShape] !PredShape | ShapeIParamPred !Text !TypeShape | ShapeIrredPred !TypeShape deriving (Eq, Ord, Show);
typeShape :: TcType -> TypeShape;
typeShape ty = case ty of {
  TcTyVar tyVar -> ShapeTyVar (tvUnique tyVar) (tvName tyVar);
  TcMetaTv unique -> ShapeMetaTv unique;
  TcTyCon tyCon arguments -> ShapeTyCon tyCon (map typeShape arguments);
  TcArrowTy -> ShapeArrowTy;
  TcFunTy argument result -> ShapeFunTy (typeShape argument) (typeShape result);
  TcForAllTy tyVar body -> ShapeForAllTy (tvUnique tyVar) (tvName tyVar) (typeShape body);
  TcQualTy predicates body -> ShapeQualTy (map predShape predicates) (typeShape body);
  TcAppTy function argument -> ShapeAppTy (typeShape function) (typeShape argument);
  TcTyLit literal -> ShapeTyLit literal};
predShape :: Pred -> PredShape;
predShape predicate = case predicate of {
  ClassPred tyCon arguments -> ShapeClassPred tyCon (map typeShape arguments);
  EqPred left right -> ShapeEqPred (typeShape left) (typeShape right);
  QuantifiedPred variables antecedents consequent -> ShapeQuantifiedPred (map tyVarIdentity variables) (map predShape antecedents) (predShape consequent);
  IParamPred name payload -> ShapeIParamPred name (typeShape payload);
  IrredPred constraint -> ShapeIrredPred (typeShape constraint)};
sameType :: TcType -> TcType -> Bool;
sameType left right = typeShape left == typeShape right;
samePred :: Pred -> Pred -> Bool;
samePred left right = predShape left == predShape right;
kindMentionsUnique :: Unique -> TcType -> Bool;
kindMentionsUnique target kind = case kind of {
  TcTyVar tyVar -> tvUnique tyVar == target || kindMentionsUnique target (tvKind tyVar);
  TcMetaTv unique -> unique == target;
  TcArrowTy -> False;
  TcTyLit {} -> False;
  TcTyCon _ arguments -> any (kindMentionsUnique target) arguments;
  TcFunTy argument result -> kindMentionsUnique target argument || kindMentionsUnique target result;
  TcForAllTy tyVar body -> tvUnique tyVar /= target && kindMentionsUnique target body;
  TcQualTy predicates body -> any (predicateMentionsUnique target) predicates || kindMentionsUnique target body;
  TcAppTy function argument -> kindMentionsUnique target function || kindMentionsUnique target argument} where {
  predicateMentionsUnique unique predicate = case predicate of {
  ClassPred _ arguments -> any (kindMentionsUnique unique) arguments;
  EqPred left right -> kindMentionsUnique unique left || kindMentionsUnique unique right;
  IParamPred _ payload -> kindMentionsUnique unique payload;
  IrredPred constraint -> kindMentionsUnique unique constraint;
  QuantifiedPred variables antecedents consequent -> all ((/= unique) . tvUnique) variables && (any (predicateMentionsUnique unique) antecedents || predicateMentionsUnique unique consequent)}};
pattern KTYPE :: TcType -> TcType;
pattern KTYPE representation <- (matchTYPEKind -> Just representation);
pattern KConstraint, KRuntimeRep, KLevity, KVecCount, KVecElem, KType :: TcType;
pattern KConstraint <- (matchesNullary ResolutionNamespaceType "Constraint" -> True);
pattern KRuntimeRep <- (matchesNullary ResolutionNamespaceType "RuntimeRep" -> True);
pattern KLevity <- (matchesNullary ResolutionNamespaceType "Levity" -> True);
pattern KVecCount <- (matchesNullary ResolutionNamespaceType "VecCount" -> True);
pattern KVecElem <- (matchesNullary ResolutionNamespaceType "VecElem" -> True);
pattern KType <- (matchesLiftedTypeKind -> True);
pattern KFun :: TcType -> TcType -> TcType;
pattern KFun argument result = TcFunTy argument result;
pattern KMeta :: Unique -> TcType;
pattern KMeta unique = TcMetaTv unique;
matchTYPEKind :: TcType -> Maybe TcType;
matchTYPEKind kind = case kind of {
  TcTyCon tyCon [representation] | tyConName tyCon == "TYPE" -> Just representation;
  _ -> Nothing};
matchesLiftedTypeKind :: TcType -> Bool;
matchesLiftedTypeKind = maybe False matchesLiftedRuntimeRep . matchTYPEKind;
matchesLiftedRuntimeRep :: TcType -> Bool;
matchesLiftedRuntimeRep representation = case representation of {
  TcTyCon boxed [TcTyCon levity []] -> tyConNamespace boxed == ResolutionNamespaceTerm && tyConName boxed == "BoxedRep" && tyConNamespace levity == ResolutionNamespaceTerm && tyConName levity == "Lifted";
  _ -> False};
pattern BoxedRep :: TcType -> TcType;
pattern BoxedRep levity <- (matchUnaryRep "BoxedRep" -> Just levity);
pattern TupleRep :: [TcType] -> TcType;
pattern TupleRep fields <- (matchListRep "TupleRep" -> Just fields);
pattern SumRep :: [TcType] -> TcType;
pattern SumRep fields <- (matchListRep "SumRep" -> Just fields);
pattern VecRep :: TcType -> TcType -> TcType;
pattern VecRep count element <- (matchBinaryRep "VecRep" -> Just (count, element));
pattern Lifted, Unlifted :: TcType;
pattern Lifted <- (matchesNullary ResolutionNamespaceTerm "Lifted" -> True);
pattern Unlifted <- (matchesNullary ResolutionNamespaceTerm "Unlifted" -> True);
pattern IntRep, Int8Rep, Int16Rep, Int32Rep, Int64Rep :: TcType;
pattern WordRep, Word8Rep, Word16Rep, Word32Rep, Word64Rep :: TcType;
pattern AddrRep, FloatRep, DoubleRep :: TcType;
pattern IntRep <- (matchesNullary ResolutionNamespaceTerm "IntRep" -> True);
pattern Int8Rep <- (matchesNullary ResolutionNamespaceTerm "Int8Rep" -> True);
pattern Int16Rep <- (matchesNullary ResolutionNamespaceTerm "Int16Rep" -> True);
pattern Int32Rep <- (matchesNullary ResolutionNamespaceTerm "Int32Rep" -> True);
pattern Int64Rep <- (matchesNullary ResolutionNamespaceTerm "Int64Rep" -> True);
pattern WordRep <- (matchesNullary ResolutionNamespaceTerm "WordRep" -> True);
pattern Word8Rep <- (matchesNullary ResolutionNamespaceTerm "Word8Rep" -> True);
pattern Word16Rep <- (matchesNullary ResolutionNamespaceTerm "Word16Rep" -> True);
pattern Word32Rep <- (matchesNullary ResolutionNamespaceTerm "Word32Rep" -> True);
pattern Word64Rep <- (matchesNullary ResolutionNamespaceTerm "Word64Rep" -> True);
pattern AddrRep <- (matchesNullary ResolutionNamespaceTerm "AddrRep" -> True);
pattern FloatRep <- (matchesNullary ResolutionNamespaceTerm "FloatRep" -> True);
pattern DoubleRep <- (matchesNullary ResolutionNamespaceTerm "DoubleRep" -> True);
matchUnaryRep :: Text -> TcType -> Maybe TcType;
matchUnaryRep expected (TcTyCon tyCon [argument]) | tyConNamespace tyCon == ResolutionNamespaceTerm, tyConName tyCon == expected = Just argument;
matchUnaryRep _ _ = Nothing;
matchBinaryRep :: Text -> TcType -> Maybe (TcType, TcType);
matchBinaryRep expected (TcTyCon tyCon [left, right]) | tyConNamespace tyCon == ResolutionNamespaceTerm, tyConName tyCon == expected = Just (left, right);
matchBinaryRep _ _ = Nothing;
matchListRep :: Text -> TcType -> Maybe [TcType];
matchListRep expected (TcTyCon tyCon [listType]) | tyConNamespace tyCon == ResolutionNamespaceTerm, tyConName tyCon == expected = decodeDataConstructorList listType;
matchListRep _ _ = Nothing;
decodeDataConstructorList :: TcType -> Maybe [TcType];
decodeDataConstructorList ty = case ty of {
  TcTyCon tyCon [] | tyConNamespace tyCon == ResolutionNamespaceTerm, tyConName tyCon == "[]" -> Just [];
  TcTyCon tyCon [field, rest] | tyConNamespace tyCon == ResolutionNamespaceTerm, tyConName tyCon == ":" -> (field :) <$> decodeDataConstructorList rest;
  _ -> Nothing};
matchesNullary :: ResolutionNamespace -> Text -> TcType -> Bool;
matchesNullary namespace expected (TcTyCon tyCon []) = tyConNamespace tyCon == namespace && tyConName tyCon == expected;
matchesNullary _ _ _ = False;
runtimeRepFromKind :: TcType -> Either String TcType;
runtimeRepFromKind kind = case kind of {
  TcTyCon tyCon [representation] | tyConName tyCon == "TYPE" -> Right representation;
  _ -> Left ("type does not have a runtime representation: " <> show kind)};
isFixedRuntimeRep :: TcType -> Bool;
isFixedRuntimeRep representation = case representation of {
  BoxedRep levity -> promotedConstructorIsOneOf ["Lifted", "Unlifted"] levity;
  TupleRep fields -> all isFixedRuntimeRep fields;
  SumRep fields -> all isFixedRuntimeRep fields;
  VecRep count element -> promotedConstructorIsOneOf ["Vec2", "Vec4", "Vec8", "Vec16", "Vec32", "Vec64"] count && promotedConstructorIsOneOf ["Int8ElemRep", "Int16ElemRep", "Int32ElemRep", "Int64ElemRep", "Word8ElemRep", "Word16ElemRep", "Word32ElemRep", "Word64ElemRep", "FloatElemRep", "DoubleElemRep"] element;
  IntRep -> True;
  Int8Rep -> True;
  Int16Rep -> True;
  Int32Rep -> True;
  Int64Rep -> True;
  WordRep -> True;
  Word8Rep -> True;
  Word16Rep -> True;
  Word32Rep -> True;
  Word64Rep -> True;
  AddrRep -> True;
  FloatRep -> True;
  DoubleRep -> True;
  _ -> False} where {
  promotedConstructorIsOneOf names ty = case ty of {
  TcTyCon tyCon [] -> tyConNamespace tyCon == ResolutionNamespaceTerm && tyConName tyCon `elem` names;
  _ -> False}};
data TcTypeApplicationKinds = TcTypeApplicationKinds { tcInvisibleKindSubstitution :: Map Unique TcType, tcVisibleArgumentKinds :: [TcType], tcSkippedArguments :: [String] } deriving (Eq, Show, Read);
typeApplicationKinds :: TcKinds -> TcKindEnv -> TyCon -> [TcType] -> Maybe TcType -> Either String TcTypeApplicationKinds;
typeApplicationKinds kinds kindEnv tyCon arguments expectedKind = do {
  scheme <- maybe (Left ("missing kind scheme for type constructor: " <> T.unpack (tyConName tyCon))) Right (Map.lookup (tyConKey tyCon) kindEnv);
  let {
  ForAll quantified _ resultKind = scheme};
  let {
  quantifiedUniques = map tvUnique quantified;
  (argumentSubstitution, remainingKind, skipped) = go quantifiedUniques 0 Map.empty resultKind arguments;
  resultSubstitution = case expectedKind of {
  Just expected -> matchKind quantifiedUniques remainingKind expected;
  Nothing -> Map.empty};
  substitution = argumentSubstitution <> resultSubstitution};
  pure (TcTypeApplicationKinds substitution (argumentKinds (applySubst substitution resultKind)) skipped)} where {
  argumentKinds (KFun argument result) = argument : argumentKinds result;
  argumentKinds _ = [];
  go quantifiedUniques position substitution (KFun formal result) (argument : rest) = case typeKindInEnv kinds kindEnv argument of {
  Right argumentKind -> let {
  found = matchKind quantifiedUniques (applySubst substitution formal) argumentKind} in go quantifiedUniques (position + 1) (substitution <> found) (applySubst found result) rest;
  Left reason -> let {
  (substitution', kind, skipped) = go quantifiedUniques (position + 1) substitution result rest} in (substitution', kind, skippedArgument position reason : skipped)};
  go _ _ substitution kind _ = (substitution, applySubst substitution kind, []);
  skippedArgument position reason = "argument " <> show (position + 1 :: Int) <> " has no usable kind: " <> reason;
  matchKind quantifiedUniques (TcTyVar tyVar) actual | tvUnique tyVar `elem` quantifiedUniques = Map.singleton (tvUnique tyVar) actual;
  matchKind quantifiedUniques (KTYPE (TcTyVar tyVar)) (KTYPE runtimeRep) | tvUnique tyVar `elem` quantifiedUniques = Map.singleton (tvUnique tyVar) runtimeRep;
  matchKind quantifiedUniques (KFun left right) (KFun left' right') = matchKind quantifiedUniques left left' <> matchKind quantifiedUniques right right';
  matchKind quantifiedUniques (TcTyCon left formalArguments) (TcTyCon right actualArguments) | left == right, length formalArguments == length actualArguments = Map.unions (zipWith (matchKind quantifiedUniques) formalArguments actualArguments);
  matchKind _ _ _ = Map.empty}
}
