{-# LANGUAGE OverloadedStrings #-}
module Aihc.Tc.Env (TyConFlavor(..), TyConInfo(..), TypeSynonymInfo(..), DataTypeInfo(..), CType(..), dataTypeKey, DataConInfo(..), PatSynDirection(..), PatSynInfo(..), patSynKey, DataConFieldInfo(..), DataConFieldUnpack(..), DataConSourceForm(..), dataConArgTypes, RecordHead(..), dataConRecordHead, patSynRecordHead, ClassInfo(..), AssociatedTypeInfo(..), FunDep(..), classInfoKey, classFieldTypes, InstanceInfo(..), instanceInfoKey, instanceClassTyCon, instanceIsForClass, InstanceEnv, emptyInstanceEnv, instanceEnvFromList, addInstanceEnv, instanceEnvList, instanceEnvSince, instanceEnvForClass, DataFamilyInstanceInfo(..), dataFamilyAxiomKey, dataFamilyAxiomName, dataFamilyRepresentationName, TypeFamilyInstanceInfo(..), typeFamilyAxiomKey, typeFamilyAxiomName) where {
import Aihc.Resolve (PackageId);
import Aihc.Tc.Types;
import Control.DeepSeq (NFData);
import Data.Map.Strict (Map);
import Data.Map.Strict qualified as Map;
import Data.Text (Text);
import Data.Text qualified as T;
import GHC.Generics (Generic);
data TyConFlavor = ClassTyCon | DataTyCon | DataFamilyTyCon | NewtypeTyCon | SynonymTyCon | TypeFamilyTyCon deriving (Eq, Show, Read, Generic);
instance NFData TyConFlavor where {};
data TyConInfo = TyConInfo { tciName :: !Text, tciArity :: !Int, tciTyCon :: !TyCon, tciKindScheme :: !TypeScheme, tciFlavor :: !TyConFlavor, tciTypeSynonym :: !(Maybe TypeSynonymInfo), tciInjectivity :: !(Maybe [Int]) } deriving (Eq, Show, Read, Generic);
instance NFData TyConInfo where {};
data TypeSynonymInfo = TypeSynonymInfo { tsiParams :: ![TyVarId], tsiBody :: !(Maybe TcType) } deriving (Eq, Show, Read, Generic);
instance NFData TypeSynonymInfo where {};
data DataTypeInfo = DataTypeInfo { dtiName :: !Text, dtiTyCon :: !TyCon, dtiTyVars :: ![TyVarId], dtiResultKind :: !TcType, dtiFlavor :: !TyConFlavor, dtiConstructors :: ![DataConInfo], dtiNominalRoles :: ![Bool], dtiCType :: !(Maybe CType) } deriving (Eq, Show, Read, Generic);
instance NFData DataTypeInfo where {};
data CType = CType { cTypeHeader :: !(Maybe Text), cTypeName :: !Text } deriving (Eq, Show, Read, Generic);
instance NFData CType where {};
dataTypeKey :: DataTypeInfo -> TcTypeKey;
dataTypeKey = tyConKey . dtiTyCon;
data DataConSourceForm = PrefixDataCon | InfixDataCon | RecordDataCon | SyntaxDataCon | UnboxedTupleDataCon | UnboxedSumDataCon !Int !Int deriving (Eq, Show, Read, Generic);
instance NFData DataConSourceForm where {};
data DataConFieldUnpack = NoFieldUnpack | UnpackField | NoUnpackField deriving (Eq, Show, Read, Generic);
instance NFData DataConFieldUnpack where {};
data DataConFieldInfo = DataConFieldInfo { dcfiLabel :: !(Maybe Text), dcfiType :: !TcType, dcfiStrict :: !Bool, dcfiLazy :: !Bool, dcfiUnpack :: !DataConFieldUnpack } deriving (Eq, Show, Read, Generic);
instance NFData DataConFieldInfo where {};
data DataConInfo = DataConInfo { dciName :: !Text, dciOrigin :: !(PackageId, Text), dciUnivTyVars :: ![TyVarId], dciExTyVars :: ![TyVarId], dciTheta :: ![Pred], dciFields :: ![DataConFieldInfo], dciResTy :: !TcType, dciSourceForm :: !DataConSourceForm } deriving (Eq, Show, Read, Generic);
instance NFData DataConInfo where {};
dataConArgTypes :: DataConInfo -> [TcType];
dataConArgTypes = map dcfiType . dciFields;
data PatSynDirection = PatSynUnidirectionalInfo | PatSynImplicitBidirectionalInfo | PatSynExplicitBidirectionalInfo deriving (Eq, Show, Read, Generic);
instance NFData PatSynDirection where {};
data PatSynInfo = PatSynInfo { psiName :: !Text, psiOrigin :: !(PackageId, Text), psiArity :: !Int, psiFields :: ![Text], psiDirection :: !PatSynDirection, psiScheme :: !TypeScheme, psiReqTheta :: ![Pred], psiProvTheta :: ![Pred] } deriving (Eq, Show, Read, Generic);
instance NFData PatSynInfo where {};
patSynKey :: PatSynInfo -> TcTermKey;
patSynKey info = let {
  (package, moduleName') = psiOrigin info} in TcTermGlobal package moduleName' (psiName info);
data RecordHead = RecordHead { rhName :: !Text, rhOrigin :: !(PackageId, Text), rhFields :: ![Maybe Text] } deriving (Eq, Show, Read, Generic);
instance NFData RecordHead where {};
dataConRecordHead :: DataConInfo -> RecordHead;
dataConRecordHead con = RecordHead {rhName = dciName con, rhOrigin = dciOrigin con, rhFields = map dcfiLabel (dciFields con)};
patSynRecordHead :: PatSynInfo -> RecordHead;
patSynRecordHead info = RecordHead {rhName = psiName info, rhOrigin = psiOrigin info, rhFields = if null (psiFields info) then replicate (psiArity info) Nothing else map Just (psiFields info)};
data ClassInfo = ClassInfo { ciName :: !Text, ciTyCon :: !TyCon, ciOrigin :: !(Maybe (Text, Text)), ciKindTyVars :: ![TyVarId], ciTyVars :: ![TyVarId], ciSuperClassTypes :: ![TcType], ciMethods :: ![(Text, TypeScheme)], ciDefaultMethods :: ![Text], ciDefaultSignatures :: ![(Text, TypeScheme)], ciAssociatedTypes :: ![AssociatedTypeInfo], ciFunDeps :: ![FunDep] } deriving (Eq, Show, Read, Generic);
instance NFData ClassInfo where {};
classFieldTypes :: ClassInfo -> Map Unique TcType -> [TcType];
classFieldTypes classInfo substitution = map (applySubst substitution) (ciSuperClassTypes classInfo) <> map (methodFieldType classInfo substitution . snd) (ciMethods classInfo);
methodFieldType :: ClassInfo -> Map Unique TcType -> TypeScheme -> TcType;
methodFieldType classInfo substitution (ForAll typeVariables predicates body) = applySubst substitution $ foldr TcForAllTy qualifiedBody extraTypeVariables where {
  classVariables = ciTyVars classInfo;
  extraTypeVariables = filter (`notElem` classVariables) typeVariables;
  remainingPredicates = filter (not . isClassPredicate) predicates;
  qualifiedBody | null remainingPredicates = body | otherwise = TcQualTy remainingPredicates body;
  isClassPredicate predicate = case predicate of {
  ClassPred className _ -> tyConKey className == tyConKey (ciTyCon classInfo);
  EqPred {} -> False;
  QuantifiedPred {} -> False;
  IParamPred {} -> False;
  IrredPred {} -> False}};
data FunDep = FunDep { fdDeterminers :: ![Int], fdDetermined :: ![Int] } deriving (Eq, Show, Read, Generic);
instance NFData FunDep where {};
data AssociatedTypeInfo = AssociatedTypeInfo { atiTyCon :: !TyCon, atiClassParams :: ![Maybe Int], atiDefault :: !(Maybe TypeFamilyInstanceInfo) } deriving (Eq, Show, Read, Generic);
instance NFData AssociatedTypeInfo where {};
classInfoKey :: ClassInfo -> TcTypeKey;
classInfoKey = tyConKey . ciTyCon;
data InstanceInfo = InstanceInfo { iiClassName :: !Text, iiDictName :: !Text, iiDictOrigin :: !(Text, Text), iiDictType :: !TcType, iiTyVars :: ![TyVarId], iiContext :: ![Pred], iiHead :: ![TcType] } deriving (Eq, Show, Read, Generic);
instance NFData InstanceInfo where {};
instanceInfoKey :: InstanceInfo -> ((Text, Text), Text);
instanceInfoKey instanceInfo = (iiDictOrigin instanceInfo, iiDictName instanceInfo);
instanceClassTyCon :: InstanceInfo -> Maybe TyCon;
instanceClassTyCon = go . iiDictType where {
  go ty = case ty of {
  TcForAllTy _ body -> go body;
  TcQualTy _ body -> go body;
  TcTyCon tyCon _ -> Just tyCon;
  _ -> Nothing}};
instanceIsForClass :: TyCon -> InstanceInfo -> Bool;
instanceIsForClass classTyCon instanceInfo = case instanceClassTyCon instanceInfo of {
  Just tyCon -> tyConKey tyCon == tyConKey classTyCon;
  Nothing -> iiClassName instanceInfo == tyConName classTyCon};
data InstanceEnv = InstanceEnv { instanceEnvSize :: !Int, instanceEnvAll :: ![InstanceInfo], instanceEnvByClass :: !(Map TcTypeKey [InstanceInfo]) } deriving (Show);
emptyInstanceEnv :: InstanceEnv;
emptyInstanceEnv = InstanceEnv 0 [] Map.empty;
instanceEnvFromList :: [InstanceInfo] -> InstanceEnv;
instanceEnvFromList = foldr addInstanceEnv emptyInstanceEnv;
addInstanceEnv :: InstanceInfo -> InstanceEnv -> InstanceEnv;
addInstanceEnv instanceInfo env = InstanceEnv {instanceEnvSize = instanceEnvSize env + 1, instanceEnvAll = instanceInfo : instanceEnvAll env, instanceEnvByClass = case instanceClassTyCon instanceInfo of {
  Nothing -> instanceEnvByClass env;
  Just classTyCon -> Map.insertWith (<>) (tyConKey classTyCon) [instanceInfo] (instanceEnvByClass env)}};
instanceEnvList :: InstanceEnv -> [InstanceInfo];
instanceEnvList = instanceEnvAll;
instanceEnvSince :: InstanceEnv -> InstanceEnv -> [InstanceInfo];
instanceEnvSince current previous = take (instanceEnvSize current - instanceEnvSize previous) (instanceEnvAll current);
instanceEnvForClass :: TyCon -> InstanceEnv -> [InstanceInfo];
instanceEnvForClass classTyCon = Map.findWithDefault [] (tyConKey classTyCon) . instanceEnvByClass;
data DataFamilyInstanceInfo = DataFamilyInstanceInfo { dfiiFamilyName :: !Text, dfiiFamilyType :: !TcType, dfiiTyVars :: ![TyVarId], dfiiRepresentationTyCon :: !TyCon, dfiiAxiomName :: !Text, dfiiConstructorNames :: ![Text], dfiiConstructors :: ![DataConInfo], dfiiIsNewtype :: !Bool } deriving (Eq, Show, Read, Generic);
instance NFData DataFamilyInstanceInfo where {};
dataFamilyAxiomKey :: DataFamilyInstanceInfo -> TcAxiomKey;
dataFamilyAxiomKey info = TcAxiomKey (tyConPackageId (dfiiRepresentationTyCon info)) (tyConModuleName (dfiiRepresentationTyCon info)) (dfiiAxiomName info);
dataFamilyRepresentationName :: Text -> Text -> Text;
dataFamilyRepresentationName familyName firstConstructor = "$R$" <> familyName <> "$" <> firstConstructor;
dataFamilyAxiomName :: Text -> Text -> Text;
dataFamilyAxiomName familyName firstConstructor = "$ax$" <> familyName <> "$" <> firstConstructor;
data TypeFamilyInstanceInfo = TypeFamilyInstanceInfo { tfiiFamilyName :: !Text, tfiiAxiomName :: !Text, tfiiOrigin :: !(PackageId, Text), tfiiTyVars :: ![TyVarId], tfiiLeft :: !TcType, tfiiRight :: !TcType, tfiiClosed :: !Bool } deriving (Eq, Show, Read, Generic);
instance NFData TypeFamilyInstanceInfo where {};
typeFamilyAxiomKey :: TypeFamilyInstanceInfo -> TcAxiomKey;
typeFamilyAxiomKey info = let {
  (package, moduleName') = tfiiOrigin info} in TcAxiomKey package moduleName' (tfiiAxiomName info);
typeFamilyAxiomName :: Text -> Int -> Text;
typeFamilyAxiomName familyName index = "$ax$" <> familyName <> "$" <> T.pack (show index)
}
