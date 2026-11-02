module Aihc.Cli.InterfaceParts (InterfacePart(..), PartIndex(..), interfaceParts) where {
import Aihc.Tc (AssociatedTypeInfo(..), ClassInfo(..), DataConFieldInfo(..), DataConInfo(..), DataFamilyInstanceInfo(..), DataTypeInfo(..), InstanceInfo(..), PatSynInfo(..), Pred(..), TcInterface, TcType(..), TyCon, TyConInfo(..), TyVarId, TypeFamilyInstanceInfo(..), TypeScheme(..), tcInterfaceClasses, tcInterfaceDataFamilyInstances, tcInterfaceDataTypes, tcInterfaceForeignImports, tcInterfaceInstances, tcInterfacePatSyns, tcInterfaceTerms, tcInterfaceTyCons, tcInterfaceTypeFamilyInstances, tvKind);
import Aihc.Tc.Annotations (TcForeignImportAnnotation(..), TcForeignImportInfo(..), TcForeignMarshal(..), TcForeignTarget(..));
import Aihc.Tc.Env (TypeSynonymInfo(..));
import Control.Monad (void);
import Control.Monad.Trans.State.Strict (State, execState, gets, state);
import Data.Foldable (traverse_);
import Data.Map.Strict (Map);
import Data.Map.Strict qualified as Map;
import Data.Maybe (mapMaybe);
import Data.Word (Word64);
data InterfacePart = PartTyVar !TyVarId | PartType !TcType | PartPred !Pred | PartScheme !TypeScheme;
data PartIndex = PartIndex { partIndexTyCons :: !(Map TyCon Word64), partIndexTyVars :: !(Map TyVarId Word64), partIndexTypes :: !(Map TcType Word64), partIndexPreds :: !(Map Pred Word64), partIndexSchemes :: !(Map TypeScheme Word64) };
interfaceParts :: Map TyCon Word64 -> TcInterface -> ([InterfacePart], PartIndex);
interfaceParts tyCons interface = (reverse (stateParts final), PartIndex {partIndexTyCons = tyCons, partIndexTyVars = stateTyVars final, partIndexTypes = stateTypes final, partIndexPreds = statePreds final, partIndexSchemes = stateSchemes final}) where {
  final = execState (walkInterface interface) emptyState};
data PartsState = PartsState { stateTyVars :: !(Map TyVarId Word64), stateTypes :: !(Map TcType Word64), statePreds :: !(Map Pred Word64), stateSchemes :: !(Map TypeScheme Word64), stateCount :: !Word64, stateParts :: [InterfacePart] };
emptyState :: PartsState;
emptyState = PartsState Map.empty Map.empty Map.empty Map.empty 0 [];
type Parts = State PartsState;
record :: InterfacePart -> (Word64 -> PartsState -> PartsState) -> Parts Word64;
record part remember = state (\current -> let {
  number = stateCount current;
  numbered = current {stateCount = number + 1, stateParts = part : stateParts current}} in (number, remember number numbered));
internTyVar :: TyVarId -> Parts Word64;
internTyVar variable = do {
  known <- gets (Map.lookup variable . stateTyVars);
  case known of {
  Just number -> pure number;
  Nothing -> do {
  void (internType (tvKind variable));
  record (PartTyVar variable) (\number current -> current {stateTyVars = Map.insert variable number (stateTyVars current)})}}};
internType :: TcType -> Parts Word64;
internType ty = do {
  known <- gets (Map.lookup ty . stateTypes);
  case known of {
  Just number -> pure number;
  Nothing -> do {
  case ty of {
  TcTyVar variable -> void (internTyVar variable);
  TcMetaTv {} -> pure ();
  TcArrowTy -> pure ();
  TcTyLit {} -> pure ();
  TcTyCon _ arguments -> traverse_ internType arguments;
  TcFunTy argument result -> traverse_ internType [argument, result];
  TcForAllTy variable body -> internTyVar variable *> void (internType body);
  TcQualTy predicates body -> traverse_ internPred predicates *> void (internType body);
  TcAppTy function argument -> traverse_ internType [function, argument]};
  record (PartType ty) (\number current -> current {stateTypes = Map.insert ty number (stateTypes current)})}}};
internPred :: Pred -> Parts Word64;
internPred predicate = do {
  known <- gets (Map.lookup predicate . statePreds);
  case known of {
  Just number -> pure number;
  Nothing -> do {
  case predicate of {
  ClassPred _ arguments -> traverse_ internType arguments;
  EqPred left right -> traverse_ internType [left, right];
  IParamPred _ payload -> void (internType payload);
  IrredPred constraint -> void (internType constraint);
  QuantifiedPred variables antecedents consequent -> traverse_ internTyVar variables *> traverse_ internPred antecedents *> void (internPred consequent)};
  record (PartPred predicate) (\number current -> current {statePreds = Map.insert predicate number (statePreds current)})}}};
internScheme :: TypeScheme -> Parts Word64;
internScheme scheme@(ForAll variables predicates body) = do {
  known <- gets (Map.lookup scheme . stateSchemes);
  case known of {
  Just number -> pure number;
  Nothing -> do {
  traverse_ internTyVar variables;
  traverse_ internPred predicates;
  void (internType body);
  record (PartScheme scheme) (\number current -> current {stateSchemes = Map.insert scheme number (stateSchemes current)})}}};
walkInterface :: TcInterface -> Parts ();
walkInterface interface = do {
  traverse_ (internScheme . snd) (tcInterfaceTerms interface);
  traverse_ walkTyConInfo (tcInterfaceTyCons interface);
  traverse_ walkDataTypeInfo (tcInterfaceDataTypes interface);
  traverse_ walkClassInfo (tcInterfaceClasses interface);
  traverse_ walkInstanceInfo (tcInterfaceInstances interface);
  traverse_ walkDataFamilyInstanceInfo (tcInterfaceDataFamilyInstances interface);
  traverse_ walkTypeFamilyInstanceInfo (tcInterfaceTypeFamilyInstances interface);
  traverse_ walkPatSynInfo (tcInterfacePatSyns interface);
  traverse_ (walkForeignImportInfo . snd) (tcInterfaceForeignImports interface)};
walkTyConInfo :: TyConInfo -> Parts ();
walkTyConInfo info = do {
  void (internScheme (tciKindScheme info));
  traverse_ walkTypeSynonymInfo (tciTypeSynonym info)};
walkTypeSynonymInfo :: TypeSynonymInfo -> Parts ();
walkTypeSynonymInfo info = do {
  traverse_ internTyVar (tsiParams info);
  traverse_ internType (tsiBody info)};
walkDataTypeInfo :: DataTypeInfo -> Parts ();
walkDataTypeInfo info = do {
  traverse_ internTyVar (dtiTyVars info);
  void (internType (dtiResultKind info));
  traverse_ walkDataConInfo (dtiConstructors info)};
walkDataConInfo :: DataConInfo -> Parts ();
walkDataConInfo info = do {
  traverse_ internTyVar (dciUnivTyVars info);
  traverse_ internTyVar (dciExTyVars info);
  traverse_ internPred (dciTheta info);
  traverse_ (internType . dcfiType) (dciFields info);
  void (internType (dciResTy info))};
walkClassInfo :: ClassInfo -> Parts ();
walkClassInfo info = do {
  traverse_ internTyVar (ciKindTyVars info);
  traverse_ internTyVar (ciTyVars info);
  traverse_ internType (ciSuperClassTypes info);
  traverse_ (internScheme . snd) (ciMethods info);
  traverse_ (internScheme . snd) (ciDefaultSignatures info);
  traverse_ walkTypeFamilyInstanceInfo (mapMaybe atiDefault (ciAssociatedTypes info))};
walkInstanceInfo :: InstanceInfo -> Parts ();
walkInstanceInfo info = do {
  void (internType (iiDictType info));
  traverse_ internTyVar (iiTyVars info);
  traverse_ internPred (iiContext info);
  traverse_ internType (iiHead info)};
walkDataFamilyInstanceInfo :: DataFamilyInstanceInfo -> Parts ();
walkDataFamilyInstanceInfo info = do {
  void (internType (dfiiFamilyType info));
  traverse_ internTyVar (dfiiTyVars info);
  traverse_ walkDataConInfo (dfiiConstructors info)};
walkTypeFamilyInstanceInfo :: TypeFamilyInstanceInfo -> Parts ();
walkTypeFamilyInstanceInfo info = do {
  traverse_ internTyVar (tfiiTyVars info);
  void (internType (tfiiLeft info));
  void (internType (tfiiRight info))};
walkPatSynInfo :: PatSynInfo -> Parts ();
walkPatSynInfo info = do {
  void (internScheme (psiScheme info));
  traverse_ internPred (psiReqTheta info);
  traverse_ internPred (psiProvTheta info)};
walkForeignImportInfo :: TcForeignImportInfo -> Parts ();
walkForeignImportInfo info = case info of {
  TcForeignPrimImport -> pure ();
  TcForeignCCallImport _ plan -> traverse_ walkForeignMarshal (tcForeignResult plan : tcForeignArguments plan) >> case tcForeignTarget plan of {
  TcForeignWrapper pointer -> walkForeignMarshal pointer;
  _ -> pure ()}};
walkForeignMarshal :: TcForeignMarshal -> Parts ();
walkForeignMarshal marshal = do {
  void (internType (tcForeignSourceType marshal));
  void (internType (tcForeignPrimitiveType marshal))}
}
