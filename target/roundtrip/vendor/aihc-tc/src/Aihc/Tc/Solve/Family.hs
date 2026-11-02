module Aihc.Tc.Solve.Family (reduceTypeFamilies, reducePredFamilies, normalizeFamilyPred, isTypeFamilyTyCon, isTypeFamilyApplication, unsaturateFamilyApplication, familyEquations) where {
import Aihc.Tc.Env (TyConFlavor(..), TyConInfo(..), TypeFamilyInstanceInfo(..));
import Aihc.Tc.Match (matchTypes);
import Aihc.Tc.Monad (TcM, TcState(tcsGlobalTyCons), getKinds, getTypeFamilyInstances, getWiring, lookupTyConByIdentity);
import Aihc.Tc.TypeLitFamily (TypeLitValue(..), evaluateTypeLitFamily);
import Aihc.Tc.Types;
import Aihc.Tc.Wiring (TcWiring(..));
import Control.Monad.Trans.Class (lift);
import Control.Monad.Trans.State.Strict (gets);
import Data.List (sortOn);
import Data.Map.Strict qualified as Map;
import Data.Maybe (fromMaybe);
import Data.Text qualified as T;
import Text.Read (readMaybe);
reduceTypeFamilies :: TcType -> TcM TcType;
reduceTypeFamilies ty = case ty of {
  TcTyCon tyCon arguments -> do {
  arguments' <- mapM reduceTypeFamilies arguments;
  reduceHead (TcTyCon tyCon arguments')};
  TcFunTy argument result -> TcFunTy <$> reduceTypeFamilies argument <*> reduceTypeFamilies result;
  TcAppTy function argument -> do {
  function' <- reduceTypeFamilies function;
  argument' <- reduceTypeFamilies argument;
  reduceHead (mkAppTy function' argument')};
  TcForAllTy tyVar body -> TcForAllTy tyVar <$> reduceTypeFamilies body;
  TcQualTy predicates body -> TcQualTy <$> mapM reducePredFamilies predicates <*> reduceTypeFamilies body;
  _ -> pure ty};
reducePredFamilies :: Pred -> TcM Pred;
reducePredFamilies predicate = case predicate of {
  ClassPred className arguments -> ClassPred className <$> mapM reduceTypeFamilies arguments;
  EqPred left right -> EqPred <$> reduceTypeFamilies left <*> reduceTypeFamilies right;
  IParamPred name payload -> IParamPred name <$> reduceTypeFamilies payload;
  IrredPred constraint -> IrredPred <$> reduceTypeFamilies constraint;
  QuantifiedPred variables antecedents consequent -> QuantifiedPred variables <$> mapM reducePredFamilies antecedents <*> reducePredFamilies consequent};
normalizeFamilyPred :: Pred -> TcM Pred;
normalizeFamilyPred predicate = do {
  family <- isTypeFamilyTyCon;
  let {
  normalize predicate' = case predicate' of {
  ClassPred tyCon arguments | family tyCon -> IrredPred (TcTyCon tyCon arguments);
  QuantifiedPred variables antecedents consequent -> QuantifiedPred variables (map normalize antecedents) (normalize consequent);
  _ -> predicate'}};
  pure (normalize predicate)};
isTypeFamilyTyCon :: TcM (TyCon -> Bool);
isTypeFamilyTyCon = do {
  tyCons <- lift $ gets tcsGlobalTyCons;
  pure (\tyCon -> maybe False ((== TypeFamilyTyCon) . tciFlavor) (Map.lookup (tyConKey tyCon) tyCons))};
isTypeFamilyApplication :: TcType -> TcM Bool;
isTypeFamilyApplication ty = case ty of {
  TcTyCon tyCon arguments -> do {
  maybeInfo <- lookupTyConByIdentity tyCon;
  pure $ case maybeInfo of {
  Just info -> tciFlavor info == TypeFamilyTyCon && length arguments == tciArity info;
  Nothing -> False}};
  _ -> pure False};
unsaturateFamilyApplication :: TcType -> TcM TcType;
unsaturateFamilyApplication ty = case ty of {
  TcTyCon tyCon arguments -> do {
  maybeInfo <- lookupTyConByIdentity tyCon;
  pure $ case maybeInfo of {
  Just info | tciFlavor info == TypeFamilyTyCon, length arguments > tciArity info -> let {
  (familyArguments, extraArguments) = splitAt (tciArity info) arguments} in foldl TcAppTy (TcTyCon tyCon familyArguments) extraArguments;
  _ -> ty}};
  _ -> pure ty};
reduceHead :: TcType -> TcM TcType;
reduceHead ty = case ty of {
  TcTyCon tyCon arguments -> do {
  maybeInfo <- lookupTyConByIdentity tyCon;
  case maybeInfo of {
  Just info | tciFlavor info == TypeFamilyTyCon, length arguments >= tciArity info -> do {
  let {
  (familyArguments, extraArguments) = splitAt (tciArity info) arguments};
  kinds <- getKinds;
  wiring <- getWiring;
  case builtinTypeLitFamily wiring kinds tyCon familyArguments of {
  Just reduced -> reduceTypeFamilies (foldl mkAppTy reduced extraArguments);
  Nothing -> do {
  equations <- familyEquations tyCon;
  family <- isTypeFamilyTyCon;
  case firstEquation family equations familyArguments of {
  Just reduced -> reduceTypeFamilies (foldl mkAppTy reduced extraArguments);
  Nothing -> pure ty}}}};
  _ -> pure ty}};
  _ -> pure ty};
builtinTypeLitFamily :: TcWiring -> TcKinds -> TyCon -> [TcType] -> Maybe TcType;
builtinTypeLitFamily wiring kinds tyCon arguments | tyConModuleName tyCon `notElem` tcWiringTypeLitFamilyModules wiring = Nothing | otherwise = do {
  literals <- traverse literal arguments;
  value <- evaluateTypeLitFamily (tyConName tyCon) literals;
  pure $ case value of {
  TypeLitNatural natural -> TcTyLit (TyLitNat natural);
  TypeLitOrdering ordering -> TcTyCon (kindsDataCon kinds (T.pack (show ordering)) 0) []}} where {
  literal ty = case ty of {
  TcTyLit value -> Just value;
  _ -> Nothing}};
familyEquations :: TyCon -> TcM [TypeFamilyInstanceInfo];
familyEquations tyCon = sortOn axiomIndex . filter isEquationOf <$> getTypeFamilyInstances where {
  isEquationOf info = case tfiiLeft info of {
  TcTyCon familyTyCon _ -> familyTyCon == tyCon;
  _ -> False}};
axiomIndex :: TypeFamilyInstanceInfo -> Int;
axiomIndex info = fromMaybe 0 (readMaybe (T.unpack (T.takeWhileEnd (/= '$') (tfiiAxiomName info))));
firstEquation :: (TyCon -> Bool) -> [TypeFamilyInstanceInfo] -> [TcType] -> Maybe TcType;
firstEquation family equations arguments = case equations of {
  [] -> Nothing;
  equation : rest -> case equationArguments equation of {
  Just patterns | Just substitution <- matchTypes patterns arguments -> Just (applySubst substitution (tfiiRight equation)) | tfiiClosed equation, and (zipWith (couldUnify family) patterns arguments) -> Nothing;
  _ -> firstEquation family rest arguments}};
equationArguments :: TypeFamilyInstanceInfo -> Maybe [TcType];
equationArguments info = case tfiiLeft info of {
  TcTyCon _ patterns -> Just patterns;
  _ -> Nothing};
couldUnify :: (TyCon -> Bool) -> TcType -> TcType -> Bool;
couldUnify family = go where {
  go patternType target = case (patternType, target) of {
  (TcTyVar _, _) -> True;
  (_, TcTyVar _) -> True;
  (_, TcMetaTv _) -> True;
  (_, TcTyCon targetTyCon _) | family targetTyCon -> True;
  (TcTyCon tyCon arguments, TcTyCon targetTyCon targetArguments) -> tyCon == targetTyCon && length arguments == length targetArguments && and (zipWith go arguments targetArguments);
  (TcFunTy argument result, TcFunTy targetArgument targetResult) -> go argument targetArgument && go result targetResult;
  (TcAppTy function argument, TcAppTy targetFunction targetArgument) -> go function targetFunction && go argument targetArgument;
  _ -> patternType == target}}
}
