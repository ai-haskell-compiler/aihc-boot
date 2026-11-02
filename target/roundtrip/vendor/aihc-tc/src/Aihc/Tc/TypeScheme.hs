module Aihc.Tc.TypeScheme (typeSchemeFromType, schemeToType, equivalentTypeSchemes) where {
import Aihc.Tc.Types;
import Data.Map.Strict (Map);
import Data.Map.Strict qualified as Map;
typeSchemeFromType :: TcType -> TypeScheme;
typeSchemeFromType = go [] [] where {
  go tyVars predicates ty = case ty of {
  TcForAllTy tyVar body -> go (tyVars <> [tyVar]) predicates body;
  TcQualTy morePredicates body -> go tyVars (predicates <> morePredicates) body;
  _ -> specifiedScheme tyVars predicates ty}};
schemeToType :: TypeScheme -> TcType;
schemeToType (Scheme [] [] [] ty) = ty;
schemeToType (ForAll tvs [] ty) = foldr TcForAllTy ty tvs;
schemeToType (ForAll [] preds ty) = TcQualTy preds ty;
schemeToType (ForAll tvs preds ty) = foldr TcForAllTy (TcQualTy preds ty) tvs;
equivalentTypeSchemes :: TypeScheme -> TypeScheme -> Bool;
equivalentTypeSchemes (ForAll leftVars leftPredicates leftBody) (ForAll rightVars rightPredicates rightBody) | length leftVars /= length rightVars = False | length leftPredicates /= length rightPredicates = False | otherwise = let {
  renaming = Map.fromList (zip (map tvUnique leftVars) (map tvUnique rightVars))} in and (zipWith (equivalentType renaming `onTyVar`) leftVars rightVars) && and (zipWith (equivalentPred renaming) leftPredicates rightPredicates) && equivalentType renaming leftBody rightBody where {
  onTyVar compareKinds left right = compareKinds (tvKind left) (tvKind right)};
equivalentType :: Map Unique Unique -> TcType -> TcType -> Bool;
equivalentType renaming left right = case (left, right) of {
  (TcTyVar leftVar, TcTyVar rightVar) -> Map.lookup (tvUnique leftVar) renaming == Just (tvUnique rightVar);
  (TcArrowTy, TcArrowTy) -> True;
  (TcMetaTv leftMeta, TcMetaTv rightMeta) -> leftMeta == rightMeta;
  (TcTyCon leftCon leftArgs, TcTyCon rightCon rightArgs) -> leftCon == rightCon && length leftArgs == length rightArgs && and (zipWith (equivalentType renaming) leftArgs rightArgs);
  (TcFunTy leftArg leftResult, TcFunTy rightArg rightResult) -> equivalentType renaming leftArg rightArg && equivalentType renaming leftResult rightResult;
  (TcForAllTy leftVar leftBody, TcForAllTy rightVar rightBody) -> equivalentType renaming (tvKind leftVar) (tvKind rightVar) && equivalentType (Map.insert (tvUnique leftVar) (tvUnique rightVar) renaming) leftBody rightBody;
  (TcQualTy leftPredicates leftBody, TcQualTy rightPredicates rightBody) -> length leftPredicates == length rightPredicates && and (zipWith (equivalentPred renaming) leftPredicates rightPredicates) && equivalentType renaming leftBody rightBody;
  (TcAppTy leftFunction leftArg, TcAppTy rightFunction rightArg) -> equivalentType renaming leftFunction rightFunction && equivalentType renaming leftArg rightArg;
  _ -> False};
equivalentPred :: Map Unique Unique -> Pred -> Pred -> Bool;
equivalentPred renaming left right = case (left, right) of {
  (ClassPred leftClass leftArgs, ClassPred rightClass rightArgs) -> leftClass == rightClass && length leftArgs == length rightArgs && and (zipWith (equivalentType renaming) leftArgs rightArgs);
  (EqPred leftA leftB, EqPred rightA rightB) -> equivalentType renaming leftA rightA && equivalentType renaming leftB rightB;
  (IParamPred leftName leftPayload, IParamPred rightName rightPayload) -> leftName == rightName && equivalentType renaming leftPayload rightPayload;
  (IrredPred leftConstraint, IrredPred rightConstraint) -> equivalentType renaming leftConstraint rightConstraint;
  (QuantifiedPred leftVariables leftAntecedents leftConsequent, QuantifiedPred rightVariables rightAntecedents rightConsequent) -> length leftVariables == length rightVariables && and (zipWith (equivalentType renaming `onKind`) leftVariables rightVariables) && length leftAntecedents == length rightAntecedents && and (zipWith (equivalentPred quantifiedRenaming) leftAntecedents rightAntecedents) && equivalentPred quantifiedRenaming leftConsequent rightConsequent where {
  quantifiedRenaming = foldr (\(leftVariable, rightVariable) -> Map.insert (tvUnique leftVariable) (tvUnique rightVariable)) renaming (zip leftVariables rightVariables);
  onKind comparison leftVariable rightVariable = comparison (tvKind leftVariable) (tvKind rightVariable)};
  _ -> False}
}
