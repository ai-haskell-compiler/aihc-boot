module Aihc.Tc.QuickLook (quickLookUnify) where {
import Aihc.Tc.Kind (refineGivenTyVarKinds, tcTypeKind, zonkKind);
import Aihc.Tc.Monad;
import Aihc.Tc.Solve.Decompose (decomposeNominalEquality);
import Aihc.Tc.Solve.Family (reduceTypeFamilies);
import Aihc.Tc.Types;
import Aihc.Tc.Unify (unifyTypes);
import Aihc.Tc.Zonk (zonkType);
import Control.Monad (unless);
import Data.IntSet (IntSet);
import Data.IntSet qualified as IntSet;
import Data.Map.Strict qualified as Map;
quickLookUnify :: IntSet -> TcType -> TcType -> TcM ();
quickLookUnify instantiationVariables = go where {
  go expected actual = do {
  expected' <- zonkType expected >>= reduceTypeFamilies;
  actual' <- zonkType actual >>= reduceTypeFamilies;
  case (expected', actual') of {
  (TcMetaTv left, TcMetaTv right) | left == right -> pure ();
  (TcMetaTv unique, _) | isInstantiationVariable unique -> bind unique actual';
  (_, TcMetaTv unique) | isInstantiationVariable unique -> bind unique expected';
  (TcMetaTv _, _) -> pure ();
  (_, TcMetaTv _) -> pure ();
  (TcForAllTy leftVar leftBody, TcForAllTy rightVar rightBody) -> go leftBody (applySubst (Map.singleton (tvUnique rightVar) (TcTyVar leftVar)) rightBody);
  (TcQualTy leftPredicates leftBody, TcQualTy rightPredicates rightBody) | length leftPredicates == length rightPredicates -> go leftBody rightBody;
  _ -> do {
  children <- decomposeNominalEquality expected' actual';
  mapM_ (mapM_ (uncurry go)) children}}};
  isInstantiationVariable (Unique key) = IntSet.member key instantiationVariables;
  bind unique rawType = do {
  ty <- refineGivenTyVarKinds rawType;
  unless (unique `elem` metaVariables ty) $ do {
  declaredKind <- readMetaTvKind unique >>= zonkKind;
  solvedKind <- tcTypeKind ty >>= zonkKind;
  kindResult <- unifyTypes declaredKind solvedKind;
  case kindResult of {
  Right () -> writeMetaTv unique ty;
  Left _ -> pure ()}}}};
metaVariables :: TcType -> [Unique];
metaVariables ty = case ty of {
  TcTyVar {} -> [];
  TcMetaTv unique -> [unique];
  TcArrowTy -> [];
  TcTyLit {} -> [];
  TcTyCon _ arguments -> concatMap metaVariables arguments;
  TcFunTy argument result -> metaVariables argument <> metaVariables result;
  TcForAllTy _ body -> metaVariables body;
  TcQualTy predicates body -> concatMap predicateMetaVariables predicates <> metaVariables body;
  TcAppTy function argument -> metaVariables function <> metaVariables argument};
predicateMetaVariables :: Pred -> [Unique];
predicateMetaVariables predicate = case predicate of {
  ClassPred _ arguments -> concatMap metaVariables arguments;
  EqPred left right -> metaVariables left <> metaVariables right;
  IParamPred _ payload -> metaVariables payload;
  IrredPred constraint -> metaVariables constraint;
  QuantifiedPred variables antecedents consequent -> concatMap (metaVariables . tvKind) variables <> concatMap predicateMetaVariables antecedents <> predicateMetaVariables consequent}
}
