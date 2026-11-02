module Aihc.Tc.Unify (unify, unifyDeferring, unifyTypes) where {
import Aihc.Parser.Syntax (SourceSpan);
import Aihc.Tc.Constraint (CtOrigin(..));
import Aihc.Tc.Error (TcErrorKind(..));
import Aihc.Tc.Kind (refineGivenTyVarKinds, tcTypeKind, unifyKindsAt);
import Aihc.Tc.Monad;
import Aihc.Tc.Solve.Decompose (decomposeNominalEquality);
import Aihc.Tc.Solve.Family (isTypeFamilyApplication, reduceTypeFamilies, unsaturateFamilyApplication);
import Aihc.Tc.Types;
import Aihc.Tc.Zonk (zonkType);
unify :: Maybe SourceSpan -> CtOrigin -> TcType -> TcType -> TcM ();
unify loc origin t1 t2 = do {
  stuck <- unifyDeferring loc origin t1 t2;
  mapM_ reportStuck stuck} where {
  reportStuck (left, right) = emitError loc (UnificationError left right origin Nothing)};
unifyDeferring :: Maybe SourceSpan -> CtOrigin -> TcType -> TcType -> TcM [(TcType, TcType)];
unifyDeferring loc origin t1 t2 = do {
  t1' <- zonkType t1 >>= reduceTypeFamilies;
  t2' <- zonkType t2 >>= reduceTypeFamilies;
  result <- unifyCollecting loc t1' t2';
  case result of {
  Left err -> report err >> pure [];
  Right deferred -> concat <$> mapM retry deferred}} where {
  report (UnificationError left right _ provenance) = emitError loc (UnificationError left right origin provenance);
  report err = emitError loc err;
  retry (left, right) = do {
  left' <- zonkType left >>= reduceTypeFamilies;
  right' <- zonkType right >>= reduceTypeFamilies;
  if (left', right') == (left, right) then pure [(left, right)] else do {
  result <- unifyTypesAt loc left' right';
  case result of {
  Right () -> pure [];
  Left err -> report err >> pure []}}}};
unifyTypes :: TcType -> TcType -> TcM (Either TcErrorKind ());
unifyTypes = unifyTypesAt Nothing;
unifyTypesAt :: Maybe SourceSpan -> TcType -> TcType -> TcM (Either TcErrorKind ());
unifyTypesAt loc t1 t2 = do {
  result <- unifyCollecting loc t1 t2;
  case result of {
  Left err -> pure (Left err);
  Right deferred -> retryDeferred loc deferred}};
unifyCollecting :: Maybe SourceSpan -> TcType -> TcType -> TcM (Either TcErrorKind [(TcType, TcType)]);
unifyCollecting _ (TcMetaTv u1) (TcMetaTv u2) | u1 == u2 = pure (Right []);
unifyCollecting loc (TcMetaTv u) ty = fmap (const []) <$> unifyMetaTv loc u ty;
unifyCollecting loc ty (TcMetaTv u) = fmap (const []) <$> unifyMetaTv loc u ty;
unifyCollecting _ (TcTyVar v1) (TcTyVar v2) | sameTyVar v1 v2 = pure (Right []);
unifyCollecting loc t1 t2 | t1 == t2 = pure (Right []) | otherwise = do {
  stuck <- isStuckFamilyEquality t1 t2;
  if stuck then pure (Right [(t1, t2)]) else do {
  children <- decomposeNominalEquality t1 t2;
  case children of {
  Just pairs -> fmap concat . sequence <$> mapM (uncurry (unifyCollecting loc)) pairs;
  Nothing -> pure (Left (UnificationError t1 t2 (UnifyOrigin Nothing) Nothing))}}};
isStuckFamilyEquality :: TcType -> TcType -> TcM Bool;
isStuckFamilyEquality t1 t2 = do {
  left <- unsaturateFamilyApplication t1 >>= isTypeFamilyApplication;
  right <- unsaturateFamilyApplication t2 >>= isTypeFamilyApplication;
  pure (left || right)};
retryDeferred :: Maybe SourceSpan -> [(TcType, TcType)] -> TcM (Either TcErrorKind ());
retryDeferred loc pairs = sequence_ <$> mapM retryOne pairs where {
  retryOne (t1, t2) = do {
  t1' <- zonkType t1 >>= reduceTypeFamilies;
  t2' <- zonkType t2 >>= reduceTypeFamilies;
  if (t1', t2') == (t1, t2) then pure (Left (UnificationError t1 t2 (UnifyOrigin Nothing) Nothing)) else unifyTypesAt loc t1' t2'}};
unifyMetaTv :: Maybe SourceSpan -> Unique -> TcType -> TcM (Either TcErrorKind ());
unifyMetaTv loc u ty = do {
  ty' <- zonkType ty >>= refineGivenTyVarKinds;
  case ty' of {
  TcMetaTv u' | u == u' -> pure (Right ());
  _ | occursIn u ty' -> pure $ Left $ OccursCheckError (TcMetaTv u) ty' | isPolyType ty' -> pure $ Left $ UnificationError (TcMetaTv u) ty' (UnifyOrigin Nothing) Nothing | otherwise -> do {
  declaredKind <- readMetaTvKind u;
  solvedKind <- tcTypeKind ty';
  unifyKindsAt loc declaredKind solvedKind;
  writeMetaTv u ty';
  pure (Right ())}}};
occursIn :: Unique -> TcType -> Bool;
occursIn u = go where {
  go (TcMetaTv u') = u == u';
  go TcArrowTy = False;
  go (TcTyLit _) = False;
  go (TcTyVar _) = False;
  go (TcTyCon _ args) = any go args;
  go (TcFunTy a b) = go a || go b;
  go (TcForAllTy _ body) = go body;
  go (TcQualTy preds body) = any goPred preds || go body;
  go (TcAppTy f a) = go f || go a;
  goPred (ClassPred _ args) = any go args;
  goPred (EqPred a b) = go a || go b;
  goPred (IParamPred _ payload) = go payload;
  goPred (IrredPred constraint) = go constraint;
  goPred (QuantifiedPred variables antecedents consequent) = any (go . tvKind) variables || any goPred antecedents || goPred consequent}
}
