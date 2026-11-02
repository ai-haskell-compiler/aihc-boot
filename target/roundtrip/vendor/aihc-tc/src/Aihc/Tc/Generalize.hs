module Aihc.Tc.Generalize (generalize, generalizeIgnoring, generalizeAndCommit, generalizeAndCommitIgnoring, generalizeGroupAndCommitIgnoring, environmentMetaVars, collectMetaVars, predMetaVars) where {
import Aihc.Tc.Kind (defaultKindMetas, deferKindMetas);
import Aihc.Tc.Monad (TcBinder(..), TcM, deferKindMeta, freshSkolemTv, getKinds, getMetaTermEnv, getPolyKinds, readMetaTv, readMetaTvKind, writeMetaTv);
import Aihc.Tc.Types;
import Aihc.Tc.Zonk (zonkType);
import Control.Monad (forM_, void, when);
import Data.Map.Strict qualified as Map;
import Data.Set qualified as Set;
import Data.Text qualified as T;
generalize :: TcType -> [Pred] -> TcM TypeScheme;
generalize = generalizeIgnoring Set.empty;
generalizeAndCommit :: TcType -> [Pred] -> TcM TypeScheme;
generalizeAndCommit = generalizeAndCommitIgnoring Set.empty;
generalizeIgnoring :: Set.Set TcTermKey -> TcType -> [Pred] -> TcM TypeScheme;
generalizeIgnoring ignoredKeys ty preds = fst <$> generalizeIgnoringWithSubst ignoredKeys ty preds;
generalizeAndCommitIgnoring :: Set.Set TcTermKey -> TcType -> [Pred] -> TcM TypeScheme;
generalizeAndCommitIgnoring ignoredKeys ty preds = do {
  (scheme, subst) <- generalizeIgnoringWithSubst ignoredKeys ty preds;
  forM_ subst (uncurry writeMetaTv);
  pure scheme};
generalizeGroupAndCommitIgnoring :: Set.Set TcTermKey -> [Unique] -> [(TcType, [Pred])] -> TcM [TypeScheme];
generalizeGroupAndCommitIgnoring ignoredKeys monoMetaVars bindings = do {
  envMetaVars <- (++ monoMetaVars) <$> environmentMetaVars ignoredKeys;
  zonked <- mapM zonkBinding bindings;
  forM_ zonked (uncurry (defaultRuntimeRepMetas envMetaVars));
  zonked' <- mapM zonkBinding zonked;
  let {
  bindingMetaVars = [nubOrd (collectMetaVars ty ++ concatMap predMetaVars preds) | (ty, preds) <- zonked'];
  uniqueMetaVars = filter (`notElem` envMetaVars) (nubOrd (concat bindingMetaVars))};
  settleKinds <- settleKindsNow;
  when settleKinds (mapM_ defaultMetaKind uniqueMetaVars);
  tvs <- metaVarsToTyVars settleKinds uniqueMetaVars;
  let {
  subst = zip uniqueMetaVars (map TcTyVar tvs)};
  forM_ subst (uncurry writeMetaTv);
  pure [Scheme [tv | (unique, tv) <- zip uniqueMetaVars tvs, unique `elem` metaVars] [] (map (substMetasPred subst) preds) (substMetas subst ty) | ((ty, preds), metaVars) <- zip zonked' bindingMetaVars]} where {
  zonkBinding (ty, preds) = (,) <$> zonkType ty <*> mapM zonkPred preds};
generalizeIgnoringWithSubst :: Set.Set TcTermKey -> TcType -> [Pred] -> TcM (TypeScheme, [(Unique, TcType)]);
generalizeIgnoringWithSubst ignoredKeys ty preds = do {
  envMetaVars <- environmentMetaVars ignoredKeys;
  ty' <- zonkType ty;
  preds' <- mapM zonkPred preds;
  defaultRuntimeRepMetas envMetaVars ty' preds';
  ty'' <- zonkType ty';
  preds'' <- mapM zonkPred preds';
  let {
  freeMetaVars = collectMetaVars ty'' ++ concatMap predMetaVars preds'';
  uniqueFreeMetaVars = nubOrd freeMetaVars;
  uniqueMetaVars = filter (`notElem` envMetaVars) uniqueFreeMetaVars};
  settleKinds <- settleKindsNow;
  when settleKinds (mapM_ defaultMetaKind uniqueMetaVars);
  tvs <- metaVarsToTyVars settleKinds uniqueMetaVars;
  let {
  subst = zip uniqueMetaVars (map TcTyVar tvs)};
  let {
  quantifiedTy = substMetas subst ty''};
  let {
  quantifiedPreds = map (substMetasPred subst) preds''};
  pure (Scheme tvs [] quantifiedPreds quantifiedTy, subst)};
defaultRuntimeRepMetas :: [Unique] -> TcType -> [Pred] -> TcM ();
defaultRuntimeRepMetas envMetaVars ty preds = do {
  reachable <- reachableMetaVars (collectMetaVars ty ++ concatMap predMetaVars preds);
  mapM_ defaultOne (filter (`notElem` envMetaVars) reachable)} where {
  defaultOne unique = do {
  solution <- readMetaTv unique;
  case solution of {
  Just {} -> pure ();
  Nothing -> do {
  kinds <- getKinds;
  kind <- zonkType =<< readMetaTvKind unique;
  case kind of {
  KRuntimeRep -> writeMetaTv unique (liftedRep kinds);
  _ -> pure ()}}}}};
reachableMetaVars :: [Unique] -> TcM [Unique];
reachableMetaVars = go [] where {
  go seen [] = pure (reverse seen);
  go seen (unique : rest) | unique `elem` seen = go seen rest | otherwise = do {
  kind <- zonkType =<< readMetaTvKind unique;
  go (unique : seen) (collectMetaVars kind ++ rest)}};
environmentMetaVars :: Set.Set TcTermKey -> TcM [Unique];
environmentMetaVars ignoredKeys = do {
  env <- getMetaTermEnv;
  nubOrd . concat <$> mapM binderMetaVars [binder | (key, binder) <- Map.toList env, key `Set.notMember` ignoredKeys]};
collectMetaVars :: TcType -> [Unique];
collectMetaVars (TcMetaTv u) = [u];
collectMetaVars TcArrowTy = [];
collectMetaVars (TcTyLit _) = [];
collectMetaVars (TcTyVar _) = [];
collectMetaVars (TcTyCon _ args) = concatMap collectMetaVars args;
collectMetaVars (TcFunTy a b) = collectMetaVars a ++ collectMetaVars b;
collectMetaVars (TcForAllTy _ body) = collectMetaVars body;
collectMetaVars (TcQualTy ps body) = concatMap predMetaVars ps ++ collectMetaVars body;
collectMetaVars (TcAppTy f a) = collectMetaVars f ++ collectMetaVars a;
predMetaVars :: Pred -> [Unique];
predMetaVars (ClassPred _ args) = concatMap collectMetaVars args;
predMetaVars (EqPred a b) = collectMetaVars a ++ collectMetaVars b;
predMetaVars (IParamPred _ payload) = collectMetaVars payload;
predMetaVars (IrredPred constraint) = collectMetaVars constraint;
predMetaVars (QuantifiedPred variables antecedents consequent) = concatMap (collectMetaVars . tvKind) variables ++ concatMap predMetaVars antecedents ++ predMetaVars consequent;
metaVarsToTyVars :: Bool -> [Unique] -> TcM [TyVarId];
metaVarsToTyVars settleKinds uniques = mapM makeTyVar (zip [0 ..] uniques) where {
  makeTyVar (index, unique) = do {
  kind <- readMetaTvKind unique >>= (if settleKinds then defaultKindMetas else deferKindMetas deferKindMeta);
  rawTyVar <- freshSkolemTv (mkName index);
  pure (setTyVarKind kind rawTyVar)};
  mkName i = let {
  c = toEnum (fromEnum 'a' + i `mod` 26)} in if i < 26 then T.singleton c else T.pack [c] <> T.pack (show (i `div` 26))};
defaultMetaKind :: Unique -> TcM ();
defaultMetaKind unique = do {
  kind <- readMetaTvKind unique;
  void (defaultKindMetas kind)};
settleKindsNow :: TcM Bool;
settleKindsNow = not <$> getPolyKinds;
substMetas :: [(Unique, TcType)] -> TcType -> TcType;
substMetas subst = go where {
  go (TcMetaTv u) = case lookup u subst of {
  Just ty -> ty;
  Nothing -> TcMetaTv u};
  go TcArrowTy = TcArrowTy;
  go ty@(TcTyLit _) = ty;
  go (TcTyVar tv) = TcTyVar tv;
  go (TcTyCon tc args) = TcTyCon tc (map go args);
  go (TcFunTy a b) = TcFunTy (go a) (go b);
  go (TcForAllTy tv body) = TcForAllTy tv (go body);
  go (TcQualTy ps body) = TcQualTy (map (substMetasPred subst) ps) (go body);
  go (TcAppTy f a) = mkAppTy (go f) (go a)};
substMetasPred :: [(Unique, TcType)] -> Pred -> Pred;
substMetasPred subst (ClassPred cls args) = ClassPred cls (map (substMetas subst) args);
substMetasPred subst (EqPred a b) = EqPred (substMetas subst a) (substMetas subst b);
substMetasPred subst (IParamPred name payload) = IParamPred name (substMetas subst payload);
substMetasPred subst (IrredPred constraint) = IrredPred (substMetas subst constraint);
substMetasPred subst (QuantifiedPred variables antecedents consequent) = QuantifiedPred (map (\variable -> setTyVarKind (substMetas subst (tvKind variable)) variable) variables) (map (substMetasPred subst) antecedents) (substMetasPred subst consequent);
zonkPred :: Pred -> TcM Pred;
zonkPred (ClassPred cls args) = ClassPred cls <$> mapM zonkType args;
zonkPred (EqPred a b) = EqPred <$> zonkType a <*> zonkType b;
zonkPred (IParamPred name payload) = IParamPred name <$> zonkType payload;
zonkPred (IrredPred constraint) = IrredPred <$> zonkType constraint;
zonkPred (QuantifiedPred variables antecedents consequent) = QuantifiedPred <$> mapM zonkVariable variables <*> mapM zonkPred antecedents <*> zonkPred consequent where {
  zonkVariable variable = setTyVarKind <$> zonkType (tvKind variable) <*> pure variable};
binderMetaVars :: TcBinder -> TcM [Unique];
binderMetaVars (TcIdBinder (ForAll _ preds ty) _) | not (typeMentionsMeta ty) && not (any predicateMentionsMeta preds) = pure [] | otherwise = do {
  ty' <- zonkType ty;
  preds' <- mapM zonkPred preds;
  pure (collectMetaVars ty' ++ concatMap predMetaVars preds')};
binderMetaVars (TcMonoIdBinder ty) | not (typeMentionsMeta ty) = pure [] | otherwise = collectMetaVars <$> zonkType ty;
nubOrd :: (Ord a) => [a] -> [a];
nubOrd = go [] where {
  go _ [] = [];
  go seen (x : xs) | x `elem` seen = go seen xs | otherwise = x : go (x : seen) xs}
}
