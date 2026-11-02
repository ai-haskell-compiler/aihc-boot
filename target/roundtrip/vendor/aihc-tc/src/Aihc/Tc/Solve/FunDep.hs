module Aihc.Tc.Solve.FunDep (improveFunDeps) where {
import Aihc.Tc.Constraint (Ct(..));
import Aihc.Tc.Env (ClassInfo(..), FunDep(..), InstanceInfo(..));
import Aihc.Tc.FunDep (atPositions);
import Aihc.Tc.Match (matchTypes);
import Aihc.Tc.Monad (TcM, getClassInstances, getKinds, lookupClass);
import Aihc.Tc.Types;
import Aihc.Tc.Unify (unifyTypes);
import Aihc.Tc.Zonk (zonkPred);
import Control.Monad (foldM, forM_, unless, void, zipWithM_);
import Data.Map.Strict (Map);
import Data.Map.Strict qualified as Map;
import Data.Maybe (catMaybes, mapMaybe);
improveFunDeps :: [Pred] -> [Ct] -> TcM Bool;
improveFunDeps givens constraints = do {
  improvable <- concat <$> mapM constraintFunDeps constraints;
  if null improvable then pure False else do {
  expandedGivens <- superClassClosure givens;
  let {
  siblings = map fst improvable};
  before <- mapM (zonkPred . ctPred) constraints;
  forM_ improvable (uncurry (improveConstraint expandedGivens siblings));
  after <- mapM (zonkPred . ctPred) constraints;
  pure (before /= after)}};
superClassClosure :: [Pred] -> TcM [Pred];
superClassClosure givens = reverse <$> foldM add [] givens where {
  add seen predicate | predicate `elem` seen = pure seen | otherwise = do {
  parents <- superClassesOf predicate;
  foldM add (predicate : seen) parents}};
superClassesOf :: Pred -> TcM [Pred];
superClassesOf predicate = case predicate of {
  ClassPred className arguments -> do {
  classInfo <- lookupClass className;
  case classInfo of {
  Nothing -> pure [];
  Just info -> do {
  kinds <- getKinds;
  let {
  substitution = Map.fromList [(tvUnique tyVar, argument) | (tyVar, argument) <- zip (ciTyVars info) arguments]};
  pure (mapMaybe (constraintTypeToPred kinds . applySubst substitution) (ciSuperClassTypes info))}}};
  _ -> pure []};
constraintFunDeps :: Ct -> TcM [(Pred, ClassInfo)];
constraintFunDeps constraint = case ctPred constraint of {
  ClassPred {} -> do {
  predicate <- zonkPred (ctPred constraint);
  closure <- superClassClosure [predicate];
  catMaybes <$> mapM withFunDeps closure};
  _ -> pure []} where {
  withFunDeps predicate = case predicate of {
  ClassPred className _ -> do {
  classInfo <- lookupClass className;
  pure $ case classInfo of {
  Just info | not (null (ciFunDeps info)) -> Just (predicate, info);
  _ -> Nothing}};
  _ -> pure Nothing}};
improveConstraint :: [Pred] -> [Pred] -> Pred -> ClassInfo -> TcM ();
improveConstraint givens siblings constraint info = do {
  siblingPredicates <- mapM zonkPred siblings;
  forM_ (ciFunDeps info) $ \dependency -> do {
  forM_ (givens <> siblingPredicates) $ \other -> improveFromPredicate (ciTyCon info) dependency constraint other;
  improveFromInstances (ciTyCon info) dependency constraint}};
improveFromPredicate :: TyCon -> FunDep -> Pred -> Pred -> TcM ();
improveFromPredicate className dependency constraint other = case other of {
  ClassPred otherClass otherArguments | tyConKey otherClass == tyConKey className -> do {
  predicate <- zonkPred constraint;
  case predicate of {
  ClassPred _ arguments -> case agreeTypes Map.empty (determiners dependency arguments) (determiners dependency otherArguments) of {
  Just substitution -> improveEqualities (map (substituteMetas substitution) (determined dependency arguments)) (map (substituteMetas substitution) (determined dependency otherArguments));
  Nothing -> pure ()};
  _ -> pure ()}};
  _ -> pure ()};
improveFromInstances :: TyCon -> FunDep -> Pred -> TcM ();
improveFromInstances className dependency constraint = do {
  instances <- getClassInstances className;
  forM_ instances $ \instanceInfo -> do {
  predicate <- zonkPred constraint;
  case predicate of {
  ClassPred _ arguments | Just substitution <- matchTypes (determiners dependency (iiHead instanceInfo)) (determiners dependency arguments) -> do {
  let {
  instanceDetermined = map (applySubst substitution) (determined dependency (iiHead instanceInfo));
  undetermined = any (\tyVar -> any (typeMentionsTyVar tyVar) instanceDetermined) (iiTyVars instanceInfo)};
  unless undetermined $ improveEqualities instanceDetermined (determined dependency arguments)};
  _ -> pure ()}}};
determiners :: FunDep -> [TcType] -> [TcType];
determiners dependency = atPositions (fdDeterminers dependency);
determined :: FunDep -> [TcType] -> [TcType];
determined dependency = atPositions (fdDetermined dependency);
improveEqualities :: [TcType] -> [TcType] -> TcM ();
improveEqualities left right | length left /= length right = pure () | otherwise = zipWithM_ improveOne left right where {
  improveOne leftType rightType | leftType == rightType = pure () | otherwise = void (unifyTypes leftType rightType)};
agreeTypes :: Map Unique TcType -> [TcType] -> [TcType] -> Maybe (Map Unique TcType);
agreeTypes substitution left right | length left /= length right = Nothing | otherwise = foldl step (Just substitution) (zip left right) where {
  step accumulator (leftType, rightType) = accumulator >>= \current -> agreeType current leftType rightType};
agreeType :: Map Unique TcType -> TcType -> TcType -> Maybe (Map Unique TcType);
agreeType substitution left right = case (substituteMetas substitution left, substituteMetas substitution right) of {
  (TcMetaTv unique, other) -> bindMeta substitution unique other;
  (other, TcMetaTv unique) -> bindMeta substitution unique other;
  (TcTyCon leftTyCon leftArguments, TcTyCon rightTyCon rightArguments) | tyConKey leftTyCon == tyConKey rightTyCon -> agreeTypes substitution leftArguments rightArguments;
  (TcFunTy leftArgument leftResult, TcFunTy rightArgument rightResult) -> agreeTypes substitution [leftArgument, leftResult] [rightArgument, rightResult];
  (TcAppTy leftFunction leftArgument, TcAppTy rightFunction rightArgument) -> agreeTypes substitution [leftFunction, leftArgument] [rightFunction, rightArgument];
  (leftType, rightType) | leftType == rightType -> Just substitution | otherwise -> Nothing};
bindMeta :: Map Unique TcType -> Unique -> TcType -> Maybe (Map Unique TcType);
bindMeta substitution unique ty | TcMetaTv other <- ty, other == unique = Just substitution | unique `elem` typeMetas ty = Nothing | otherwise = Just (Map.insert unique ty substitution);
substituteMetas :: Map Unique TcType -> TcType -> TcType;
substituteMetas substitution ty | Map.null substitution = ty | otherwise = go ty where {
  go current = case current of {
  TcMetaTv unique -> maybe current go (Map.lookup unique substitution);
  TcTyCon tyCon arguments -> TcTyCon tyCon (map go arguments);
  TcFunTy argument result -> TcFunTy (go argument) (go result);
  TcAppTy function argument -> mkAppTy (go function) (go argument);
  _ -> current}};
typeMetas :: TcType -> [Unique];
typeMetas ty = case ty of {
  TcMetaTv unique -> [unique];
  TcTyCon _ arguments -> concatMap typeMetas arguments;
  TcFunTy argument result -> typeMetas argument <> typeMetas result;
  TcAppTy function argument -> typeMetas function <> typeMetas argument;
  TcForAllTy _ body -> typeMetas body;
  _ -> []}
}
