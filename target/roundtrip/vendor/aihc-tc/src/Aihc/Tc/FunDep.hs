module Aihc.Tc.FunDep (checkInstanceFunDeps, atPositions) where {
import Aihc.Parser.Syntax (SourceSpan);
import Aihc.Tc.Env (ClassInfo(..), FunDep(..), InstanceInfo(..));
import Aihc.Tc.Error (TcErrorKind(..));
import Aihc.Tc.Monad (TcM, emitError, freshUnique, getClassInstances, getUndecidableInstances, lookupClass);
import Aihc.Tc.Types;
import Aihc.Tc.Zonk (zonkType);
import Control.Monad (forM_, unless);
import Data.Map.Strict (Map);
import Data.Map.Strict qualified as Map;
import Data.Text (Text);
checkInstanceFunDeps :: Maybe SourceSpan -> ClassInfo -> [TyVarId] -> [TcType] -> [Pred] -> TcM ();
checkInstanceFunDeps loc classInfo tyVars headTypes context = unless (null (ciFunDeps classInfo)) $ do {
  headTypes' <- mapM zonkType headTypes;
  contextDependencies <- predicateFunDeps tyVars context;
  forM_ (ciFunDeps classInfo) (checkCoverage loc classInfo tyVars headTypes' contextDependencies);
  others <- getClassInstances (ciTyCon classInfo);
  forM_ others $ \other -> do {
  otherHead <- freshenTypes (iiTyVars other) (iiHead other);
  forM_ (ciFunDeps classInfo) (checkConsistency loc classInfo headTypes' otherHead)}};
checkCoverage :: Maybe SourceSpan -> ClassInfo -> [TyVarId] -> [TcType] -> [([TyVarId], [TyVarId])] -> FunDep -> TcM ();
checkCoverage loc classInfo tyVars headTypes contextDependencies dependency = do {
  liberal <- getUndecidableInstances;
  let {
  determiners = atPositions (fdDeterminers dependency) headTypes;
  determined = atPositions (fdDetermined dependency) headTypes;
  mentioned types = [tyVar | tyVar <- tyVars, any (typeMentionsTyVar tyVar) types];
  reached | liberal = closeOver contextDependencies (mentioned determiners) | otherwise = mentioned determiners;
  escaping = filter (`notElem` reached) (mentioned determined)};
  unless (null escaping) $ emitError loc (funDepCoverageError classInfo headTypes dependency)};
predicateFunDeps :: [TyVarId] -> [Pred] -> TcM [([TyVarId], [TyVarId])];
predicateFunDeps tyVars context = concat <$> mapM predicateDependencies context where {
  predicateDependencies predicate = case predicate of {
  ClassPred className arguments -> do {
  classInfo <- lookupClass className;
  pure [(variables (fdDeterminers dependency), variables (fdDetermined dependency)) | Just info <- [classInfo], dependency <- ciFunDeps info]} where {
  variables positions = [tyVar | tyVar <- tyVars, any (typeMentionsTyVar tyVar) (atPositions positions arguments)]};
  _ -> pure []}};
closeOver :: [([TyVarId], [TyVarId])] -> [TyVarId] -> [TyVarId];
closeOver dependencies = go where {
  go reached = let {
  step = [determined | (determiners, determineds) <- dependencies, all (`elem` reached) determiners, determined <- determineds, determined `notElem` reached]} in if null step then reached else go (reached <> step)};
checkConsistency :: Maybe SourceSpan -> ClassInfo -> [TcType] -> [TcType] -> FunDep -> TcM ();
checkConsistency loc classInfo headTypes otherHead dependency = case unifyOpen Map.empty (determiners headTypes) (determiners otherHead) of {
  Unified substitution -> case unifyOpen substitution (determined headTypes) (determined otherHead) of {
  NotUnified -> emitError loc (funDepConflictError classInfo headTypes otherHead dependency);
  _ -> pure ()};
  _ -> pure ()} where {
  determiners = atPositions (fdDeterminers dependency);
  determined = atPositions (fdDetermined dependency)};
funDepCoverageError :: ClassInfo -> [TcType] -> FunDep -> TcErrorKind;
funDepCoverageError classInfo headTypes dependency = InstanceFunDepCoverage (ClassPred (ciTyCon classInfo) headTypes) (funDepNames classInfo (fdDeterminers dependency)) (funDepNames classInfo (fdDetermined dependency));
funDepConflictError :: ClassInfo -> [TcType] -> [TcType] -> FunDep -> TcErrorKind;
funDepConflictError classInfo headTypes otherHead dependency = InstanceFunDepConflict (ClassPred (ciTyCon classInfo) headTypes) (ClassPred (ciTyCon classInfo) otherHead) (funDepNames classInfo (fdDeterminers dependency)) (funDepNames classInfo (fdDetermined dependency));
funDepNames :: ClassInfo -> [Int] -> [Text];
funDepNames classInfo positions = map tvName (atPositions positions (ciTyVars classInfo));
atPositions :: [Int] -> [a] -> [a];
atPositions positions values = [value | position <- positions, value <- take 1 (drop position values)];
freshenTypes :: [TyVarId] -> [TcType] -> TcM [TcType];
freshenTypes tyVars types = do {
  renamings <- mapM freshen tyVars;
  let {
  substitution = Map.fromList renamings};
  pure (map (applySubst substitution) types)} where {
  freshen tyVar = do {
  unique <- freshUnique;
  pure (tvUnique tyVar, TcTyVar (mkTyVarId (tvName tyVar) unique (tvKind tyVar)))}};
data UnifyOpen = Unified !(Map Unique TcType) | NotUnified | Unknown;
unifyOpen :: Map Unique TcType -> [TcType] -> [TcType] -> UnifyOpen;
unifyOpen substitution left right | length left /= length right = Unknown | otherwise = foldl step (Unified substitution) (zip left right) where {
  step (Unified current) (l, r) = unifyOpenType current l r;
  step outcome _ = outcome};
unifyOpenType :: Map Unique TcType -> TcType -> TcType -> UnifyOpen;
unifyOpenType substitution left right = case (walk substitution left, walk substitution right) of {
  (TcMetaTv {}, _) -> Unknown;
  (_, TcMetaTv {}) -> Unknown;
  (TcTyVar tyVar, other) -> bind substitution tyVar other;
  (other, TcTyVar tyVar) -> bind substitution tyVar other;
  (TcForAllTy {}, _) -> Unknown;
  (_, TcForAllTy {}) -> Unknown;
  (TcQualTy {}, _) -> Unknown;
  (_, TcQualTy {}) -> Unknown;
  (TcFunTy leftArgument leftResult, TcFunTy rightArgument rightResult) -> unifyOpen substitution [leftArgument, leftResult] [rightArgument, rightResult];
  (TcTyCon leftTyCon [], TcTyCon rightTyCon []) | tyConKey leftTyCon == tyConKey rightTyCon -> Unified substitution | otherwise -> NotUnified;
  (TcArrowTy, TcArrowTy) -> Unified substitution;
  (leftType, rightType) -> case (splitApp leftType, splitApp rightType) of {
  (Just (leftFunction, leftArgument), Just (rightFunction, rightArgument)) -> unifyOpen substitution [leftFunction, leftArgument] [rightFunction, rightArgument];
  (Nothing, Nothing) -> NotUnified;
  _ -> Unknown}};
splitApp :: TcType -> Maybe (TcType, TcType);
splitApp ty = case ty of {
  TcAppTy function argument -> Just (function, argument);
  TcTyCon tyCon arguments | not (null arguments) -> Just (TcTyCon tyCon (init arguments), last arguments);
  _ -> Nothing};
walk :: Map Unique TcType -> TcType -> TcType;
walk substitution ty = case ty of {
  TcTyVar tyVar | Just bound <- Map.lookup (tvUnique tyVar) substitution -> walk substitution bound;
  _ -> ty};
bind :: Map Unique TcType -> TyVarId -> TcType -> UnifyOpen;
bind substitution tyVar ty | TcTyVar other <- ty, tvUnique other == tvUnique tyVar = Unified substitution | typeMentionsTyVar tyVar ty = NotUnified | otherwise = Unified (Map.insert (tvUnique tyVar) ty substitution)
}
