module Aihc.Tc.Solve.Injective (improveInjectivity) where {
import Aihc.Tc.Env (TyConFlavor(..), TyConInfo(..), TypeFamilyInstanceInfo(..));
import Aihc.Tc.FunDep (atPositions);
import Aihc.Tc.Match (matchTypes);
import Aihc.Tc.Monad (TcM, lookupTyConByIdentity);
import Aihc.Tc.Solve.Decompose (decomposeNominalEquality);
import Aihc.Tc.Solve.Family (familyEquations, isTypeFamilyApplication, reduceTypeFamilies, unsaturateFamilyApplication);
import Aihc.Tc.Types;
import Aihc.Tc.Unify (unifyTypes);
import Aihc.Tc.Zonk (zonkPred, zonkType);
import Control.Monad (forM_, unless, void, zipWithM_);
improveInjectivity :: [Pred] -> [Pred] -> TcM Bool;
improveInjectivity givens wanteds = do {
  before <- mapM zonkPred wanteds;
  equalities <- givenEqualities givens;
  forM_ wanteds (improveConstraint equalities);
  after <- mapM zonkPred wanteds;
  pure (before /= after)};
givenEqualities :: [Pred] -> TcM [(TcType, TcType)];
givenEqualities givens = concat <$> mapM equality givens where {
  equality predicate = case predicate of {
  EqPred rawLeft rawRight -> do {
  left <- normalize rawLeft;
  right <- normalize rawRight;
  pure [(left, right), (right, left)]};
  _ -> pure []}};
normalize :: TcType -> TcM TcType;
normalize ty = zonkType ty >>= reduceTypeFamilies >>= unsaturateFamilyApplication;
improveConstraint :: [(TcType, TcType)] -> Pred -> TcM ();
improveConstraint equalities predicate = case predicate of {
  EqPred left right -> improveTypes equalities left right;
  _ -> pure ()};
improveTypes :: [(TcType, TcType)] -> TcType -> TcType -> TcM ();
improveTypes equalities rawLeft rawRight = do {
  left <- normalize rawLeft;
  right <- normalize rawRight;
  sequence_ [improvePair leftReading rightReading | leftReading <- readings left, rightReading <- readings right];
  children <- decomposeNominalEquality left right;
  forM_ (concat children) (uncurry (improveTypes equalities))} where {
  readings ty = ty : [other | (one, other) <- equalities, sameType one ty]};
data InjectiveApplication = InjectiveApplication !TyCon ![Int] ![TcType];
injectiveApplication :: TcType -> TcM (Maybe InjectiveApplication);
injectiveApplication ty = case ty of {
  TcTyCon tyCon arguments -> do {
  maybeInfo <- lookupTyConByIdentity tyCon;
  pure $ case maybeInfo of {
  Just info | tciFlavor info == TypeFamilyTyCon, length arguments == tciArity info, Just positions <- tciInjectivity info, not (null positions) -> Just (InjectiveApplication tyCon positions arguments);
  _ -> Nothing}};
  _ -> pure Nothing};
improvePair :: TcType -> TcType -> TcM ();
improvePair left right = do {
  leftFamily <- injectiveApplication left;
  rightFamily <- injectiveApplication right;
  case (leftFamily, rightFamily) of {
  (Just (InjectiveApplication leftTyCon positions leftArguments), Just (InjectiveApplication rightTyCon _ rightArguments)) | leftTyCon == rightTyCon -> improveEqualities (atPositions positions leftArguments) (atPositions positions rightArguments);
  (Just family, Nothing) -> improveFromEquations family right;
  (Nothing, Just family) -> improveFromEquations family left;
  _ -> pure ()}};
improveFromEquations :: InjectiveApplication -> TcType -> TcM ();
improveFromEquations (InjectiveApplication tyCon positions arguments) target = do {
  targetIsFamily <- isTypeFamilyApplication target;
  unless (targetIsFamily || isMetaTv target) $ do {
  equations <- familyEquations tyCon;
  case concatMap (determinedByEquation positions target) equations of {
  [determined] -> improveEqualities determined (atPositions positions arguments);
  _ -> pure ()}}};
isMetaTv :: TcType -> Bool;
isMetaTv ty = case ty of {
  TcMetaTv _ -> True;
  TcArrowTy -> True;
  _ -> False};
determinedByEquation :: [Int] -> TcType -> TypeFamilyInstanceInfo -> [[TcType]];
determinedByEquation positions target equation = case (tfiiLeft equation, matchTypes [tfiiRight equation] [target]) of {
  (TcTyCon _ patterns, Just substitution) | length patterns > maximum positions, determined <- map (applySubst substitution) (atPositions positions patterns), not (any (\tyVar -> any (typeMentionsTyVar tyVar) determined) (tfiiTyVars equation)) -> [determined];
  _ -> []};
improveEqualities :: [TcType] -> [TcType] -> TcM ();
improveEqualities left right | length left /= length right = pure () | otherwise = zipWithM_ improveOne left right where {
  improveOne leftType rightType | sameType leftType rightType = pure () | otherwise = void (unifyTypes leftType rightType)}
}
