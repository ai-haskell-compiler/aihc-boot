{-# LANGUAGE OverloadedStrings #-}
module Aihc.Tc.Solve.Defaulting (defaultAmbiguousMetas, isNumericClassName, isStandardClassName, standardDefaultTypeNames) where {
import Aihc.Tc.Constraint (Ct(..), CtOrigin(..), mkWantedCt);
import Aihc.Tc.Env (TyConInfo(..));
import Aihc.Tc.Generalize (predMetaVars);
import Aihc.Tc.Monad (TcM, freshEvVar, getDefaultTypes, lookupTyCon, tcSpeculate, writeMetaTv);
import Aihc.Tc.Solve.Dict (DictResult(..), solveDict);
import Aihc.Tc.Types (Pred(..), TcType(..), TyCon(..), Unique);
import Aihc.Tc.Zonk (zonkPred);
import Data.List (nub);
import Data.Maybe (mapMaybe);
import Data.Text (Text);
defaultAmbiguousMetas :: [Unique] -> [Ct] -> TcM Bool;
defaultAmbiguousMetas keep constraints = do {
  zonked <- mapM zonkCtPred constraints;
  candidates <- defaultCandidateTypes;
  if null candidates then pure False else do {
  let {
  ambiguous = filter (`notElem` keep) (nub (concatMap (predMetaVars . ctPred) zonked))};
  results <- mapM (defaultOneMeta candidates zonked) ambiguous;
  pure (or results)}} where {
  zonkCtPred ct = do {
  predicate <- zonkPred (ctPred ct);
  pure (ct {ctPred = predicate})}};
defaultOneMeta :: [TcType] -> [Ct] -> Unique -> TcM Bool;
defaultOneMeta candidates constraints unique = case defaultableGroup unique constraints of {
  Nothing -> pure False;
  Just classes -> do {
  solution <- firstSatisfying candidates classes;
  case solution of {
  Nothing -> pure False;
  Just ty -> do {
  writeMetaTv unique ty;
  pure True}}}};
defaultableGroup :: Unique -> [Ct] -> Maybe [TyCon];
defaultableGroup unique constraints = do {
  classes <- traverse classOfConstraint mentioning;
  let {
  names = map tyConName classes};
  if any isNumericClassName names && all isStandardClassName names then Just (nub classes) else Nothing} where {
  mentioning = [ctPred ct | ct <- constraints, unique `elem` predMetaVars (ctPred ct)];
  classOfConstraint predicate = case predicate of {
  ClassPred className [TcMetaTv argument] | argument == unique -> Just className;
  _ -> Nothing}};
firstSatisfying :: [TcType] -> [TyCon] -> TcM (Maybe TcType);
firstSatisfying [] _ = pure Nothing;
firstSatisfying (candidate : rest) classes = do {
  ok <- allM (hasInstance candidate) classes;
  if ok then pure (Just candidate) else firstSatisfying rest classes};
hasInstance :: TcType -> TyCon -> TcM Bool;
hasInstance ty className = tcSpeculate $ do {
  evidence <- freshEvVar;
  let {
  constraint = mkWantedCt (ClassPred className [ty]) evidence (InstOrigin (tyConName className)) Nothing};
  result <- solveDict constraint;
  case result of {
  DictSolved -> pure True;
  DictStuck _ -> pure False}};
defaultCandidateTypes :: TcM [TcType];
defaultCandidateTypes = do {
  declared <- getDefaultTypes;
  case declared of {
  Just types -> pure types;
  Nothing -> mapMaybe (fmap standardType) <$> mapM lookupTyCon standardDefaultTypeNames}} where {
  standardType info = TcTyCon (tciTyCon info) []};
standardDefaultTypeNames :: [Text];
standardDefaultTypeNames = ["Integer", "Double"];
isNumericClassName :: Text -> Bool;
isNumericClassName name = name `elem` ["Num", "Real", "Integral", "Fractional", "Floating", "RealFrac", "RealFloat"];
isStandardClassName :: Text -> Bool;
isStandardClassName name = isNumericClassName name || name `elem` ["Eq", "Ord", "Show", "Read", "Enum", "Bounded", "Ix", "Functor", "Applicative", "Alternative", "Monad", "MonadPlus", "MonadFail", "Foldable", "Traversable", "Semigroup", "Monoid", "IsString"];
allM :: (Monad m) => (a -> m Bool) -> [a] -> m Bool;
allM _ [] = pure True;
allM predicate (x : xs) = do {
  ok <- predicate x;
  if ok then allM predicate xs else pure False}
}
