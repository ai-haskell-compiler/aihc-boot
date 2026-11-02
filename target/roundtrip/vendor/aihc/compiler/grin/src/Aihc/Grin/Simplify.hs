module Aihc.Grin.Simplify (simplifyGrinProgram) where {
import Aihc.Grin.Analysis (freeExprVars);
import Aihc.Grin.Syntax;
import Control.Applicative ((<|>));
import Data.Graph (SCC(..), stronglyConnComp);
import Data.List qualified as List;
import Data.Map.Strict (Map);
import Data.Map.Strict qualified as Map;
import Data.Set (Set);
import Data.Set qualified as Set;
import Data.Text (Text);
data Env = Env { envFunctions :: !(Map FunctionName (Int, GrinResultRep)), envConstructors :: !(Map Text [[GrinRep]]), envStatics :: !(Map Text GrinNode), envKnown :: !(Map GrinVar GrinNode), envEvaluated :: !(Set GrinVar) };
simplifyGrinProgram :: GrinProgram -> GrinProgram;
simplifyGrinProgram program = program {grinFunctions = map simplifyFunction (grinFunctions program)} where {
  env = Env {envFunctions = Map.fromList [(grinFunctionName function, (length (grinFunctionParameters function), grinFunctionResultRep function)) | function <- grinFunctions program], envConstructors = Map.fromList [(grinConstructorName c, grinConstructorLayouts c) | c <- grinConstructors program], envStatics = Map.fromList ([(grinConstructorName c, GrinNode (GrinConstructor (grinConstructorName c) 0) []) | c <- grinConstructors program, null (grinConstructorLayouts c)] <> [(grinGlobalName global, grinGlobalNode global) | global <- grinGlobals program]), envKnown = Map.empty, envEvaluated = Set.empty};
  simplifyFunction function = function {grinFunctionBody = fst (simplifyExpr env (grinFunctionBody function))}};
knownNode :: Env -> GrinValue -> Maybe GrinNode;
knownNode env value = case value of {
  GrinVarValue var -> Map.lookup var (envKnown env);
  GrinGlobalValue name -> Map.lookup name (envStatics env);
  GrinLitValue {} -> Nothing};
isEvaluated :: Env -> GrinValue -> Bool;
isEvaluated env value = case knownNode env value of {
  Just node -> isValueNode node;
  Nothing -> case value of {
  GrinVarValue var -> Set.member var (envEvaluated env);
  _ -> False}};
isValueNode :: GrinNode -> Bool;
isValueNode node = case grinNodeTag node of {
  GrinThunk {} -> False;
  GrinConstructor {} -> True;
  GrinClosure {} -> True};
forget :: [GrinVar] -> Env -> Env;
forget vars env = env {envKnown = List.foldl' (flip Map.delete) (envKnown env) vars, envEvaluated = envEvaluated env `Set.difference` Set.fromList vars};
know :: GrinVar -> GrinNode -> Env -> Env;
know var node env = env {envKnown = Map.insert var node (envKnown env)};
evaluated :: GrinVar -> Env -> Env;
evaluated var env = env {envEvaluated = Set.insert var (envEvaluated env)};
bindResults :: Env -> [GrinVar] -> GrinExpr -> Env;
bindResults env vars value = case (vars, value) of {
  ([var], GrinStore node) -> know var node rebound;
  ([var], GrinConstant [copied]) | Just node <- knownNode rebound copied -> know var node rebound | isEvaluated rebound copied -> evaluated var rebound;
  ([var], GrinEval {}) -> evaluated var rebound;
  _ -> rebound} where {
  rebound = forget vars env};
isPureExpression :: GrinExpr -> Bool;
isPureExpression expression = case expression of {
  GrinStore {} -> True;
  GrinConstant {} -> True;
  _ -> False};
simplifyExpr :: Env -> GrinExpr -> (GrinExpr, Set GrinVar);
simplifyExpr env expression = case expression of {
  GrinBind vars valueExpression body -> let {
  (value', valueFree) = simplifyExpr env valueExpression} in bindSimplified vars (value', valueFree) (simplifyExpr (bindResults env vars value') body);
  GrinStoreRec bindings body -> simplifyStoreRec env bindings body;
  GrinStoreRecUnchecked bindings body -> let {
  inner = List.foldl' (\current (var, node) -> know var node current) (forget (map fst bindings) env) bindings;
  (body', bodyFree) = simplifyExpr inner body} in (GrinStoreRecUnchecked bindings body', (foldMap (freeNodeVars . snd) bindings <> bodyFree) `Set.difference` Set.fromList (map fst bindings));
  GrinEval _ _ value | isEvaluated env value -> (GrinConstant [value], freeValueVars value);
  GrinApply resultRep function arguments | Just node <- knownNode env function, Just applied <- applyKnown env resultRep node arguments -> simplifyExpr env applied;
  GrinCase scrutinee binder alternatives | Just selected <- selectAlternative env scrutinee binder alternatives -> simplifyExpr env selected | otherwise -> let {
  simplified = map (simplifyAlternative env scrutinee binder) alternatives} in (GrinCase scrutinee binder (map fst simplified), freeValueVars scrutinee <> foldMap snd simplified);
  _ -> (expression, freeExprVars expression)};
bindSimplified :: [GrinVar] -> (GrinExpr, Set GrinVar) -> (GrinExpr, Set GrinVar) -> (GrinExpr, Set GrinVar);
bindSimplified vars (value, valueFree) (body, bodyFree) | isPureExpression value && not (any (`Set.member` bodyFree) vars) = (body, bodyFree) | body == GrinConstant (map GrinVarValue vars) = (value, valueFree) | otherwise = (GrinBind vars value body, valueFree <> (bodyFree `Set.difference` Set.fromList vars));
simplifyAlternative :: Env -> GrinValue -> GrinVar -> GrinAlt -> (GrinAlt, Set GrinVar);
simplifyAlternative env scrutinee binder alternative = (alternative {grinAltRhs = rhs}, rhsFree `Set.difference` Set.fromList (binder : grinAltBinders alternative)) where {
  (rhs, rhsFree) = simplifyExpr altEnv (grinAltRhs alternative);
  matched = binder : case scrutinee of {
  GrinVarValue var -> [var];
  _ -> []};
  altEnv | grinValueRuntimeRep scrutinee /= liftedGrinRep = env | otherwise = case grinAltCon alternative of {
  GrinDataAlt name -> let {
  node = GrinNode (GrinConstructor name 0) (map GrinVarValue (grinAltBinders alternative))} in List.foldl' (\current var -> know var node current) (forget matched env) matched;
  _ -> List.foldl' (flip evaluated) (forget matched env) matched}};
selectAlternative :: Env -> GrinValue -> GrinVar -> [GrinAlt] -> Maybe GrinExpr;
selectAlternative env scrutinee binder alternatives = case knownNode env scrutinee of {
  Just (GrinNode (GrinConstructor name 0) fields) -> do {
  alternative <- List.find ((== GrinDataAlt name) . grinAltCon) alternatives <|> defaultAlternative;
  fieldBinds <- case grinAltCon alternative of {
  GrinDataAlt {} | length (grinAltBinders alternative) == length fields, and (zipWith sameRep (grinAltBinders alternative) fields) -> Just (zip (grinAltBinders alternative) fields);
  GrinDataAlt {} -> Nothing;
  _ -> Just []};
  pure (bindCopies ((binder, scrutinee) : fieldBinds) (grinAltRhs alternative))};
  _ -> case scrutinee of {
  GrinLitValue literal -> do {
  alternative <- List.find ((== GrinLitAlt literal) . grinAltCon) alternatives <|> defaultAlternative;
  pure (bindCopies [(binder, scrutinee)] (grinAltRhs alternative))};
  _ -> Nothing}} where {
  defaultAlternative = List.find ((== GrinDefaultAlt) . grinAltCon) alternatives;
  sameRep var value = grinVarRuntimeRep var == grinValueRuntimeRep value};
bindCopies :: [(GrinVar, GrinValue)] -> GrinExpr -> GrinExpr;
bindCopies copies body = foldr (\(var, value) rest -> GrinBind [var] (GrinConstant [value]) rest) body copies;
applyKnown :: Env -> GrinResultRep -> GrinNode -> [GrinValue] -> Maybe GrinExpr;
applyKnown env resultRep node arguments = case grinNodeTag node of {
  GrinClosure functionName (layout : remaining) | map grinValueRuntimeRep arguments == layout -> case remaining of {
  [] | Just (arity, declared) <- Map.lookup functionName (envFunctions env), arity == length fields, declared == ResultForwarded || declared == resultRep -> Just (GrinCall resultRep functionName fields);
  _ : _ | resultRep == liftedResultRep -> Just (GrinStore (GrinNode (GrinClosure functionName remaining) fields));
  _ -> Nothing};
  GrinConstructor name remaining | remaining >= 1, resultRep == liftedResultRep, Just layouts <- Map.lookup name (envConstructors env), length layouts >= remaining, map grinValueRuntimeRep arguments == layouts !! (length layouts - remaining) -> Just (GrinStore (GrinNode (GrinConstructor name (remaining - 1)) fields));
  _ -> Nothing} where {
  fields = grinNodeFields node <> arguments};
simplifyStoreRec :: Env -> [(GrinVar, GrinNode)] -> GrinExpr -> (GrinExpr, Set GrinVar);
simplifyStoreRec env bindings body = allocate (forget (map fst bindings) env) components where {
  groupVars = Set.fromList (map fst bindings);
  components = stronglyConnComp [(binding, var, Set.toList (freeNodeVars node `Set.intersection` groupVars)) | binding@(var, node) <- bindings];
  allocate current remaining = case remaining of {
  [] -> simplifyExpr current body;
  AcyclicSCC (var, node) : rest -> bindSimplified [var] (GrinStore node, freeNodeVars node) (allocate (know var node current) rest);
  CyclicSCC group : rest -> let {
  inner = List.foldl' (\accumulated (var, node) -> know var node accumulated) current group;
  (rest', restFree) = allocate inner rest;
  vars = map fst group} in if any (`Set.member` restFree) vars then (GrinStoreRec group rest', (foldMap (freeNodeVars . snd) group <> restFree) `Set.difference` Set.fromList vars) else (rest', restFree)}};
freeNodeVars :: GrinNode -> Set GrinVar;
freeNodeVars = foldMap freeValueVars . grinNodeFields;
freeValueVars :: GrinValue -> Set GrinVar;
freeValueVars value = case value of {
  GrinVarValue var -> Set.singleton var;
  GrinGlobalValue {} -> Set.empty;
  GrinLitValue {} -> Set.empty}
}
