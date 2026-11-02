module Aihc.Grin.Anf (normalizeGrinProgram, normalizeGrinExpr) where {
import Aihc.Grin.Syntax;
import Data.Map.Strict (Map);
import Data.Map.Strict qualified as Map;
import Data.Set qualified as Set;
type Substitution = Map GrinVar GrinValue;
normalizeGrinProgram :: GrinProgram -> GrinProgram;
normalizeGrinProgram program = program {grinFunctions = map normalizeFunction (grinFunctions program)} where {
  normalizeFunction function = function {grinFunctionBody = normalizeGrinExpr (grinFunctionBody function)}};
normalizeGrinExpr :: GrinExpr -> GrinExpr;
normalizeGrinExpr expression = normalizeInto Map.empty expression (\_ tailExpression -> tailExpression) where {
  normalizeInto substitution current continue = case current of {
  GrinBind resultVars valueExpression body -> normalizeInto substitution valueExpression $ \valueSubstitution normalizedValue -> case normalizedValue of {
  GrinConstant values | Just extended <- extend valueSubstitution resultVars values -> normalizeInto extended body continue;
  _ -> GrinBind resultVars normalizedValue (normalizeInto (shadow resultVars valueSubstitution) body continue)};
  GrinStoreRec bindings body -> storeRec GrinStoreRec bindings body;
  GrinStoreRecUnchecked bindings body -> storeRec GrinStoreRecUnchecked bindings body;
  GrinIfWhnf value ready slow -> continue substitution (GrinIfWhnf (useValue substitution value) (normalizeExpr substitution ready) (normalizeExpr substitution slow));
  GrinCase scrutinee binder alternatives -> continue substitution (GrinCase (useValue substitution scrutinee) binder (map (normalizeAlternative (shadow [binder] substitution)) alternatives));
  _ -> continue substitution (mapExprValues (useValue substitution) current)} where {
  storeRec rebuild bindings body = let {
  inner = shadow (map fst bindings) substitution;
  nodes = [(var, useNode inner node) | (var, node) <- bindings]} in continue substitution (rebuild nodes (normalizeExpr inner body))};
  normalizeAlternative substitution alternative = alternative {grinAltRhs = normalizeExpr (shadow (grinAltBinders alternative) substitution) (grinAltRhs alternative)};
  normalizeExpr substitution expression' = normalizeInto substitution expression' (\_ tailExpression -> tailExpression)};
extend :: Substitution -> [GrinVar] -> [GrinValue] -> Maybe Substitution;
extend substitution vars values | length vars /= length values = Nothing | not (and (zipWith sameRep vars substituted)) = Nothing | otherwise = Just (foldl' record (shadow vars substitution) (zip vars substituted)) where {
  substituted = map (useValue substitution) values;
  sameRep var value = grinVarRuntimeRep var == grinValueRuntimeRep value;
  record current (var, value) = Map.insert var value current};
shadow :: [GrinVar] -> Substitution -> Substitution;
shadow vars substitution | null vars || Map.null substitution = substitution | otherwise = Map.filterWithKey keep substitution where {
  shadowed = Set.fromList vars;
  keep var value = not (Set.member var shadowed) && not (captures value);
  captures value = case value of {
  GrinVarValue var -> Set.member var shadowed;
  GrinGlobalValue {} -> False;
  GrinLitValue {} -> False}};
useNode :: Substitution -> GrinNode -> GrinNode;
useNode substitution node = node {grinNodeFields = map (useValue substitution) (grinNodeFields node)};
useValue :: Substitution -> GrinValue -> GrinValue;
useValue substitution value = case value of {
  GrinVarValue var -> Map.findWithDefault value var substitution;
  GrinGlobalValue {} -> value;
  GrinLitValue {} -> value};
mapExprValues :: (GrinValue -> GrinValue) -> GrinExpr -> GrinExpr;
mapExprValues f expression = case expression of {
  GrinConstant values -> GrinConstant (map f values);
  GrinBind vars valueExpression body -> GrinBind vars valueExpression body;
  GrinStore node -> GrinStore (node' node);
  GrinEnsureHeap requiredWords roots -> GrinEnsureHeap (f requiredWords) (map f roots);
  GrinStoreUnchecked node -> GrinStoreUnchecked (node' node);
  GrinStoreRec bindings body -> GrinStoreRec (nodes bindings) body;
  GrinStoreRecUnchecked bindings body -> GrinStoreRecUnchecked (nodes bindings) body;
  GrinUpdate pointer value -> GrinUpdate (f pointer) (f value);
  GrinEval update runtimeRep value -> GrinEval update runtimeRep (f value);
  GrinCpsEval update runtimeRep value continuation -> GrinCpsEval update runtimeRep (f value) (f continuation);
  GrinFetch tag value -> GrinFetch tag (f value);
  GrinCall runtimeRep functionName arguments -> GrinCall runtimeRep functionName (map f arguments);
  GrinPrimitiveCall runtimeRep name arguments -> GrinPrimitiveCall runtimeRep name (map f arguments);
  GrinCpsPrimitiveCall runtimeRep name arguments continuation -> GrinCpsPrimitiveCall runtimeRep name (map f arguments) (f continuation);
  GrinApply runtimeRep function arguments -> GrinApply runtimeRep (f function) (map f arguments);
  GrinForward -> GrinForward;
  GrinCpsApply runtimeRep function arguments continuation -> GrinCpsApply runtimeRep (f function) (map f arguments) (f continuation);
  GrinContinue continuation arguments -> GrinContinue (f continuation) (map f arguments);
  GrinCpsRaise exception continuation -> GrinCpsRaise (f exception) (f continuation);
  GrinUpdateBlackhole pointer value -> GrinUpdateBlackhole (f pointer) (f value);
  GrinHalt values -> GrinHalt (map f values);
  GrinExit status -> GrinExit (f status);
  GrinIfWhnf value ready slow -> GrinIfWhnf (f value) ready slow;
  GrinCase scrutinee binder alternatives -> GrinCase (f scrutinee) binder alternatives;
  GrinThrow exception -> GrinThrow (f exception);
  GrinCatch runtimeRep action handler state -> GrinCatch runtimeRep (f action) (f handler) (map f state);
  GrinForeignCallExpr foreignCall arguments -> GrinForeignCallExpr foreignCall (map f arguments)} where {
  node' node = node {grinNodeFields = map f (grinNodeFields node)};
  nodes bindings = [(var, node' node) | (var, node) <- bindings]}
}
