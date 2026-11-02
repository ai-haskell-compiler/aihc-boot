{-# LANGUAGE OverloadedStrings #-}
module Aihc.Fc.Inline (InlinePolicy(..), shrinkPolicy, growPolicy, InlineConfig(..), InlineReport(..), inlineProgram) where {
import Aihc.Fc.Fold (hasLiteralPrimitiveCall);
import Aihc.Fc.Imports (pruneImports);
import Aihc.Fc.Name;
import Aihc.Fc.Rules (RuleTable, ruleActiveIn, ruleTable);
import Aihc.Fc.Simplify;
import Aihc.Fc.Size (exprSize, programSize);
import Aihc.Fc.Syntax;
import Aihc.Fc.Tidy (tidyProgram);
import Aihc.Fc.TypeOf (TypeEnv, typeEnvFromProgram);
import Aihc.Fc.Wired (primPackageFromScopes);
import Control.Monad.Trans.State.Strict (runState);
import Data.Graph (SCC(..), stronglyConnComp);
import Data.List qualified as List;
import Data.Map.Strict (Map);
import Data.Map.Strict qualified as Map;
import Data.Maybe (mapMaybe);
import Data.Set (Set);
import Data.Set qualified as Set;
import Data.Text (Text);
data InlinePolicy = InlinePolicy { policyName :: !Text, policyCalleeLimit :: !Int, policySiteLimit :: !Int, policyFunctionArgumentDiscount :: !Int, policyValueGrowth :: !Int, policyValueSlack :: !Int } deriving (Eq, Show);
shrinkPolicy :: InlinePolicy;
shrinkPolicy = InlinePolicy {policyName = "shrink", policyCalleeLimit = 80, policySiteLimit = 0, policyFunctionArgumentDiscount = 0, policyValueGrowth = 0, policyValueSlack = 0};
growPolicy :: InlinePolicy;
growPolicy = InlinePolicy {policyName = "grow", policyCalleeLimit = 80, policySiteLimit = 100, policyFunctionArgumentDiscount = 6, policyValueGrowth = 100, policyValueSlack = 20};
data InlineConfig = InlineConfig { inlinePolicy :: !InlinePolicy, inlineRoots :: !(Maybe [Name]), inlineRounds :: !Int, inlinePhase :: !Int } deriving (Eq, Show);
data InlineReport = InlineReport { reportSizeBefore :: !Int, reportSizeAfter :: !Int, reportInlinedSites :: !Int, reportDroppedValues :: !Int, reportRulesFired :: !Int } deriving (Eq, Show);
inlineProgram :: InlineConfig -> Program -> (Program, InlineReport);
inlineProgram config program = case primPackageFromScopes (programScopes program) of {
  Nothing -> (program, InlineReport size0 size0 0 0 0);
  Just primPackage -> let {
  env = typeEnvFromProgram primPackage program;
  supply0 = maxLocalUnique program + 1;
  lifted = programDecls program;
  state0 = initialInliner config env lifted supply0;
  final = runRounds config (inlineRounds config) state0;
  decls = rebuildDecls lifted final;
  result = tidyProgram (pruneImports program {programDecls = decls});
  report = InlineReport {reportSizeBefore = size0, reportSizeAfter = programSize result, reportInlinedSites = inSites final, reportDroppedValues = length lifted - length decls, reportRulesFired = inRulesFired final}} in (result, report)} where {
  size0 = programSize program};
data Inliner = Inliner { inEnv :: !TypeEnv, inDecls :: !(Map Name ValDecl), inBodies :: !(Map Name Expr), inRefs :: !(Map Name (Set Name)), inLimits :: !(Map Name Int), inCounts :: !(Map Name Int), inCalls :: !(Map Name Int), inArities :: !(Map Name Int), inDead :: !(Set Name), inSupply :: !Int, inSites :: !Int, inChanged :: !Bool, inRoots :: !(Set Name), inRules :: !RuleTable, inSpecs :: !(Map Name InlineSpec), inRulesFired :: !Int };
initialInliner :: InlineConfig -> TypeEnv -> [Decl] -> Int -> Inliner;
initialInliner config env decls supply = Inliner {inEnv = env, inDecls = declarations, inBodies = bodies, inRefs = Map.map (valueReferences declarations) bodies, inLimits = Map.map (valueLimit (inlinePolicy config) . exprSize env) bodies, inCounts = occurrenceCounts (Map.elems bodies), inCalls = callCounts arities (Map.elems bodies), inArities = arities, inDead = Set.empty, inSupply = supply, inSites = 0, inChanged = False, inRoots = roots, inRules = ruleTable (inlinePhase config) decls, inSpecs = Map.map valInline declarations, inRulesFired = 0} where {
  declarations = Map.fromList [(valName declaration, declaration) | DeclVal declaration <- decls];
  bodies = Map.map valBody declarations;
  arities = Map.map functionArity bodies;
  roots = Set.filter (`Map.member` declarations) ruleReferences <> case inlineRoots config of {
  Nothing -> Map.keysSet (Map.filter ((== Pub) . valVis) declarations);
  Just names -> Set.fromList names};
  ruleReferences = Set.unions [exprValueNames (ruleLhs rule) <> exprValueNames (ruleRhs rule) | DeclRule rule <- decls]};
inliningAllowed :: Int -> InlineSpec -> Bool;
inliningAllowed phase spec = case spec of {
  InlineDefault -> True;
  InlineAlways activation -> ruleActiveIn phase activation;
  InlineWhenUseful activation -> ruleActiveIn phase activation;
  InlineNever activation -> ruleActiveIn phase activation};
inliningRequested :: Int -> InlineSpec -> Bool;
inliningRequested phase spec = case spec of {
  InlineAlways activation -> ruleActiveIn phase activation;
  _ -> False};
valueLimit :: InlinePolicy -> Int -> Int;
valueLimit policy size = size + size * policyValueGrowth policy `div` 100 + policyValueSlack policy;
valueReferences :: Map Name ValDecl -> Expr -> Set Name;
valueReferences declarations body = Set.filter (`Map.member` declarations) (exprValueNames body);
rebuildDecls :: [Decl] -> Inliner -> [Decl];
rebuildDecls decls final = mapMaybe rebuild decls where {
  rebuild decl = case decl of {
  DeclVal declaration -> case Map.lookup (valName declaration) (inBodies final) of {
  Nothing -> Nothing;
  Just body -> Just (DeclVal declaration {valBody = body})};
  _ -> Just decl}};
runRounds :: InlineConfig -> Int -> Inliner -> Inliner;
runRounds config rounds st | rounds <= 0 = st | otherwise = let {
  st' = dropUnused (inlineRound config st {inChanged = False})} in if inChanged st' then runRounds config (rounds - 1) st' else st';
inlineRound :: InlineConfig -> Inliner -> Inliner;
inlineRound config st0 = List.foldl' step st0 (stronglyConnComp graph) where {
  graph = [(name, name, Set.toList references) | (name, references) <- Map.toList (inRefs st0)];
  known = knownValues st0;
  recursive = Set.fromList (concat [names | CyclicSCC names <- stronglyConnComp graph]);
  step st scc = case scc of {
  AcyclicSCC name -> simplifyValue config known recursive st name;
  CyclicSCC names -> List.foldl' (simplifyValue config known recursive) st names}};
simplifyValue :: InlineConfig -> Map Name Expr -> Set Name -> Inliner -> Name -> Inliner;
simplifyValue config known recursive st name | name `Set.member` inDead st = st | otherwise = case Map.lookup name (inBodies st) of {
  Nothing -> st;
  Just body -> let {
  references = Map.findWithDefault Set.empty name (inRefs st);
  reachable = calleesOf (inRefs st) references;
  candidates = Map.fromList [(callee, Candidate calleeBody every) | callee <- Set.toList reachable, callee /= name, callee `Set.notMember` recursive, let {
  spec = Map.findWithDefault InlineDefault callee (inSpecs st)}, inliningAllowed (inlinePhase config) spec, Just calleeBody <- [Map.lookup callee (inBodies st)], isInlinable calleeBody, let {
  size = exprSize (inEnv st) calleeBody;
  every = unconditional callee size}, every || inliningRequested (inlinePhase config) spec || size <= policyCalleeLimit policy];
  skip = Map.null candidates && Map.null known && not (hasLiteralPrimitiveCall body) && not (any (`Map.member` inRules st) (Set.toList (exprValueNames body)))} in if skip then st else let {
  oldSize = exprSize (inEnv st) body;
  simpl = Simpl {spEnv = inEnv st, spInline = candidates, spKnown = known, spArity = arities, spLocals = Map.empty, spCse = Map.empty, spSiteLimit = policySiteLimit policy, spDiscount = policyFunctionArgumentDiscount policy, spRules = inRules st};
  allowance = max 0 (Map.findWithDefault 0 name (inLimits st) - oldSize);
  (body', simplState) = runState (simplifyExpr simpl body) (initialSimplState (inSupply st) allowance);
  oldUses = countTopUses body;
  newUses = countTopUses body';
  counts' = Map.unionWith (+) (Map.unionWith (+) (inCounts st) newUses) (Map.map negate oldUses);
  calls' = Map.unionWith (+) (Map.unionWith (+) (inCalls st) (countTopCalls (inArities st) body')) (Map.map negate (countTopCalls (inArities st) body))} in killDead st {inBodies = Map.insert name body' (inBodies st), inRefs = Map.insert name (valueReferences (inDecls st) body') (inRefs st), inCounts = counts', inCalls = calls', inSupply = ssSupply simplState, inSites = inSites st + ssInlined simplState, inRulesFired = inRulesFired st + ssRulesFired simplState, inChanged = inChanged st || body' /= body} (Map.keys oldUses)} where {
  policy = inlinePolicy config;
  arities = Map.map functionArity (inBodies st);
  unconditional callee size = removable callee && let {
  uses = Map.findWithDefault 0 callee (inCounts st);
  calls = Map.findWithDefault 0 callee (inCalls st)} in calls >= uses && uses * (size - 1) - (size + 1) <= 0;
  removable callee = callee `Set.notMember` inRoots st};
killDead :: Inliner -> [Name] -> Inliner;
killDead st names = case names of {
  [] -> st;
  name : rest | name `Set.member` inDead st || name `Set.member` inRoots st || Map.findWithDefault 0 name (inCounts st) > 0 -> killDead st rest | Just body <- Map.lookup name (inBodies st) -> let {
  uses = countTopUses body;
  counts' = Map.unionWith (+) (inCounts st) (Map.map negate uses);
  calls' = Map.unionWith (+) (inCalls st) (Map.map negate (countTopCalls (inArities st) body))} in killDead st {inDead = Set.insert name (inDead st), inCounts = counts', inCalls = calls'} (Map.keys uses ++ rest) | otherwise -> killDead st rest};
calleesOf :: Map Name (Set Name) -> Set Name -> Set Name;
calleesOf references names = Set.unions (names : [Map.findWithDefault Set.empty name references | name <- Set.toList names]);
dropUnused :: Inliner -> Inliner;
dropUnused st = st {inBodies = live, inRefs = Map.restrictKeys (inRefs st) reachable, inCounts = occurrenceCounts (Map.elems live), inCalls = callCounts arities (Map.elems live), inArities = arities, inDead = Set.empty} where {
  live = Map.restrictKeys (inBodies st) reachable;
  arities = Map.map functionArity live;
  reachable = close Set.empty (Set.toList (Set.filter (`Map.member` inBodies st) (inRoots st)));
  close visited pending = case pending of {
  [] -> visited;
  name : rest | Set.member name visited -> close visited rest | otherwise -> close (Set.insert name visited) (Set.toList (Map.findWithDefault Set.empty name (inRefs st)) <> rest)}};
occurrenceCounts :: [Expr] -> Map Name Int;
occurrenceCounts = List.foldl' (\counts body -> Map.unionWith (+) counts (countTopUses body)) Map.empty;
callCounts :: Map Name Int -> [Expr] -> Map Name Int;
callCounts arities = List.foldl' (\counts body -> Map.unionWith (+) counts (countTopCalls arities body)) Map.empty;
countTopCalls :: Map Name Int -> Expr -> Map Name Int;
countTopCalls arities = go where {
  go expr = case castedSpine expr of {
  (ExVar name, args) | Just arity <- Map.lookup name arities, length [() | Right _ <- args] >= arity -> Map.unionWith (+) (Map.singleton name 1) (arguments args);
  (function, args) -> Map.unionWith (+) (bare function) (arguments args)};
  arguments args = List.foldl' (Map.unionWith (+)) Map.empty [go argument | Right argument <- args];
  bare expr = case expr of {
  ExLam _ body -> go body;
  ExTyLam _ body -> go body;
  ExLet bind body -> Map.unionWith (+) (go (bindRhs bind)) (go body);
  ExRec binds body -> List.foldl' (Map.unionWith (+)) (go body) (map (go . bindRhs) binds);
  ExCase scrutinee _ _ alternatives -> List.foldl' (Map.unionWith (+)) (go scrutinee) (map (go . altRhs) alternatives);
  ExForeignCall _ _ args -> List.foldl' (Map.unionWith (+)) Map.empty (map go args);
  _ -> Map.empty}};
countTopUses :: Expr -> Map Name Int;
countTopUses = go where {
  go expr = case expr of {
  ExVar name | isTop name -> Map.singleton name 1 | otherwise -> Map.empty;
  ExLit {} -> Map.empty;
  ExCoercion {} -> Map.empty;
  ExApp function argument -> Map.unionWith (+) (go function) (go argument);
  ExTyApp function _ -> go function;
  ExLam _ body -> go body;
  ExTyLam _ body -> go body;
  ExLet bind body -> Map.unionWith (+) (go (bindRhs bind)) (go body);
  ExRec binds body -> List.foldl' (Map.unionWith (+)) (go body) (map (go . bindRhs) binds);
  ExCase scrutinee _ _ alternatives -> List.foldl' (Map.unionWith (+)) (go scrutinee) (map (go . altRhs) alternatives);
  ExCast body _ -> go body;
  ExForeignCall _ _ arguments -> List.foldl' (Map.unionWith (+)) Map.empty (map go arguments)};
  isTop name = case nameOrigin name of {
  OriginTop {} -> True;
  OriginLocal {} -> False}};
knownValues :: Inliner -> Map Name Expr;
knownValues st = Map.filter (isKnownConstructor arities) (inBodies st) where {
  arities = Map.map functionArity (inBodies st)}
}
