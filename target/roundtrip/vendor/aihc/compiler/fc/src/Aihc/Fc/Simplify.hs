{-# LANGUAGE OverloadedStrings #-}
module Aihc.Fc.Simplify (SimplifyReport(..), simplifyProgram, Simpl(..), SimplState(..), initialSimplState, SimplM, Candidate(..), simplifyExpr, isConstructorName, isKnownConstructor, isCheapValue, isInlinable, isTrivial, functionArity, collectSpine, castedSpine, exprValueNames, maxLocalUnique) where {
import Aihc.Fc.Fold (foldForeignCall);
import Aihc.Fc.Imports (declReferences, pruneImports);
import Aihc.Fc.Name;
import Aihc.Fc.Rules (RuleMatch(..), RuleTable, matchRule, ruleTable);
import Aihc.Fc.Size (exprSize, isLiftedBinder, isStrictBinder, programSize);
import Aihc.Fc.Syntax;
import Aihc.Fc.Tidy (tidyProgram);
import Aihc.Fc.TypeOf (TypeEnv(..), coercionEndpoints, extendBinder, lookupHeaderType, reduceType, repOf, substType, substTypes, typeEnvFromProgram, viewForAll, viewFun);
import Aihc.Fc.Wired (primPackageFromScopes);
import Aihc.Tc.Types (Unique(..));
import Control.Applicative ((<|>));
import Control.Monad (foldM, guard);
import Control.Monad.Trans.State.Strict (State, get, gets, modify', runState, state);
import Data.Either (lefts, rights);
import Data.List qualified as List;
import Data.Map.Strict (Map);
import Data.Map.Strict qualified as Map;
import Data.Maybe (fromMaybe, mapMaybe);
import Data.Set (Set);
import Data.Set qualified as Set;
import Data.Text (Text);
data SimplifyReport = SimplifyReport { simplifySizeBefore :: !Int, simplifySizeAfter :: !Int, simplifyRulesFired :: !Int } deriving (Eq, Show);
simplifyProgram :: Int -> Program -> (Program, SimplifyReport);
simplifyProgram phase program = case primPackageFromScopes (programScopes program) of {
  Nothing -> (program, SimplifyReport size0 size0 0);
  Just primPackage -> let {
  env = typeEnvFromProgram primPackage program;
  bodies = Map.fromList [(valName declaration, valBody declaration) | DeclVal declaration <- programDecls program];
  arities = Map.map functionArity bodies;
  simpl = Simpl {spEnv = env, spInline = Map.empty, spKnown = Map.filter (isKnownConstructor arities) bodies, spArity = arities, spLocals = Map.empty, spCse = Map.empty, spSiteLimit = 0, spDiscount = 0, spRules = ruleTable phase (programDecls program)};
  simplifyDecl decl = case decl of {
  DeclVal declaration -> (\body -> DeclVal declaration {valBody = body}) <$> simplifyExpr simpl (valBody declaration);
  _ -> pure decl};
  (decls, final) = runState (mapM simplifyDecl (programDecls program)) (initialSimplState (maxLocalUnique program + 1) 0);
  result = tidyProgram (pruneImports program {programDecls = decls})} in (result, SimplifyReport size0 (programSize result) (ssRulesFired final))} where {
  size0 = programSize program};
isConstructorName :: Name -> Bool;
isConstructorName name = nameSort name == SortDataConstructor;
isKnownConstructor :: Map Name Int -> Expr -> Bool;
isKnownConstructor arities expr = case expr of {
  ExTyLam _ body -> isKnownConstructor arities body;
  ExLam _ body -> isKnownConstructor arities body;
  ExCast body _ -> isKnownConstructor arities body;
  _ -> case collectSpine expr of {
  (ExVar name, args) | isConstructorName name -> all (either (const True) (isCheapValue arities)) args;
  _ -> False}};
isInlinable :: Expr -> Bool;
isInlinable body = functionArity body > 0 || isTrivial body;
isCheapValue :: Map Name Int -> Expr -> Bool;
isCheapValue arities expr = case expr of {
  ExLit {} -> True;
  ExVar {} -> True;
  ExCoercion {} -> True;
  ExLam {} -> True;
  ExTyLam _ body -> isCheapValue arities body;
  ExCast body _ -> isCheapValue arities body;
  ExTyApp body _ -> isCheapValue arities body;
  ExApp {} -> case collectSpine expr of {
  (ExVar name, args) | isConstructorName name -> cheapArgs args | Just arity <- Map.lookup name arities -> length [() | Right _ <- args] < arity && cheapArgs args;
  _ -> False};
  _ -> False} where {
  cheapArgs = all (either (const True) (isCheapValue arities))};
functionArity :: Expr -> Int;
functionArity expr = case expr of {
  ExLam _ body -> 1 + functionArity body;
  ExTyLam _ body -> functionArity body;
  _ -> 0};
data Candidate = Candidate { candidateBody :: !Expr, candidateUnconditional :: !Bool };
data Simpl = Simpl { spEnv :: !TypeEnv, spInline :: !(Map Name Candidate), spKnown :: !(Map Name Expr), spArity :: !(Map Name Int), spLocals :: !(Map Name Expr), spCse :: !(Map Expr Name), spSiteLimit :: !Int, spDiscount :: !Int, spRules :: !RuleTable };
data SimplState = SimplState { ssSupply :: !Int, ssAllowance :: !Int, ssInlined :: !Int, ssRulesFired :: !Int, ssRuleFuel :: !Int };
initialSimplState :: Int -> Int -> SimplState;
initialSimplState supply allowance = SimplState {ssSupply = supply, ssAllowance = allowance, ssInlined = 0, ssRulesFired = 0, ssRuleFuel = ruleFuel};
ruleFuel :: Int;
ruleFuel = 1000;
type SimplM = State SimplState;
type Arg = Either Type Expr;
simplifyExpr :: Simpl -> Expr -> SimplM Expr;
simplifyExpr env expr = case expr of {
  ExVar {} -> simplifyApp env expr [];
  ExLit {} -> pure expr;
  ExCoercion {} -> pure expr;
  ExApp {} | Just pushed <- pushHeadCasts expr -> simplifyExpr env pushed | otherwise -> uncurry (simplifyApp env) (collectSpine expr);
  ExTyApp {} -> uncurry (simplifyApp env) (collectSpine expr);
  ExLam binder body -> ExLam binder <$> simplifyExpr env body;
  ExTyLam binder body -> ExTyLam binder <$> simplifyExpr (extendTypeBinder env binder) body;
  ExLet bind body -> do {
  rhs <- simplifyExpr env (bindRhs bind);
  let {
  binder = bindBinder bind};
  if isTrivial rhs then simplifyExpr env (substExpr (Map.singleton (binderName binder) rhs) body) else do {
  body' <- simplifyExpr (bindingEnv env binder rhs) body;
  mkLet env (Bind binder rhs) body'}};
  ExRec binds body -> do {
  binds' <- mapM (\bind -> (\rhs -> bind {bindRhs = rhs}) <$> simplifyExpr env (bindRhs bind)) binds;
  ExRec binds' <$> simplifyExpr env body};
  ExCase scrutinee binder resultType alternatives | (ExVar name, args) <- collectSpine (fromMaybe scrutinee (pushHeadCasts scrutinee)), Just candidate <- Map.lookup name (spInline env), takesArgument env candidate args -> inlineScrutinee env name candidate args binder resultType alternatives | otherwise -> do {
  scrutinee' <- simplifyExpr env scrutinee;
  simplifyCase env scrutinee' binder resultType alternatives};
  ExCast body coercion -> do {
  body' <- simplifyExpr env body;
  mkCast body' coercion};
  ExForeignCall call types arguments -> do {
  arguments' <- mapM (simplifyExpr env) arguments;
  let {
  call' = fromMaybe (ExForeignCall call types arguments') (foldForeignCall (spEnv env) call types arguments')};
  pure (maybe call' ExVar (Map.lookup call' (spCse env)))}};
simplifyCase :: Simpl -> Expr -> Binder -> Type -> [Alt] -> SimplM Expr;
simplifyCase env scrutinee binder resultType alternatives | Just rewritten <- literalEqualityCase (spEnv env) scrutinee binder resultType alternatives = simplifyExpr env rewritten | otherwise = do {
  reduced <- caseOfKnown env scrutinee binder alternatives;
  case reduced of {
  Just result -> simplifyExpr env result;
  Nothing -> do {
  pushed <- caseOfCaseRaw env scrutinee binder resultType alternatives;
  accepted <- case pushed of {
  Just pushed' -> acceptGrowth env (pushedGrowth env pushed');
  Nothing -> pure False};
  case pushed of {
  Just pushed' | accepted -> expandJoins env (pushedJoins pushed') (pushedSmall pushed');
  _ -> do {
  alternatives' <- mapM (simplifyAlt env scrutinee binder) alternatives;
  pure (mkCase (spEnv env) scrutinee binder resultType alternatives')}}}}};
inlineScrutinee :: Simpl -> Name -> Candidate -> [Arg] -> Binder -> Type -> [Alt] -> SimplM Expr;
inlineScrutinee env name candidate args binder resultType alternatives = do {
  args' <- mapM (either (pure . Left) (fmap Right . simplifyExpr env)) args;
  before <- get;
  inlined <- inlineCandidate env name candidate args';
  paid <- gets (nestedPaid before);
  let {
  original = rebuildSpine (ExVar name) args';
  discount = callDiscount env (candidateBody candidate) args' + (if tailsAreKnown env inlined then spDiscount env else 0) + paid;
  callGrowth = exprSize (spEnv env) inlined - exprSize (spEnv env) original - discount;
  fallback = do {
  restoreSite before;
  alternatives' <- mapM (simplifyAlt env original binder) alternatives;
  pure (mkCase (spEnv env) original binder resultType alternatives')};
  decide growth result = do {
  accepted <- acceptSite env candidate growth;
  if accepted then result else fallback}};
  reduced <- caseOfKnown env inlined binder alternatives;
  case reduced of {
  Just result -> decide (-1) (simplifyExpr env result);
  Nothing -> do {
  pushed <- caseOfCaseRaw env inlined binder resultType alternatives;
  case pushed of {
  Just pushed' -> decide (pushedGrowth env pushed' + callGrowth) (expandJoins env (pushedJoins pushed') (pushedSmall pushed'));
  Nothing -> decide callGrowth $ do {
  alternatives' <- mapM (simplifyAlt env inlined binder) alternatives;
  pure (mkCase (spEnv env) inlined binder resultType alternatives')}}}}};
nestedPaid :: SimplState -> SimplState -> Int;
nestedPaid before after = ssAllowance before - ssAllowance after;
acceptSite :: Simpl -> Candidate -> Int -> SimplM Bool;
acceptSite env candidate growth | candidateUnconditional candidate = do {
  modify' (\st -> st {ssAllowance = ssAllowance st - growth, ssInlined = ssInlined st + 1});
  pure True} | otherwise = do {
  accepted <- acceptGrowth env growth;
  if accepted then do {
  modify' (\st -> st {ssInlined = ssInlined st + 1});
  pure True} else pure False};
restoreSite :: SimplState -> SimplM ();
restoreSite before = modify' (\st -> st {ssAllowance = ssAllowance before, ssInlined = ssInlined before});
tailsAreKnown :: Simpl -> Expr -> Bool;
tailsAreKnown env expr = case expr of {
  ExCase _ _ _ alternatives -> all (tailsAreKnown env . altRhs) alternatives;
  ExLet _ body -> tailsAreKnown env body;
  ExRec _ body -> tailsAreKnown env body;
  ExCast body _ -> tailsAreKnown env body;
  ExTyLam _ body -> tailsAreKnown env body;
  ExLit {} -> True;
  _ -> isKnownConstructor (spArity env) expr};
takesArgument :: Simpl -> Candidate -> [Arg] -> Bool;
takesArgument env candidate args = count >= functionArity (candidateBody candidate) || (count > 0 && any interesting valueArgs) where {
  valueArgs = rights args;
  count = length valueArgs;
  interesting argument = case argument of {
  ExVar name -> Map.member name (spArity env);
  ExTyApp body _ -> interesting body;
  ExCast body _ -> interesting body;
  _ -> True}};
inlineCandidate :: Simpl -> Name -> Candidate -> [Arg] -> SimplM Expr;
inlineCandidate env name candidate args = do {
  copy <- freshenExpr (candidateBody candidate);
  betaReduce env {spInline = Map.delete name (spInline env)} copy args};
bindingEnv :: Simpl -> Binder -> Expr -> Simpl;
bindingEnv env binder rhs | isKnownConstructor (spArity env) rhs = env {spLocals = Map.insert (binderName binder) rhs (spLocals env)} | isStrictBinder (spEnv env) binder, isPurePrimitiveCall (spEnv env) rhs, Map.notMember rhs (spCse env) = env {spCse = Map.insert rhs (binderName binder) (spCse env)} | otherwise = env;
simplifyAlt :: Simpl -> Expr -> Binder -> Alt -> SimplM Alt;
simplifyAlt env scrutinee binder alternative = do {
  let {
  body = case (scrutinee, altCon alternative) of {
  (ExVar name, AltDefault) -> substExpr (Map.singleton name (ExVar (binderName binder))) (altRhs alternative);
  _ -> altRhs alternative}};
  rhs <- simplifyExpr (alternativeEnv env scrutinee binder alternative) body;
  pure alternative {altRhs = rhs}};
alternativeEnv :: Simpl -> Expr -> Binder -> Alt -> Simpl;
alternativeEnv env scrutinee binder alternative = case known of {
  Just application -> typeEnv {spLocals = Map.insert (binderName binder) application . maybe id (\name -> Map.insert name (List.foldl' (\body co -> ExCast body (coSym co)) application (reverse scrutineeCasts))) scrutineeName $ spLocals typeEnv};
  Nothing -> typeEnv} where {
  typeEnv = List.foldl' extendTypeBinder env (altTypeBinders alternative);
  known = case altCon alternative of {
  AltData con -> constructorApplication (spEnv env) con (binderType binder) alternative;
  _ -> Nothing};
  (scrutineeCore, scrutineeCasts) = peelCasts scrutinee;
  scrutineeName = case scrutineeCore of {
  ExVar name -> Just name;
  _ -> Nothing}};
peelCasts :: Expr -> (Expr, [Coercion]);
peelCasts expr = case expr of {
  ExCast body coercion -> let {
  (core, casts) = peelCasts body} in (core, casts <> [coercion]);
  _ -> (expr, [])};
constructorApplication :: TypeEnv -> Name -> Type -> Alt -> Maybe Expr;
constructorApplication env con scrutineeType alternative = do {
  conType <- lookupHeaderType env con;
  let {
  (foralls, result) = splitForAlls env conType;
  (_, resultArgs) = typeSpine (reduceType env result);
  (_, scrutineeArgs) = typeSpine (reduceType env scrutineeType)};
  if length resultArgs /= length scrutineeArgs then Nothing else Just ();
  let {
  fixed = Map.fromList [(name, ty) | (TyVar name, ty) <- zip resultArgs scrutineeArgs]};
  typeArgs <- fill foralls (altTypeBinders alternative) fixed;
  Just (rebuildSpine (ExVar con) (map Left typeArgs <> map (Right . ExVar . binderName) (altBinders alternative)))} where {
  fill foralls existentials fixed = case foralls of {
  [] -> if null existentials then Just [] else Nothing;
  binder : rest | Just ty <- Map.lookup (binderName binder) fixed -> (ty :) <$> fill rest existentials fixed | existential : more <- existentials -> (TyVar (binderName existential) :) <$> fill rest more fixed | otherwise -> Nothing}};
extendTypeBinder :: Simpl -> Binder -> Simpl;
extendTypeBinder env binder = env {spEnv = extendBinder (spEnv env) binder};
simplifyApp :: Simpl -> Expr -> [Arg] -> SimplM Expr;
simplifyApp env headExpr args = do {
  headExpr' <- case headExpr of {
  ExVar {} -> pure headExpr;
  _ -> simplifyExpr env headExpr};
  args' <- mapM (either (pure . Left) (fmap Right . simplifyExpr env)) args;
  fired <- fireRule env headExpr' args';
  case headExpr' of {
  _ | Just rewritten <- fired -> simplifyExpr env rewritten;
  ExVar name | Just candidate <- Map.lookup name (spInline env), takesArgument env candidate args' -> do {
  let {
  original = rebuildSpine headExpr' args'};
  before <- get;
  result <- inlineCandidate env name candidate args';
  paid <- gets (nestedPaid before);
  let {
  discount = callDiscount env (candidateBody candidate) args' + paid;
  growth = exprSize (spEnv env) result - exprSize (spEnv env) original - discount};
  accepted <- acceptSite env candidate growth;
  if accepted then pure result else do {
  restoreSite before;
  pure original}};
  ExLam {} | not (null args') -> betaReduce env headExpr' args';
  ExTyLam {} | not (null args') -> betaReduce env headExpr' args';
  ExLet bind body | not (null args'), all (either (const True) (unused (binderName (bindBinder bind)))) args' -> do {
  inner <- simplifyApp env body args';
  mkLet env bind inner};
  ExCase scrutinee binder resultType alternatives | not (null args'), all (either (const True) isTrivial) args', Just resultType' <- appliedType (spEnv env) resultType args' -> do {
  alternatives' <- mapM (\alternative -> (\rhs -> alternative {altRhs = rhs}) <$> simplifyApp env (altRhs alternative) args') alternatives;
  pure (ExCase scrutinee binder resultType' alternatives')};
  _ -> pure (rebuildSpine headExpr' args')}};
fireRule :: Simpl -> Expr -> [Arg] -> SimplM (Maybe Expr);
fireRule env headExpr args = case headExpr of {
  ExVar name | Just rules <- Map.lookup name (spRules env) -> do {
  fuel <- gets ssRuleFuel;
  case [(rule, match) | fuel > 0, rule <- rules, Just match <- [matchRule (spEnv env) rule args]] of {
  (rule, match) : _ -> do {
  rhs <- freshenExpr (ruleRhs rule);
  modify' (\st -> st {ssRulesFired = ssRulesFired st + 1, ssRuleFuel = ssRuleFuel st - 1});
  let {
  instantiated = substExpr (matchValues match) (substTypeExpr (matchTypes match) rhs)};
  pure (Just (rebuildSpine instantiated (matchSurplus match)))};
  [] -> pure Nothing}};
  _ -> pure Nothing};
callDiscount :: Simpl -> Expr -> [Arg] -> Int;
callDiscount env body args = spDiscount env * length [() | (binder, argument) <- valueArguments body args, valueArity (spArity env) argument > 0, saturatedCalls (binderName binder) 1 body > 0];
valueArguments :: Expr -> [Arg] -> [(Binder, Expr)];
valueArguments body args = case (body, args) of {
  (ExTyLam _ inner, Left _ : rest) -> valueArguments inner rest;
  (ExLam binder inner, Right argument : rest) -> (binder, argument) : valueArguments inner rest;
  _ -> []};
appliedType :: TypeEnv -> Type -> [Arg] -> Maybe Type;
appliedType env ty args = case args of {
  [] -> Just ty;
  Left argument : rest -> do {
  (binder, body) <- viewForAll env ty;
  appliedType env (substType (binderName binder) argument body) rest};
  Right _ : rest -> do {
  (_, _, _, result) <- viewFun env ty;
  appliedType env result rest}};
betaReduce :: Simpl -> Expr -> [Arg] -> SimplM Expr;
betaReduce env expr args = case (expr, args) of {
  (ExTyLam binder body, Left ty : rest) -> betaReduce env (substTypeExpr (Map.singleton (binderName binder) ty) body) rest;
  (ExLam binder body, Right argument : rest) | isTrivial argument -> betaReduce env (substExpr (Map.singleton (binderName binder) argument) body) rest | otherwise -> do {
  body' <- betaReduce (bindingEnv env binder argument) body rest;
  mkLet env (Bind binder argument) body'};
  _ -> simplifyExpr env (rebuildSpine expr args)};
mkCast :: Expr -> Coercion -> SimplM Expr;
mkCast body coercion = case coercion of {
  CoRefl _ -> pure body;
  _ -> case body of {
  ExCast inner innerCoercion | cancels innerCoercion coercion -> pure inner;
  ExLet bind inner -> ExLet bind <$> mkCast inner coercion;
  _ -> pure (ExCast body coercion)}} where {
  cancels left right = left == CoSym right || right == CoSym left};
unused :: Name -> Expr -> Bool;
unused name expr = case occurrences name expr of {
  Occurrences count _ -> count == 0};
valueArity :: Map Name Int -> Expr -> Int;
valueArity arities expr = case expr of {
  ExLam _ body -> 1 + valueArity arities body;
  ExTyLam _ body -> valueArity arities body;
  _ -> case castedSpine expr of {
  (ExVar name, args) | not (isConstructorName name), Just arity <- Map.lookup name arities, given <- length [() | Right _ <- args], given < arity, all (either (const True) isTrivial) args -> arity - given;
  _ -> 0}};
saturatedCalls :: Name -> Int -> Expr -> Int;
saturatedCalls name arity = go where {
  go expr = case castedSpine expr of {
  (ExVar var, args) | var == name, length [() | Right _ <- args] >= arity -> 1 + sum (map argument args);
  (function, args) -> bare function + sum (map argument args)};
  argument = either (const 0) go;
  bare expr = case expr of {
  ExVar {} -> 0;
  ExLit {} -> 0;
  ExCoercion {} -> 0;
  ExLam _ body -> go body;
  ExTyLam _ body -> go body;
  ExLet bind body -> go (bindRhs bind) + go body;
  ExRec binds body -> sum (map (go . bindRhs) binds) + go body;
  ExCase scrutinee _ _ alternatives -> go scrutinee + sum (map (go . altRhs) alternatives);
  ExForeignCall _ _ arguments -> sum (map go arguments);
  ExApp {} -> 0;
  ExTyApp {} -> 0;
  ExCast {} -> 0}};
mkLet :: Simpl -> Bind -> Expr -> SimplM Expr;
mkLet env bind body | isTrivial rhs = simplifyExpr env (substExpr (Map.singleton name rhs) body) | lifted, Occurrences 0 _ <- uses = pure body | lifted, Occurrences 1 False <- uses = do {
  copy <- freshenExpr rhs;
  simplifyExpr env (substExpr (Map.singleton name copy) body)} | lifted, Occurrences 1 True <- uses, arity <- valueArity (spArity env) rhs, arity > 0, saturatedCalls name arity body == 1 = do {
  copy <- freshenExpr rhs;
  simplifyExpr env (substExpr (Map.singleton name copy) body)} | lifted = pure (ExLet bind body) | Occurrences 1 False <- uses, ExCase (ExVar scrutinee) caseBinder resultType alternatives <- body, scrutinee == name, Just rewritten <- literalEqualityCase (spEnv env) rhs caseBinder resultType alternatives = pure rewritten | otherwise = letOfCase env bind body where {
  rhs = bindRhs bind;
  binder = bindBinder bind;
  name = binderName binder;
  lifted = isLiftedBinder (spEnv env) binder;
  uses = occurrences name body};
acceptGrowth :: Simpl -> Int -> SimplM Bool;
acceptGrowth env growth | growth <= 0 = pure True | otherwise = do {
  allowance <- gets ssAllowance;
  if growth <= allowance && growth <= spSiteLimit env then do {
  modify' (\st -> st {ssAllowance = ssAllowance st - growth});
  pure True} else pure False};
data Pushed = Pushed { pushedSmall :: !Expr, pushedFallback :: !Expr, pushedJoins :: !(Map Name Expr) };
pushedGrowth :: Simpl -> Pushed -> Int;
pushedGrowth env pushed = exprSize (spEnv env) (pushedSmall pushed) - exprSize (spEnv env) (pushedFallback pushed) + sum [(uses - 1) * exprSize (spEnv env) rhs | (name, rhs) <- Map.toList (pushedJoins pushed), Occurrences uses _ <- [occurrences name (pushedSmall pushed)], uses /= 1];
expandJoins :: Simpl -> Map Name Expr -> Expr -> SimplM Expr;
expandJoins env joins = go env where {
  go env' expr = case expr of {
  ExLet bind body | isTrivial (bindRhs bind) -> go env' (substExpr (Map.singleton (binderName (bindBinder bind)) (bindRhs bind)) body) | otherwise -> ExLet bind <$> go (bindingEnv env' (bindBinder bind) (bindRhs bind)) body;
  ExRec binds body -> ExRec binds <$> go env' body;
  ExCase scrutinee binder resultType alternatives -> do {
  alternatives' <- mapM (\alternative -> (\rhs -> alternative {altRhs = rhs}) <$> go (alternativeEnv env' scrutinee binder alternative) (altRhs alternative)) alternatives;
  pure (mkCase (spEnv env') scrutinee binder resultType alternatives')};
  _ -> case collectSpine expr of {
  (ExVar name, _) | Map.member name joins -> simplifyExpr env' (substExpr joins expr);
  _ -> pure expr}}};
caseOfCaseRaw :: Simpl -> Expr -> Binder -> Type -> [Alt] -> SimplM (Maybe Pushed);
caseOfCaseRaw env scrutinee binder resultType alternatives | not (hasCaseTail scrutinee) = pure Nothing | otherwise = do {
  joined <- mapM joinPoint alternatives;
  let {
  joins = Map.fromList [(name, rhs) | (Just (name, rhs), _) <- joined];
  small = map snd joined};
  pushed <- pushSmall env binder resultType small scrutinee;
  pure (Just (Pushed pushed (ExCase scrutinee binder resultType small) joins))} where {
  hasCaseTail expr = case expr of {
  ExCase {} -> True;
  ExLet _ body -> hasCaseTail body;
  ExRec _ body -> hasCaseTail body;
  _ -> False};
  joinPoint alternative | isTrivial (altRhs alternative) || not (null (altTypeBinders alternative)) = pure (Nothing, alternative) | otherwise = do {
  name <- freshLocal (binderName binder);
  let {
  binders = binder : altBinders alternative;
  call = rebuildSpine (ExVar name) (map (Right . ExVar . binderName) binders)};
  pure (Just (name, foldr ExLam (altRhs alternative) binders), alternative {altRhs = call})}};
pushSmall :: Simpl -> Binder -> Type -> [Alt] -> Expr -> SimplM Expr;
pushSmall env binder resultType small = go env where {
  go env' expr = case expr of {
  ExLet bind body -> ExLet bind <$> go (bindingEnv env' (bindBinder bind) (bindRhs bind)) body;
  ExRec binds body -> ExRec binds <$> go env' body;
  ExCase scrutinee innerBinder _ alternatives -> do {
  alternatives' <- mapM (\alternative -> (\rhs -> alternative {altRhs = rhs}) <$> go (alternativeEnv env' scrutinee innerBinder alternative) (altRhs alternative)) alternatives;
  pure (mkCase (spEnv env') scrutinee innerBinder resultType alternatives')};
  _ -> do {
  copy <- freshenExpr (ExCase expr binder resultType small);
  case copy of {
  ExCase leaf binder' _ small' -> fromMaybe copy <$> caseOfKnown env' leaf binder' small';
  _ -> pure copy}}}};
freshLocal :: Name -> SimplM Name;
freshLocal name = state (\st -> (name {nameOrigin = OriginLocal (Unique (ssSupply st))}, st {ssSupply = ssSupply st + 1}));
letOfCase :: Simpl -> Bind -> Expr -> SimplM Expr;
letOfCase env bind body = case syntacticResultType body of {
  Just resultType -> pushIntoCase env (bindRhs bind) (\inner -> ExLet bind {bindRhs = inner} body) resultType fallback;
  Nothing -> pure fallback} where {
  fallback = ExLet bind body};
syntacticResultType :: Expr -> Maybe Type;
syntacticResultType expr = case expr of {
  ExCase _ _ resultType _ -> Just resultType;
  ExLet _ body -> syntacticResultType body;
  ExRec _ body -> syntacticResultType body;
  _ -> Nothing};
pushIntoCase :: Simpl -> Expr -> (Expr -> Expr) -> Type -> Expr -> SimplM Expr;
pushIntoCase env scrutinee context resultType fallback = do {
  pushed <- pushIntoCaseRaw env scrutinee context resultType;
  case pushed of {
  Nothing -> pure fallback;
  Just result -> do {
  let {
  growth = exprSize (spEnv env) result - exprSize (spEnv env) fallback};
  accepted <- acceptGrowth env growth;
  pure (if accepted then result else fallback)}}};
pushIntoCaseRaw :: Simpl -> Expr -> (Expr -> Expr) -> Type -> SimplM (Maybe Expr);
pushIntoCaseRaw env scrutinee context resultType = case core of {
  ExCase inner innerBinder _ innerAlternatives -> do {
  innerAlternatives' <- mapM (push inner innerBinder) innerAlternatives;
  pure (Just (foldr ExLet (mkCase (spEnv env) inner innerBinder resultType innerAlternatives') floated))};
  _ -> pure Nothing} where {
  (floated, core) = peelLets scrutinee;
  floatedEnv = List.foldl' (\acc bind -> bindingEnv acc (bindBinder bind) (bindRhs bind)) env floated;
  push inner innerBinder alternative = do {
  copy <- freshenExpr (context (altRhs alternative));
  rhs <- simplifyExpr (alternativeEnv floatedEnv inner innerBinder alternative) copy;
  pure alternative {altRhs = rhs}};
  peelLets expr = case expr of {
  ExLet bind inner -> let {
  (binds, deepest) = peelLets inner} in (bind : binds, deepest);
  _ -> ([], expr)}};
caseOfKnown :: Simpl -> Expr -> Binder -> [Alt] -> SimplM (Maybe Expr);
caseOfKnown env scrutinee binder alternatives | ExLit literal <- scrutinee = pure $ do {
  alternative <- List.find (matchesLiteral literal . altCon) alternatives <|> List.find ((== AltDefault) . altCon) alternatives;
  Just (substExpr (Map.singleton (binderName binder) scrutinee) (altRhs alternative))} | otherwise = caseOfKnownConstructor env scrutinee binder alternatives;
caseOfKnownConstructor :: Simpl -> Expr -> Binder -> [Alt] -> SimplM (Maybe Expr);
caseOfKnownConstructor env scrutinee binder alternatives = do {
  known <- knownConstructor env scrutinee;
  pure $ do {
  (binds, con, types, fields) <- known;
  alternative <- List.find ((== AltData con) . altCon) alternatives <|> List.find ((== AltDefault) . altCon) alternatives;
  let {
  rhs = altRhs alternative;
  caseBinderBind | Occurrences 0 _ <- occurrences (binderName binder) rhs = Just [] | isLiftedBinder (spEnv env) binder = Just [Bind binder scrutinee] | otherwise = Nothing};
  caseBinds <- caseBinderBind;
  body <- case altCon alternative of {
  AltDefault -> Just rhs;
  _ -> do {
  existentials <- existentialTypes (spEnv env) con types;
  if length existentials /= length (altTypeBinders alternative) || length fields /= length (altBinders alternative) then Nothing else do {
  let {
  typeSubst = Map.fromList (zip (map binderName (altTypeBinders alternative)) existentials);
  fieldBinds = zipWith Bind (altBinders alternative) fields};
  Just (foldr ExLet (substTypeExpr typeSubst rhs) fieldBinds)}}};
  Just (foldr ExLet body (binds <> caseBinds))}};
knownConstructor :: Simpl -> Expr -> SimplM (Maybe ([Bind], Name, [Type], [Expr]));
knownConstructor env expr = do {
  known <- case collectSpine core of {
  (ExVar name, args) | isConstructorName name -> pure (Just ([], name, lefts args, rights args, [])) | Just body <- Map.lookup name (spLocals env) -> unfold body args | Just body <- Map.lookup name (spKnown env) -> unfold body args;
  _ -> pure Nothing};
  pure $ do {
  (binds, con, types, fields, innerCasts) <- known;
  foldM (flip (pushCast (spEnv env))) (binds, con, types, fields) (reduceCasts (innerCasts <> casts))}} where {
  (core, casts) = peelCasts expr;
  unfold body args = do {
  copy <- freshenExpr body;
  pure (peel [] copy args)};
  peel binds body args = case (body, args) of {
  (ExTyLam binder inner, Left ty : rest) -> peel binds (substTypeExpr (Map.singleton (binderName binder) ty) inner) rest;
  (ExLam binder inner, Right argument : rest) | isTrivial argument -> peel binds (substExpr (Map.singleton (binderName binder) argument) inner) rest | otherwise -> peel (Bind binder argument : binds) inner rest;
  (ExLam {}, []) -> Nothing;
  (ExTyLam {}, []) -> Nothing;
  (ExCast inner coercion, []) -> do {
  (binds', con, types, fields, innerCasts) <- peel binds inner [];
  Just (binds', con, types, fields, innerCasts <> [coercion])};
  _ -> case collectSpine body of {
  (ExVar con, conArgs) | isConstructorName con, null args -> Just (reverse binds, con, lefts conArgs, rights conArgs, []);
  _ -> Nothing}}};
reduceCasts :: [Coercion] -> [Coercion];
reduceCasts = reverse . List.foldl' step [] where {
  step stack coercion = case coercion of {
  CoRefl _ -> stack;
  _ -> case stack of {
  top : rest | cancels top coercion -> rest;
  _ -> coercion : stack}};
  cancels left right = left == CoSym right || right == CoSym left};
pushCast :: TypeEnv -> Coercion -> ([Bind], Name, [Type], [Expr]) -> Maybe ([Bind], Name, [Type], [Expr]);
pushCast env coercion (binds, con, types, fields) = do {
  (tyCon, argumentCoercions) <- case coercion of {
  CoTyConApp name arguments -> Just (name, arguments);
  _ -> Nothing};
  conType <- lookupHeaderType env con;
  let {
  (binders, fieldTypes, result) = splitConstructorType env conType;
  (resultHead, resultArgs) = typeSpine (reduceType env result)};
  guard (resultHead == TyCon tyCon);
  guard (resultArgs == map (TyVar . binderName) binders);
  guard (length binders == length types);
  guard (length binders == length argumentCoercions);
  guard (length fieldTypes == length fields);
  types' <- mapM (fmap snd . coercionEndpoints env) argumentCoercions;
  let {
  subst = Map.fromList (zip (map binderName binders) argumentCoercions)};
  fieldCoercions <- mapM (liftCoercion env subst) fieldTypes;
  Just (binds, con, types', zipWith cast fields fieldCoercions)} where {
  cast field fieldCoercion = case fieldCoercion of {
  CoRefl _ -> field;
  _ -> ExCast field fieldCoercion}};
liftCoercion :: TypeEnv -> Map Name Coercion -> Type -> Maybe Coercion;
liftCoercion env subst ty | Set.disjoint (typeVariables ty) (Map.keysSet subst) = Just (CoRefl ty) | otherwise = case ty of {
  TyVar name -> Map.lookup name subst;
  TyApp function argument -> do {
  function' <- liftCoercion env subst function;
  argument' <- liftCoercion env subst argument;
  if isNominalCoercion env function' && isNominalCoercion env argument' then Just (CoApp function' argument') else Nothing};
  TyFun rep1 rep2 argument result | Set.disjoint (typeVariables rep1 <> typeVariables rep2) (Map.keysSet subst) -> CoFun <$> liftCoercion env subst argument <*> liftCoercion env subst result;
  _ -> Nothing};
isNominalCoercion :: TypeEnv -> Coercion -> Bool;
isNominalCoercion env coercion = case coercion of {
  CoVar _ -> True;
  CoRefl _ -> True;
  CoSym inner -> isNominalCoercion env inner;
  CoTrans left right -> isNominalCoercion env left && isNominalCoercion env right;
  CoApp function argument -> isNominalCoercion env function && isNominalCoercion env argument;
  CoNth _ inner -> isNominalCoercion env inner;
  CoFun domain range -> isNominalCoercion env domain && isNominalCoercion env range;
  CoTyConApp _ arguments -> all (isNominalCoercion env) arguments;
  CoAxiom name _ -> case Map.lookup name (teAxioms env) of {
  Just declaration -> axiomRole declaration == Nominal;
  Nothing -> False}};
splitConstructorType :: TypeEnv -> Type -> ([Binder], [Type], Type);
splitConstructorType env ty = case ty of {
  TyForAll binder body -> let {
  (binders, fields, result) = splitConstructorType env body} in (binder : binders, fields, result);
  TyFun _ _ argument body -> let {
  (binders, fields, result) = splitConstructorType env body} in (binders, argument : fields, result);
  _ -> let {
  reduced = reduceType env ty} in if reduced == ty then ([], [], ty) else splitConstructorType env reduced};
typeSpine :: Type -> (Type, [Type]);
typeSpine ty = case ty of {
  TyApp function argument -> let {
  (headType, args) = typeSpine function} in (headType, args <> [argument]);
  _ -> (ty, [])};
existentialTypes :: TypeEnv -> Name -> [Type] -> Maybe [Type];
existentialTypes env con types = do {
  conType <- lookupHeaderType env con;
  let {
  (foralls, result) = splitForAlls env conType;
  free = typeVariables result};
  if length foralls /= length types then Nothing else Just [ty | (binder, ty) <- zip foralls types, binderName binder `Set.notMember` free]};
splitForAlls :: TypeEnv -> Type -> ([Binder], Type);
splitForAlls env ty = case ty of {
  TyForAll binder body -> let {
  (binders, result) = splitForAlls env body} in (binder : binders, result);
  TyFun _ _ _ body -> splitForAlls env body;
  _ -> let {
  reduced = reduceType env ty} in if reduced == ty then ([], ty) else splitForAlls env reduced};
typeVariables :: Type -> Set Name;
typeVariables ty = case ty of {
  TyVar name -> Set.singleton name;
  TyCon {} -> Set.empty;
  TyLit {} -> Set.empty;
  TyApp function argument -> typeVariables function <> typeVariables argument;
  TyFun r1 r2 argument result -> Set.unions (map typeVariables [r1, r2, argument, result]);
  TyForAll binder body -> Set.delete (binderName binder) (typeVariables body) <> typeVariables (binderType binder);
  TyEq left right -> typeVariables left <> typeVariables right};
isTrivial :: Expr -> Bool;
isTrivial expr = case expr of {
  ExVar {} -> True;
  ExLit {} -> True;
  ExCoercion {} -> True;
  ExTyApp body _ -> isTrivial body;
  ExTyLam _ body -> isTrivial body;
  ExCast body _ -> isTrivial body;
  _ -> False};
collectSpine :: Expr -> (Expr, [Arg]);
collectSpine = go [] where {
  go args expr = case expr of {
  ExApp function argument -> go (Right argument : args) function;
  ExTyApp function ty -> go (Left ty : args) function;
  _ -> (expr, args)}};
castedSpine :: Expr -> (Expr, [Arg]);
castedSpine = go [] where {
  go args expr = case expr of {
  ExApp function argument -> go (Right argument : args) function;
  ExTyApp function ty -> go (Left ty : args) function;
  ExCast body _ -> go args body;
  _ -> (expr, args)}};
pushHeadCasts :: Expr -> Maybe Expr;
pushHeadCasts expr = case collectSpine expr of {
  (ExCast body coercion, args) -> push body coercion args;
  _ -> Nothing} where {
  push body coercion args = case (funCoercion coercion, args) of {
  (Just (argCo, resultCo), Right argument : rest) -> let {
  applied = ExApp body (mkCoercionCast argument (coSym argCo))} in Just (fromMaybe (rebuildSpine (mkCoercionCast applied resultCo) rest) (push applied resultCo rest));
  _ -> Nothing}};
funCoercion :: Coercion -> Maybe (Coercion, Coercion);
funCoercion coercion = case coercion of {
  CoFun argCo resultCo -> Just (argCo, resultCo);
  CoSym (CoFun argCo resultCo) -> Just (coSym argCo, coSym resultCo);
  _ -> Nothing};
coSym :: Coercion -> Coercion;
coSym coercion = case coercion of {
  CoSym inner -> inner;
  CoRefl ty -> CoRefl ty;
  _ -> CoSym coercion};
mkCoercionCast :: Expr -> Coercion -> Expr;
mkCoercionCast body coercion = case coercion of {
  CoRefl _ -> body;
  _ -> ExCast body coercion};
rebuildSpine :: Expr -> [Arg] -> Expr;
rebuildSpine = List.foldl' apply where {
  apply function arg = case arg of {
  Left ty -> ExTyApp function ty;
  Right argument -> ExApp function argument}};
mkCase :: TypeEnv -> Expr -> Binder -> Type -> [Alt] -> Expr;
mkCase env scrutinee binder resultType alternatives = case List.partition ((== AltDefault) . altCon) alternatives of {
  ([defaultAlt], others) | ExCase inner innerBinder _ innerAlternatives <- altRhs defaultAlt, inner == scrutinee, isTrivial scrutinee || isPurePrimitiveCall env scrutinee -> let {
  renamed = substExpr (Map.singleton (binderName innerBinder) (ExVar (binderName binder)));
  covered = Set.fromList (map altCon others);
  continued = [alternative {altRhs = renamed (altRhs alternative)} | alternative <- innerAlternatives, altCon alternative `Set.notMember` covered];
  (innerDefaults, innerOthers) = List.partition ((== AltDefault) . altCon) continued} in ExCase scrutinee binder resultType (others <> innerOthers <> innerDefaults);
  _ -> ExCase scrutinee binder resultType alternatives};
literalEqualityCase :: TypeEnv -> Expr -> Binder -> Type -> [Alt] -> Maybe Expr;
literalEqualityCase env scrutinee binder resultType alternatives = do {
  (call, arguments) <- case scrutinee of {
  ExForeignCall call [] arguments | foreignCallConvention call == Prim -> Just (call, arguments);
  _ -> Nothing};
  negated <- List.lookup (nameText (foreignCallName call)) literalEqualities;
  (argumentTypes, resultType') <- foreignSignature env (foreignCallType call);
  resultRep <- reduceType env <$> repOf env resultType';
  (compared, comparedType, literal) <- case (arguments, argumentTypes) of {
  ([ExLit literal, other], [_, ty]) -> Just (other, ty, literal);
  ([other, ExLit literal], [ty, _]) -> Just (other, ty, literal);
  _ -> Nothing};
  let {
  select value = do {
  alternative <- List.find (matchesLiteral (LitInt resultRep value) . altCon) alternatives <|> List.find ((== AltDefault) . altCon) alternatives;
  Just (substExpr (Map.singleton (binderName binder) (ExLit (LitInt resultRep value))) (altRhs alternative))}};
  hit <- select (if negated then 0 else 1);
  miss <- select (if negated then 1 else 0);
  Just (mkCase env compared (Binder (binderName binder) comparedType) resultType [Alt (AltLit literal) [] [] hit, Alt AltDefault [] [] miss])};
literalEqualities :: [(Text, Bool)];
literalEqualities = [("==#", False), ("/=#", True), ("eqChar#", False), ("neChar#", True), ("eqWord#", False), ("neWord#", True)];
matchesLiteral :: Literal -> AltCon -> Bool;
matchesLiteral literal con = case (con, literal) of {
  (AltLit (LitInt _ left), LitInt _ right) -> left == right;
  (AltLit (LitChar _ left), LitChar _ right) -> left == right;
  (AltLit (LitAddr _ left), LitAddr _ right) -> left == right;
  _ -> False};
foreignSignature :: TypeEnv -> Type -> Maybe ([Type], Type);
foreignSignature env ty = case viewFun env ty of {
  Just (_, _, argument, result) -> do {
  (arguments, final) <- foreignSignature env result;
  Just (argument : arguments, final)};
  Nothing | Just _ <- viewForAll env ty -> Nothing | otherwise -> Just ([], ty)};
isPurePrimitiveCall :: TypeEnv -> Expr -> Bool;
isPurePrimitiveCall env expr = case expr of {
  ExForeignCall call types arguments -> foreignCallConvention call == Prim && null types && all isTrivial arguments && case foreignSignature env (foreignCallType call) of {
  Just (argumentTypes, resultType) -> not (any mentionsState (resultType : argumentTypes));
  Nothing -> False};
  _ -> False} where {
  mentionsState ty = case typeSpine (reduceType env ty) of {
  (TyCon name, args) -> nameText name `elem` ["State#", "MutVar#", "MVar#", "TVar#", "MutableArray#", "MutableByteArray#", "SmallMutableArray#", "MutableArrayArray#", "Weak#", "StablePtr#", "StableName#", "ThreadId#", "BCO", "Addr#"] || any mentionsState args;
  (_, args) -> any mentionsState args}};
data Occurrences = Occurrences !Int !Bool;
instance Semigroup Occurrences where {
  Occurrences count1 repeated1 <> Occurrences count2 repeated2 = Occurrences (count1 + count2) (repeated1 || repeated2)};
instance Monoid Occurrences where {
  mempty = Occurrences 0 False};
occurrences :: Name -> Expr -> Occurrences;
occurrences name = go where {
  go expr = case expr of {
  ExVar var | var == name -> Occurrences 1 False | otherwise -> mempty;
  ExLit {} -> mempty;
  ExApp function argument -> go function <> go argument;
  ExTyApp function _ -> go function;
  ExLam _ body -> repeated (go body);
  ExTyLam _ body -> go body;
  ExLet bind body -> go (bindRhs bind) <> go body;
  ExRec binds body -> repeated (foldMap (go . bindRhs) binds) <> go body;
  ExCase scrutinee _ _ alternatives -> go scrutinee <> foldMap (go . altRhs) alternatives;
  ExCast body coercion -> go body <> coercionUses coercion;
  ExCoercion coercion -> coercionUses coercion;
  ExForeignCall _ _ arguments -> foldMap go arguments};
  coercionUses coercion = Occurrences (length (filter (== name) (coercionVariables coercion))) False;
  repeated (Occurrences count _) = Occurrences count (count > 0)};
coercionVariables :: Coercion -> [Name];
coercionVariables coercion = case coercion of {
  CoVar name -> [name];
  CoRefl {} -> [];
  CoSym inner -> coercionVariables inner;
  CoTrans left right -> coercionVariables left <> coercionVariables right;
  CoApp left right -> coercionVariables left <> coercionVariables right;
  CoFun left right -> coercionVariables left <> coercionVariables right;
  CoNth _ inner -> coercionVariables inner;
  CoTyConApp _ inners -> concatMap coercionVariables inners;
  CoAxiom {} -> []};
exprValueNames :: Expr -> Set Name;
exprValueNames = go where {
  go expr = case expr of {
  ExVar name -> Set.singleton name;
  ExLit {} -> Set.empty;
  ExCoercion {} -> Set.empty;
  ExApp function argument -> go function <> go argument;
  ExTyApp function _ -> go function;
  ExLam _ body -> go body;
  ExTyLam _ body -> go body;
  ExLet bind body -> go (bindRhs bind) <> go body;
  ExRec binds body -> foldMap (go . bindRhs) binds <> go body;
  ExCase scrutinee _ _ alternatives -> go scrutinee <> foldMap (go . altRhs) alternatives;
  ExCast body _ -> go body;
  ExForeignCall _ _ arguments -> foldMap go arguments}};
substExpr :: Map Name Expr -> Expr -> Expr;
substExpr subst = go where {
  go expr = case expr of {
  ExVar name -> Map.findWithDefault expr name subst;
  ExLit {} -> expr;
  ExApp function argument -> ExApp (go function) (go argument);
  ExTyApp function ty -> ExTyApp (go function) ty;
  ExLam binder body -> ExLam binder (go body);
  ExTyLam binder body -> ExTyLam binder (go body);
  ExLet bind body -> ExLet bind {bindRhs = go (bindRhs bind)} (go body);
  ExRec binds body -> ExRec [bind {bindRhs = go (bindRhs bind)} | bind <- binds] (go body);
  ExCase scrutinee binder resultType alternatives -> ExCase (go scrutinee) binder resultType [alternative {altRhs = go (altRhs alternative)} | alternative <- alternatives];
  ExCast body coercion -> ExCast (go body) (substCoercion coercion);
  ExCoercion coercion -> ExCoercion (substCoercion coercion);
  ExForeignCall call types arguments -> ExForeignCall call types (map go arguments)};
  substCoercion coercion = case coercion of {
  CoVar name -> case Map.lookup name subst of {
  Just (ExCoercion replacement) -> replacement;
  Just (ExVar replacement) -> CoVar replacement;
  _ -> coercion};
  CoRefl {} -> coercion;
  CoSym inner -> CoSym (substCoercion inner);
  CoTrans left right -> CoTrans (substCoercion left) (substCoercion right);
  CoApp left right -> CoApp (substCoercion left) (substCoercion right);
  CoFun left right -> CoFun (substCoercion left) (substCoercion right);
  CoNth index inner -> CoNth index (substCoercion inner);
  CoTyConApp name inners -> CoTyConApp name (map substCoercion inners);
  CoAxiom {} -> coercion}};
substTypeExpr :: Map Name Type -> Expr -> Expr;
substTypeExpr subst = go where {
  onType = substTypes subst;
  onBinder binder = binder {binderType = onType (binderType binder)};
  go expr = case expr of {
  ExVar {} -> expr;
  ExLit literal -> ExLit (onLiteral literal);
  ExApp function argument -> ExApp (go function) (go argument);
  ExTyApp function ty -> ExTyApp (go function) (onType ty);
  ExLam binder body -> ExLam (onBinder binder) (go body);
  ExTyLam binder body -> ExTyLam (onBinder binder) (go body);
  ExLet bind body -> ExLet (onBind bind) (go body);
  ExRec binds body -> ExRec (map onBind binds) (go body);
  ExCase scrutinee binder resultType alternatives -> ExCase (go scrutinee) (onBinder binder) (onType resultType) (map onAlt alternatives);
  ExCast body coercion -> ExCast (go body) (onCoercion coercion);
  ExCoercion coercion -> ExCoercion (onCoercion coercion);
  ExForeignCall call types arguments -> ExForeignCall call (map onType types) (map go arguments)};
  onBind bind = Bind (onBinder (bindBinder bind)) (go (bindRhs bind));
  onAlt alternative = alternative {altCon = onAltCon (altCon alternative), altTypeBinders = map onBinder (altTypeBinders alternative), altBinders = map onBinder (altBinders alternative), altRhs = go (altRhs alternative)};
  onAltCon con = case con of {
  AltLit literal -> AltLit (onLiteral literal);
  _ -> con};
  onLiteral literal = case literal of {
  LitInt ty value -> LitInt (onType ty) value;
  LitChar ty value -> LitChar (onType ty) value;
  LitAddr ty value -> LitAddr (onType ty) value};
  onCoercion coercion = case coercion of {
  CoVar {} -> coercion;
  CoRefl ty -> CoRefl (onType ty);
  CoSym inner -> CoSym (onCoercion inner);
  CoTrans left right -> CoTrans (onCoercion left) (onCoercion right);
  CoApp left right -> CoApp (onCoercion left) (onCoercion right);
  CoFun left right -> CoFun (onCoercion left) (onCoercion right);
  CoNth index inner -> CoNth index (onCoercion inner);
  CoTyConApp name inners -> CoTyConApp name (map onCoercion inners);
  CoAxiom name types -> CoAxiom name (map onType types)}};
freshenExpr :: Expr -> SimplM Expr;
freshenExpr expr = state (\st -> let {
  (result, supply) = runState (renameExpr Map.empty expr) (ssSupply st)} in (result, st {ssSupply = supply}));
type FreshM = State Int;
freshName :: Name -> FreshM Name;
freshName name = state (\supply -> (name {nameOrigin = OriginLocal (Unique supply)}, supply + 1));
renameBinder :: Map Name Name -> Binder -> FreshM (Binder, Map Name Name);
renameBinder renaming binder = do {
  ty <- renameType renaming (binderType binder);
  name <- freshName (binderName binder);
  pure (Binder name ty, Map.insert (binderName binder) name renaming)};
renameBinders :: Map Name Name -> [Binder] -> FreshM ([Binder], Map Name Name);
renameBinders renaming = foldM (\(done, current) binder -> (\(binder', next) -> (done <> [binder'], next)) <$> renameBinder current binder) ([], renaming);
renameUse :: Map Name Name -> Name -> Name;
renameUse renaming name = Map.findWithDefault name name renaming;
renameType :: Map Name Name -> Type -> FreshM Type;
renameType renaming ty = case ty of {
  TyVar name -> pure (TyVar (renameUse renaming name));
  TyCon {} -> pure ty;
  TyLit {} -> pure ty;
  TyApp function argument -> TyApp <$> renameType renaming function <*> renameType renaming argument;
  TyFun r1 r2 argument result -> TyFun <$> renameType renaming r1 <*> renameType renaming r2 <*> renameType renaming argument <*> renameType renaming result;
  TyForAll binder body -> do {
  (binder', bodyRenaming) <- renameBinder renaming binder;
  TyForAll binder' <$> renameType bodyRenaming body};
  TyEq left right -> TyEq <$> renameType renaming left <*> renameType renaming right};
renameCoercion :: Map Name Name -> Coercion -> FreshM Coercion;
renameCoercion renaming coercion = case coercion of {
  CoVar name -> pure (CoVar (renameUse renaming name));
  CoRefl ty -> CoRefl <$> renameType renaming ty;
  CoSym inner -> CoSym <$> renameCoercion renaming inner;
  CoTrans left right -> CoTrans <$> renameCoercion renaming left <*> renameCoercion renaming right;
  CoApp left right -> CoApp <$> renameCoercion renaming left <*> renameCoercion renaming right;
  CoFun left right -> CoFun <$> renameCoercion renaming left <*> renameCoercion renaming right;
  CoNth index inner -> CoNth index <$> renameCoercion renaming inner;
  CoTyConApp name inners -> CoTyConApp name <$> mapM (renameCoercion renaming) inners;
  CoAxiom name types -> CoAxiom name <$> mapM (renameType renaming) types};
renameLiteral :: Map Name Name -> Literal -> FreshM Literal;
renameLiteral renaming literal = case literal of {
  LitInt ty value -> (`LitInt` value) <$> renameType renaming ty;
  LitChar ty value -> (`LitChar` value) <$> renameType renaming ty;
  LitAddr ty value -> (`LitAddr` value) <$> renameType renaming ty};
renameExpr :: Map Name Name -> Expr -> FreshM Expr;
renameExpr renaming expr = case expr of {
  ExVar name -> pure (ExVar (renameUse renaming name));
  ExLit literal -> ExLit <$> renameLiteral renaming literal;
  ExApp function argument -> ExApp <$> renameExpr renaming function <*> renameExpr renaming argument;
  ExTyApp function ty -> ExTyApp <$> renameExpr renaming function <*> renameType renaming ty;
  ExLam binder body -> do {
  (binder', bodyRenaming) <- renameBinder renaming binder;
  ExLam binder' <$> renameExpr bodyRenaming body};
  ExTyLam binder body -> do {
  (binder', bodyRenaming) <- renameBinder renaming binder;
  ExTyLam binder' <$> renameExpr bodyRenaming body};
  ExLet bind body -> do {
  rhs <- renameExpr renaming (bindRhs bind);
  (binder', bodyRenaming) <- renameBinder renaming (bindBinder bind);
  ExLet (Bind binder' rhs) <$> renameExpr bodyRenaming body};
  ExRec binds body -> do {
  (binders, groupRenaming) <- renameBinders renaming (map bindBinder binds);
  rhss <- mapM (renameExpr groupRenaming . bindRhs) binds;
  ExRec (zipWith Bind binders rhss) <$> renameExpr groupRenaming body};
  ExCase scrutinee binder resultType alternatives -> do {
  scrutinee' <- renameExpr renaming scrutinee;
  (binder', caseRenaming) <- renameBinder renaming binder;
  resultType' <- renameType renaming resultType;
  ExCase scrutinee' binder' resultType' <$> mapM (renameAlt caseRenaming) alternatives};
  ExCast body coercion -> ExCast <$> renameExpr renaming body <*> renameCoercion renaming coercion;
  ExCoercion coercion -> ExCoercion <$> renameCoercion renaming coercion;
  ExForeignCall call types arguments -> do {
  foreignType <- renameType Map.empty (foreignCallType call);
  ExForeignCall call {foreignCallType = foreignType} <$> mapM (renameType renaming) types <*> mapM (renameExpr renaming) arguments}};
renameAlt :: Map Name Name -> Alt -> FreshM Alt;
renameAlt renaming alternative = do {
  con <- case altCon alternative of {
  AltLit literal -> AltLit <$> renameLiteral renaming literal;
  other -> pure other};
  (typeBinders, typeRenaming) <- renameBinders renaming (altTypeBinders alternative);
  (binders, rhsRenaming) <- renameBinders typeRenaming (altBinders alternative);
  rhs <- renameExpr rhsRenaming (altRhs alternative);
  pure (Alt con typeBinders binders rhs)};
maxLocalUnique :: Program -> Int;
maxLocalUnique program = maximum (0 : mapMaybe localValue (Set.toList names)) where {
  names = foldMap declReferences (programDecls program) <> foldMap declBinderNames (programDecls program);
  localValue name = case nameOrigin name of {
  OriginLocal (Unique unique) -> Just unique;
  OriginTop {} -> Nothing}};
declBinderNames :: Decl -> Set Name;
declBinderNames decl = case decl of {
  DeclVal declaration -> exprBinderNames (valBody declaration) <> typeBinderNames (valType declaration);
  DeclRule declaration -> Set.fromList (map binderName (ruleTypeBinders declaration <> ruleBinders declaration)) <> foldMap (typeBinderNames . binderType) (ruleTypeBinders declaration <> ruleBinders declaration) <> typeBinderNames (ruleType declaration) <> exprBinderNames (ruleLhs declaration) <> exprBinderNames (ruleRhs declaration);
  DeclType declaration -> Set.fromList (map binderName (typeBinders declaration)) <> foldMap (typeBinderNames . conType) (typeCons declaration);
  DeclSynonym declaration -> Set.fromList (map binderName (synBinders declaration)) <> typeBinderNames (synBody declaration);
  DeclAxiom declaration -> Set.fromList (map binderName (axiomBinders declaration))};
typeBinderNames :: Type -> Set Name;
typeBinderNames ty = case ty of {
  TyVar {} -> Set.empty;
  TyCon {} -> Set.empty;
  TyLit {} -> Set.empty;
  TyApp function argument -> typeBinderNames function <> typeBinderNames argument;
  TyFun r1 r2 argument result -> foldMap typeBinderNames [r1, r2, argument, result];
  TyForAll binder body -> Set.insert (binderName binder) (typeBinderNames (binderType binder) <> typeBinderNames body);
  TyEq left right -> typeBinderNames left <> typeBinderNames right};
exprBinderNames :: Expr -> Set Name;
exprBinderNames = go where {
  binderNames binder = Set.insert (binderName binder) (typeBinderNames (binderType binder));
  go expr = case expr of {
  ExVar {} -> Set.empty;
  ExLit {} -> Set.empty;
  ExCoercion {} -> Set.empty;
  ExApp function argument -> go function <> go argument;
  ExTyApp function ty -> go function <> typeBinderNames ty;
  ExLam binder body -> binderNames binder <> go body;
  ExTyLam binder body -> binderNames binder <> go body;
  ExLet bind body -> binderNames (bindBinder bind) <> go (bindRhs bind) <> go body;
  ExRec binds body -> foldMap (\bind -> binderNames (bindBinder bind) <> go (bindRhs bind)) binds <> go body;
  ExCase scrutinee binder resultType alternatives -> go scrutinee <> binderNames binder <> typeBinderNames resultType <> foldMap altNames alternatives;
  ExCast body _ -> go body;
  ExForeignCall call types arguments -> typeBinderNames (foreignCallType call) <> foldMap typeBinderNames types <> foldMap go arguments};
  altNames alternative = foldMap binderNames (altTypeBinders alternative <> altBinders alternative) <> go (altRhs alternative)}
}
