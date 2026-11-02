{-# LANGUAGE OverloadedStrings #-}
module Aihc.Fc.Arity (etaExpandProgram, EtaReport(..)) where {
import Aihc.Fc.Imports (pruneImports);
import Aihc.Fc.Name;
import Aihc.Fc.Syntax;
import Aihc.Fc.TypeOf (TypeEnv(..), coercionEndpoints, extendBinder, matchRepresentationalAxiom, reduceType, substType, typeEnvFromProgram, typeHead, viewForAll, viewFun);
import Aihc.Fc.Wired (primPackageFromScopes);
import Aihc.Tc.Types (Unique(..));
import Control.Monad.Trans.State.Strict (State, get, put, runState);
import Data.Graph (SCC(..), stronglyConnComp);
import Data.List qualified as List;
import Data.Map.Strict (Map);
import Data.Map.Strict qualified as Map;
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe);
import Data.Set (Set);
import Data.Set qualified as Set;
data EtaReport = EtaReport { reportExpandedValues :: !Int, reportAddedLambdas :: !Int } deriving (Eq, Show);
data Cost = IsCheap | IsExpensive deriving (Eq, Show);
addCost :: Cost -> Cost -> Cost;
addCost IsCheap IsCheap = IsCheap;
addCost _ _ = IsExpensive;
data OneShot = OneShot | NotOneShot deriving (Eq, Show);
type LamInfo = (Cost, OneShot);
newtype ArityType = AT [LamInfo] deriving (Eq, Show);
topArityType :: ArityType;
topArityType = AT [];
arityLam :: Env -> Binder -> ArityType -> ArityType;
arityLam env binder (AT lams) = AT ((IsCheap, oneShotBinder env binder) : lams);
arityApp :: ArityType -> Cost -> ArityType;
arityApp (AT ((cost, _) : lams)) argumentCost = floatIn (addCost cost argumentCost) (AT lams);
arityApp at _ = at;
floatIn :: Cost -> ArityType -> ArityType;
floatIn cost at@(AT lams) = case lams of {
  [] -> at;
  (IsExpensive, _) : _ -> at;
  (_, oneShot) : rest -> AT ((cost, oneShot) : rest)};
addWork :: ArityType -> ArityType;
addWork at@(AT lams) = case lams of {
  [] -> at;
  (_, oneShot) : rest -> AT ((IsExpensive, oneShot) : rest)};
andArityType :: ArityType -> ArityType -> ArityType;
andArityType (AT left) (AT right) = AT (zipWith both left right) where {
  both (leftCost, leftOneShot) (rightCost, rightOneShot) = (addCost leftCost rightCost, andOneShot leftOneShot rightOneShot);
  andOneShot OneShot OneShot = OneShot;
  andOneShot _ _ = NotOneShot};
safeArity :: ArityType -> Int;
safeArity (AT lams) = go 0 IsCheap lams where {
  go arity _ [] = arity;
  go arity before ((cost, oneShot) : rest) = case (addCost before cost, oneShot) of {
  (IsExpensive, NotOneShot) -> arity;
  (combined, _) -> go (arity + 1) combined rest}};
data Env = Env { envTypes :: !TypeEnv, envSigs :: !(Map Name ArityType), envNewtypes :: !(Map Name [AxiomDecl]) };
extendSig :: Env -> Name -> ArityType -> Env;
extendSig env name at = env {envSigs = Map.insert name at (envSigs env)};
shadow :: Env -> Name -> Env;
shadow env name = env {envSigs = Map.delete name (envSigs env)};
extendType :: Env -> Binder -> Env;
extendType env binder = env {envTypes = extendBinder (envTypes env) binder};
newtypeAxioms :: TypeEnv -> Map Name [AxiomDecl];
newtypeAxioms types = Map.fromListWith (<>) [(head', [declaration]) | declaration <- Map.elems (teAxioms types), axiomRole declaration == Representational, Just head' <- [typeHead (axiomLeft declaration)]];
unfoldNewtype :: Env -> Set Name -> Type -> Maybe (Coercion, Type);
unfoldNewtype env unfolded ty = do {
  head' <- typeHead (reduceType (envTypes env) ty);
  listToMaybe [(CoAxiom (axiomName declaration) arguments, right) | declaration <- Map.findWithDefault [] head' (envNewtypes env), axiomName declaration `Set.notMember` unfolded, Just (arguments, right) <- [matchRepresentationalAxiom (envTypes env) declaration ty]]};
oneShotType :: Env -> Type -> OneShot;
oneShotType env ty = case typeHead (reduceType (envTypes env) ty) of {
  Just name | nameText name == "State#" -> OneShot;
  _ -> NotOneShot};
oneShotBinder :: Env -> Binder -> OneShot;
oneShotBinder env = oneShotType env . binderType;
typeOneShots :: Env -> Type -> [OneShot];
typeOneShots env = go Set.empty where {
  go unfolded ty = case viewForAll (envTypes env) ty of {
  Just (_, body) -> go unfolded body;
  Nothing -> case viewFun (envTypes env) ty of {
  Just (_, _, argument, result) -> oneShotType env argument : go unfolded result;
  Nothing -> case unfoldNewtype env unfolded ty of {
  Just (CoAxiom name _, right) -> go (Set.insert name unfolded) right;
  _ -> []}}}};
typeArity :: Env -> Type -> Int;
typeArity env = length . typeOneShots env;
arityType :: Env -> Expr -> ArityType;
arityType env expr = case expr of {
  ExVar name -> fromMaybe topArityType (Map.lookup name (envSigs env));
  ExLit {} -> topArityType;
  ExCoercion {} -> topArityType;
  ExForeignCall {} -> topArityType;
  ExCast body _ -> arityType env body;
  ExTyApp function _ -> arityType env function;
  ExApp function argument -> arityApp (arityType env function) (exprCost env argument);
  ExLam binder body -> arityLam env binder (arityType (shadow env (binderName binder)) body);
  ExTyLam binder body -> arityType (extendType env binder) body;
  ExLet bind body -> floatIn (exprCost env (bindRhs bind)) (arityType (extendSig env (binderName (bindBinder bind)) (arityType env (bindRhs bind))) body);
  ExRec binds body -> floatIn (List.foldl' addCost IsCheap (map (exprCost env . bindRhs) binds)) (arityType (List.foldl' shadow env (map (binderName . bindBinder) binds)) body);
  ExCase scrutinee binder _ alternatives -> case alternatives of {
  [] -> topArityType;
  first : rest -> let {
  inner = shadow env (binderName binder);
  alternativeArity alternative = arityType (List.foldl' shadow (List.foldl' extendType inner (altTypeBinders alternative)) (map binderName (altBinders alternative))) (altRhs alternative);
  alternativesArity = List.foldl' andArityType (alternativeArity first) (map alternativeArity rest)} in if isCheap env scrutinee then alternativesArity else addWork alternativesArity}};
exprCost :: Env -> Expr -> Cost;
exprCost env expr = if isCheap env expr then IsCheap else IsExpensive;
isCheap :: Env -> Expr -> Bool;
isCheap env expr = case expr of {
  ExLit {} -> True;
  ExVar {} -> True;
  ExCoercion {} -> True;
  ExLam {} -> True;
  ExTyLam _ body -> isCheap env body;
  ExCast body _ -> isCheap env body;
  ExTyApp body _ -> isCheap env body;
  ExApp {} -> case collectSpine expr of {
  (ExVar name, arguments) | nameSort name == SortDataConstructor -> all cheapArgument arguments | otherwise -> let {
  AT lams = fromMaybe topArityType (Map.lookup name (envSigs env))} in length [() | Right _ <- arguments] < length lams && all cheapArgument arguments;
  _ -> False};
  ExLet bind body -> isCheap env (bindRhs bind) && isCheap (extendSig env (binderName (bindBinder bind)) (arityType env (bindRhs bind))) body;
  ExCase scrutinee binder _ alternatives -> isCheap env scrutinee && all cheapAlternative alternatives where {
  inner = shadow env (binderName binder);
  cheapAlternative alternative = isCheap (List.foldl' shadow (List.foldl' extendType inner (altTypeBinders alternative)) (map binderName (altBinders alternative))) (altRhs alternative)};
  _ -> False} where {
  cheapArgument = either (const True) (isCheap env)};
type Arg = Either Type Expr;
collectSpine :: Expr -> (Expr, [Arg]);
collectSpine = go [] where {
  go arguments expr = case expr of {
  ExApp function argument -> go (Right argument : arguments) function;
  ExTyApp function argument -> go (Left argument : arguments) function;
  _ -> (expr, arguments)}};
manifestArityType :: Env -> Expr -> ArityType;
manifestArityType env expr = case expr of {
  ExLam binder body -> arityLam env binder (manifestArityType env body);
  ExTyLam binder body -> manifestArityType (extendType env binder) body;
  ExCast body _ -> manifestArityType env body;
  _ -> topArityType};
manifestArity :: Env -> Expr -> Int;
manifestArity env expr = let {
  AT lams = manifestArityType env expr} in length lams;
etaExpandProgram :: Program -> (Program, EtaReport);
etaExpandProgram program = case primPackageFromScopes (programScopes program) of {
  Nothing -> (program, EtaReport 0 0);
  Just primPackage -> let {
  types = typeEnvFromProgram primPackage program;
  env0 = Env {envTypes = types, envSigs = Map.empty, envNewtypes = newtypeAxioms types};
  env = valueSignatures env0 (programDecls program);
  supply = maxLocalUnique program + 1;
  (decls, (_, report)) = runState (traverse (expandDecl env) (programDecls program)) (supply, EtaReport 0 0)} in (pruneImports program {programDecls = decls}, report)};
valueSignatures :: Env -> [Decl] -> Env;
valueSignatures env decls = List.foldl' addComponent env (stronglyConnComp graph) where {
  declarations = [declaration | DeclVal declaration <- decls];
  names = Set.fromList (map valName declarations);
  graph = [(declaration, valName declaration, Set.toList (Set.intersection names (valueReferences (valBody declaration)))) | declaration <- declarations];
  addComponent current component = case component of {
  AcyclicSCC declaration -> extendSig current (valName declaration) (declarationArityType current declaration);
  CyclicSCC members -> List.foldl' (\inner declaration -> extendSig inner (valName declaration) (manifestArityType inner (valBody declaration))) current members};
  declarationArityType current declaration = trimArityType (typeArity current (valType declaration)) (arityType current (valBody declaration))};
trimArityType :: Int -> ArityType -> ArityType;
trimArityType limit (AT lams) = AT (take limit lams);
valueReferences :: Expr -> Set Name;
valueReferences = go where {
  go expr = case expr of {
  ExVar name -> Set.singleton name;
  ExLit {} -> Set.empty;
  ExCoercion {} -> Set.empty;
  ExApp function argument -> go function <> go argument;
  ExTyApp function _ -> go function;
  ExLam _ body -> go body;
  ExTyLam _ body -> go body;
  ExCast body _ -> go body;
  ExLet bind body -> go (bindRhs bind) <> go body;
  ExRec binds body -> foldMap (go . bindRhs) binds <> go body;
  ExCase scrutinee _ _ alternatives -> go scrutinee <> foldMap (go . altRhs) alternatives;
  ExForeignCall _ _ arguments -> foldMap go arguments}};
type ExpandM = State (Int, EtaReport);
expandDecl :: Env -> Decl -> ExpandM Decl;
expandDecl env decl = case decl of {
  DeclVal declaration -> do {
  let {
  body = valBody declaration;
  wanted = safeArity (trimArityType (typeArity env (valType declaration)) (arityType env body));
  manifest = manifestArity env body};
  if wanted <= manifest then pure decl else do {
  expanded <- expand env Set.empty (valType declaration) (wanted - manifest) body;
  case expanded of {
  Nothing -> pure decl;
  Just body' | body' == body -> pure decl;
  Just body' -> do {
  (supply, report) <- get;
  put (supply, report {reportExpandedValues = reportExpandedValues report + 1, reportAddedLambdas = reportAddedLambdas report + (wanted - manifest)});
  pure (DeclVal declaration {valBody = body'})}}}};
  _ -> pure decl};
expand :: Env -> Set Name -> Type -> Int -> Expr -> ExpandM (Maybe Expr);
expand env unfolded ty extra expr | extra <= 0 = pure (Just expr) | otherwise = case expr of {
  ExTyLam binder body | Just (typeBinder, rest) <- viewForAll (envTypes env) ty -> let {
  renamed = substType (binderName typeBinder) (TyVar (binderName binder)) rest} in fmap (ExTyLam binder) <$> expand (extendType env binder) unfolded renamed extra body;
  ExLam binder body | Just (_, _, _, result) <- viewFun (envTypes env) ty -> fmap (ExLam binder) <$> expand env unfolded result extra body;
  _ | Just (_, _, argument, result) <- viewFun (envTypes env) ty -> do {
  binder <- freshBinder argument;
  fmap (ExLam binder) <$> expand env unfolded result (extra - 1) (applyToVar env expr (binderName binder))} | Just (coercion@(CoAxiom axiom _), right) <- unfoldNewtype env unfolded ty -> fmap (`mkCast` CoSym coercion) <$> expand env (Set.insert axiom unfolded) right extra (mkCast expr coercion) | otherwise -> pure Nothing};
applyToVar :: Env -> Expr -> Name -> Expr;
applyToVar env expr arg = case expr of {
  ExLet bind body -> ExLet bind (applyToVar env body arg);
  ExRec binds body -> ExRec binds (applyToVar env body arg);
  ExCase scrutinee binder resultType alternatives | Just (_, _, _, result) <- viewFun (envTypes env) resultType -> ExCase scrutinee binder result (map (applyAlternative (shadow env (binderName binder))) alternatives);
  ExLam binder body -> substVar (binderName binder) arg body;
  ExCast (ExLet bind body) coercion -> applyToVar env (ExLet bind (mkCast body coercion)) arg;
  ExCast (ExRec binds body) coercion -> applyToVar env (ExRec binds (mkCast body coercion)) arg;
  ExCast (ExCase scrutinee binder _ alternatives) coercion | Just (_, resultType) <- coercionEndpoints (envTypes env) coercion -> applyToVar env (ExCase scrutinee binder resultType [alternative {altRhs = mkCast (altRhs alternative) coercion} | alternative <- alternatives]) arg;
  _ -> ExApp expr (ExVar arg)} where {
  applyAlternative inner alternative = alternative {altRhs = applyToVar (List.foldl' extendType inner (altTypeBinders alternative)) (altRhs alternative) arg}};
substVar :: Name -> Name -> Expr -> Expr;
substVar from to = go where {
  go expr = case expr of {
  ExVar name | name == from -> ExVar to | otherwise -> expr;
  ExLit {} -> expr;
  ExCoercion {} -> expr;
  ExApp function argument -> ExApp (go function) (go argument);
  ExTyApp function argument -> ExTyApp (go function) argument;
  ExLam binder body | binderName binder == from -> expr | otherwise -> ExLam binder (go body);
  ExTyLam binder body -> ExTyLam binder (go body);
  ExCast body coercion -> ExCast (go body) coercion;
  ExLet bind body | binderName (bindBinder bind) == from -> ExLet bind {bindRhs = go (bindRhs bind)} body | otherwise -> ExLet bind {bindRhs = go (bindRhs bind)} (go body);
  ExRec binds body | any ((== from) . binderName . bindBinder) binds -> expr | otherwise -> ExRec [bind {bindRhs = go (bindRhs bind)} | bind <- binds] (go body);
  ExCase scrutinee binder resultType alternatives -> ExCase (go scrutinee) binder resultType (map goAlternative alternatives) where {
  goAlternative alternative | binderName binder == from || any ((== from) . binderName) (altBinders alternative) = alternative | otherwise = alternative {altRhs = go (altRhs alternative)}};
  ExForeignCall call types arguments -> ExForeignCall call types (map go arguments)}};
freshBinder :: Type -> ExpandM Binder;
freshBinder ty = do {
  (supply, report) <- get;
  put (supply + 1, report);
  pure (Binder (Name "eta" SortValue (OriginLocal (Unique supply))) ty)};
mkCast :: Expr -> Coercion -> Expr;
mkCast expr coercion = case expr of {
  ExCast inner inner' | cancels inner' coercion -> inner;
  _ -> ExCast expr coercion} where {
  cancels left right = CoSym left == right || left == CoSym right};
maxLocalUnique :: Program -> Int;
maxLocalUnique program = maximum (0 : concatMap (mapMaybe localUniqueOf . declNames) (programDecls program)) where {
  localUniqueOf name = case nameOrigin name of {
  OriginLocal (Unique unique) -> Just unique;
  OriginTop {} -> Nothing};
  declNames decl = case decl of {
  DeclVal declaration -> Set.toList (exprNames (valBody declaration));
  _ -> []}};
exprNames :: Expr -> Set Name;
exprNames = go where {
  go expr = case expr of {
  ExVar name -> Set.singleton name;
  ExLit {} -> Set.empty;
  ExCoercion {} -> Set.empty;
  ExApp function argument -> go function <> go argument;
  ExTyApp function _ -> go function;
  ExLam binder body -> Set.insert (binderName binder) (go body);
  ExTyLam binder body -> Set.insert (binderName binder) (go body);
  ExCast body _ -> go body;
  ExLet bind body -> Set.insert (binderName (bindBinder bind)) (go (bindRhs bind) <> go body);
  ExRec binds body -> Set.fromList (map (binderName . bindBinder) binds) <> foldMap (go . bindRhs) binds <> go body;
  ExCase scrutinee binder _ alternatives -> Set.insert (binderName binder) (go scrutinee) <> foldMap alternativeNames alternatives;
  ExForeignCall _ _ arguments -> foldMap go arguments};
  alternativeNames alternative = Set.fromList (map binderName (altTypeBinders alternative)) <> Set.fromList (map binderName (altBinders alternative)) <> go (altRhs alternative)}
}
