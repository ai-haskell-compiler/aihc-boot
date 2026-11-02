{-# LANGUAGE OverloadedStrings #-}
module Aihc.Grin.Cps (CpsGrinProgram, CpsGrinError(..), ContinuationFrameKind(..), continuationFrameKindCode, cpsContinuationFrames, cpsContinuationFunctions, cpsFunctionContinuations, cpsGrinProgram, toCpsGrin) where {
import Aihc.Grin.Analysis (freeExprVars, maximumProgramVarUnique);
import Aihc.Grin.Anf (normalizeGrinProgram);
import Aihc.Grin.Syntax;
import Control.Monad.Trans.Class (lift);
import Control.Monad.Trans.State.Strict (StateT, get, modify', put, runStateT);
import Data.Map.Strict (Map);
import Data.Map.Strict qualified as Map;
import Data.Set (Set);
import Data.Set qualified as Set;
import Data.Text qualified as T;
data CpsGrinProgram = CpsGrinProgram { cpsGrinProgram :: !GrinProgram, cpsContinuationFunctions :: !(Set FunctionName), cpsContinuationFrames :: !(Map FunctionName ContinuationFrameKind), cpsFunctionContinuations :: !(Map FunctionName GrinVar) } deriving (Eq, Show, Read);
data ContinuationFrameKind = ContinuationFrameNormal | ContinuationFrameCatch | ContinuationFrameUpdate | ContinuationFrameStop | ContinuationFramePrompt | ContinuationFrameForward deriving (Eq, Ord, Show, Read, Enum, Bounded);
continuationFrameKindCode :: Maybe ContinuationFrameKind -> Int;
continuationFrameKindCode frameKind = case frameKind of {
  Nothing -> 0;
  Just ContinuationFrameNormal -> 1;
  Just ContinuationFrameCatch -> 2;
  Just ContinuationFrameUpdate -> 3;
  Just ContinuationFrameStop -> 5;
  Just ContinuationFramePrompt -> 6;
  Just ContinuationFrameForward -> 7};
data CpsGrinError = CpsGrinAlreadyTransformed !FunctionName | CpsGrinInvalidContinuationParent !FunctionName | CpsGrinForwardedDirectResult !FunctionName deriving (Eq, Show);
data CpsState = CpsState { cpsNextVarUnique :: !Int, cpsUsedFunctionNames :: !(Set FunctionName), cpsGeneratedFunctionsRev :: ![GrinFunction], cpsContinuationFramesState :: !(Map FunctionName ContinuationFrameKind), cpsComputationContinuations :: !(Map FunctionName GrinVar) };
type CpsM = StateT CpsState (Either CpsGrinError);
toCpsGrin :: GrinProgram -> Either CpsGrinError CpsGrinProgram;
toCpsGrin sourceProgram = do {
  (functions, finalState) <- runStateT (mapM transformFunction sourceFunctions) initialState;
  let {
  continuationFrames = cpsContinuationFramesState finalState};
  pure CpsGrinProgram {cpsGrinProgram = program {grinFunctions = functions <> reverse (cpsGeneratedFunctionsRev finalState)}, cpsContinuationFunctions = Map.keysSet continuationFrames, cpsContinuationFrames = continuationFrames, cpsFunctionContinuations = cpsComputationContinuations finalState}} where {
  program = normalizeGrinProgram sourceProgram;
  sourceFunctions = grinFunctions program;
  initialState = CpsState {cpsNextVarUnique = 1 + maximumProgramVarUnique program, cpsUsedFunctionNames = Set.fromList (map grinFunctionName sourceFunctions), cpsGeneratedFunctionsRev = [], cpsContinuationFramesState = Map.empty, cpsComputationContinuations = Map.empty}};
transformFunction :: GrinFunction -> CpsM GrinFunction;
transformFunction function = do {
  continuation <- freshVar "$cps_return" liftedGrinRep;
  let {
  parameters = grinFunctionParameters function;
  bound = Set.fromList (continuation : parameters)};
  body <- transformTail (grinFunctionName function) bound (grinFunctionResultRep function) (GrinVarValue continuation) (grinFunctionBody function);
  modify' $ \state -> state {cpsComputationContinuations = Map.insert (grinFunctionName function) continuation (cpsComputationContinuations state)};
  pure function {grinFunctionParameters = parameters <> [continuation], grinFunctionBody = body}};
transformTail :: FunctionName -> Set GrinVar -> GrinResultRep -> GrinValue -> GrinExpr -> CpsM GrinExpr;
transformTail parent bound resultRep continuation expression = case expression of {
  GrinConstant values -> pure (GrinContinue continuation values);
  GrinBind resultVars (GrinCase scrutinee binder alternatives) body -> do {
  afterCase <- if worthSharing then shareBody else pure body;
  GrinCase scrutinee binder <$> mapM (transformBoundAlternative afterCase) alternatives} where {
  transformBoundAlternative afterCase alternative = do {
  let {
  alternativeBound = bound <> Set.fromList (binder : grinAltBinders alternative)};
  rhs <- transformTail parent alternativeBound resultRep continuation (GrinBind resultVars (grinAltRhs alternative) afterCase);
  pure alternative {grinAltRhs = rhs}};
  shareBody = do {
  joinName <- liftJoinPoint parent resultRep captures resultVars body;
  pure (GrinCall resultRep joinName (map GrinVarValue (captures <> resultVars)))};
  captures = Set.toAscList (freeExprVars body `Set.intersection` bound);
  callSize = 1 + length captures + length resultVars;
  worthSharing = length alternatives > 1 && (length alternatives - 1) * grinExprSize body > length alternatives * callSize};
  GrinBind resultVars valueExpression body | isDirectExpression valueExpression -> do {
  transformedBody <- transformTail parent (bound <> Set.fromList resultVars) resultRep continuation body;
  pure (GrinBind resultVars valueExpression transformedBody)} | otherwise -> do {
  next <- reifyContinuation parent bound resultRep continuation resultVars body;
  let {
  nextVar = reifiedPointer next};
  transformedValue <- transformTail parent (Set.insert nextVar bound) (ResultRep (varsRuntimeRep resultVars)) (GrinVarValue nextVar) valueExpression;
  let {
  slow = GrinBind [nextVar] (GrinStore (reifiedNode next)) transformedValue};
  pure $ case valueExpression of {
  GrinEval _ _ value -> GrinIfWhnf value (GrinCall (ResultRep cpsResultRep) (reifiedEntry next) (reifiedCaptures next <> [value])) slow;
  _ -> slow}};
  GrinStore node -> continuePlaced (GrinStore node);
  GrinEnsureHeap requiredWords roots -> continuePlaced (GrinEnsureHeap requiredWords roots);
  GrinStoreUnchecked {} -> alreadyTransformed;
  GrinStoreRec bindings body -> do {
  let {
  recursiveVars = Set.fromList (map fst bindings)};
  GrinStoreRec bindings <$> transformTail parent (bound <> recursiveVars) resultRep continuation body};
  GrinStoreRecUnchecked {} -> alreadyTransformed;
  GrinUpdate pointer value -> continueDirect (grinValueRuntimeRep value) continuation (GrinUpdate pointer value);
  GrinUpdateBlackhole pointer value -> continueDirect (grinValueRuntimeRep value) continuation (GrinUpdateBlackhole pointer value);
  GrinEval update runtimeRep value -> pure (GrinCpsEval update runtimeRep value continuation);
  GrinIfWhnf {} -> alreadyTransformed;
  GrinCpsEval {} -> alreadyTransformed;
  GrinCall _ functionName arguments -> pure (GrinCall (ResultRep cpsResultRep) functionName (arguments <> [continuation]));
  GrinPrimitiveCall runtimeRep name [tag, action] | name == "prompt#" -> do {
  (promptVar, promptNode) <- makePromptContinuation parent runtimeRep continuation tag;
  evaluatedAction <- freshVar "$cps_prompt_action" (grinValueRuntimeRep action);
  delimitedAction <- transformTail parent (Set.insert promptVar bound) (ResultRep runtimeRep) (GrinVarValue promptVar) (GrinBind [evaluatedAction] (GrinEval EvalUpdate (grinValueRuntimeRep action) action) (GrinApply (ResultRep runtimeRep) (GrinVarValue evaluatedAction) []));
  pure (GrinBind [promptVar] (GrinStore promptNode) delimitedAction)};
  GrinPrimitiveCall runtimeRep name arguments | isControlPrimitive name -> pure (GrinCpsPrimitiveCall runtimeRep name arguments continuation) | otherwise -> continueDirect runtimeRep continuation (GrinPrimitiveCall runtimeRep name arguments);
  GrinCpsPrimitiveCall {} -> alreadyTransformed;
  GrinForward -> pure GrinForward;
  GrinApply runtimeRep function arguments -> pure (GrinCpsApply runtimeRep function arguments continuation);
  GrinCpsApply {} -> alreadyTransformed;
  GrinContinue {} -> alreadyTransformed;
  GrinCpsRaise {} -> alreadyTransformed;
  GrinHalt {} -> alreadyTransformed;
  GrinExit status -> pure (GrinExit status);
  GrinCase scrutinee binder alternatives -> GrinCase scrutinee binder <$> mapM transformAlternative alternatives where {
  transformAlternative alternative = do {
  let {
  alternativeBound = bound <> Set.fromList (binder : grinAltBinders alternative)};
  rhs <- transformTail parent alternativeBound resultRep continuation (grinAltRhs alternative);
  pure alternative {grinAltRhs = rhs}}};
  GrinThrow exception -> pure (GrinCpsRaise exception continuation);
  GrinCatch runtimeRep action handler state -> do {
  (catchVar, catchNode) <- makeCatchContinuation parent runtimeRep continuation handler;
  evaluatedAction <- freshVar "$cps_catch_action" (grinValueRuntimeRep action);
  protectedAction <- transformTail parent (Set.insert catchVar bound) (ResultRep runtimeRep) (GrinVarValue catchVar) (GrinBind [evaluatedAction] (GrinEval EvalUpdate (grinValueRuntimeRep action) action) (GrinApply (ResultRep runtimeRep) (GrinVarValue evaluatedAction) state));
  pure (GrinBind [catchVar] (GrinStore catchNode) protectedAction)};
  GrinForeignCallExpr foreignCall arguments -> continuePlaced (GrinForeignCallExpr foreignCall arguments);
  GrinFetch tag value -> continuePlaced (GrinFetch tag value)} where {
  alreadyTransformed = lift (Left (CpsGrinAlreadyTransformed parent));
  continuePlaced directExpression = case resultRep of {
  ResultRep runtimeRep -> continueDirect runtimeRep continuation directExpression;
  ResultForwarded -> lift (Left (CpsGrinForwardedDirectResult parent))}};
liftJoinPoint :: FunctionName -> GrinResultRep -> [GrinVar] -> [GrinVar] -> GrinExpr -> CpsM FunctionName;
liftJoinPoint parent resultRep captures resultVars body = do {
  joinName <- freshFunctionName (unFunctionName parent <> "_join");
  joinContinuation <- freshVar "$cps_join" liftedGrinRep;
  let {
  parameters = captures <> resultVars <> [joinContinuation]};
  transformedBody <- transformTail parent (Set.fromList parameters) resultRep (GrinVarValue joinContinuation) body;
  addComputationFunction GrinFunction {grinFunctionName = joinName, grinFunctionParameters = parameters, grinFunctionResultRep = resultRep, grinFunctionBody = transformedBody} joinContinuation;
  pure joinName};
grinExprSize :: GrinExpr -> Int;
grinExprSize expression = case expression of {
  GrinBind _ valueExpression body -> 1 + grinExprSize valueExpression + grinExprSize body;
  GrinIfWhnf _ ready slow -> 1 + grinExprSize ready + grinExprSize slow;
  GrinCase _ _ alternatives -> 1 + sum [1 + grinExprSize (grinAltRhs alternative) | alternative <- alternatives];
  GrinStoreRec _ body -> 1 + grinExprSize body;
  GrinStoreRecUnchecked _ body -> 1 + grinExprSize body;
  _ -> 1};
reifyContinuation :: FunctionName -> Set GrinVar -> GrinResultRep -> GrinValue -> [GrinVar] -> GrinExpr -> CpsM ReifiedContinuation;
reifyContinuation parent bound resultRep outerContinuation resultVars body = do {
  transformedBody <- transformTail parent (bound <> Set.fromList resultVars) resultRep outerContinuation body;
  continuationName <- freshContinuationName parent;
  pointer <- freshVar "$cps_continuation" liftedGrinRep;
  parentContinuation <- case outerContinuation of {
  GrinVarValue var -> pure var;
  GrinGlobalValue {} -> lift (Left (CpsGrinInvalidContinuationParent parent));
  GrinLitValue {} -> lift (Left (CpsGrinInvalidContinuationParent parent))};
  let {
  freeCaptures = freeExprVars transformedBody `Set.intersection` bound;
  captures = parentContinuation : Set.toAscList (Set.delete parentContinuation freeCaptures);
  continuationFunction = GrinFunction {grinFunctionName = continuationName, grinFunctionParameters = captures <> resultVars, grinFunctionResultRep = resultRep, grinFunctionBody = transformedBody};
  continuationNode = GrinNode (GrinClosure continuationName [map grinVarRuntimeRep resultVars]) (map GrinVarValue captures)};
  let {
  frameKind = case forwardedResultUses transformedBody of {
  Just _ -> ContinuationFrameForward;
  Nothing -> ContinuationFrameNormal}};
  addContinuationFunction frameKind continuationFunction;
  pure (ReifiedContinuation pointer continuationName (map GrinVarValue captures) continuationNode)};
data ReifiedContinuation = ReifiedContinuation { reifiedPointer :: !GrinVar, reifiedEntry :: !FunctionName, reifiedCaptures :: ![GrinValue], reifiedNode :: !GrinNode };
continueDirect :: GrinRep -> GrinValue -> GrinExpr -> CpsM GrinExpr;
continueDirect runtimeRep continuation directExpression = do {
  resultVars <- mapM (freshVar "$cps_result") (runtimeRepComponents runtimeRep);
  pure (GrinBind resultVars directExpression (GrinContinue continuation (map GrinVarValue resultVars)))};
makeCatchContinuation :: FunctionName -> GrinRep -> GrinValue -> GrinValue -> CpsM (GrinVar, GrinNode);
makeCatchContinuation parent resultRep outerContinuation handler = do {
  parentContinuation <- case outerContinuation of {
  GrinVarValue var -> pure var;
  GrinGlobalValue {} -> lift (Left (CpsGrinInvalidContinuationParent parent));
  GrinLitValue {} -> lift (Left (CpsGrinInvalidContinuationParent parent))};
  catchName <- freshContinuationName parent;
  pointer <- freshVar "$cps_catch" liftedGrinRep;
  capturedHandler <- freshVar "$cps_handler" (grinValueRuntimeRep handler);
  resultVars <- mapM (freshVar "$cps_catch_result") (runtimeRepComponents resultRep);
  let {
  catchFunction = GrinFunction {grinFunctionName = catchName, grinFunctionParameters = parentContinuation : capturedHandler : resultVars, grinFunctionResultRep = ResultRep resultRep, grinFunctionBody = GrinContinue (GrinVarValue parentContinuation) (map GrinVarValue resultVars)};
  catchNode = GrinNode (GrinClosure catchName [runtimeRepComponents resultRep]) [outerContinuation, handler]};
  addContinuationFunction ContinuationFrameCatch catchFunction;
  pure (pointer, catchNode)};
makePromptContinuation :: FunctionName -> GrinRep -> GrinValue -> GrinValue -> CpsM (GrinVar, GrinNode);
makePromptContinuation parent resultRep outerContinuation tag = do {
  parentContinuation <- case outerContinuation of {
  GrinVarValue var -> pure var;
  GrinGlobalValue {} -> lift (Left (CpsGrinInvalidContinuationParent parent));
  GrinLitValue {} -> lift (Left (CpsGrinInvalidContinuationParent parent))};
  promptName <- freshContinuationName parent;
  pointer <- freshVar "$cps_prompt" liftedGrinRep;
  capturedTag <- freshVar "$cps_prompt_tag" (grinValueRuntimeRep tag);
  resultVars <- mapM (freshVar "$cps_prompt_result") (runtimeRepComponents resultRep);
  let {
  promptFunction = GrinFunction {grinFunctionName = promptName, grinFunctionParameters = parentContinuation : capturedTag : resultVars, grinFunctionResultRep = ResultRep resultRep, grinFunctionBody = GrinContinue (GrinVarValue parentContinuation) (map GrinVarValue resultVars)};
  promptNode = GrinNode (GrinClosure promptName [runtimeRepComponents resultRep]) [outerContinuation, tag]};
  addContinuationFunction ContinuationFramePrompt promptFunction;
  pure (pointer, promptNode)};
cpsResultRep :: GrinRep;
cpsResultRep = TupleRep [];
isDirectExpression :: GrinExpr -> Bool;
isDirectExpression expression = case expression of {
  GrinConstant {} -> True;
  GrinStore {} -> True;
  GrinUpdate {} -> True;
  GrinUpdateBlackhole {} -> True;
  GrinPrimitiveCall _ name _ -> not (isControlPrimitive name);
  GrinCpsPrimitiveCall {} -> False;
  GrinForeignCallExpr {} -> True;
  GrinFetch {} -> True;
  _ -> False};
freshContinuationName :: FunctionName -> CpsM FunctionName;
freshContinuationName parent = freshFunctionName (unFunctionName parent <> "_cont");
freshFunctionName :: T.Text -> CpsM FunctionName;
freshFunctionName base = do {
  state <- get;
  let {
  candidate = unusedFunctionName base (cpsUsedFunctionNames state)};
  put state {cpsUsedFunctionNames = Set.insert candidate (cpsUsedFunctionNames state)};
  pure candidate};
freshVar :: T.Text -> GrinRep -> CpsM GrinVar;
freshVar name runtimeRep = do {
  state <- get;
  let {
  unique = cpsNextVarUnique state};
  put state {cpsNextVarUnique = unique + 1};
  pure (GrinVar name unique runtimeRep)};
addComputationFunction :: GrinFunction -> GrinVar -> CpsM ();
addComputationFunction function continuation = modify' $ \state -> state {cpsGeneratedFunctionsRev = function : cpsGeneratedFunctionsRev state, cpsComputationContinuations = Map.insert (grinFunctionName function) continuation (cpsComputationContinuations state)};
addContinuationFunction :: ContinuationFrameKind -> GrinFunction -> CpsM ();
addContinuationFunction frameKind function = do {
  modify' $ \state -> state {cpsGeneratedFunctionsRev = function : cpsGeneratedFunctionsRev state};
  modify' $ \state -> state {cpsContinuationFramesState = Map.insert (grinFunctionName function) frameKind (cpsContinuationFramesState state)}};
varsRuntimeRep :: [GrinVar] -> GrinRep;
varsRuntimeRep vars = case map grinVarRuntimeRep vars of {
  [runtimeRep] -> runtimeRep;
  runtimeReps -> TupleRep runtimeReps};
isControlPrimitive :: T.Text -> Bool;
isControlPrimitive name = name `elem` ["awaitIO#", "fork#", "newMVar#", "putMVar#", "readMVar#", "takeMVar#", "yield#", "prompt#", "aihcControl0#", "aihcResume#"]
}
