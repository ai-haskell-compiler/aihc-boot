{-# LANGUAGE LambdaCase #-}
module Aihc.Grin.PointsTo (PointsTo, PointsToStats(..), PointsToRewrites(..), analyzePointsTo, pointsToStats, rewriteWithPointsTo, totalPointsToRewrites) where {
import Aihc.Grin.Analysis (maximumProgramVarUnique);
import Aihc.Grin.Syntax;
import Control.Monad (forM, forM_, guard, unless, when, zipWithM_);
import Control.Monad.ST (ST, runST);
import Control.Monad.Trans.State.Strict (State, get, modify', put, runState);
import Data.Foldable (for_);
import Data.IntSet (IntSet);
import Data.IntSet qualified as IntSet;
import Data.List qualified as List;
import Data.Map.Strict (Map);
import Data.Map.Strict qualified as Map;
import Data.Maybe (catMaybes, fromMaybe, isJust);
import Data.STRef (STRef, modifySTRef', newSTRef, readSTRef, writeSTRef);
import Data.Set qualified as Set;
import Data.Text (Text);
import Data.Vector qualified as V;
import Data.Vector.Mutable qualified as MV;
import Data.Vector.Unboxed.Mutable qualified as MU;
data PointsTo = PointsTo { resultSets :: !(V.Vector IntSet), resultLocationNodes :: !(V.Vector (Map GrinNodeTag [Int])), resultVariables :: !(Map FunctionName (Map GrinVar Int)), resultSingleEntry :: !IntSet, resultStats :: !PointsToStats };
data PointsToStats = PointsToStats { statsIterations :: !Int, statsVariables :: !Int, statsSetNodes :: !Int, statsLocations :: !Int, statsSharedLocations :: !Int, statsSingleEntryThunks :: !Int } deriving (Eq, Show);
data PointsToRewrites = PointsToRewrites { rewritesDeadAlternatives :: !Int, rewritesEvaluatedEvals :: !Int, rewritesDirectCalls :: !Int, rewritesSingleEntryEvals :: !Int } deriving (Eq, Show);
totalPointsToRewrites :: PointsToRewrites -> Int;
totalPointsToRewrites rewrites = rewritesDeadAlternatives rewrites + rewritesEvaluatedEvals rewrites + rewritesDirectCalls rewrites + rewritesSingleEntryEvals rewrites;
pointsToStats :: PointsTo -> PointsToStats;
pointsToStats = resultStats;
data PointerKind = NotPointer | LiftedPointer | UnliftedPointer deriving (Eq);
pointerKind :: GrinRep -> PointerKind;
pointerKind runtimeRep = case runtimeRep of {
  BoxedRep Lifted -> LiftedPointer;
  BoxedRep Unlifted -> UnliftedPointer;
  _ -> NotPointer};
data Slot = Slot { slotNode :: !Int, slotKind :: !PointerKind };
data Results = ResultSlots ![Slot] | ResultMerged !Int;
data FunctionInfo = FunctionInfo { functionParameters :: ![Slot], functionResults :: !Results };
unknownLocation :: Int;
unknownLocation = 0;
unknownValueLocation :: Int;
unknownValueLocation = 1;
isUnknownLocation :: Int -> Bool;
isUnknownLocation location = location == unknownLocation || location == unknownValueLocation;
escapeNode :: Int;
escapeNode = 0;
data Trigger = TriggerEval !Int | TriggerCase !(Map Text [Slot]) ![Slot] | TriggerApply !Int ![Maybe Int] !Results | TriggerFetch !GrinNodeTag !Results | TriggerEscape;
data Solver s = Solver { solverNodeCount :: !(STRef s Int), solverSets :: !(STRef s (MV.MVector s IntSet)), solverDeltas :: !(STRef s (MV.MVector s IntSet)), solverSuccessors :: !(STRef s (MV.MVector s IntSet)), solverTriggers :: !(STRef s (MV.MVector s [Trigger])), solverQueued :: !(STRef s (MU.MVector s Bool)), solverWorklist :: !(STRef s [Int]), solverIterations :: !(STRef s Int), solverLocationCount :: !(STRef s Int), solverLocationNodes :: !(STRef s (MV.MVector s (Map GrinNodeTag [Int]))), solverLocationReaders :: !(STRef s (MV.MVector s [Trigger])), solverEvalNodes :: !(STRef s (Map FunctionName Int)), solverGlobalNodes :: !(STRef s (Map Text Int)), solverStatics :: !(STRef s (Map Text Int)), solverFunctions :: !(STRef s (Map FunctionName FunctionInfo)) };
analyzePointsTo :: GrinProgram -> Maybe PointsTo;
analyzePointsTo program | all (supportedExpr . grinFunctionBody) (grinFunctions program) = Just (runAnalysis program) | otherwise = Nothing;
supportedExpr :: GrinExpr -> Bool;
supportedExpr expression = case expression of {
  GrinBind _ valueExpression body -> supportedExpr valueExpression && supportedExpr body;
  GrinStoreRec _ body -> supportedExpr body;
  GrinCase _ _ alternatives -> all (supportedExpr . grinAltRhs) alternatives;
  GrinConstant {} -> True;
  GrinStore {} -> True;
  GrinEval {} -> True;
  GrinFetch {} -> True;
  GrinCall {} -> True;
  GrinPrimitiveCall {} -> True;
  GrinApply {} -> True;
  GrinForward -> True;
  GrinExit {} -> True;
  GrinThrow {} -> True;
  GrinCatch {} -> True;
  GrinForeignCallExpr {} -> True;
  GrinEnsureHeap {} -> False;
  GrinStoreUnchecked {} -> False;
  GrinStoreRecUnchecked {} -> False;
  GrinUpdate {} -> False;
  GrinUpdateBlackhole {} -> False;
  GrinCpsEval {} -> False;
  GrinIfWhnf {} -> False;
  GrinCpsPrimitiveCall {} -> False;
  GrinCpsApply {} -> False;
  GrinContinue {} -> False;
  GrinCpsRaise {} -> False;
  GrinHalt {} -> False};
runAnalysis :: GrinProgram -> PointsTo;
runAnalysis program = runST $ do {
  solver <- newSolver;
  escape <- newNode solver;
  when (escape /= escapeNode) (error "points-to: the escape set node must come first");
  addTrigger solver escapeNode TriggerEscape;
  unknown <- newLocation solver;
  unknownValue <- newLocation solver;
  when (unknown /= unknownLocation || unknownValue /= unknownValueLocation) (error "points-to: the unknown locations must come first");
  functionVars <- fmap Map.fromList $ forM (grinFunctions program) $ \function -> do {
  parameters <- mapM (newSlot solver . grinVarRuntimeRep) (grinFunctionParameters function);
  results <- case resultRepComponents (grinFunctionResultRep function) of {
  Just components -> ResultSlots <$> mapM (newSlot solver) components;
  Nothing -> ResultMerged <$> newNode solver};
  modifySTRef' (solverFunctions solver) (Map.insert (grinFunctionName function) (FunctionInfo parameters results));
  pure (grinFunctionName function, Map.fromList (zip (grinFunctionParameters function) (map slotNode parameters)))};
  constructorStatics <- forM [c | c <- grinConstructors program, null (grinConstructorLayouts c)] $ \constructor -> do {
  location <- newLocation solver;
  _ <- addNode solver location (GrinConstructor (grinConstructorName constructor) 0) 0;
  pure (grinConstructorName constructor, location)};
  globalStatics <- forM (grinGlobals program) $ \global -> do {
  location <- newLocation solver;
  pure (grinGlobalName global, location)};
  writeSTRef (solverStatics solver) (Map.fromList (constructorStatics <> globalStatics));
  forM_ (zip (grinGlobals program) (map snd globalStatics)) $ \(global, location) -> do {
  let {
  GrinNode tag fields = grinGlobalNode global};
  fieldNodes <- addNode solver location tag (length fields);
  zipWithM_ (\field fieldNode -> flowValue solver Map.empty field (Slot fieldNode (pointerKind (grinValueRuntimeRep field)))) fields fieldNodes;
  connectThunk solver tag fieldNodes;
  when (grinGlobalVis global == GrinPub) (addLocation solver escapeNode location)};
  variables <- fmap Map.fromList $ forM (grinFunctions program) $ \function -> do {
  let {
  name = grinFunctionName function};
  info <- lookupFunction solver name;
  varsRef <- newSTRef (Map.findWithDefault Map.empty name functionVars);
  for_ info $ \known -> generateExpr solver varsRef (functionResults known) (grinFunctionBody function);
  (,) name <$> readSTRef varsRef};
  solve solver;
  nodeCount <- readSTRef (solverNodeCount solver);
  locationCount <- readSTRef (solverLocationCount solver);
  sets <- V.freeze . MV.take nodeCount =<< readSTRef (solverSets solver);
  locationNodes <- V.freeze . MV.take locationCount =<< readSTRef (solverLocationNodes solver);
  iterations <- readSTRef (solverIterations solver);
  functions <- readSTRef (solverFunctions solver);
  evalNodes <- readSTRef (solverEvalNodes solver);
  statics <- readSTRef (solverStatics solver);
  let {
  usesByFunction = Map.fromList [(grinFunctionName function, useCounts (grinFunctionBody function)) | function <- grinFunctions program];
  shared = sharedLocations sets locationNodes functions evalNodes variables usesByFunction (IntSet.fromList (Map.elems statics));
  captures = any (capturesContinuation . grinFunctionBody) (grinFunctions program);
  singleEntry | captures = IntSet.empty | otherwise = singleEntryThunks sets locationNodes functions shared locationCount};
  pure PointsTo {resultSets = sets, resultLocationNodes = locationNodes, resultVariables = variables, resultSingleEntry = singleEntry, resultStats = PointsToStats {statsIterations = iterations, statsVariables = sum (map Map.size (Map.elems variables)), statsSetNodes = nodeCount, statsLocations = locationCount, statsSharedLocations = IntSet.size shared, statsSingleEntryThunks = IntSet.size singleEntry}}};
newSolver :: ST s (Solver s);
newSolver = do {
  let {
  capacity = 1024};
  Solver <$> newSTRef 0 <*> (newSTRef =<< MV.replicate capacity IntSet.empty) <*> (newSTRef =<< MV.replicate capacity IntSet.empty) <*> (newSTRef =<< MV.replicate capacity IntSet.empty) <*> (newSTRef =<< MV.replicate capacity []) <*> (newSTRef =<< MU.replicate capacity False) <*> newSTRef [] <*> newSTRef 0 <*> newSTRef 0 <*> (newSTRef =<< MV.replicate capacity Map.empty) <*> (newSTRef =<< MV.replicate capacity []) <*> newSTRef Map.empty <*> newSTRef Map.empty <*> newSTRef Map.empty <*> newSTRef Map.empty};
ensureSize :: STRef s (MV.MVector s a) -> a -> Int -> ST s ();
ensureSize ref def size = do {
  vector <- readSTRef ref;
  let {
  capacity = MV.length vector};
  when (size > capacity) $ do {
  let {
  capacity' = max size (2 * capacity)};
  grown <- MV.grow vector (capacity' - capacity);
  MV.set (MV.slice capacity (capacity' - capacity) grown) def;
  writeSTRef ref grown}};
ensureUnboxedSize :: (MU.Unbox a) => STRef s (MU.MVector s a) -> a -> Int -> ST s ();
ensureUnboxedSize ref def size = do {
  vector <- readSTRef ref;
  let {
  capacity = MU.length vector};
  when (size > capacity) $ do {
  let {
  capacity' = max size (2 * capacity)};
  grown <- MU.grow vector (capacity' - capacity);
  MU.set (MU.slice capacity (capacity' - capacity) grown) def;
  writeSTRef ref grown}};
newNode :: Solver s -> ST s Int;
newNode solver = do {
  node <- readSTRef (solverNodeCount solver);
  writeSTRef (solverNodeCount solver) (node + 1);
  ensureSize (solverSets solver) IntSet.empty (node + 1);
  ensureSize (solverDeltas solver) IntSet.empty (node + 1);
  ensureSize (solverSuccessors solver) IntSet.empty (node + 1);
  ensureSize (solverTriggers solver) [] (node + 1);
  ensureUnboxedSize (solverQueued solver) False (node + 1);
  pure node};
newSlot :: Solver s -> GrinRep -> ST s Slot;
newSlot solver runtimeRep = do {
  node <- newNode solver;
  pure (Slot node (pointerKind runtimeRep))};
newLocation :: Solver s -> ST s Int;
newLocation solver = do {
  location <- readSTRef (solverLocationCount solver);
  writeSTRef (solverLocationCount solver) (location + 1);
  ensureSize (solverLocationNodes solver) Map.empty (location + 1);
  ensureSize (solverLocationReaders solver) [] (location + 1);
  pure location};
readAt :: STRef s (MV.MVector s a) -> Int -> ST s a;
readAt ref index = do {
  vector <- readSTRef ref;
  MV.read vector index};
writeAt :: STRef s (MV.MVector s a) -> Int -> a -> ST s ();
writeAt ref index value = do {
  vector <- readSTRef ref;
  MV.write vector index value};
modifyAt :: STRef s (MV.MVector s a) -> Int -> (a -> a) -> ST s ();
modifyAt ref index f = do {
  vector <- readSTRef ref;
  MV.modify vector f index};
lookupFunction :: Solver s -> FunctionName -> ST s (Maybe FunctionInfo);
lookupFunction solver name = Map.lookup name <$> readSTRef (solverFunctions solver);
addLocations :: Solver s -> Int -> IntSet -> ST s ();
addLocations solver node locations = do {
  current <- readAt (solverSets solver) node;
  let {
  new = locations `IntSet.difference` current};
  unless (IntSet.null new) $ do {
  modifyAt (solverDeltas solver) node (<> new);
  queued <- readSTRef (solverQueued solver);
  already <- MU.read queued node;
  unless already $ do {
  MU.write queued node True;
  modifySTRef' (solverWorklist solver) (node :)}}};
addLocation :: Solver s -> Int -> Int -> ST s ();
addLocation solver node location = addLocations solver node (IntSet.singleton location);
addEdge :: Solver s -> Int -> Int -> ST s ();
addEdge solver from to = unless (from == to) $ do {
  successors <- readAt (solverSuccessors solver) from;
  unless (IntSet.member to successors) $ do {
  writeAt (solverSuccessors solver) from (IntSet.insert to successors);
  current <- readAt (solverSets solver) from;
  unless (IntSet.null current) (addLocations solver to current)}};
edgeToSlot :: Solver s -> Int -> Slot -> ST s ();
edgeToSlot solver from slot = unless (slotKind slot == NotPointer) (addEdge solver from (slotNode slot));
addTrigger :: Solver s -> Int -> Trigger -> ST s ();
addTrigger solver node trigger = do {
  modifyAt (solverTriggers solver) node (trigger :);
  current <- readAt (solverSets solver) node;
  forM_ (IntSet.toList current) (seeLocation solver trigger)};
addNode :: Solver s -> Int -> GrinNodeTag -> Int -> ST s [Int];
addNode solver location tag fieldCount = do {
  nodes <- readAt (solverLocationNodes solver) location;
  case Map.lookup tag nodes of {
  Just fields -> pure fields;
  Nothing -> do {
  fields <- mapM (const (newNode solver)) [1 .. fieldCount];
  writeAt (solverLocationNodes solver) location (Map.insert tag fields nodes);
  readers <- readAt (solverLocationReaders solver) location;
  forM_ readers $ \trigger -> fireNode solver trigger location tag fields;
  pure fields}}};
solve :: Solver s -> ST s ();
solve solver = do {
  worklist <- readSTRef (solverWorklist solver);
  case worklist of {
  [] -> pure ();
  node : rest -> do {
  writeSTRef (solverWorklist solver) rest;
  queued <- readSTRef (solverQueued solver);
  MU.write queued node False;
  delta <- readAt (solverDeltas solver) node;
  writeAt (solverDeltas solver) node IntSet.empty;
  current <- readAt (solverSets solver) node;
  let {
  new = delta `IntSet.difference` current};
  unless (IntSet.null new) $ do {
  modifySTRef' (solverIterations solver) (+ 1);
  writeAt (solverSets solver) node (current <> new);
  successors <- readAt (solverSuccessors solver) node;
  forM_ (IntSet.toList successors) $ \successor -> addLocations solver successor new;
  triggers <- readAt (solverTriggers solver) node;
  forM_ triggers $ \trigger -> forM_ (IntSet.toList new) (seeLocation solver trigger)};
  solve solver}}};
seeLocation :: Solver s -> Trigger -> Int -> ST s ();
seeLocation solver trigger location | isUnknownLocation location = fireUnknown solver trigger | otherwise = do {
  modifyAt (solverLocationReaders solver) location (trigger :);
  nodes <- readAt (solverLocationNodes solver) location;
  forM_ (Map.toList nodes) (uncurry (fireNode solver trigger location))};
fireNode :: Solver s -> Trigger -> Int -> GrinNodeTag -> [Int] -> ST s ();
fireNode solver trigger location tag fields = case trigger of {
  TriggerEval target -> case tag of {
  GrinThunk functionName -> do {
  result <- evalNode solver functionName;
  addEdge solver result target};
  _ -> addLocation solver target location};
  TriggerCase byConstructor _ -> case tag of {
  GrinConstructor name 0 | Just binders <- Map.lookup name byConstructor -> zipWithM_ (edgeToSlot solver) fields binders;
  _ -> pure ()};
  TriggerApply site arguments results -> applyNode solver site arguments results tag fields;
  TriggerFetch expected results -> when (tag == expected) $ case results of {
  ResultSlots slots -> zipWithM_ (edgeToSlot solver) fields slots;
  ResultMerged merged -> forM_ fields (\field -> addEdge solver field merged)};
  TriggerEscape -> escapeNode' solver tag fields};
fireUnknown :: Solver s -> Trigger -> ST s ();
fireUnknown solver trigger = case trigger of {
  TriggerEval target -> addLocation solver target unknownValueLocation;
  TriggerCase _ binders -> mapM_ (unknownSlot solver) binders;
  TriggerApply _ arguments results -> do {
  forM_ (catMaybes arguments) $ \argument -> addEdge solver argument escapeNode;
  unknownResults solver results};
  TriggerFetch _ results -> unknownResults solver results;
  TriggerEscape -> pure ()};
applyNode :: Solver s -> Int -> [Maybe Int] -> Results -> GrinNodeTag -> [Int] -> ST s ();
applyNode solver site arguments results tag fields = case tag of {
  GrinClosure functionName (_ : remaining) -> do {
  known <- lookupFunction solver functionName;
  case known of {
  Nothing -> do {
  forM_ (fields <> catMaybes arguments) $ \node -> addEdge solver node escapeNode;
  unknownResults solver results};
  Just info | null remaining -> do {
  zipWithM_ (\value slot -> for_ value (\node -> edgeToSlot solver node slot)) (map Just fields <> arguments) (functionParameters info);
  flowResults solver (functionResults info) results} | otherwise -> grow (GrinClosure functionName remaining)}};
  GrinConstructor name remaining | remaining >= 1 -> grow (GrinConstructor name (remaining - 1));
  _ -> pure ()} where {
  grow grown = do {
  grownFields <- addNode solver site grown (length fields + length arguments);
  zipWithM_ (addEdge solver) fields grownFields;
  zipWithM_ (\value field -> for_ value (\node -> addEdge solver node field)) arguments (drop (length fields) grownFields);
  addLocationResult solver results site}};
escapeNode' :: Solver s -> GrinNodeTag -> [Int] -> ST s ();
escapeNode' solver tag fields = do {
  forM_ fields $ \field -> addEdge solver field escapeNode;
  case tag of {
  GrinClosure functionName _ -> do {
  known <- lookupFunction solver functionName;
  for_ known $ \info -> do {
  let {
  parameters = functionParameters info};
  zipWithM_ (edgeToSlot solver) fields parameters;
  mapM_ (unknownSlot solver) (drop (length fields) parameters);
  escapeResults solver (functionResults info)}};
  GrinThunk functionName -> do {
  known <- lookupFunction solver functionName;
  for_ known (escapeResults solver . functionResults)};
  GrinConstructor {} -> pure ()}};
evalNode :: Solver s -> FunctionName -> ST s Int;
evalNode solver functionName = do {
  existing <- Map.lookup functionName <$> readSTRef (solverEvalNodes solver);
  case existing of {
  Just node -> pure node;
  Nothing -> do {
  node <- newNode solver;
  modifySTRef' (solverEvalNodes solver) (Map.insert functionName node);
  known <- lookupFunction solver functionName;
  case known of {
  Nothing -> addLocation solver node unknownValueLocation;
  Just info -> case functionResults info of {
  ResultSlots slots -> forM_ [slot | slot <- slots, slotKind slot /= NotPointer] $ \slot -> addTrigger solver (slotNode slot) (TriggerEval node);
  ResultMerged merged -> addTrigger solver merged (TriggerEval node)}};
  pure node}}};
unknownSlot :: Solver s -> Slot -> ST s ();
unknownSlot solver slot = case slotKind slot of {
  NotPointer -> pure ();
  LiftedPointer -> addLocation solver (slotNode slot) unknownLocation;
  UnliftedPointer -> addLocation solver (slotNode slot) unknownValueLocation};
unknownResults :: Solver s -> Results -> ST s ();
unknownResults solver results = case results of {
  ResultSlots slots -> mapM_ (unknownSlot solver) slots;
  ResultMerged merged -> addLocation solver merged unknownLocation};
escapeResults :: Solver s -> Results -> ST s ();
escapeResults solver results = case results of {
  ResultSlots slots -> forM_ slots $ \slot -> unless (slotKind slot == NotPointer) (addEdge solver (slotNode slot) escapeNode);
  ResultMerged merged -> addEdge solver merged escapeNode};
flowResults :: Solver s -> Results -> Results -> ST s ();
flowResults solver source target = case (source, target) of {
  (ResultSlots sources, ResultSlots targets) -> zipWithM_ (\from to -> unless (slotKind from == NotPointer) (edgeToSlot solver (slotNode from) to)) sources targets;
  (ResultSlots sources, ResultMerged merged) -> forM_ sources $ \from -> unless (slotKind from == NotPointer) (addEdge solver (slotNode from) merged);
  (ResultMerged merged, ResultSlots targets) -> forM_ targets (edgeToSlot solver merged);
  (ResultMerged from, ResultMerged to) -> addEdge solver from to};
addLocationResult :: Solver s -> Results -> Int -> ST s ();
addLocationResult solver results location = case results of {
  ResultSlots slots -> case [slot | slot <- slots, slotKind slot /= NotPointer] of {
  slot : _ -> addLocation solver (slotNode slot) location;
  [] -> pure ()};
  ResultMerged merged -> addLocation solver merged location};
evalTarget :: Solver s -> Results -> ST s Int;
evalTarget solver results = case results of {
  ResultMerged merged -> pure merged;
  ResultSlots slots -> case [slot | slot <- slots, slotKind slot /= NotPointer] of {
  [slot] -> pure (slotNode slot);
  pointers -> do {
  node <- newNode solver;
  forM_ pointers (edgeToSlot solver node);
  pure node}}};
valueNode :: Solver s -> Map GrinVar Int -> GrinValue -> ST s (Maybe Int);
valueNode solver vars value = case value of {
  GrinVarValue var -> case Map.lookup var vars of {
  Just node -> pure (Just node);
  Nothing -> Just <$> globalNode solver (grinVarName var)};
  GrinGlobalValue name -> Just <$> globalNode solver name;
  GrinLitValue {} -> pure Nothing};
globalNode :: Solver s -> Text -> ST s Int;
globalNode solver name = do {
  existing <- Map.lookup name <$> readSTRef (solverGlobalNodes solver);
  case existing of {
  Just node -> pure node;
  Nothing -> do {
  node <- newNode solver;
  modifySTRef' (solverGlobalNodes solver) (Map.insert name node);
  statics <- readSTRef (solverStatics solver);
  addLocation solver node (Map.findWithDefault unknownLocation name statics);
  pure node}}};
flowValue :: Solver s -> Map GrinVar Int -> GrinValue -> Slot -> ST s ();
flowValue solver vars value slot = unless (slotKind slot == NotPointer) $ do {
  source <- valueNode solver vars value;
  for_ source $ \node -> addEdge solver node (slotNode slot)};
escapeValue :: Solver s -> Map GrinVar Int -> GrinValue -> ST s ();
escapeValue solver vars value = when (pointerKind (grinValueRuntimeRep value) /= NotPointer) $ do {
  source <- valueNode solver vars value;
  for_ source $ \node -> addEdge solver node escapeNode};
connectThunk :: Solver s -> GrinNodeTag -> [Int] -> ST s ();
connectThunk solver tag fields = case tag of {
  GrinThunk functionName -> do {
  known <- lookupFunction solver functionName;
  case known of {
  Just info -> zipWithM_ (edgeToSlot solver) fields (functionParameters info);
  Nothing -> forM_ fields $ \field -> addEdge solver field escapeNode}};
  _ -> pure ()};
generateExpr :: Solver s -> STRef s (Map GrinVar Int) -> Results -> GrinExpr -> ST s ();
generateExpr solver varsRef results expression = case expression of {
  GrinConstant values -> do {
  vars <- readSTRef varsRef;
  case results of {
  ResultSlots slots -> zipWithM_ (flowValue solver vars) values slots;
  ResultMerged merged -> forM_ values $ \value -> flowValue solver vars value (Slot merged LiftedPointer)}};
  GrinBind [] valueExpression body | isJust (forwardedResultUses body) -> generateExpr solver varsRef results valueExpression;
  GrinBind vars valueExpression body -> do {
  slots <- mapM bindVar vars;
  generateExpr solver varsRef (ResultSlots slots) valueExpression;
  generateExpr solver varsRef results body};
  GrinStore node -> do {
  location <- storeNode node;
  addLocationResult solver results location};
  GrinStoreRec bindings body -> do {
  slots <- mapM (bindVar . fst) bindings;
  forM_ (zip slots bindings) $ \(slot, (_, node)) -> do {
  location <- storeNode node;
  addLocation solver (slotNode slot) location};
  generateExpr solver varsRef results body};
  GrinEval _ _ value -> do {
  target <- evalTarget solver results;
  withValue value (\node -> addTrigger solver node (TriggerEval target))};
  GrinFetch tag value -> withValue value (\node -> addTrigger solver node (TriggerFetch tag results));
  GrinCall _ functionName arguments -> do {
  vars <- readSTRef varsRef;
  known <- lookupFunction solver functionName;
  case known of {
  Just info -> do {
  zipWithM_ (flowValue solver vars) arguments (functionParameters info);
  flowResults solver (functionResults info) results};
  Nothing -> do {
  mapM_ (escapeValue solver vars) arguments;
  unknownResults solver results}}};
  GrinPrimitiveCall _ _ arguments -> unknownCall arguments;
  GrinApply _ function arguments -> do {
  vars <- readSTRef varsRef;
  site <- newLocation solver;
  argumentNodes <- mapM (valueNode solver vars) arguments;
  withValue function (\node -> addTrigger solver node (TriggerApply site argumentNodes results))};
  GrinForward -> pure ();
  GrinExit {} -> pure ();
  GrinCase scrutinee binder alternatives -> do {
  vars <- readSTRef varsRef;
  binderSlot <- bindVar binder;
  flowValue solver vars scrutinee binderSlot;
  alternativeSlots <- forM alternatives (mapM bindVar . grinAltBinders);
  forM_ (zip alternatives alternativeSlots) $ \(alternative, slots) -> when (grinAltCon alternative == GrinDefaultAlt) $ mapM_ (flowValue solver vars scrutinee) slots;
  when (pointerKind (grinValueRuntimeRep scrutinee) /= NotPointer) $ do {
  let {
  dataSlots = [(name, slots) | (alternative, slots) <- zip alternatives alternativeSlots, GrinDataAlt name <- [grinAltCon alternative]];
  byConstructor = Map.fromListWith (\_ first -> first) dataSlots};
  withValue scrutinee (\node -> addTrigger solver node (TriggerCase byConstructor (concatMap snd dataSlots)))};
  forM_ alternatives $ \alternative -> generateExpr solver varsRef results (grinAltRhs alternative)};
  GrinThrow exception -> do {
  vars <- readSTRef varsRef;
  escapeValue solver vars exception};
  GrinCatch _ action handler state -> unknownCall (action : handler : state);
  GrinForeignCallExpr _ arguments -> unknownCall arguments;
  _ -> pure ()} where {
  bindVar var = do {
  slot <- newSlot solver (grinVarRuntimeRep var);
  modifySTRef' varsRef (Map.insert var (slotNode slot));
  pure slot};
  withValue value action = do {
  vars <- readSTRef varsRef;
  node <- valueNode solver vars value;
  for_ node action};
  unknownCall arguments = do {
  vars <- readSTRef varsRef;
  mapM_ (escapeValue solver vars) arguments;
  unknownResults solver results};
  storeNode (GrinNode tag fields) = do {
  vars <- readSTRef varsRef;
  location <- newLocation solver;
  fieldNodes <- addNode solver location tag (length fields);
  zipWithM_ (\field fieldNode -> flowValue solver vars field (Slot fieldNode (pointerKind (grinValueRuntimeRep field)))) fields fieldNodes;
  connectThunk solver tag fieldNodes;
  pure location}};
capturesContinuation :: GrinExpr -> Bool;
capturesContinuation expression = case expression of {
  GrinBind _ valueExpression body -> capturesContinuation valueExpression || capturesContinuation body;
  GrinStoreRec _ body -> capturesContinuation body;
  GrinCase _ _ alternatives -> any (capturesContinuation . grinAltRhs) alternatives;
  GrinPrimitiveCall _ name _ -> name == "aihcControl0#";
  _ -> False};
useCounts :: GrinExpr -> Map GrinVar Int;
useCounts expression = case expression of {
  GrinBind _ valueExpression body -> Map.unionWith (+) (useCounts valueExpression) (useCounts body);
  GrinStoreRec bindings body -> Map.unionWith (+) (valueUses (concatMap (grinNodeFields . snd) bindings)) (useCounts body);
  GrinCase scrutinee binder alternatives -> let {
  alternativeCounts = [(alternative, useCounts (grinAltRhs alternative)) | alternative <- alternatives];
  aliasUses (alternative, counts) = Map.findWithDefault 0 binder counts + case grinAltCon alternative of {
  GrinDefaultAlt -> sum [Map.findWithDefault 0 var counts | var <- grinAltBinders alternative];
  _ -> 0};
  scrutineeUses = 1 + maximum (0 : map aliasUses alternativeCounts);
  branches = Map.unionsWith max (map snd alternativeCounts)} in Map.unionWith (+) (valueUsesTimes scrutineeUses scrutinee) branches;
  _ -> valueUses (exprOperands expression)} where {
  valueUses values = Map.fromListWith (+) [(var, 1) | GrinVarValue var <- values];
  valueUsesTimes count value = case value of {
  GrinVarValue var -> Map.singleton var count;
  _ -> Map.empty}};
exprOperands :: GrinExpr -> [GrinValue];
exprOperands expression = case expression of {
  GrinConstant values -> values;
  GrinStore node -> grinNodeFields node;
  GrinStoreUnchecked node -> grinNodeFields node;
  GrinEnsureHeap requiredWords roots -> requiredWords : roots;
  GrinUpdate pointer value -> [pointer, value];
  GrinUpdateBlackhole pointer value -> [pointer, value];
  GrinEval _ _ value -> [value];
  GrinCpsEval _ _ value continuation -> [value, continuation];
  GrinFetch _ value -> [value];
  GrinCall _ _ arguments -> arguments;
  GrinPrimitiveCall _ _ arguments -> arguments;
  GrinCpsPrimitiveCall _ _ arguments continuation -> continuation : arguments;
  GrinApply _ function arguments -> function : arguments;
  GrinCpsApply _ function arguments continuation -> function : continuation : arguments;
  GrinContinue continuation values -> continuation : values;
  GrinCpsRaise exception continuation -> [exception, continuation];
  GrinHalt values -> values;
  GrinExit status -> [status];
  GrinThrow exception -> [exception];
  GrinCatch _ action handler state -> action : handler : state;
  GrinForeignCallExpr _ arguments -> arguments;
  GrinForward -> [];
  GrinBind {} -> [];
  GrinStoreRec {} -> [];
  GrinStoreRecUnchecked _ _ -> [];
  GrinIfWhnf value _ _ -> [value];
  GrinCase scrutinee _ _ -> [scrutinee]};
sharedLocations :: V.Vector IntSet -> V.Vector (Map GrinNodeTag [Int]) -> Map FunctionName FunctionInfo -> Map FunctionName Int -> Map FunctionName (Map GrinVar Int) -> Map FunctionName (Map GrinVar Int) -> IntSet -> IntSet;
sharedLocations sets locationNodes functions evalNodes variables uses statics = close IntSet.empty (IntSet.toList seeds) where {
  setOf node = fromMaybe IntSet.empty (sets V.!? node);
  seeds = IntSet.unions (statics : setOf escapeNode : [setOf node | (functionName, counts) <- Map.toList uses, let {
  nodes = Map.findWithDefault Map.empty functionName variables}, (var, count) <- Map.toList counts, count >= 2, Just node <- [Map.lookup var nodes]]);
  close shared pending = case pending of {
  [] -> shared;
  location : rest | IntSet.member location shared || isUnknownLocation location -> close shared rest | otherwise -> let {
  nodes = fromMaybe Map.empty (locationNodes V.!? location);
  reached = concat [concatMap (IntSet.toList . setOf) fields <> thunkResults tag | (tag, fields) <- Map.toList nodes]} in close (IntSet.insert location shared) (reached <> rest)};
  thunkResults tag = case tag of {
  GrinThunk functionName -> maybe [] (IntSet.toList . setOf) (Map.lookup functionName evalNodes) <> maybe [] (concatMap (IntSet.toList . setOf) . resultNodes . functionResults) (Map.lookup functionName functions);
  _ -> []}};
resultNodes :: Results -> [Int];
resultNodes results = case results of {
  ResultSlots slots -> [slotNode slot | slot <- slots, slotKind slot /= NotPointer];
  ResultMerged merged -> [merged]};
singleEntryThunks :: V.Vector IntSet -> V.Vector (Map GrinNodeTag [Int]) -> Map FunctionName FunctionInfo -> IntSet -> Int -> IntSet;
singleEntryThunks sets locationNodes functions shared locationCount = IntSet.fromList [location | location <- [0 .. locationCount - 1], not (isUnknownLocation location), not (IntSet.member location shared), [GrinThunk functionName] <- [Map.keys (fromMaybe Map.empty (locationNodes V.!? location))], Just info <- [Map.lookup functionName functions], all (all (isValueLocation locationNodes) . IntSet.toList . setOf) (resultNodes (functionResults info))] where {
  setOf node = fromMaybe IntSet.empty (sets V.!? node)};
isValueLocation :: V.Vector (Map GrinNodeTag [Int]) -> Int -> Bool;
isValueLocation locationNodes location | location == unknownValueLocation = True | location == unknownLocation = False | otherwise = case locationNodes V.!? location of {
  Just nodes -> not (Map.null nodes) && all isValueTag (Map.keys nodes);
  Nothing -> False} where {
  isValueTag tag = case tag of {
  GrinThunk {} -> False;
  _ -> True}};
data RewriteState = RewriteState { rewriteCounts :: !PointsToRewrites, rewriteNextUnique :: !Int };
type RewriteM = State RewriteState;
rewriteWithPointsTo :: PointsTo -> GrinProgram -> (GrinProgram, PointsToRewrites);
rewriteWithPointsTo analysis program = (program {grinFunctions = functions}, rewriteCounts finalState) where {
  (functions, finalState) = runState (mapM rewriteFunction (grinFunctions program)) (RewriteState (PointsToRewrites 0 0 0 0) (maximumProgramVarUnique program + 1));
  declared = Map.fromList [(grinFunctionName function, function) | function <- grinFunctions program];
  rewriteFunction function = do {
  let {
  vars = Map.findWithDefault Map.empty (grinFunctionName function) (resultVariables analysis)};
  body <- rewriteExpr analysis declared vars (grinFunctionBody function);
  pure function {grinFunctionBody = body}}};
rewriteExpr :: PointsTo -> Map FunctionName GrinFunction -> Map GrinVar Int -> GrinExpr -> RewriteM GrinExpr;
rewriteExpr analysis declared vars = go where {
  go expression = case expression of {
  GrinBind binders valueExpression body -> GrinBind binders <$> go valueExpression <*> go body;
  GrinStoreRec bindings body -> GrinStoreRec bindings <$> go body;
  GrinCase scrutinee binder alternatives -> do {
  kept <- case liveAlternatives scrutinee alternatives of {
  Just live -> do {
  count (\counts -> counts {rewritesDeadAlternatives = rewritesDeadAlternatives counts + length alternatives - length live});
  pure live};
  Nothing -> pure alternatives};
  GrinCase scrutinee binder <$> mapM (\alternative -> (\rhs -> alternative {grinAltRhs = rhs}) <$> go (grinAltRhs alternative)) kept};
  GrinEval EvalUpdate runtimeRep value@(GrinVarValue var) | Just locations <- locationsOf value, all isValue (IntSet.toList locations), runtimeRepComponents runtimeRep == [grinVarRuntimeRep var] -> do {
  count (\counts -> counts {rewritesEvaluatedEvals = rewritesEvaluatedEvals counts + 1});
  pure (GrinConstant [value])} | Just locations <- locationsOf value, all (\location -> isValue location || isSingleEntry location) (IntSet.toList locations), any isSingleEntry (IntSet.toList locations) -> do {
  count (\counts -> counts {rewritesSingleEntryEvals = rewritesSingleEntryEvals counts + 1});
  pure (GrinEval EvalSingleEntry runtimeRep value)};
  GrinApply resultRep function arguments | Just (tag, functionName, stored) <- knownClosure function arguments, Just callee <- Map.lookup functionName declared, grinFunctionResultRep callee == ResultForwarded || grinFunctionResultRep callee == resultRep -> do {
  count (\counts -> counts {rewritesDirectCalls = rewritesDirectCalls counts + 1});
  fields <- mapM freshLike (take stored (grinFunctionParameters callee));
  let {
  call = GrinCall resultRep functionName (map GrinVarValue fields <> arguments)};
  pure $ if null fields then call else GrinBind fields (GrinFetch tag function) call};
  _ -> pure expression};
  setOf node = fromMaybe IntSet.empty (resultSets analysis V.!? node);
  locationsOf value = case value of {
  GrinVarValue var | Just node <- Map.lookup var vars, let {
  locations = setOf node}, not (IntSet.null locations), not (IntSet.member unknownLocation locations) -> Just locations;
  _ -> Nothing};
  isValue = isValueLocation (resultLocationNodes analysis);
  isSingleEntry location = IntSet.member location (resultSingleEntry analysis);
  nodesOf location = fromMaybe Map.empty (resultLocationNodes analysis V.!? location);
  liveAlternatives scrutinee alternatives = do {
  locations <- locationsOf scrutinee;
  guard (not (IntSet.member unknownValueLocation locations));
  names <- fmap concat $ forM (IntSet.toList locations) $ \location -> forM (Map.keys (nodesOf location)) $ \case {
  GrinConstructor name 0 -> Just name;
  _ -> Nothing};
  let {
  explicit = Set.fromList [name | GrinDataAlt name <- map grinAltCon alternatives];
  nameSet = Set.fromList names;
  live alternative = case grinAltCon alternative of {
  GrinDataAlt name -> Set.member name nameSet;
  GrinDefaultAlt -> not (Set.null (nameSet `Set.difference` explicit));
  GrinLitAlt {} -> True};
  kept = filter live alternatives};
  guard (not (null kept) && length kept < length alternatives);
  pure kept};
  knownClosure function arguments = do {
  locations <- locationsOf function;
  guard (not (IntSet.member unknownValueLocation locations));
  tags <- fmap concat $ forM (IntSet.toList locations) $ \location -> Just (Map.keys (nodesOf location));
  case List.nub tags of {
  [tag@(GrinClosure functionName [layout])] | map grinValueRuntimeRep arguments == layout, Just callee <- Map.lookup functionName declared, let {
  stored = length (grinFunctionParameters callee) - length layout}, stored >= 0 -> Just (tag, functionName, stored);
  _ -> Nothing}};
  count f = modify' (\state -> state {rewriteCounts = f (rewriteCounts state)});
  freshLike parameter = do {
  state <- get;
  put state {rewriteNextUnique = rewriteNextUnique state + 1};
  pure parameter {grinVarUnique = rewriteNextUnique state}}}
}
