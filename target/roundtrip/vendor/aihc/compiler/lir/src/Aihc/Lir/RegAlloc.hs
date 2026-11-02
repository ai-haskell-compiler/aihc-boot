module Aihc.Lir.RegAlloc (Allocation(..), Registers(..), allocateRegistersFor, Interval(..), functionIntervals, readCounts) where {
import Aihc.Lir.Flat;
import Aihc.Lir.Syntax;
import Control.Applicative (Const(..));
import Control.Monad (when, (>=>));
import Control.Monad.ST (ST, runST);
import Data.Array.ST (STUArray, readArray, writeArray);
import Data.Array.Unboxed (Array, UArray, elems, listArray, (!));
import Data.Array.Unsafe (unsafeFreeze);
import Data.Foldable (traverse_);
import Data.List (elemIndex);
import Data.Map.Strict (Map);
import Data.Map.Strict qualified as Map;
import Data.Maybe (fromMaybe);
import Data.STRef (newSTRef, readSTRef, writeSTRef);
data Allocation register = Allocation { allocationRegisters :: !(Map Var register), allocationSpills :: ![Var], allocationUsed :: ![register] } deriving (Eq, Show);
data Interval = Interval { intervalVar :: !Var, intervalStart :: !Int, intervalEnd :: !Int } deriving (Eq, Show);
data Registers register = Registers { registersVolatile :: ![register], registersPreserved :: ![register], registersPreservedCost :: !Bool, registersArgument :: !(Int -> Maybe register), registersResult :: !(Int -> Maybe register) };
allocateRegistersFor :: (Ord register) => Registers register -> Map Symbol Signature -> Function -> Allocation register;
allocateRegistersFor target signatures function = Allocation {allocationRegisters = Map.fromDistinctAscList [(encNames encoded ! value, registers ! register) | value <- [0 .. encValues encoded - 1], let {
  register = assigned ! value}, register >= 0], allocationSpills = [encNames encoded ! value | value <- elems (encDefinitions encoded), assigned ! value < 0], allocationUsed = [register | (index, register) <- zip [0 ..] pool, used ! index]} where {
  pool = registersVolatile target <> registersPreserved target;
  registers = boxedArray (length pool) pool;
  carrier which = memoise (maybe (-1) indexOf . which target);
  indexOf register = fromMaybe (-1) (elemIndex register pool);
  encoded = encodeFunction (carrier registersArgument) (carrier registersResult) signatures function;
  (assigned, used) = runAllocation encoded (length pool) (length (registersVolatile target)) (registersPreservedCost target)};
functionIntervals :: Function -> [Interval];
functionIntervals function = [Interval {intervalVar = encNames encoded ! value, intervalStart = starts ! value, intervalEnd = ends ! value} | value <- [0 .. encValues encoded - 1]] where {
  encoded = encodeFunction unhinted unhinted Map.empty function;
  unhinted _ = -1;
  (starts, ends) = runST (functionSpans encoded)};
memoise :: (Int -> Int) -> Int -> Int;
memoise carrier = let {
  table = listArray (0, limit) (map carrier [0 .. limit]) :: UArray Int Int} in \position -> if position <= limit then table ! position else carrier position where {
  limit = 15};
data Encoded = Encoded { encNames :: !(Array Int Var), encValues :: !Int, encPositions :: !Int, encBlocks :: !Int, encExits :: !Int, encParameters :: !(UArray Int Int), encDefinitions :: !(UArray Int Int), encBlockStart :: !(UArray Int Int), encBlockTerminator :: !(UArray Int Int), encBlockInstructions :: !(UArray Int Int), encInstructionCall :: !(UArray Int Int), encBlockParameters :: !Rows, encTerminatorReads :: !Rows, encTargets :: !Rows, encResults :: !Rows, encReads :: !Rows, encTargetArguments :: !Rows, encDefiner :: !(UArray Int Int), encCallHints :: !(UArray Int Int), encExitHints :: !(UArray Int Int) };
callNone, callAihc, callC :: Int;
callNone = 0;
callAihc = 1;
callC = 2;
forBlocks :: Encoded -> (Int -> ST s ()) -> ST s ();
forBlocks encoded = forUpTo (encBlocks encoded);
forBlockInstructions :: Encoded -> Int -> (Int -> Int -> ST s ()) -> ST s ();
forBlockInstructions encoded index act = go (encBlockInstructions encoded ! index) (encBlockStart encoded ! index + 1) where {
  go number position = when (number < encBlockInstructions encoded ! (index + 1)) (act number position >> go (number + 1) (position + 1))};
forHints :: Encoded -> (Int -> Int -> ST s ()) -> ST s ();
forHints encoded act = pairs (encCallHints encoded) >> pairs (encExitHints encoded) where {
  pairs table = forUpTo (lengthOf table `div` 2) (\at -> act (table ! (2 * at)) (table ! (2 * at + 1)))};
forJumpPairs :: Encoded -> (Int -> Int -> ST s ()) -> ST s ();
forJumpPairs encoded act = forBlocks encoded $ \index -> forUpTo (rowTo targets index - rowFrom targets index) $ \step -> do {
  let {
  at = rowFrom targets index + step;
  successor = rowAt targets at;
  count = min (rowTo arguments at - rowFrom arguments at) (rowTo parameters successor - rowFrom parameters successor)};
  forUpTo count $ \position -> do {
  let {
  value = rowAt arguments (rowFrom arguments at + position)};
  when (value >= 0) (act value (rowAt parameters (rowFrom parameters successor + position)))}} where {
  targets = encTargets encoded;
  arguments = encTargetArguments encoded;
  parameters = encBlockParameters encoded};
encodeFunction :: (Int -> Int) -> (Int -> Int) -> Map Symbol Signature -> Function -> Encoded;
encodeFunction argumentCarrier resultCarrier signatures function = runST $ do {
  let {
  blocks = functionBlocks function;
  blockCount = length blocks;
  instructionCount = foldl' (\total block -> total + length (blockInstructions block)) 0 blocks;
  parameterCount = length (functionParameters function);
  labels = Map.fromList (zip (map blockLabel blocks) [0 :: Int ..])};
  identifiers <- newSTRef (Map.empty :: Map Var Int);
  nextIdentifier <- newInts 1 0;
  exitCount <- newInts 1 0;
  blockStart <- newInts blockCount 0;
  terminatorPositions <- newInts blockCount 0;
  blockRanges <- newInts (blockCount + 1) 0;
  conventions <- newInts instructionCount callNone;
  parameters <- newInts parameterCount 0;
  parameterTable <- newTable blockCount 4;
  terminatorReadTable <- newTable blockCount 8;
  targetTable <- newTable blockCount 4;
  resultTable <- newTable instructionCount (instructionCount + 4);
  readTable <- newTable instructionCount (2 * instructionCount + 4);
  argumentTable <- newTable 4 4;
  callHints <- newIntBuffer 4;
  exitHints <- newIntBuffer 4;
  let {
  intern var = do {
  known <- readSTRef identifiers;
  case Map.lookup var known of {
  Just identifier -> pure identifier;
  Nothing -> do {
  identifier <- readArray nextIdentifier 0;
  writeSTRef identifiers (Map.insert var identifier known);
  writeArray nextIdentifier 0 (identifier + 1);
  pure identifier}}};
  internOperand operand = case operand of {
  OperandVar var -> intern var;
  OperandLiteral _ -> pure (-1)};
  pushRead table operand = do {
  value <- internOperand operand;
  when (value >= 0) (pushValue table value)};
  pushHint buffer carrier position var = intern var >>= pushHintValue buffer carrier position;
  hintOperands buffer carrier operands = forEach operands $ \position operand -> case operand of {
  OperandVar var -> pushHint buffer carrier position var;
  OperandLiteral _ -> pure ()};
  goInstruction first offset (Instruction results operation) = do {
  let {
  number = first + offset};
  openRow resultTable;
  traverse_ (intern >=> pushValue resultTable) results;
  openRow readTable;
  forOperationOperands (pushRead readTable) operation;
  let {
  callee arguments convention = do {
  writeArray conventions number convention;
  hintOperands callHints argumentCarrier arguments;
  forEach results (pushHint callHints resultCarrier)}};
  case operation of {
  Call symbol arguments -> callee arguments (conventionOf (maybe AihcConvention signatureConvention (Map.lookup symbol signatures)));
  CallIndirect _ arguments signature -> callee arguments (conventionOf (signatureConvention signature));
  _ -> pure ()}};
  goBlock index start block = do {
  writeArray blockStart index start;
  openRow parameterTable;
  traverse_ (\(var, _) -> intern var >>= pushValue parameterTable) (blockParameters block);
  first <- readArray blockRanges index;
  forEach (blockInstructions block) (goInstruction first);
  writeArray blockRanges (index + 1) (first + length (blockInstructions block));
  let {
  terminator = blockTerminator block;
  terminatorPosition = start + 1 + length (blockInstructions block)};
  writeArray terminatorPositions index terminatorPosition;
  openRow terminatorReadTable;
  forTerminatorOperands (pushRead terminatorReadTable) terminator;
  openRow targetTable;
  traverse_ (\jump -> do {
  pushValue targetTable (labels Map.! targetLabel jump);
  openRow argumentTable;
  traverse_ (internOperand >=> pushValue argumentTable) (targetArguments jump)}) (terminatorTargets terminator);
  when (restoresRegisters terminator) (addTo exitCount 0 1);
  case terminator of {
  TailCall _ arguments -> hintOperands exitHints argumentCarrier arguments;
  TailCallIndirect _ arguments _ -> hintOperands exitHints argumentCarrier arguments;
  Return values -> hintOperands exitHints resultCarrier values;
  _ -> pure ()};
  pure (terminatorPosition + 2)};
  goBlocks _ start [] = pure start;
  goBlocks index start (block : rest) = goBlock index start block >>= \next -> goBlocks (index + 1) next rest};
  forEach (functionParameters function) $ \position (var, _) -> do {
  value <- intern var;
  writeArray parameters position value;
  pushHintValue callHints argumentCarrier position value};
  _ <- goBlocks 0 1 blocks;
  known <- readSTRef identifiers;
  let {
  valueCount = Map.size known};
  rankArray <- newInts valueCount 0;
  forEach (Map.elems known) (flip (writeArray rankArray));
  ranks <- freezeInts rankArray;
  let {
  rename value = if value < 0 then value else ranks ! value;
  renamePairs at value = if even at then rename value else value};
  forUpTo parameterCount (\at -> readArray parameters at >>= writeArray parameters at . rename);
  blockParameterRows <- freezeTableWith rename parameterTable;
  terminatorReadRows <- freezeTableWith rename terminatorReadTable;
  resultRows <- freezeTableWith rename resultTable;
  readRows <- freezeTableWith rename readTable;
  argumentRows <- freezeTableWith rename argumentTable;
  targetRows <- freezeTableWith id targetTable;
  callHintPairs <- freezeBufferWith renamePairs callHints;
  exitHintPairs <- freezeBufferWith renamePairs exitHints;
  blockRangesFrozen <- freezeInts blockRanges;
  definitions <- newInts valueCount 0;
  definitionCount <- newInts 1 0;
  definer <- newInts valueCount (-1);
  let {
  take' value = readArray definitionCount 0 >>= \at -> writeArray definitions at value >> writeArray definitionCount 0 (at + 1)};
  forUpTo parameterCount (readArray parameters >=> take');
  forUpTo blockCount $ \index -> do {
  forRow blockParameterRows index take';
  forUpTo (blockRangesFrozen ! (index + 1) - blockRangesFrozen ! index) $ \offset -> forRow resultRows (blockRangesFrozen ! index + offset) $ \value -> do {
  take' value;
  writeArray definer value (blockRangesFrozen ! index + offset)}};
  lastTerminator <- if blockCount == 0 then pure (-1) else readArray terminatorPositions (blockCount - 1);
  Encoded (listArray (0, valueCount - 1) (Map.keys known)) valueCount (lastTerminator + 2) blockCount <$> readArray exitCount 0 <*> freezeInts parameters <*> freezeInts definitions <*> freezeInts blockStart <*> freezeInts terminatorPositions <*> pure blockRangesFrozen <*> freezeInts conventions <*> pure blockParameterRows <*> pure terminatorReadRows <*> pure targetRows <*> pure resultRows <*> pure readRows <*> pure argumentRows <*> freezeInts definer <*> pure callHintPairs <*> pure exitHintPairs} where {
  conventionOf callee = case callee of {
  AihcConvention -> callAihc;
  CConvention -> callC};
  restoresRegisters terminator = case terminator of {
  Return _ -> True;
  TailCall _ _ -> True;
  TailCallIndirect {} -> True;
  _ -> False}};
pushHintValue :: IntBuffer s -> (Int -> Int) -> Int -> Int -> ST s ();
pushHintValue buffer carrier position value = when (carrier position >= 0) (pushInt buffer value >> pushInt buffer (carrier position));
functionSpans :: Encoded -> ST s (UArray Int Int, UArray Int Int);
functionSpans encoded = do {
  starts <- newInts (encValues encoded) maxBound;
  ends <- newInts (encValues encoded) minBound;
  let {
  touch position value = do {
  start <- readArray starts value;
  when (position < start) (writeArray starts value position);
  end <- readArray ends value;
  when (position > end) (writeArray ends value position)}};
  forUpTo (lengthOf (encParameters encoded)) (touch 0 . (encParameters encoded !));
  live <- liveness encoded;
  forBlocks encoded $ \index -> do {
  let {
  start = encBlockStart encoded ! index};
  forRow (encBlockParameters encoded) index (touch start);
  forBlockInstructions encoded index $ \number position -> do {
  forRow (encResults encoded) number (touch position);
  forRow (encReads encoded) number (touch position)};
  forRow (encTerminatorReads encoded) index (touch (encBlockTerminator encoded ! index));
  case live of {
  Nothing -> pure ();
  Just (liveIn, liveOut) -> do {
  forBits liveIn index (touch start);
  forBits liveOut index (touch (encBlockTerminator encoded ! index + 1))}}};
  (,) <$> freezeInts starts <*> freezeInts ends};
liveness :: Encoded -> ST s (Maybe (Bits s, Bits s));
liveness encoded | rowsSize (encTargets encoded) == 0 = pure Nothing | otherwise = do {
  let {
  blockCount = encBlocks encoded};
  liveIn <- newBits blockCount (encValues encoded);
  liveOut <- newBits blockCount (encValues encoded);
  scratch <- newBits 1 (encValues encoded);
  upward <- newTable blockCount 8;
  forBlocks encoded $ \index -> do {
  clearRow scratch 0;
  forRow (encTerminatorReads encoded) index (setBit scratch 0);
  forDownFrom (encBlockInstructions encoded ! (index + 1) - 1) (encBlockInstructions encoded ! index) $ \number -> do {
  forRow (encResults encoded) number (clearBit scratch 0);
  forRow (encReads encoded) number (setBit scratch 0)};
  forRow (encBlockParameters encoded) index (clearBit scratch 0);
  openRow upward;
  forBits scratch 0 (pushValue upward)};
  upwardRows <- freezeTableWith id upward;
  let {
  update index = do {
  clearRow liveOut index;
  forRow (encTargets encoded) index (unionRow liveOut index liveIn);
  _ <- replaceRow scratch 0 liveOut index;
  forRow (encBlockParameters encoded) index (clearBit scratch 0);
  forBlockInstructions encoded index (\number _ -> forRow (encResults encoded) number (clearBit scratch 0));
  forRow upwardRows index (setBit scratch 0);
  replaceRow liveIn index scratch 0};
  sweep index changed | index < 0 = pure changed | otherwise = update index >>= \here -> sweep (index - 1) (changed || here);
  converge = sweep (blockCount - 1) False >>= \changed -> when changed converge};
  converge;
  pure (Just (liveIn, liveOut))};
reachAny, reachPreserved, reachNone :: Int;
reachAny = 0;
reachPreserved = 1;
reachNone = 2;
runAllocation :: Encoded -> Int -> Int -> Bool -> (UArray Int Int, UArray Int Bool);
runAllocation encoded poolSize volatileCount preservedCost = runST $ do {
  let {
  valueCount = encValues encoded};
  (starts, ends) <- functionSpans encoded;
  aihcCalls <- callsBefore encoded callAihc;
  cCalls <- callsBefore encoded callC;
  earns <- earnedRegisters encoded preservedCost;
  hints <- buildRows valueCount (forHints encoded);
  partners <- buildRows valueCount (\act -> forJumpPairs encoded act >> forJumpPairs encoded (flip act));
  order <- orderByStart encoded starts hints partners;
  assigned <- newInts valueCount (-1);
  used <- newBools poolSize False;
  holder <- newInts poolSize (-1);
  active <- newInts poolSize 0;
  activeCount <- newInts 1 0;
  let {
  reachOf value | crosses aihcCalls (starts ! value) (ends ! value) = reachNone | crosses cCalls (starts ! value) (ends ! value) = reachPreserved | otherwise = reachAny;
  accepts value register = do {
  earned <- readArray earns value;
  let {
  preserved = register >= volatileCount};
  pure $ case reachOf value of {
  reach | reach == reachNone -> False | reach == reachPreserved -> preserved && earned | otherwise -> not preserved || earned}};
  usable value register | register < 0 = pure False | otherwise = do {
  taken <- readArray holder register;
  if taken >= 0 then pure False else accepts value register};
  valueIn index = readArray active index >>= readArray holder;
  expire position = do {
  count <- readArray activeCount 0;
  let {
  finished index | index >= count = pure index | otherwise = do {
  value <- valueIn index;
  if ends ! value < position || (ends ! value == position && starts ! value < ends ! value) then readArray active index >>= \register -> writeArray holder register (-1) >> finished (index + 1) else pure index}};
  done <- finished 0;
  when (done > 0) (dropActive 0 done count)};
  dropActive at gone count = do {
  forUpTo (count - at - gone) (\step -> readArray active (at + gone + step) >>= writeArray active (at + step));
  writeArray activeCount 0 (count - gone)};
  activate value register = do {
  count <- readArray activeCount 0;
  let {
  place index | index >= count = pure index | otherwise = valueIn index >>= \other -> if ends ! value <= ends ! other then pure index else place (index + 1)};
  at <- place 0;
  forDownFrom count (at + 1) (\index -> readArray active (index - 1) >>= writeArray active index);
  writeArray active at register;
  writeArray activeCount 0 (count + 1);
  writeArray holder register value;
  writeArray used register True;
  writeArray assigned value register};
  searchRow rows index pick value | index < 0 = pure (-1) | otherwise = go (rowFrom rows index) where {
  go at | at >= rowTo rows index = pure (-1) | otherwise = do {
  register <- pick (rowAt rows at);
  ok <- usable value register;
  if ok then pure register else go (at + 1)}};
  firstFree value register | register >= poolSize = pure (-1) | otherwise = usable value register >>= \ok -> if ok then pure register else firstFree value (register + 1);
  preferred value = searchRow hints value pure value `orElse` searchRow partners value (readArray assigned) value `orElse` searchRow partners value (\partner -> searchRow hints partner pure value) value `orElse` searchRow (encReads encoded) (encDefiner encoded ! value) (readArray assigned) value `orElse` firstFree value 0;
  spill value = do {
  count <- readArray activeCount 0;
  let {
  victim index | index < 0 = pure (-1) | otherwise = do {
  register <- readArray active index;
  accepts value register >>= \ok -> if ok then pure index else victim (index - 1)}};
  at <- victim (count - 1);
  when (at >= 0) $ do {
  register <- readArray active at;
  loser <- readArray holder register;
  when (ends ! loser > ends ! value) $ do {
  writeArray assigned loser (-1);
  writeArray holder register (-1);
  dropActive at 1 count;
  activate value register}}}};
  forUpTo valueCount $ \at -> do {
  let {
  value = order ! at};
  expire (starts ! value);
  register <- preferred value;
  if register >= 0 then activate value register else spill value};
  (,) <$> freezeInts assigned <*> unsafeFreeze used};
crosses :: UArray Int Int -> Int -> Int -> Bool;
crosses calls start end = end > start + 1 && calls ! end > calls ! (start + 1);
orElse :: ST s Int -> ST s Int -> ST s Int;
orElse first second = first >>= \register -> if register >= 0 then pure register else second;
callsBefore :: Encoded -> Int -> ST s (UArray Int Int);
callsBefore encoded convention = do {
  counts <- newInts (encPositions encoded + 2) 0;
  forBlocks encoded $ \index -> forBlockInstructions encoded index $ \number position -> when (encInstructionCall encoded ! number == convention) (addTo counts (position + 1) 1);
  scanSums counts (encPositions encoded + 1);
  freezeInts counts};
earnedRegisters :: Encoded -> Bool -> ST s (STUArray s Int Bool);
earnedRegisters encoded preservedCost = do {
  earns <- newBools (encValues encoded) True;
  when preservedCost $ do {
  counts <- accessCounts encoded;
  forUpTo (encValues encoded) $ \value -> do {
  count <- readArray counts value;
  writeArray earns value (count > 1 + encExits encoded)}};
  pure earns};
accessCounts :: Encoded -> ST s (STUArray s Int Int);
accessCounts encoded = do {
  depths <- loopDepths encoded;
  counts <- newInts (encValues encoded) 0;
  forBlocks encoded $ \index -> do {
  depth <- readArray depths index;
  let {
  add value = addTo counts value (10 ^ min 3 depth)};
  forRow (encBlockParameters encoded) index add;
  forBlockInstructions encoded index $ \number _ -> do {
  forRow (encResults encoded) number add;
  forRow (encReads encoded) number add};
  forRow (encTerminatorReads encoded) index add};
  forUpTo (lengthOf (encParameters encoded)) (\at -> addTo counts (encParameters encoded ! at) 1);
  pure counts};
loopDepths :: Encoded -> ST s (STUArray s Int Int);
loopDepths encoded = do {
  let {
  blockCount = encBlocks encoded};
  edges <- newInts (blockCount + 1) 0;
  forBlocks encoded $ \index -> forRow (encTargets encoded) index $ \successor -> when (successor <= index) (addTo edges successor 1 >> addTo edges (index + 1) (-1));
  depths <- newInts blockCount 0;
  forUpTo blockCount $ \index -> do {
  enclosing <- if index == 0 then pure 0 else readArray depths (index - 1);
  opened <- readArray edges index;
  writeArray depths index (enclosing + opened)};
  pure depths};
orderByStart :: Encoded -> UArray Int Int -> Rows -> Rows -> ST s (UArray Int Int);
orderByStart encoded starts hints partners = do {
  let {
  positionCount = encPositions encoded;
  earlier value at | at >= rowTo partners value = False | otherwise = starts ! rowAt partners at < starts ! value || earlier value (at + 1);
  leads value = rowTo hints value > rowFrom hints value || earlier value (rowFrom partners value)};
  leadCursor <- newInts (positionCount + 1) 0;
  restCursor <- newInts (positionCount + 1) 0;
  let {
  cursorFor value = if leads value then leadCursor else restCursor};
  forUpTo (encValues encoded) (\value -> addTo (cursorFor value) (starts ! value) 1);
  total <- newInts 1 0;
  forUpTo (positionCount + 1) $ \position -> do {
  leaders <- readArray leadCursor position;
  rest <- readArray restCursor position;
  placed <- readArray total 0;
  writeArray leadCursor position placed;
  writeArray restCursor position (placed + leaders);
  writeArray total 0 (placed + leaders + rest)};
  order <- newInts (encValues encoded) 0;
  forUpTo (encValues encoded) $ \value -> do {
  at <- readArray (cursorFor value) (starts ! value);
  writeArray (cursorFor value) (starts ! value) (at + 1);
  writeArray order at value};
  freezeInts order};
readCounts :: Function -> Map Var Int;
readCounts function = Map.fromListWith (+) [(var, 1) | block <- functionBlocks function, var <- concatMap (operationReads . instructionOperation) (blockInstructions block) <> terminatorReads (blockTerminator block)];
operationReads :: Operation -> [Var];
operationReads = getConst . forOperationOperands (Const . operandVar);
terminatorReads :: Terminator -> [Var];
terminatorReads = getConst . forTerminatorOperands (Const . operandVar);
operandVar :: Operand -> [Var];
operandVar operand = case operand of {
  OperandVar var -> [var];
  OperandLiteral _ -> []}
}
