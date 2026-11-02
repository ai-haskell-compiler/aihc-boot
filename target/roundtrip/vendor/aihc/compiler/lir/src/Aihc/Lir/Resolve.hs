module Aihc.Lir.Resolve (LoadError(..), renderLoadError, expandIncludes, expandIncludesWith, loadModule, loadModuleWithIncludes, resolveConstants, evaluateConstants, unresolvedConstant, resolvedSwitchCaseValue) where {
import Aihc.Lir.Parser (LirParseError, parseModule, renderParseError);
import Aihc.Lir.Pretty (prettySymbol, renderDoc);
import Aihc.Lir.Syntax;
import Control.Monad.Trans.Class (lift);
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE);
import Control.Monad.Trans.State.Strict (State, StateT, evalState, get, gets, modify', runStateT);
import Data.Map.Strict (Map);
import Data.Map.Strict qualified as Map;
import Data.Set (Set);
import Data.Set qualified as Set;
import Data.Text (Text);
import Data.Text qualified as T;
import Data.Text.IO qualified as TIO;
import System.FilePath (normalise, takeDirectory, (</>));
data LoadError = LoadParseError !FilePath !LirParseError | LoadIncludeCycle ![FilePath] | LoadIncludeConflict !Symbol deriving (Eq, Show);
renderLoadError :: LoadError -> String;
renderLoadError err = case err of {
  LoadParseError path parseError -> path <> ": Lir parse failed: " <> renderParseError parseError;
  LoadIncludeCycle chain -> "include cycle: " <> unwords (map show chain);
  LoadIncludeConflict symbol -> "included declaration of " <> T.unpack (renderDoc (prettySymbol symbol)) <> " does not match its definition"};
loadModule :: FilePath -> IO (Either LoadError Module);
loadModule = fmap (fmap fst) . loadModuleWithIncludes;
loadModuleWithIncludes :: FilePath -> IO (Either LoadError (Module, [FilePath]));
loadModuleWithIncludes path = do {
  text <- TIO.readFile path;
  case parseModule text of {
  Left err -> pure (Left (LoadParseError path err));
  Right lirModule -> expandIncludesWith TIO.readFile path lirModule}};
expandIncludes :: (FilePath -> IO Text) -> FilePath -> Module -> IO (Either LoadError Module);
expandIncludes reader path = fmap (fmap fst) . expandIncludesWith reader path;
expandIncludesWith :: (FilePath -> IO Text) -> FilePath -> Module -> IO (Either LoadError (Module, [FilePath]));
expandIncludesWith reader path lirModule = runExceptT $ do {
  (items, visited) <- runStateT (expandItems path [] (moduleItems lirModule)) Set.empty;
  merged <- either throwE pure (mergeDeclarations items);
  pure (Module merged, Set.toAscList visited)} where {
  expandItems :: FilePath -> [FilePath] -> [Item] -> StateT (Set FilePath) (ExceptT LoadError IO) [Item];
  expandItems current chain items = concat <$> mapM (expandItem current chain) items;
  expandItem current chain item = case item of {
  ItemInclude relative -> includeFile (current : chain) (normalise (takeDirectory current </> T.unpack relative));
  _ -> pure [item]};
  includeFile chain included | included `elem` chain = lift (throwE (LoadIncludeCycle (reverse (included : chain)))) | otherwise = do {
  seen <- gets (Set.member included);
  if seen then pure [] else do {
  modify' (Set.insert included);
  text <- lift (lift (reader included));
  case parseModule text of {
  Left err -> lift (throwE (LoadParseError included err));
  Right (Module items) -> expandItems included chain items}}}};
mergeDeclarations :: [Item] -> Either LoadError [Item];
mergeDeclarations items = go Map.empty Set.empty items where {
  definedFunctions = Map.fromList [(functionName function, functionSignature function) | ItemFunction function <- items];
  definedData = Set.fromList ([dataName dataItem | ItemData dataItem <- items] <> [globalName global | ItemGlobal global <- items]);
  go _ _ [] = Right [];
  go functions dataObjects (item : rest) = case item of {
  ItemExternFunction external | Just defined <- Map.lookup name definedFunctions -> if defined == declared then go functions dataObjects rest else Left (LoadIncludeConflict name) | Just earlier <- Map.lookup name functions -> if earlier == declared then go functions dataObjects rest else Left (LoadIncludeConflict name) | otherwise -> (item :) <$> go (Map.insert name declared functions) dataObjects rest where {
  name = externFunctionName external;
  declared = externFunctionSignature external};
  ItemExternData symbol | Set.member symbol definedData -> go functions dataObjects rest | Set.member symbol dataObjects -> go functions dataObjects rest | otherwise -> (item :) <$> go functions (Set.insert symbol dataObjects) rest;
  _ -> (item :) <$> go functions dataObjects rest}};
evaluateConstants :: Integer -> Module -> Map Symbol (Either Text Integer);
evaluateConstants wordBytes (Module items) = evalState (Map.traverseWithKey (\name _ -> evaluateName Set.empty name) definitions) Map.empty where {
  definitions = Map.fromListWith (\_ first -> first) [(constantName constant, constantValue constant) | ItemConstant constant <- items];
  otherSymbols = Set.fromList [name | item <- items, Just name <- [otherSymbol item]];
  otherSymbol item = case item of {
  ItemFunction function -> Just (functionName function);
  ItemExternFunction external -> Just (externFunctionName external);
  ItemGlobal global -> Just (globalName global);
  ItemData dataItem -> Just (dataName dataItem);
  ItemExternData name -> Just name;
  _ -> Nothing};
  evaluateName :: Set.Set Symbol -> Symbol -> State (Map Symbol (Either Text Integer)) (Either Text Integer);
  evaluateName active name = do {
  cache <- get;
  case Map.lookup name cache of {
  Just result -> pure result;
  Nothing | Set.member name active -> pure (Left "constant dependency cycle") | otherwise -> do {
  result <- case Map.lookup name definitions of {
  Just expression -> evaluateExpression (Set.insert name active) expression;
  Nothing -> pure (Left (if Set.member name otherSymbols then renderDoc (prettySymbol name) <> " is not a constant" else "unknown symbol " <> renderDoc (prettySymbol name)))};
  modify' (Map.insert name result);
  pure result}}};
  evaluateExpression active expression = case expression of {
  ConstantInt value -> pure (Right value);
  ConstantRef name -> evaluateName active name;
  ConstantWords value -> fmap (fmap (* wordBytes)) (evaluateExpression active value);
  ConstantNegate value -> fmap (fmap negate) (evaluateExpression active value);
  ConstantBinary op left right -> do {
  leftResult <- evaluateExpression active left;
  rightResult <- evaluateExpression active right;
  pure $ do {
  leftValue <- leftResult;
  rightValue <- rightResult;
  case op of {
  ConstantAdd -> Right (leftValue + rightValue);
  ConstantSub -> Right (leftValue - rightValue);
  ConstantMul -> Right (leftValue * rightValue);
  ConstantQuot | rightValue == 0 -> Left "division by zero" | otherwise -> Right (leftValue `quot` rightValue);
  ConstantRem | rightValue == 0 -> Left "remainder by zero" | otherwise -> Right (leftValue `rem` rightValue)}}}}};
resolveConstants :: Integer -> Module -> Module;
resolveConstants wordBytes (Module items) = Module [resolveItem item | item <- items, not (isConstant item)] where {
  constants :: Map Symbol Integer;
  constants = Map.mapMaybe (either (const Nothing) Just) (evaluateConstants wordBytes (Module items));
  isConstant item = case item of {
  ItemConstant _ -> True;
  _ -> False};
  resolveItem item = case item of {
  ItemFunction function -> ItemFunction function {functionBlocks = map resolveBlock (functionBlocks function)};
  ItemData dataItem -> ItemData dataItem {dataFields = map resolveField (dataFields dataItem)};
  _ -> item};
  resolveField field = case field of {
  DataIntConstant ty symbol | Just value <- Map.lookup symbol constants -> DataInt ty value;
  DataWordConstant symbol | Just value <- Map.lookup symbol constants -> DataWord value;
  _ -> field};
  resolveBlock block = block {blockInstructions = [instruction {instructionOperation = resolveOperation (instructionOperation instruction)} | instruction <- blockInstructions block], blockTerminator = resolveTerminator (blockTerminator block)};
  resolveOperation operation = case operation of {
  Binary op ty left right -> Binary op ty (operand left) (operand right);
  Unary op ty value -> Unary op ty (operand value);
  Wide op ty left right -> Wide op ty (operand left) (operand right);
  Compare op ty left right -> Compare op ty (operand left) (operand right);
  FloatBinary op ty left right -> FloatBinary op ty (operand left) (operand right);
  FloatUnary op ty value -> FloatUnary op ty (operand value);
  Convert op from value to -> Convert op from (operand value) to;
  PtrToInt value -> PtrToInt (operand value);
  PtrFromInt value -> PtrFromInt (operand value);
  Select ty condition whenTrue whenFalse -> Select ty (operand condition) (operand whenTrue) (operand whenFalse);
  Load ty address align -> Load ty (resolveAddress address) align;
  Store ty value address align -> Store ty (operand value) (resolveAddress address) align;
  PtrAdd base offset -> PtrAdd (operand base) (operand offset);
  StackAlloc _ _ -> operation;
  GlobalGet _ -> operation;
  GlobalSet symbol value -> GlobalSet symbol (operand value);
  Call symbol arguments -> Call symbol (map operand arguments);
  CallIndirect callee arguments signature -> CallIndirect (operand callee) (map operand arguments) signature};
  resolveAddress address = foldr resolveAddressConstant (address {addressBase = operand (addressBase address), addressConstants = []}) (addressConstants address);
  resolveAddressConstant constant address = case Map.lookup (addressConstantName constant) constants of {
  Just value -> let {
  signed = if addressConstantNegative constant then negate value else value} in if addressConstantInWords constant then address {addressWordOffset = addressWordOffset address + signed} else address {addressOffset = addressOffset address + signed};
  Nothing -> address {addressConstants = constant : addressConstants address}};
  resolveCase switchCase = let {
  target = resolveTarget (switchCaseTarget switchCase)} in case switchCase of {
  SwitchCase value _ -> SwitchCase value target;
  SwitchCaseConstant symbol _ -> maybe (SwitchCaseConstant symbol target) (`SwitchCase` target) (Map.lookup symbol constants)};
  resolveTerminator terminator = case terminator of {
  Jump target -> Jump (resolveTarget target);
  Branch condition whenTrue whenFalse -> Branch (operand condition) (resolveTarget whenTrue) (resolveTarget whenFalse);
  Switch ty scrutinee cases fallback -> Switch ty (operand scrutinee) [resolveCase switchCase | switchCase <- cases] (fmap resolveTarget fallback);
  Return values -> Return (map operand values);
  TailCall symbol arguments -> TailCall symbol (map operand arguments);
  TailCallIndirect callee arguments signature -> TailCallIndirect (operand callee) (map operand arguments) signature;
  Trap _ -> terminator};
  resolveTarget target = target {targetArguments = map operand (targetArguments target)};
  operand value = case value of {
  OperandLiteral (LitSymbol symbol) | Just constant <- Map.lookup symbol constants -> OperandLiteral (LitInt constant);
  _ -> value}};
unresolvedConstant :: Symbol -> a;
unresolvedConstant symbol = error ("Lir constant " <> T.unpack (renderDoc (prettySymbol symbol)) <> " reached a backend unresolved");
resolvedSwitchCaseValue :: SwitchCase -> Integer;
resolvedSwitchCaseValue (SwitchCase value _) = value;
resolvedSwitchCaseValue (SwitchCaseConstant symbol _) = unresolvedConstant symbol
}
