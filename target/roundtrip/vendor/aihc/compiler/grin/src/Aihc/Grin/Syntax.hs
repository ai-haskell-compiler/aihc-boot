module Aihc.Grin.Syntax (GrinRep(..), GrinLevity(..), GrinVecCount(..), GrinVecElem(..), liftedGrinRep, GrinResultRep(..), liftedResultRep, resultRepComponents, GrinProgram(..), GrinVis(..), GrinConstructorDecl(..), pubConstructor, GrinGlobal(..), pubGlobal, grinGlobalRoots, grinConstructorRoots, GrinFunction(..), FunctionName(..), GrinVar(..), GrinExpr(..), GrinEvalUpdate(..), forwardedResultUses, GrinValue(..), GrinNode(..), GrinNodeTag(..), GrinAlt(..), GrinAltCon(..), GrinLiteral(..), GrinForeignCall(..), GrinForeignTarget(..), GrinForeignSignature(..), GrinForeignEffect(..), GrinForeignType(..), runtimeRepComponents, SumLayout(..), sumLayout, sumSlotRep, grinForeignOperandReps, grinForeignCallResultReps, foreignTypeRuntimeRep, GrinScope(..), grinScopedName, grinNameScope, grinProgramScopes, grinProgramLiterals, grinExprGlobalReferences, grinProgramGlobalReferences, grinNodeGlobalReferences, grinExprFunctionNames, grinNodeFunctionNames, grinExprConstructorTags, grinNodeConstructorTags, grinValueRuntimeRep, grinVarNameNeedsNumber, unusedFunctionName, isLiftedRuntimeRep, isPointerRuntimeRep) where {
import Data.ByteString (ByteString);
import Data.Char (isDigit);
import Data.List (mapAccumL);
import Data.Map.Strict qualified as Map;
import Data.Maybe (mapMaybe);
import Data.Set (Set);
import Data.Set qualified as Set;
import Data.Text (Text);
import Data.Text qualified as T;
data GrinRep = BoxedRep !GrinLevity | IntRep | Int8Rep | Int16Rep | Int32Rep | Int64Rep | WordRep | Word8Rep | Word16Rep | Word32Rep | Word64Rep | AddrRep | FloatRep | DoubleRep | SumRep ![GrinRep] | TupleRep ![GrinRep] | VecRep !GrinVecCount !GrinVecElem deriving (Eq, Ord, Show, Read);
data GrinLevity = Lifted | Unlifted deriving (Eq, Ord, Show, Read);
data GrinVecCount = Vec16 | Vec2 | Vec32 | Vec4 | Vec64 | Vec8 deriving (Eq, Ord, Show, Read);
data GrinVecElem = Int16ElemRep | Int32ElemRep | Int64ElemRep | Int8ElemRep | Word8ElemRep | Word16ElemRep | Word32ElemRep | Word64ElemRep | FloatElemRep | DoubleElemRep deriving (Eq, Ord, Show, Read);
liftedGrinRep :: GrinRep;
liftedGrinRep = BoxedRep Lifted;
data GrinResultRep = ResultRep !GrinRep | ResultForwarded deriving (Eq, Ord, Show, Read);
liftedResultRep :: GrinResultRep;
liftedResultRep = ResultRep liftedGrinRep;
resultRepComponents :: GrinResultRep -> Maybe [GrinRep];
resultRepComponents resultRep = case resultRep of {
  ResultRep runtimeRep -> Just (runtimeRepComponents runtimeRep);
  ResultForwarded -> Nothing};
data GrinScope = GrinScope { grinScopePackage :: !Text, grinScopeModule :: !Text } deriving (Eq, Ord, Show, Read);
grinScopedName :: Text -> Text -> Text -> Text;
grinScopedName packageName moduleName baseName = T.intercalate "\0" [packageName, moduleName, baseName];
grinNameScope :: Text -> Maybe (GrinScope, Text);
grinNameScope name = case T.splitOn "\0" name of {
  [packageName, moduleName, baseName] -> Just (GrinScope packageName moduleName, baseName);
  _ -> Nothing};
data GrinProgram = GrinProgram { grinConstructors :: ![GrinConstructorDecl], grinPrimitives :: ![(GrinVar, Int)], grinForeignCalls :: ![GrinForeignCall], grinGlobals :: ![GrinGlobal], grinFunctions :: ![GrinFunction] } deriving (Eq, Show, Read);
data GrinVis = GrinPub | GrinPrivate deriving (Eq, Ord, Show, Read);
data GrinConstructorDecl = GrinConstructorDecl { grinConstructorName :: !Text, grinConstructorLayouts :: ![[GrinRep]], grinConstructorVis :: !GrinVis } deriving (Eq, Show, Read);
pubConstructor :: Text -> [[GrinRep]] -> GrinConstructorDecl;
pubConstructor name layouts = GrinConstructorDecl name layouts GrinPub;
data GrinGlobal = GrinGlobal { grinGlobalName :: !Text, grinGlobalNode :: !GrinNode, grinGlobalVis :: !GrinVis } deriving (Eq, Show, Read);
pubGlobal :: Text -> GrinNode -> GrinGlobal;
pubGlobal name node = GrinGlobal name node GrinPub;
grinGlobalRoots :: GrinProgram -> [Text];
grinGlobalRoots program = [grinGlobalName global | global <- grinGlobals program, grinGlobalVis global == GrinPub];
grinConstructorRoots :: GrinProgram -> [Text];
grinConstructorRoots program = [grinConstructorName constructor | constructor <- grinConstructors program, grinConstructorVis constructor == GrinPub];
data GrinFunction = GrinFunction { grinFunctionName :: !FunctionName, grinFunctionParameters :: ![GrinVar], grinFunctionResultRep :: !GrinResultRep, grinFunctionBody :: !GrinExpr } deriving (Eq, Show, Read);
newtype FunctionName = FunctionName { unFunctionName :: Text } deriving (Eq, Ord, Show, Read);
data GrinVar = GrinVar { grinVarName :: !Text, grinVarUnique :: !Int, grinVarRuntimeRep :: !GrinRep } deriving (Show, Read);
instance Eq GrinVar where {
  left == right = grinVarName left == grinVarName right && grinVarUnique left == grinVarUnique right};
instance Ord GrinVar where {
  compare left right = compare (grinVarName left, grinVarUnique left) (grinVarName right, grinVarUnique right)};
data GrinExpr = GrinConstant ![GrinValue] | GrinBind ![GrinVar] !GrinExpr !GrinExpr | GrinStore !GrinNode | GrinEnsureHeap !GrinValue ![GrinValue] | GrinStoreUnchecked !GrinNode | GrinStoreRec ![(GrinVar, GrinNode)] !GrinExpr | GrinStoreRecUnchecked ![(GrinVar, GrinNode)] !GrinExpr | GrinUpdate !GrinValue !GrinValue | GrinEval !GrinEvalUpdate !GrinRep !GrinValue | GrinCpsEval !GrinEvalUpdate !GrinRep !GrinValue !GrinValue | GrinFetch !GrinNodeTag !GrinValue | GrinIfWhnf !GrinValue !GrinExpr !GrinExpr | GrinCall !GrinResultRep !FunctionName ![GrinValue] | GrinPrimitiveCall !GrinRep !Text ![GrinValue] | GrinCpsPrimitiveCall !GrinRep !Text ![GrinValue] !GrinValue | GrinApply !GrinResultRep !GrinValue ![GrinValue] | GrinForward | GrinCpsApply !GrinResultRep !GrinValue ![GrinValue] !GrinValue | GrinContinue !GrinValue ![GrinValue] | GrinCpsRaise !GrinValue !GrinValue | GrinUpdateBlackhole !GrinValue !GrinValue | GrinHalt ![GrinValue] | GrinExit !GrinValue | GrinCase !GrinValue !GrinVar ![GrinAlt] | GrinThrow !GrinValue | GrinCatch !GrinRep !GrinValue !GrinValue ![GrinValue] | GrinForeignCallExpr !GrinForeignCall ![GrinValue] deriving (Eq, Show, Read);
data GrinEvalUpdate = EvalUpdate | EvalSingleEntry deriving (Eq, Ord, Show, Read);
data GrinValue = GrinVarValue !GrinVar | GrinGlobalValue !Text | GrinLitValue !GrinLiteral deriving (Eq, Show, Read);
data GrinNode = GrinNode { grinNodeTag :: !GrinNodeTag, grinNodeFields :: ![GrinValue] } deriving (Eq, Show, Read);
data GrinNodeTag = GrinConstructor !Text !Int | GrinClosure !FunctionName ![[GrinRep]] | GrinThunk !FunctionName deriving (Eq, Ord, Show, Read);
data GrinAlt = GrinAlt { grinAltCon :: !GrinAltCon, grinAltBinders :: ![GrinVar], grinAltRhs :: !GrinExpr } deriving (Eq, Show, Read);
data GrinAltCon = GrinDataAlt !Text | GrinLitAlt !GrinLiteral | GrinDefaultAlt deriving (Eq, Show, Read);
data GrinLiteral = GrinLitInt !GrinRep !Integer | GrinLitChar !GrinRep !Char | GrinLitAddr !ByteString deriving (Eq, Show, Read);
unusedFunctionName :: Text -> Set FunctionName -> FunctionName;
unusedFunctionName base used = search (FunctionName base) 2 where {
  search candidate index | Set.member candidate used = search (FunctionName (base <> "_" <> T.pack (show index))) (index + 1 :: Int) | otherwise = candidate};
grinProgramScopes :: GrinProgram -> [GrinScope];
grinProgramScopes program = Set.toAscList (Set.fromList (mapMaybe (fmap fst . grinNameScope) names)) where {
  names = map grinConstructorName (grinConstructors program) <> map grinGlobalName (grinGlobals program) <> map grinForeignCallName (grinForeignCalls program) <> grinProgramGlobalReferences program <> concatMap (nodeTagNames . grinGlobalNode) (grinGlobals program) <> concatMap (exprTagNames . grinFunctionBody) (grinFunctions program)};
exprTagNames :: GrinExpr -> [Text];
exprTagNames expression = case expression of {
  GrinBind _ valueExpression body -> exprTagNames valueExpression <> exprTagNames body;
  GrinStore node -> nodeTagNames node;
  GrinStoreUnchecked node -> nodeTagNames node;
  GrinFetch tag _ -> nodeTagNames (GrinNode tag []);
  GrinStoreRec bindings body -> concatMap (nodeTagNames . snd) bindings <> exprTagNames body;
  GrinStoreRecUnchecked bindings body -> concatMap (nodeTagNames . snd) bindings <> exprTagNames body;
  GrinIfWhnf _ ready slow -> exprTagNames ready <> exprTagNames slow;
  GrinCase _ _ alternatives -> concatMap altTagNames alternatives;
  _ -> []} where {
  altTagNames alternative = altConTagNames (grinAltCon alternative) <> exprTagNames (grinAltRhs alternative);
  altConTagNames altCon = case altCon of {
  GrinDataAlt name -> [name];
  _ -> []}};
nodeTagNames :: GrinNode -> [Text];
nodeTagNames node = case grinNodeTag node of {
  GrinConstructor name _ -> [name];
  _ -> []};
grinProgramLiterals :: GrinProgram -> [GrinLiteral];
grinProgramLiterals program = concatMap (nodeLiterals . grinGlobalNode) (grinGlobals program) <> concatMap (exprLiterals . grinFunctionBody) (grinFunctions program) where {
  exprLiterals expression = case expression of {
  GrinConstant values -> concatMap valueLiterals values;
  GrinBind _ valueExpression body -> exprLiterals valueExpression <> exprLiterals body;
  GrinStore node -> nodeLiterals node;
  GrinEnsureHeap requiredWords roots -> valueLiterals requiredWords <> concatMap valueLiterals roots;
  GrinStoreUnchecked node -> nodeLiterals node;
  GrinStoreRec bindings body -> concatMap (nodeLiterals . snd) bindings <> exprLiterals body;
  GrinStoreRecUnchecked bindings body -> concatMap (nodeLiterals . snd) bindings <> exprLiterals body;
  GrinUpdate pointer value -> valueLiterals pointer <> valueLiterals value;
  GrinEval _ _ value -> valueLiterals value;
  GrinCpsEval _ _ value continuation -> valueLiterals value <> valueLiterals continuation;
  GrinFetch _ value -> valueLiterals value;
  GrinCall _ _ arguments -> concatMap valueLiterals arguments;
  GrinPrimitiveCall _ _ arguments -> concatMap valueLiterals arguments;
  GrinCpsPrimitiveCall _ _ arguments continuation -> concatMap valueLiterals arguments <> valueLiterals continuation;
  GrinApply _ function arguments -> valueLiterals function <> concatMap valueLiterals arguments;
  GrinForward -> [];
  GrinCpsApply _ function arguments continuation -> valueLiterals function <> concatMap valueLiterals arguments <> valueLiterals continuation;
  GrinContinue continuation values -> valueLiterals continuation <> concatMap valueLiterals values;
  GrinCpsRaise exception continuation -> valueLiterals exception <> valueLiterals continuation;
  GrinUpdateBlackhole pointer value -> valueLiterals pointer <> valueLiterals value;
  GrinHalt values -> concatMap valueLiterals values;
  GrinExit status -> valueLiterals status;
  GrinIfWhnf value ready slow -> valueLiterals value <> exprLiterals ready <> exprLiterals slow;
  GrinCase scrutinee _ alternatives -> valueLiterals scrutinee <> concatMap altLiterals alternatives;
  GrinThrow exception -> valueLiterals exception;
  GrinCatch _ action handler state -> valueLiterals action <> valueLiterals handler <> concatMap valueLiterals state;
  GrinForeignCallExpr _ arguments -> concatMap valueLiterals arguments};
  altLiterals alternative = altConLiterals (grinAltCon alternative) <> exprLiterals (grinAltRhs alternative);
  altConLiterals altCon = case altCon of {
  GrinLitAlt literal -> [literal];
  _ -> []};
  nodeLiterals = concatMap valueLiterals . grinNodeFields;
  valueLiterals value = case value of {
  GrinLitValue literal -> [literal];
  GrinVarValue {} -> [];
  GrinGlobalValue {} -> []}};
grinProgramGlobalReferences :: GrinProgram -> [Text];
grinProgramGlobalReferences program = concatMap (nodeReferences . grinGlobalNode) (grinGlobals program) <> concatMap (grinExprGlobalReferences . grinFunctionBody) (grinFunctions program);
grinExprGlobalReferences :: GrinExpr -> [Text];
grinExprGlobalReferences = exprReferences where {
  exprReferences expression = case expression of {
  GrinConstant values -> valuesReferences values;
  GrinBind _ valueExpression body -> exprReferences valueExpression <> exprReferences body;
  GrinStore node -> nodeReferences node;
  GrinEnsureHeap requiredWords roots -> valueReferences requiredWords <> valuesReferences roots;
  GrinStoreUnchecked node -> nodeReferences node;
  GrinStoreRec bindings body -> concatMap (nodeReferences . snd) bindings <> exprReferences body;
  GrinStoreRecUnchecked bindings body -> concatMap (nodeReferences . snd) bindings <> exprReferences body;
  GrinUpdate pointer value -> valueReferences pointer <> valueReferences value;
  GrinEval _ _ value -> valueReferences value;
  GrinCpsEval _ _ value continuation -> valuesReferences [value, continuation];
  GrinFetch _ value -> valueReferences value;
  GrinCall _ _ arguments -> valuesReferences arguments;
  GrinPrimitiveCall _ _ arguments -> valuesReferences arguments;
  GrinCpsPrimitiveCall _ _ arguments continuation -> valuesReferences arguments <> valueReferences continuation;
  GrinApply _ function arguments -> valueReferences function <> valuesReferences arguments;
  GrinForward -> [];
  GrinCpsApply _ function arguments continuation -> valueReferences function <> valuesReferences arguments <> valueReferences continuation;
  GrinContinue continuation values -> valueReferences continuation <> valuesReferences values;
  GrinCpsRaise exception continuation -> valueReferences exception <> valueReferences continuation;
  GrinUpdateBlackhole pointer value -> valueReferences pointer <> valueReferences value;
  GrinHalt values -> valuesReferences values;
  GrinExit status -> valueReferences status;
  GrinIfWhnf value ready slow -> valueReferences value <> exprReferences ready <> exprReferences slow;
  GrinCase scrutinee _ alternatives -> valueReferences scrutinee <> concatMap (exprReferences . grinAltRhs) alternatives;
  GrinThrow exception -> valueReferences exception;
  GrinCatch _ action handler state -> valuesReferences (action : handler : state);
  GrinForeignCallExpr _ arguments -> valuesReferences arguments};
  valuesReferences = concatMap valueReferences};
grinNodeGlobalReferences :: GrinNode -> [Text];
grinNodeGlobalReferences = nodeReferences;
nodeReferences :: GrinNode -> [Text];
nodeReferences = concatMap valueReferences . grinNodeFields;
grinNodeFunctionNames :: GrinNode -> [FunctionName];
grinNodeFunctionNames node = case grinNodeTag node of {
  GrinClosure name _ -> [name];
  GrinThunk name -> [name];
  GrinConstructor {} -> []};
grinExprFunctionNames :: GrinExpr -> [FunctionName];
grinExprFunctionNames = exprNames where {
  exprNames expression = case expression of {
  GrinCall _ name _ -> [name];
  GrinBind _ valueExpression body -> exprNames valueExpression <> exprNames body;
  GrinStore node -> grinNodeFunctionNames node;
  GrinFetch tag _ -> grinNodeFunctionNames (GrinNode tag []);
  GrinStoreUnchecked node -> grinNodeFunctionNames node;
  GrinStoreRec bindings body -> concatMap (grinNodeFunctionNames . snd) bindings <> exprNames body;
  GrinStoreRecUnchecked bindings body -> concatMap (grinNodeFunctionNames . snd) bindings <> exprNames body;
  GrinIfWhnf _ ready slow -> exprNames ready <> exprNames slow;
  GrinCase _ _ alternatives -> concatMap (exprNames . grinAltRhs) alternatives;
  _ -> []}};
grinNodeConstructorTags :: GrinNode -> [Text];
grinNodeConstructorTags = nodeTagNames;
grinExprConstructorTags :: GrinExpr -> [Text];
grinExprConstructorTags = exprTagNames;
valueReferences :: GrinValue -> [Text];
valueReferences value = case value of {
  GrinGlobalValue name -> [name];
  GrinVarValue {} -> [];
  GrinLitValue {} -> []};
grinVarNameNeedsNumber :: Text -> Bool;
grinVarNameNeedsNumber name = case T.uncons name of {
  Nothing -> False;
  Just ('\'', _) -> True;
  Just (character, rest) | character == '+' || character == '-' -> isIntegerShaped rest | otherwise -> isIntegerShaped name} where {
  isIntegerShaped digits = not (T.null digits) && T.all isDigit digits};
grinValueRuntimeRep :: GrinValue -> GrinRep;
grinValueRuntimeRep value = case value of {
  GrinVarValue var -> grinVarRuntimeRep var;
  GrinGlobalValue {} -> liftedGrinRep;
  GrinLitValue literal -> case literal of {
  GrinLitInt runtimeRep _ -> runtimeRep;
  GrinLitChar runtimeRep _ -> runtimeRep;
  GrinLitAddr {} -> AddrRep}};
isLiftedRuntimeRep :: GrinRep -> Bool;
isLiftedRuntimeRep runtimeRep = runtimeRep == liftedGrinRep;
runtimeRepComponents :: GrinRep -> [GrinRep];
runtimeRepComponents runtimeRep = case runtimeRep of {
  TupleRep fieldReps -> concatMap runtimeRepComponents fieldReps;
  SumRep alternatives -> IntRep : sumSlots (sumLayout alternatives);
  _ -> [runtimeRep]};
data SumLayout = SumLayout { sumSlots :: ![GrinRep], sumAlternativeSlots :: ![[Int]] } deriving (Eq, Show, Read);
sumLayout :: [GrinRep] -> SumLayout;
sumLayout alternatives = SumLayout slots (map positions components) where {
  components = map (map sumSlotRep . runtimeRepComponents) alternatives;
  counts = Map.unionsWith max (map (Map.fromListWith (+) . map (,1 :: Int)) components);
  slots = concat [replicate count representation | (representation, count) <- Map.toAscList counts];
  offsets = Map.fromListWith (flip (<>)) [(representation, [index]) | (index, representation) <- zip [0 ..] slots];
  positions = snd . mapAccumL select offsets;
  select available representation = case Map.findWithDefault [] representation available of {
  index : rest -> (Map.insert representation rest available, index);
  [] -> error "sum layout has no slot for an alternative component"}};
sumSlotRep :: GrinRep -> GrinRep;
sumSlotRep representation = case representation of {
  Int8Rep -> IntRep;
  Int16Rep -> IntRep;
  Int32Rep -> IntRep;
  Int64Rep -> IntRep;
  WordRep -> IntRep;
  Word8Rep -> IntRep;
  Word16Rep -> IntRep;
  Word32Rep -> IntRep;
  Word64Rep -> IntRep;
  _ -> representation};
isPointerRuntimeRep :: GrinRep -> Bool;
isPointerRuntimeRep runtimeRep = case runtimeRep of {
  BoxedRep {} -> True;
  _ -> False};
data GrinForeignCall = GrinForeignCall { grinForeignCallName :: !Text, grinForeignCallSymbol :: !Text, grinForeignCallTarget :: !GrinForeignTarget, grinForeignCallSignature :: !GrinForeignSignature } deriving (Eq, Show, Read);
data GrinForeignTarget = GrinForeignFunction | GrinForeignAddress | GrinForeignDynamic | GrinForeignUnsafeFunction | GrinForeignUnsafeDynamic | GrinForeignWrapper !GrinForeignSignature deriving (Eq, Show, Read);
data GrinForeignSignature = GrinForeignSignature { grinForeignArgumentTypes :: ![GrinForeignType], grinForeignResultType :: !GrinForeignType, grinForeignEffect :: !GrinForeignEffect } deriving (Eq, Show, Read);
data GrinForeignEffect = GrinForeignPure | GrinForeignRealWorld deriving (Eq, Show, Read);
data GrinForeignType = GrinForeignInt | GrinForeignInt8 | GrinForeignInt16 | GrinForeignInt32 | GrinForeignInt64 | GrinForeignWord | GrinForeignWord8 | GrinForeignWord16 | GrinForeignWord32 | GrinForeignWord64 | GrinForeignFloat | GrinForeignDouble | GrinForeignAddr | GrinForeignClosure | GrinForeignVoid deriving (Eq, Show, Read, Enum, Bounded);
grinForeignOperandReps :: GrinForeignSignature -> [GrinRep];
grinForeignOperandReps signature = map foreignTypeRuntimeRep (grinForeignArgumentTypes signature);
grinForeignCallResultReps :: GrinForeignSignature -> [GrinRep];
grinForeignCallResultReps signature = runtimeRepComponents (foreignTypeRuntimeRep (grinForeignResultType signature));
foreignTypeRuntimeRep :: GrinForeignType -> GrinRep;
foreignTypeRuntimeRep foreignType = case foreignType of {
  GrinForeignInt -> IntRep;
  GrinForeignInt8 -> Int8Rep;
  GrinForeignInt16 -> Int16Rep;
  GrinForeignInt32 -> Int32Rep;
  GrinForeignInt64 -> Int64Rep;
  GrinForeignWord -> WordRep;
  GrinForeignWord8 -> Word8Rep;
  GrinForeignWord16 -> Word16Rep;
  GrinForeignWord32 -> Word32Rep;
  GrinForeignWord64 -> Word64Rep;
  GrinForeignFloat -> FloatRep;
  GrinForeignDouble -> DoubleRep;
  GrinForeignAddr -> AddrRep;
  GrinForeignClosure -> liftedGrinRep;
  GrinForeignVoid -> TupleRep []};
forwardedResultUses :: GrinExpr -> Maybe [GrinValue];
forwardedResultUses expr = case expr of {
  GrinForward -> Just [];
  GrinBind [] (GrinPrimitiveCall (TupleRep []) "touch#" [owner]) body -> (owner :) <$> forwardedResultUses body;
  _ -> Nothing}
}
