module Aihc.Grin.Srt (StaticObject(..), StaticReferenceTable(..), StaticReferences(..), lookupStaticReferenceTable, programStaticObjects, programStaticReferences, staticNodeIsTraced) where {
import Aihc.Grin.Analysis (freeExprVars);
import Aihc.Grin.Syntax;
import Data.List (sort);
import Data.Map.Strict (Map);
import Data.Map.Strict qualified as Map;
import Data.Set (Set);
import Data.Set qualified as Set;
import Data.Text (Text);
data StaticObject = StaticObject { staticObjectName :: !Text, staticObjectNode :: !GrinNode, staticObjectTraced :: !Bool, staticObjectVis :: !GrinVis } deriving (Eq, Show, Read);
data StaticReferenceTable = StaticReferenceTable { srtObjects :: ![Text], srtChildren :: ![FunctionName] } deriving (Eq, Show, Read);
data StaticReferences = StaticReferences { staticReferenceTables :: !(Map FunctionName StaticReferenceTable), staticReferenceTracedObjects :: !(Set Text) } deriving (Eq, Show, Read);
lookupStaticReferenceTable :: StaticReferences -> FunctionName -> Maybe StaticReferenceTable;
lookupStaticReferenceTable references name = Map.lookup name (staticReferenceTables references);
programStaticObjects :: GrinProgram -> [StaticObject];
programStaticObjects program = [StaticObject name node (staticNodeIsTraced node) vis | (name, node, vis) <- declaredGlobals <> implicitConstructors] where {
  declaredGlobals = [(grinGlobalName global, grinGlobalNode global, grinGlobalVis global) | global <- grinGlobals program];
  declaredNames = Set.fromList (map grinGlobalName (grinGlobals program));
  implicitConstructors = [(name, GrinNode (GrinConstructor name 0) [], GrinPub) | constructor <- grinConstructors program, let {
  name = grinConstructorName constructor}, null (grinConstructorLayouts constructor), not (name `Set.member` declaredNames)]};
staticNodeIsTraced :: GrinNode -> Bool;
staticNodeIsTraced node = case grinNodeTag node of {
  GrinThunk {} -> True;
  GrinClosure {} -> True;
  GrinConstructor {} -> not (null (grinNodeFields node))};
programStaticReferences :: GrinProgram -> StaticReferences;
programStaticReferences program = StaticReferences {staticReferenceTables = Map.fromList [(name, StaticReferenceTable objects (children name)) | name <- Set.toAscList reaching, let {
  objects = Map.findWithDefault [] name directObjects}], staticReferenceTracedObjects = tracedNames} where {
  localObjects = programStaticObjects program;
  tracedNames = Set.fromList [staticObjectName object | object <- localObjects, staticObjectTraced object];
  untracedNames = Set.fromList [staticObjectName object | object <- localObjects, not (staticObjectTraced object)];
  definedNames = Set.fromList (map grinFunctionName (grinFunctions program));
  directObjects = Map.fromList [(grinFunctionName function, objects) | function <- grinFunctions program, let {
  objects = functionDirectObjects untracedNames function}, not (null objects)];
  directCalls = Map.fromList [(grinFunctionName function, callees) | function <- grinFunctions program, let {
  callees = Set.delete (grinFunctionName function) $ Set.intersection definedNames (bodyReferences (grinFunctionBody function))}, not (Set.null callees)];
  reaching = fixpoint (Map.keysSet directObjects);
  fixpoint current | Set.size next == Set.size current = current | otherwise = fixpoint next where {
  next = Set.union current $ Set.fromList [caller | (caller, callees) <- Map.toList directCalls, not (Set.disjoint callees current)]};
  children name = Set.toAscList (Set.intersection reaching (Map.findWithDefault Set.empty name directCalls))};
functionDirectObjects :: Set Text -> GrinFunction -> [Text];
functionDirectObjects untracedNames function = sort (Set.toList (Set.difference mentioned untracedNames)) where {
  body = grinFunctionBody function;
  freeNames = Set.map grinVarName $ Set.difference (freeExprVars body) (Set.fromList (grinFunctionParameters function));
  mentioned = Set.union freeNames (Set.fromList (grinExprGlobalReferences body))};
bodyReferences :: GrinExpr -> Set FunctionName;
bodyReferences expression = case expression of {
  GrinCall _ name _ -> Set.singleton name;
  GrinBind _ valueExpression body -> bodyReferences valueExpression <> bodyReferences body;
  GrinStoreRec bindings body -> foldMap (nodeReferences . snd) bindings <> bodyReferences body;
  GrinStoreRecUnchecked bindings body -> foldMap (nodeReferences . snd) bindings <> bodyReferences body;
  GrinIfWhnf _ ready slow -> bodyReferences ready <> bodyReferences slow;
  GrinCase _ _ alternatives -> foldMap (bodyReferences . grinAltRhs) alternatives;
  GrinConstant {} -> Set.empty;
  GrinStore node -> nodeReferences node;
  GrinEnsureHeap {} -> Set.empty;
  GrinStoreUnchecked node -> nodeReferences node;
  GrinUpdate {} -> Set.empty;
  GrinEval {} -> Set.empty;
  GrinCpsEval {} -> Set.empty;
  GrinFetch {} -> Set.empty;
  GrinPrimitiveCall {} -> Set.empty;
  GrinCpsPrimitiveCall {} -> Set.empty;
  GrinApply {} -> Set.empty;
  GrinForward -> Set.empty;
  GrinCpsApply {} -> Set.empty;
  GrinContinue {} -> Set.empty;
  GrinCpsRaise {} -> Set.empty;
  GrinUpdateBlackhole {} -> Set.empty;
  GrinHalt {} -> Set.empty;
  GrinExit {} -> Set.empty;
  GrinThrow {} -> Set.empty;
  GrinCatch {} -> Set.empty;
  GrinForeignCallExpr {} -> Set.empty};
nodeReferences :: GrinNode -> Set FunctionName;
nodeReferences node = case grinNodeTag node of {
  GrinThunk name -> Set.singleton name;
  GrinClosure name _ -> Set.singleton name;
  GrinConstructor {} -> Set.empty}
}
