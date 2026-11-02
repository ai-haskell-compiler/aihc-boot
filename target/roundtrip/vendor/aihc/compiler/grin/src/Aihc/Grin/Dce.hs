module Aihc.Grin.Dce (dceGrinProgram, sweptGrinProgram) where {
import Aihc.Grin.Analysis (freeExprVars);
import Aihc.Grin.Syntax;
import Data.List (sort);
import Data.Map.Strict (Map);
import Data.Map.Strict qualified as Map;
import Data.Set (Set);
import Data.Set qualified as Set;
import Data.Text (Text);
import Data.Text qualified as T;
data Key = GlobalKey !Text | FunctionKey !FunctionName | ConstructorKey !Text deriving (Eq, Ord, Show);
sweptGrinProgram :: GrinProgram -> Either String GrinProgram;
sweptGrinProgram program | null dangling = Right swept | otherwise = Left ("GRIN dead-code elimination dropped definitions that the program still names: " <> T.unpack (T.intercalate ", " dangling)) where {
  swept = dceGrinProgram program;
  droppedGlobals = Set.difference (definedGlobals program) (definedGlobals swept);
  droppedConstructors = Set.difference (definedConstructors program) (definedConstructors swept);
  definedGlobals = Set.fromList . map grinGlobalName . grinGlobals;
  definedConstructors = Set.fromList . map grinConstructorName . grinConstructors;
  droppedFunctions = Set.difference (definedFunctions program) (definedFunctions swept);
  definedFunctions = Set.fromList . map grinFunctionName . grinFunctions;
  dangling = sort (Set.toList (Set.intersection droppedGlobals (Set.fromList (grinProgramGlobalReferences swept))) <> Set.toList (Set.intersection droppedConstructors (Set.fromList (referencedConstructors <> grinProgramGlobalReferences swept))) <> map unFunctionName (Set.toList (Set.intersection droppedFunctions (Set.fromList referencedFunctions))));
  referencedConstructors = concatMap (grinNodeConstructorTags . grinGlobalNode) (grinGlobals swept) <> concatMap (grinExprConstructorTags . grinFunctionBody) (grinFunctions swept);
  referencedFunctions = concatMap (grinNodeFunctionNames . grinGlobalNode) (grinGlobals swept) <> concatMap (grinExprFunctionNames . grinFunctionBody) (grinFunctions swept)};
dceGrinProgram :: GrinProgram -> GrinProgram;
dceGrinProgram program = program {grinGlobals = filter (reaches . GlobalKey . grinGlobalName) (grinGlobals program), grinFunctions = filter (reaches . FunctionKey . grinFunctionName) (grinFunctions program), grinConstructors = filter (reaches . ConstructorKey . grinConstructorName) (grinConstructors program)} where {
  reaches key = Set.member key reachable;
  reachable = close Set.empty (map GlobalKey (grinGlobalRoots program) <> map ConstructorKey (grinConstructorRoots program));
  close :: Set Key -> [Key] -> Set Key;
  close visited pending = case pending of {
  [] -> visited;
  key : rest | Set.member key visited -> close visited rest | otherwise -> close (Set.insert key visited) (Map.findWithDefault [] key references <> rest)};
  references :: Map Key [Key];
  references = Map.fromListWith (<>) ([(GlobalKey (grinGlobalName global), concatMap staticKeys (grinNodeGlobalReferences node) <> map FunctionKey (grinNodeFunctionNames node) <> map ConstructorKey (grinNodeConstructorTags node)) | global <- grinGlobals program, let {
  node = grinGlobalNode global}] <> [(FunctionKey (grinFunctionName function), concatMap staticKeys (bodyGlobals function) <> map FunctionKey (grinExprFunctionNames body) <> map ConstructorKey (grinExprConstructorTags body)) | function <- grinFunctions program, let {
  body = grinFunctionBody function}]);
  staticKeys name = [GlobalKey name, ConstructorKey name];
  bodyGlobals function = grinExprGlobalReferences body <> [grinVarName var | var <- Set.toList (Set.difference (freeExprVars body) parameters)] where {
  body = grinFunctionBody function;
  parameters = Set.fromList (grinFunctionParameters function)}}
}
