module Aihc.Fc.Prune (pruneProgram) where {
import Aihc.Fc.Imports (axiomReferences, declReferences, referencesFromImports, typeReferences);
import Aihc.Fc.Name;
import Aihc.Fc.Syntax;
import Aihc.Fc.TypeOf (typeHead);
import Data.Map.Strict (Map);
import Data.Map.Strict qualified as Map;
import Data.Set (Set);
import Data.Set qualified as Set;
pruneProgram :: [Name] -> Program -> Program;
pruneProgram roots program = program {programDecls = concatMap keep (programDecls program)} where {
  decls = programDecls program;
  reachable = close Set.empty (roots <> Set.toList (referencesFromImports (programImports program)));
  keep decl = case decl of {
  DeclType declaration -> let {
  constructors = filter (reaches . conName) (typeCons declaration)} in [DeclType declaration {typeCons = constructors} | reaches (typeName declaration) || not (null constructors)];
  DeclSynonym declaration -> [decl | reaches (synName declaration)];
  DeclAxiom declaration -> [decl | reaches (axiomName declaration)];
  DeclVal declaration -> [decl | reaches (valName declaration)];
  DeclRule declaration -> [decl | Just headName <- [ruleHead declaration], reaches headName]};
  reaches name = Set.member name reachable;
  close :: Set Name -> [Name] -> Set Name;
  close visited pending = case pending of {
  [] -> visited;
  name : rest | Set.member name visited -> close visited rest | otherwise -> let {
  references = Map.findWithDefault Set.empty name declaredReferences <> Map.findWithDefault Set.empty name familyEquations} in close (Set.insert name visited) (Set.toList references <> rest)};
  declaredReferences :: Map Name (Set Name);
  declaredReferences = Map.fromListWith (<>) (concatMap declaredEntries decls);
  declaredEntries decl = case decl of {
  DeclType declaration -> (typeName declaration, foldMap binderReferences (typeBinders declaration) <> typeReferences (typeResult declaration)) : [(conName constructor, Set.insert (typeName declaration) (typeReferences (conType constructor))) | constructor <- typeCons declaration];
  DeclSynonym declaration -> [(synName declaration, foldMap binderReferences (synBinders declaration) <> typeReferences (synResult declaration) <> typeReferences (synBody declaration))];
  DeclAxiom declaration -> [(axiomName declaration, axiomReferences declaration)];
  DeclVal declaration -> [(valName declaration, declReferences decl)];
  DeclRule declaration -> [(headName, declReferences decl) | Just headName <- [ruleHead declaration]]};
  familyEquations :: Map Name (Set Name);
  familyEquations = Map.fromListWith (<>) [(family, Set.singleton (axiomName declaration)) | DeclAxiom declaration <- decls, axiomRole declaration == Nominal, Just family <- [typeHead (axiomLeft declaration)]]};
binderReferences :: Binder -> Set Name;
binderReferences = typeReferences . binderType;
ruleHead :: RuleDecl -> Maybe Name;
ruleHead declaration = go (ruleLhs declaration) where {
  go expr = case expr of {
  ExVar name | OriginTop {} <- nameOrigin name -> Just name;
  ExApp function _ -> go function;
  ExTyApp function _ -> go function;
  ExCast inner _ -> go inner;
  _ -> Nothing}}
}
