module Aihc.PackagePlan.Solver (SolverInputs(..), Candidate(..), CandidateSource(..), SolverConfig(..), Stanzas(..), noStanzas, Constraint(..), Preference(..), Solution, Assignment(..), SolveFailure(..), CandidateFailure(..), Rejection(..), Dependent(..), renderSolveFailure, solve, verifySolution, candidateDependencies, searchableFlags, unknownBuildTools) where {
import Aihc.Hackage.Cabal (BuildContext(..), collectCondTreeData, conditionEvaluatorIn, targetFlagOverrides);
import Aihc.Hackage.Preprocessor (Preprocessor, preprocessorToolName);
import Control.Monad (foldM);
import Control.Monad.Trans.Class (lift);
import Control.Monad.Trans.State.Strict (StateT, evalStateT, get, gets, modify', put);
import Data.List (intercalate, sortBy, sortOn, subsequences);
import Data.Map.Strict (Map);
import Data.Map.Strict qualified as Map;
import Data.Maybe (catMaybes, fromMaybe);
import Data.Ord (Down(..), comparing);
import Data.Set qualified as Set;
import Distribution.Package (PackageName, packageName, unPackageName);
import Distribution.PackageDescription (BuildInfo, Executable, FlagName, Library, benchmarkBuildInfo, buildInfo, buildToolDepends, buildTools, buildable, condBenchmarks, condExecutables, condLibrary, condSubLibraries, condTestSuites, flagDefault, flagManual, flagName, libBuildInfo, packageDescription, testBuildInfo, unFlagName);
import Distribution.Pretty (prettyShow);
import Distribution.System (Arch, OS);
import Distribution.Types.BuildInfo (targetBuildDepends);
import Distribution.Types.CondTree (CondBranch(..), CondTree(..));
import Distribution.Types.Condition (Condition(..));
import Distribution.Types.ConfVar (ConfVar(..));
import Distribution.Types.Dependency (Dependency(..));
import Distribution.Types.ExeDependency (ExeDependency(..));
import Distribution.Types.Flag (FlagAssignment, lookupFlagAssignment, mkFlagAssignment, unFlagAssignment);
import Distribution.Types.GenericPackageDescription (GenericPackageDescription, genPackageFlags);
import Distribution.Types.LegacyExeDependency (LegacyExeDependency(..));
import Distribution.Types.UnqualComponentName (unUnqualComponentName);
import Distribution.Types.Version (Version);
import Distribution.Types.VersionRange (VersionRange, anyVersion, intersectVersionRanges, withinRange);
import Distribution.Version (simplifyVersionRange);
data CandidateSource = CandidateHackage | CandidateLocal FilePath | CandidateCore FilePath deriving (Eq, Ord, Show);
data Candidate = Candidate { candidateName :: !PackageName, candidateVersion :: !Version, candidateRevision :: !Int, candidateDeprecated :: !Bool, candidateSource :: !CandidateSource } deriving (Eq, Ord, Show);
data SolverInputs m = SolverInputs { inputsCandidates :: PackageName -> Maybe Preference -> m [Candidate], inputsDescription :: Candidate -> m GenericPackageDescription };
data Stanzas = Stanzas { stanzasTests :: !Bool, stanzasBenchmarks :: !Bool } deriving (Eq, Show);
noStanzas :: Stanzas;
noStanzas = Stanzas False False;
data Constraint = ConstraintVersion !PackageName !VersionRange | ConstraintFlag !PackageName !FlagName !Bool deriving (Eq, Show);
data Preference = Preference { preferredVersion :: !Version, preferredRevision :: !(Maybe Int), preferredFlags :: !FlagAssignment } deriving (Eq, Show);
data SolverConfig = SolverConfig { configPlatform :: !(OS, Arch), configAliases :: !(Map PackageName PackageName), configConstraints :: ![Constraint], configPreferences :: !(Map PackageName Preference), configRoots :: !(Map PackageName Stanzas), configGoals :: ![(PackageName, VersionRange)], configMaxBacktracks :: !Int };
data Assignment = Assignment { assignmentVersion :: !Version, assignmentRevision :: !Int, assignmentFlags :: !FlagAssignment, assignmentSource :: !CandidateSource, assignmentDependencies :: !(Map PackageName VersionRange) } deriving (Eq, Show);
type Solution = Map PackageName Assignment;
data Dependent = DependentRoot | DependentConstraint | DependentPackage !PackageName !Version !FlagAssignment deriving (Eq, Show);
data Rejection = RejectedDependency !PackageName !VersionRange !Version | RejectedSubtree !SolveFailure deriving (Eq, Show);
data CandidateFailure = CandidateFailure { failedCandidate :: !Candidate, failedFlags :: !FlagAssignment, failedRejection :: !Rejection } deriving (Eq, Show);
data SolveFailure = UnknownPackage !PackageName ![(Dependent, VersionRange)] | NoCandidates !PackageName !VersionRange ![(Dependent, VersionRange)] ![Version] ![CandidateFailure] | BacktrackLimit !Int !SolveFailure deriving (Eq, Show);
renderSolveFailure :: SolveFailure -> String;
renderSolveFailure = intercalate "\n" . render where {
  render failure = case failure of {
  BacktrackLimit limit inner -> ("Gave up after " <> show limit <> " backtracks; the last failure was:") : render inner;
  NoCandidates _ _ _ _ [CandidateFailure _ _ (RejectedSubtree inner)] -> render inner;
  UnknownPackage name dependents -> ["Unknown package " <> unPackageName name <> ", needed by " <> renderDependents dependents];
  NoCandidates name range dependents outside failures -> (if range == anyVersion then "Every version of " <> unPackageName name <> " was rejected; it is needed by " <> renderDependents dependents else "No version of " <> unPackageName name <> " satisfies " <> prettyShow range <> ", needed by " <> renderDependents dependents) : ["  versions outside the range: " <> intercalate ", " (map prettyShow (take listedVersions outside)) <> more | not (null outside), let {
  more = if length outside > listedVersions then ", and " <> show (length outside - listedVersions) <> " more" else ""}] <> concatMap renderCandidate failures};
  renderCandidate (CandidateFailure candidate flags rejection) = let {
  label = "  " <> renderCandidateName candidate flags} in case rejection of {
  RejectedDependency dependency range assigned -> [label <> " needs " <> unPackageName dependency <> " " <> prettyShow range <> ", but " <> unPackageName dependency <> "-" <> prettyShow assigned <> " is chosen"];
  RejectedSubtree inner -> (label <> " was tried, and then:") : map ("    " <>) (render inner)};
  renderDependents dependents = intercalate ", " [renderDependent dependent <> rangeSuffix range | (dependent, range) <- dependents];
  rangeSuffix range | range == anyVersion = "" | otherwise = " (" <> prettyShow range <> ")";
  renderDependent dependent = case dependent of {
  DependentRoot -> "the root";
  DependentConstraint -> "a constraint";
  DependentPackage name version flags -> renderCandidateName (Candidate name version 0 False CandidateHackage) flags};
  renderCandidateName candidate flags = unPackageName (candidateName candidate) <> "-" <> prettyShow (candidateVersion candidate) <> concat [" " <> (if value then "+" else "-") <> unFlagName flag | (flag, value) <- unFlagAssignment flags];
  listedVersions = 8 :: Int};
data Goal = Goal { goalRange :: !VersionRange, goalDependents :: ![(Dependent, VersionRange)] };
data Search = Search { searchAssigned :: !(Map PackageName Assignment), searchGoals :: !(Map PackageName Goal) };
type ConflictSet = Set.Set PackageName;
data Failure = Failure { failureConflict :: !ConflictSet, failureReason :: !SolveFailure };
dependentNames :: Goal -> ConflictSet;
dependentNames goal = Set.fromList [name | (DependentPackage name _ _, _) <- goalDependents goal];
data Memo = Memo { memoCandidates :: !(Map PackageName [Candidate]), memoDescriptions :: !(Map (PackageName, Version, Int, CandidateSource) GenericPackageDescription), memoBacktracks :: !Int };
type Solve m = StateT Memo m;
solve :: (Monad m) => SolverInputs m -> SolverConfig -> m (Either SolveFailure Solution);
solve inputs config = either (Left . failureReason) Right <$> evalStateT (search inputs config initial) (Memo Map.empty Map.empty 0) where {
  initial = Search {searchAssigned = Map.empty, searchGoals = foldr (\(name, range) -> addGoal config name DependentRoot range) Map.empty ([(alias config name, anyVersion) | name <- Map.keys (configRoots config)] <> [(alias config name, range) | (name, range) <- configGoals config])}};
alias :: SolverConfig -> PackageName -> PackageName;
alias config name = fromMaybe name (Map.lookup name (configAliases config));
constraintRange :: SolverConfig -> PackageName -> VersionRange;
constraintRange config name = simplifyVersionRange (foldr intersectVersionRanges anyVersion [range | ConstraintVersion constrained range <- configConstraints config, alias config constrained == name]);
constrainedFlags :: SolverConfig -> PackageName -> Map FlagName Bool;
constrainedFlags config name = Map.fromList [(flag, value) | ConstraintFlag constrained flag value <- configConstraints config, alias config constrained == name];
addGoal :: SolverConfig -> PackageName -> Dependent -> VersionRange -> Map PackageName Goal -> Map PackageName Goal;
addGoal config name dependent range goals = Map.insert name narrowed goals where {
  narrowed = case Map.lookup name goals of {
  Nothing -> Goal {goalRange = simplifyVersionRange (intersectVersionRanges (constraintRange config name) range), goalDependents = [(dependent, range)] <> [(DependentConstraint, constraintRange config name) | constraintRange config name /= anyVersion]};
  Just goal -> Goal {goalRange = simplifyVersionRange (intersectVersionRanges (goalRange goal) range), goalDependents = goalDependents goal <> [(dependent, range)]}}};
candidatesOf :: (Monad m) => SolverInputs m -> SolverConfig -> PackageName -> Solve m [Candidate];
candidatesOf inputs config name = do {
  known <- gets (Map.lookup name . memoCandidates);
  case known of {
  Just candidates -> pure candidates;
  Nothing -> do {
  candidates <- lift (inputsCandidates inputs name (Map.lookup name (configPreferences config)));
  modify' (\memo -> memo {memoCandidates = Map.insert name candidates (memoCandidates memo)});
  pure candidates}}};
descriptionOf :: (Monad m) => SolverInputs m -> Candidate -> Solve m GenericPackageDescription;
descriptionOf inputs candidate = do {
  let {
  key = (candidateName candidate, candidateVersion candidate, candidateRevision candidate, candidateSource candidate)};
  known <- gets (Map.lookup key . memoDescriptions);
  case known of {
  Just gpd -> pure gpd;
  Nothing -> do {
  gpd <- lift (inputsDescription inputs candidate);
  modify' (\memo -> memo {memoDescriptions = Map.insert key gpd (memoDescriptions memo)});
  pure gpd}}};
orderedCandidates :: SolverConfig -> PackageName -> [Candidate] -> [Candidate];
orderedCandidates config name = sortBy (comparing preferredFirst <> comparing candidateDeprecated <> comparing (Down . candidateVersion)) where {
  preferredFirst candidate = case Map.lookup name (configPreferences config) of {
  Just preference | preferredVersion preference == candidateVersion candidate -> False;
  _ -> True}};
search :: (Monad m) => SolverInputs m -> SolverConfig -> Search -> Solve m (Either Failure Solution);
search inputs config state = do {
  goals <- mapM (\(name, goal) -> (,) (name, goal) <$> goalCandidates name goal) (Map.toAscList (searchGoals state));
  case sortOn (length . snd) goals of {
  [] -> pure (Right (searchAssigned state));
  ((name, goal), candidates) : _ -> do {
  allCandidates <- candidatesOf inputs config name;
  if null allCandidates then failWith (dependentNames goal) (UnknownPackage name (goalDependents goal)) else do {
  let {
  outside = sortOn Down [candidateVersion candidate | candidate <- allCandidates, not (withinRange (candidateVersion candidate) (goalRange goal))]};
  tryCandidates name goal outside candidates [] Set.empty}}}} where {
  goalCandidates name goal = do {
  candidates <- candidatesOf inputs config name;
  pure (orderedCandidates config name [candidate | candidate <- candidates, withinRange (candidateVersion candidate) (goalRange goal)])};
  failWith conflict failure = do {
  memo <- get;
  let {
  backtracks = memoBacktracks memo + 1};
  put memo {memoBacktracks = backtracks};
  pure (Left (Failure conflict (if backtracks > configMaxBacktracks config then BacktrackLimit (configMaxBacktracks config) failure else failure)))};
  tryCandidates name goal outside [] failures conflicts = failWith (Set.delete name (Set.union conflicts (dependentNames goal))) (NoCandidates name (goalRange goal) (goalDependents goal) outside (reverse failures));
  tryCandidates name goal outside (candidate : rest) failures conflicts = do {
  gpd <- descriptionOf inputs candidate;
  let {
  root = Map.lookup name (configRoots config);
  flagChoices = flagAssignments config name gpd root};
  tryFlags name goal outside candidate gpd root flagChoices rest failures conflicts};
  tryFlags name goal outside _ _ _ [] rest failures conflicts = tryCandidates name goal outside rest failures conflicts;
  tryFlags name goal outside candidate gpd root (flags : moreFlags) rest failures conflicts = do {
  let {
  dependencies = candidateDependencies (configPlatform config) (configAliases config) flags root gpd;
  conflicting = [(dependency, RejectedDependency dependency range (assignmentVersion assigned)) | (dependency, range) <- Map.toAscList dependencies, Just assigned <- [Map.lookup dependency (searchAssigned state)], not (withinRange (assignmentVersion assigned) range)]};
  case conflicting of {
  (dependency, rejection) : _ -> tryFlags name goal outside candidate gpd root moreFlags rest (CandidateFailure candidate flags rejection : failures) (Set.insert name (Set.insert dependency conflicts));
  [] -> do {
  let {
  assignment = Assignment {assignmentVersion = candidateVersion candidate, assignmentRevision = candidateRevision candidate, assignmentFlags = flags, assignmentSource = candidateSource candidate, assignmentDependencies = dependencies};
  dependent = DependentPackage name (candidateVersion candidate) flags;
  goals' = foldr (\(dependency, range) -> addGoal config dependency dependent range) (Map.delete name (searchGoals state)) [(dependency, range) | (dependency, range) <- Map.toAscList dependencies, not (Map.member dependency (searchAssigned state))];
  state' = Search {searchAssigned = Map.insert name assignment (searchAssigned state), searchGoals = goals'}};
  result <- search inputs config state';
  case result of {
  Right solution -> pure (Right solution);
  Left failure | BacktrackLimit _ _ <- failureReason failure -> pure (Left failure);
  Left failure | not (Set.member name (failureConflict failure)) -> pure (Left failure);
  Left failure -> tryFlags name goal outside candidate gpd root moreFlags rest (CandidateFailure candidate flags (RejectedSubtree (failureReason failure)) : failures) (Set.union conflicts (failureConflict failure))}}}}};
flagAssignments :: SolverConfig -> PackageName -> GenericPackageDescription -> Maybe Stanzas -> [FlagAssignment];
flagAssignments config name gpd root = dedupe (preferred <> [mkFlagAssignment (Map.toAscList (Map.union fixed (Map.fromList flips))) | flips <- flipOrders]) where {
  fixed = Map.union (constrainedFlags config name) (Map.fromList (targetFlagOverrides (snd (configPlatform config)) (packageName (packageDescription gpd))));
  searched = [flag | flag <- searchableFlags (configPlatform config) root gpd, not (Map.member flag fixed)];
  defaults = Map.fromList [(flagName flag, flagDefault flag) | flag <- genPackageFlags gpd];
  flipOrders = [[(flag, if flag `elem` flipped then not (defaults Map.! flag) else defaults Map.! flag) | flag <- searched] | flipped <- sortOn length (subsequences searched)];
  preferred = case Map.lookup name (configPreferences config) of {
  Just preference | any ((`elem` searched) . fst) (unFlagAssignment (preferredFlags preference)) -> [mkFlagAssignment (Map.toAscList (Map.union fixed (Map.fromList [(flag, fromMaybe (defaults Map.! flag) (lookupFlagAssignment flag (preferredFlags preference))) | flag <- searched])))];
  _ -> []};
  dedupe = go Set.empty where {
  go _ [] = [];
  go seen (assignment : rest) | Set.member (unFlagAssignment assignment) seen = go seen rest | otherwise = assignment : go (Set.insert (unFlagAssignment assignment) seen) rest}};
searchableFlags :: (OS, Arch) -> Maybe Stanzas -> GenericPackageDescription -> [FlagName];
searchableFlags _ root gpd = [flagName flag | flag <- genPackageFlags gpd, not (flagManual flag), flagName flag `Set.member` guarding] where {
  guarding = Set.unions (map (guardingFlags libBuildInfo) (libraryTrees gpd) <> concat [map (guardingFlags buildInfo) (executableTrees gpd) <> [guardingFlags testBuildInfo tree | stanzasTests stanzas, tree <- map snd (condTestSuites gpd)] <> [guardingFlags benchmarkBuildInfo tree | stanzasBenchmarks stanzas, tree <- map snd (condBenchmarks gpd)] | Just stanzas <- [root]])};
guardingFlags :: (a -> BuildInfo) -> CondTree ConfVar c a -> Set.Set FlagName;
guardingFlags toBuildInfo tree = Set.unions (map branch (condTreeComponents tree)) where {
  branch (CondBranch condition thenTree elseTree) = Set.unions ([conditionFlags condition | hasDependencies thenTree || maybe False hasDependencies elseTree] <> [guardingFlags toBuildInfo thenTree] <> [guardingFlags toBuildInfo subtree | Just subtree <- [elseTree]]);
  hasDependencies subtree = not (null (targetBuildDepends (toBuildInfo (condTreeData subtree)))) || any (\(CondBranch _ t e) -> hasDependencies t || maybe False hasDependencies e) (condTreeComponents subtree)};
conditionFlags :: Condition ConfVar -> Set.Set FlagName;
conditionFlags condition = case condition of {
  Var (PackageFlag flag) -> Set.singleton flag;
  Var _ -> Set.empty;
  Lit _ -> Set.empty;
  CNot inner -> conditionFlags inner;
  COr left right -> Set.union (conditionFlags left) (conditionFlags right);
  CAnd left right -> Set.union (conditionFlags left) (conditionFlags right)};
libraryTrees :: GenericPackageDescription -> [CondTree ConfVar [Dependency] Library];
libraryTrees gpd = maybe [] pure (condLibrary gpd) <> map snd (condSubLibraries gpd);
executableTrees :: GenericPackageDescription -> [CondTree ConfVar [Dependency] Executable];
executableTrees gpd = map snd (condExecutables gpd);
contributingBuildInfos :: (OS, Arch) -> FlagAssignment -> Maybe Stanzas -> GenericPackageDescription -> [BuildInfo];
contributingBuildInfos (os, arch) flags root gpd = filter buildable (map (merged libBuildInfo) (libraryTrees gpd) <> concat [map (merged buildInfo) (executableTrees gpd) <> [merged testBuildInfo tree | stanzasTests stanzas, tree <- map snd (condTestSuites gpd)] <> [merged benchmarkBuildInfo tree | stanzasBenchmarks stanzas, tree <- map snd (condBenchmarks gpd)] | Just stanzas <- [root]]) where {
  evalCond = conditionEvaluatorIn (BuildContext os arch flags) gpd;
  merged :: (a -> BuildInfo) -> CondTree ConfVar c a -> BuildInfo;
  merged toBuildInfo tree = mconcat (map toBuildInfo (collectCondTreeData evalCond tree))};
candidateDependencies :: (OS, Arch) -> Map PackageName PackageName -> FlagAssignment -> Maybe Stanzas -> GenericPackageDescription -> Map PackageName VersionRange;
candidateDependencies platform aliases flags root gpd = Map.map simplifyVersionRange (Map.fromListWith intersectVersionRanges [(resolved, range) | build <- contributingBuildInfos platform flags root gpd, Dependency name range _ <- targetBuildDepends build, let {
  resolved = fromMaybe name (Map.lookup name aliases)}, resolved /= self]) where {
  self = fromMaybe (packageName (packageDescription gpd)) (Map.lookup (packageName (packageDescription gpd)) aliases)};
unknownBuildTools :: (OS, Arch) -> FlagAssignment -> Maybe Stanzas -> GenericPackageDescription -> [String];
unknownBuildTools platform flags root gpd = Set.toAscList (Set.fromList [tool | build <- contributingBuildInfos platform flags root gpd, tool <- [unUnqualComponentName exe | ExeDependency _ exe _ <- buildToolDepends build] <> [tool | LegacyExeDependency tool _ <- buildTools build], tool `notElem` knownTools]) where {
  knownTools = map preprocessorToolName [minBound .. maxBound :: Preprocessor]};
verifySolution :: (Monad m) => SolverInputs m -> SolverConfig -> Map PackageName (Version, Maybe Int, FlagAssignment) -> m (Either String Solution);
verifySolution inputs config recorded = do {
  candidates <- foldM candidateFor (Right Map.empty) (Map.toAscList recorded);
  case candidates of {
  Left problem -> pure (Left problem);
  Right chosen -> do {
  solution <- Map.traverseWithKey assignmentFor chosen;
  pure (checks solution)}}} where {
  candidateFor acc (name, (version, revision, flags)) = case acc of {
  Left problem -> pure (Left problem);
  Right chosen -> do {
  available <- inputsCandidates inputs name (Just (Preference version revision flags));
  case [candidate | candidate <- available, candidateVersion candidate == version] of {
  candidate : _ | maybe True (== candidateRevision candidate) revision -> pure (Right (Map.insert name (candidate, flags) chosen)) | otherwise -> pure (Left (unPackageName name <> "-" <> prettyShow version <> " has no revision " <> maybe "" show revision));
  [] -> pure (Left ("no candidate " <> unPackageName name <> "-" <> prettyShow version))}}};
  assignmentFor name (candidate, flags) = do {
  gpd <- inputsDescription inputs candidate;
  let {
  root = Map.lookup name (configRoots config);
  decided = Map.union (constrainedFlags config name) (Map.fromList (targetFlagOverrides (snd (configPlatform config)) (packageName (packageDescription gpd))));
  full = mkFlagAssignment (Map.toAscList (Map.union decided (Map.fromList (unFlagAssignment flags))))};
  pure Assignment {assignmentVersion = candidateVersion candidate, assignmentRevision = candidateRevision candidate, assignmentFlags = full, assignmentSource = candidateSource candidate, assignmentDependencies = candidateDependencies (configPlatform config) (configAliases config) full root gpd}};
  checks solution = case catMaybes (rootChecks solution <> constraintChecks solution <> dependencyChecks solution <> reachabilityChecks solution) of {
  problem : _ -> Left problem;
  [] -> Right solution};
  rootChecks solution = [satisfied solution "the root" name range | (name, range) <- [(alias config root, anyVersion) | root <- Map.keys (configRoots config)] <> [(alias config name, range) | (name, range) <- configGoals config]];
  constraintChecks solution = [satisfied solution "a constraint" (alias config name) range | ConstraintVersion name range <- configConstraints config] <> [if lookupFlagAssignment flag (assignmentFlags assignment) == Just value then Nothing else Just ("the constraint on the flag " <> unFlagName flag <> " of " <> unPackageName name <> " is not met") | ConstraintFlag name flag value <- configConstraints config, Just assignment <- [Map.lookup (alias config name) solution]];
  dependencyChecks solution = [satisfied solution (unPackageName name <> "-" <> prettyShow (assignmentVersion assignment)) dependency range | (name, assignment) <- Map.toAscList solution, (dependency, range) <- Map.toAscList (assignmentDependencies assignment)];
  reachabilityChecks solution = [Just (unPackageName name <> " is no longer needed") | name <- Map.keys solution, not (Set.member name (reachable solution))];
  reachable solution = go Set.empty (map (alias config) (Map.keys (configRoots config)) <> map (alias config . fst) (configGoals config)) where {
  go seen [] = seen;
  go seen (name : rest) | Set.member name seen = go seen rest | otherwise = go (Set.insert name seen) (maybe [] (Map.keys . assignmentDependencies) (Map.lookup name solution) <> rest)};
  satisfied solution who name range = case Map.lookup name solution of {
  Nothing -> Just (who <> " needs " <> unPackageName name <> ", which is not listed");
  Just assignment | withinRange (assignmentVersion assignment) range -> Nothing | otherwise -> Just (who <> " needs " <> unPackageName name <> " " <> prettyShow range <> ", but " <> prettyShow (assignmentVersion assignment) <> " is listed")}}
}
