{-# LANGUAGE OverloadedStrings #-}
module Aihc.Cli.Build (build, runBuild) where {
import Aihc.Cli.BuildModule (ExecutableInputs(..), InstalledPackage(..), finishExecutable, generatedEntryText, installedPackage, plannedPackage, requirePackageArchive, runBuildModule, validateSelectedPackageNames);
import Aihc.Cli.CompilerHeaders (ensureCompilerHeaders);
import Aihc.Cli.Install (InstallLocations(..), ModuleCompileConfig(..), ModuleCompileRequest(..), buildEnvironmentIdentity, cabalPlatformForTarget, capiStubOptions, compileModules, compilePackageCFiles, defaultBuildRoot, dependencyIncludeDirs, installPlanPackages, installTargetRoot, planRequestFor);
import Aihc.Cli.OptimizationPlan (OptimizationPlan(..), optimizationPlan);
import Aihc.Cli.Options (BuildOptions(..));
import Aihc.Cli.PackageManifest (PackageManifest(..));
import Aihc.Cli.Store (defaultStoreRoot);
import Aihc.Hackage.Cabal (ExecutableInfo(..));
import Aihc.Hackage.Cabal qualified as HackageCabal;
import Aihc.Hackage.IndexCache (defaultIndexOptions, newHackageIndex);
import Aihc.Native (NativeTarget(..), nativeTargetStoreDirectory);
import Aihc.PackagePlan (PackagePlan(..), PlanOrigin(..), PlanRequest(..), PlannedPackages(..), planBuildContext, planPackages);
import Aihc.Resolve (Package(..), PackageId(..));
import Control.Monad (forM, when);
import Data.List (nub);
import Data.Maybe (fromMaybe);
import Data.Text (Text);
import Data.Text qualified as T;
import Data.Text.IO qualified as TIO;
import Distribution.Package (mkPackageName, unPackageName);
import Distribution.Types.Dependency (depPkgName);
import System.Directory (canonicalizePath, createDirectoryIfMissing, doesFileExist, getCurrentDirectory);
import System.FilePath (takeDirectory, (<.>), (</>));
runBuild :: BuildOptions -> IO ();
runBuild options = do {
  outputs <- build options;
  let {
  label = if buildNoLink options then "bundle: " else "executable: "};
  mapM_ (putStrLn . (label <>)) outputs};
build :: BuildOptions -> IO [FilePath];
build options = do {
  isFile <- doesFileExist (buildInput options);
  if isFile then pure <$> runBuildModule options else buildPackage options};
buildPackage :: BuildOptions -> IO [FilePath];
buildPackage options = do {
  storeRoot <- maybe defaultStoreRoot pure (buildStoreRoot options);
  currentDirectory <- getCurrentDirectory;
  hackageIndex <- newHackageIndex defaultIndexOptions;
  (rootPackage, origin, lockDirectory) <- installTargetRoot (buildInput options);
  let {
  target = buildTarget options;
  targetDirectory = nativeTargetStoreDirectory target;
  (os, arch) = cabalPlatformForTarget target;
  verbose message = when (buildVerbose options) (putStrLn message)};
  request <- planRequestFor hackageIndex (buildPlanOptions options) (os, arch) (maybe [] pure (buildWorkspace options)) lockDirectory verbose;
  planned <- planPackages request {requestRoots = [rootPackage]};
  rootPlan <- case plannedRoots planned of {
  [plan] -> pure plan;
  _ -> ioError (userError "The plan has no root")};
  let {
  root = planSourcePath rootPlan;
  gpd = planDescription rootPlan;
  localBuildRoot = fromMaybe (if origin == PlanLocal then defaultBuildRoot root else currentDirectory </> ".aihc-target") (buildBuildRoot options);
  buildRoot = localBuildRoot </> targetDirectory;
  outputDirectory = fromMaybe (buildRoot </> "bin") (buildOutput options)};
  executables <- HackageCabal.collectExecutablesIn (planBuildContext (os, arch) rootPlan) gpd root;
  when (null executables) $ ioError (userError ("The package " <> unPackageName (planName rootPlan) <> " has no buildable executable"));
  buildIdentity <- buildEnvironmentIdentity target;
  headerDirectory <- ensureCompilerHeaders target buildRoot;
  let {
  plan = optimizationPlan (buildLto options) (buildOptimization options);
  compileConfig = ModuleCompileConfig {compileBuildIdentity = buildIdentity, compileKeepCore = buildKeepCore options, compileKeepGrin = buildKeepGrin options, compileKeepLir = buildKeepLir options, compileKeepNative = buildKeepNative options, compileLint = buildLint options, compileCheckPrimBounds = buildCheckPrimBounds options, compileLto = planWholeProgram plan, compilePasses = planPasses plan, compileGrinPointsTo = planGrinPointsTo plan, compileNoCode = False, compileOptimization = buildOptimization options, compileTarget = target, compileHeaderDirectory = headerDirectory, compileVerbose = verbose, compilePrintTimings = const (pure ()), compileUseColor = False};
  dependencyConfig = compileConfig {compileKeepCore = False, compileKeepGrin = False, compileKeepLir = False, compileKeepNative = False};
  locations = InstallLocations {locationStoreRoot = storeRoot </> targetDirectory, locationBuildRoot = buildRoot, locationImmutable = False, locationReinstall = False}};
  canonicalRoot <- canonicalizePath root;
  forM executables $ \executable -> do {
  let {
  name = executableInfoName executable};
  verbose ("Build executable: " <> name);
  let {
  dependencyPackages = nub (map depPkgName (executableInfoDependencies executable) <> map mkPackageName ["aihc-base", "aihc-prim"])};
  plans <- mapM (plannedPackage planned) dependencyPackages;
  rootedPlans <- mapM (markRootPlan canonicalRoot origin) plans;
  installed <- installPlanPackages dependencyConfig locations rootedPlans;
  let {
  selected = map installedPackage installed};
  validateSelectedPackageNames selected;
  mapM_ requirePackageArchive selected;
  let {
  outputRoot = buildRoot </> "exe" </> name;
  dependencyNames = map (packageManifestName . installedManifest) selected};
  entryFile <- writeEntryModule outputRoot dependencyNames;
  headerDirs <- dependencyIncludeDirs installed;
  let {
  sourceFiles = executableInfoFiles executable <> [entryFile];
  ownCInfo = executableInfoCCompileInfo executable;
  cCompileInfo = ownCInfo {HackageCabal.cCompileIncludeDirs = nub (HackageCabal.cCompileIncludeDirs ownCInfo <> headerDirs)};
  compileRequest = ModuleCompileRequest {compileOutputRoot = outputRoot, compilePackageRoot = root, compilePackage = Package (T.pack name) (PackageId "exe"), compileSourceFiles = sourceFiles, compileDependencies = installed, compileCapiStubOptions = capiStubOptions sourceFiles cCompileInfo}};
  compiled <- compileModules compileConfig compileRequest;
  cObjects <- compilePackageCFiles target (buildOptimization options) headerDirectory verbose root outputRoot cCompileInfo;
  let {
  output = outputDirectory </> executableFileName target name};
  finishExecutable compileConfig ExecutableInputs {executableStoreRoot = storeRoot, executableNoLink = buildNoLink options, executableOutput = output, executableBuildRoot = outputRoot, executableModules = compiled, executableExtraObjects = cObjects, executableCxxStdLib = not (null (HackageCabal.cCompileCxxSources cCompileInfo)), executablePackages = selected};
  pure output}};
markRootPlan :: FilePath -> PlanOrigin -> PackagePlan -> IO PackagePlan;
markRootPlan canonicalRoot origin plan = do {
  source <- canonicalizePath (planSourcePath plan);
  pure (if source == canonicalRoot then plan {planOrigin = origin} else plan)};
executableFileName :: NativeTarget -> String -> FilePath;
executableFileName target name = case target of {
  Wasm32Wasip3 -> name <.> "wasm";
  _ -> name};
writeEntryModule :: FilePath -> [Text] -> IO HackageCabal.FileInfo;
writeEntryModule outputRoot dependencyNames = do {
  let {
  path = outputRoot </> "generated" </> "Aihc" </> "Entry.hs"};
  createDirectoryIfMissing True (takeDirectory path);
  TIO.writeFile path generatedEntryText;
  pure HackageCabal.FileInfo {HackageCabal.fileInfoPath = path, HackageCabal.fileInfoExtensions = [], HackageCabal.fileInfoCppOptions = [], HackageCabal.fileInfoIncludeDirs = [], HackageCabal.fileInfoLanguage = Nothing, HackageCabal.fileInfoDependencies = dependencyNames, HackageCabal.fileInfoPreprocessor = Nothing}}
}
