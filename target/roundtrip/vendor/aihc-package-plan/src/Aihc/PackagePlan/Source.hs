module Aihc.PackagePlan.Source (ParsedInterfaceFile(..), ModuleDeps(..), moduleDepsDigest, parseInterfaceFile, parseInterfaceBytes) where {
import Aihc.Cpp qualified as Cpp;
import Aihc.Hackage.Cabal qualified as HackageCabal;
import Aihc.Hackage.Cpp (DependencyVersions, cppMacrosFromOptions, injectSyntheticCppMacros);
import Aihc.PackagePlan.Diagnostic (DiagnosticSourceMap, cppDiagnosticValue, diagnosticSourceMap, parseDiagnosticValue);
import Aihc.Parser (ParserConfig(..), defaultConfig, parseModule);
import Aihc.Parser.Syntax (Extension(..), LanguageEdition(..), Module(..), effectiveExtensions, headerExtensionSettings, headerLanguageEdition, parseExtensionSettingName, parseLanguageEdition);
import Aihc.Parser.Token (readModuleHeaderPragmas);
import Crypto.Hash.SHA256 qualified as SHA256;
import Data.Aeson qualified as Aeson;
import Data.ByteString qualified as BS;
import Data.ByteString.Char8 qualified as BS8;
import Data.List (nub, sort);
import Data.Map.Strict (Map);
import Data.Map.Strict qualified as M;
import Data.Maybe (fromMaybe, mapMaybe);
import Data.Set qualified as S;
import Data.Text (Text);
import Data.Text qualified as T;
import Data.Text.Encoding qualified as TE;
import Data.Text.Encoding.Error (lenientDecode);
import Numeric (showHex);
import System.Directory (doesFileExist);
import System.FilePath (makeRelative, normalise, splitDirectories, takeDirectory, takeExtension, (</>));
data ParsedInterfaceFile = ParsedInterfaceFile { parsedFilePath :: !FilePath, parsedFileModule :: Module, parsedFileSourceLines :: DiagnosticSourceMap, parsedFileParseDiagnostics :: [Aeson.Value], parsedFileCppDiagnostics :: [Aeson.Value], parsedFileExtensions :: [Extension], parsedFileSource :: !Text, parsedFileDeps :: !ModuleDeps };
data ModuleDeps = ModuleDeps { moduleDepsSource :: !Text, moduleDepsConfig :: !Text, moduleDepsIncludes :: !(Map FilePath Text), moduleDepsVersions :: !DependencyVersions } deriving (Eq, Show);
moduleDepsDigest :: ModuleDeps -> Text;
moduleDepsDigest deps = digestChunks ([TE.encodeUtf8 (moduleDepsSource deps), TE.encodeUtf8 (moduleDepsConfig deps)] <> concat [[BS8.pack path, TE.encodeUtf8 digest] | (path, digest) <- M.toAscList (moduleDepsIncludes deps)] <> [BS8.pack (show (M.toAscList (moduleDepsVersions deps)))]);
digestChunks :: [BS.ByteString] -> Text;
digestChunks = T.pack . concatMap hex . BS.unpack . SHA256.hash . BS.concat . concatMap field where {
  field bytes = [BS8.pack (show (BS.length bytes) <> ":"), bytes];
  hex byte = let {
  value = showHex byte ""} in replicate (2 - length value) '0' <> value};
parseInterfaceFile :: FilePath -> FilePath -> DependencyVersions -> HackageCabal.FileInfo -> IO ParsedInterfaceFile;
parseInterfaceFile headerDir packageRoot versions fileInfo = do {
  bytes <- BS.readFile (HackageCabal.fileInfoPath fileInfo);
  parseInterfaceBytes headerDir packageRoot versions fileInfo bytes};
parseInterfaceBytes :: FilePath -> FilePath -> DependencyVersions -> HackageCabal.FileInfo -> BS.ByteString -> IO ParsedInterfaceFile;
parseInterfaceBytes headerDir packageRoot versions fileInfo bytes = do {
  let {
  normalized = normalizeSource path (TE.decodeUtf8With lenientDecode bytes);
  normalizedExtensions = extensionsOf normalized;
  cppEnabled = CPP `elem` normalizedExtensions};
  (source, cppDiagnostics, includes) <- if cppEnabled then preprocessInterfaceSource headerDir packageRoot versions fileInfo normalized else pure (normalized, [], M.empty);
  let {
  extensions = if cppEnabled then extensionsOf source else normalizedExtensions;
  cfg = defaultConfig {parserSourceName = path, parserExtensions = extensions};
  (parseErrs, modu) = parseModule cfg source;
  parseDiagnostics = map (parseDiagnosticValue path) parseErrs;
  sourceLines = diagnosticSourceMap path source;
  dependencies = HackageCabal.fileInfoDependencies fileInfo;
  deps = ModuleDeps {moduleDepsSource = digestChunks [bytes], moduleDepsConfig = digestChunks (map BS8.pack (HackageCabal.fileInfoExtensions fileInfo) <> [BS8.pack (show (HackageCabal.fileInfoLanguage fileInfo))] <> [BS8.pack (show (HackageCabal.fileInfoCppOptions fileInfo, sort dependencies)) | cppEnabled]), moduleDepsIncludes = includes, moduleDepsVersions = if cppEnabled then M.restrictKeys versions (S.fromList dependencies) else M.empty}};
  pure ParsedInterfaceFile {parsedFilePath = path, parsedFileModule = modu, parsedFileSourceLines = sourceLines, parsedFileParseDiagnostics = parseDiagnostics, parsedFileCppDiagnostics = cppDiagnostics, parsedFileExtensions = extensions, parsedFileSource = source, parsedFileDeps = deps}} where {
  path = HackageCabal.fileInfoPath fileInfo;
  cabalExtSettings = mapMaybe (parseExtensionSettingName . T.pack) (HackageCabal.fileInfoExtensions fileInfo);
  extensionsOf text = let {
  headerPragmas = readModuleHeaderPragmas text;
  language = headerLanguageEdition headerPragmas `orElse` (HackageCabal.fileInfoLanguage fileInfo >>= parseLanguageEdition . T.pack)} in effectiveExtensions (fromMaybe Haskell98Edition language) (cabalExtSettings <> headerExtensionSettings headerPragmas);
  orElse (Just value) _ = Just value;
  orElse Nothing fallback = fallback};
preprocessInterfaceSource :: FilePath -> FilePath -> DependencyVersions -> HackageCabal.FileInfo -> Text -> IO (Text, [Aeson.Value], Map FilePath Text);
preprocessInterfaceSource headerDir packageRoot versions fileInfo source = do {
  drive M.empty (Cpp.preprocess cppConfig (TE.encodeUtf8 injectedSource))} where {
  path = HackageCabal.fileInfoPath fileInfo;
  cppOptions = HackageCabal.fileInfoCppOptions fileInfo;
  injectedSource = injectSyntheticCppMacros path cppOptions versions (HackageCabal.fileInfoDependencies fileInfo) source;
  cppConfig = Cpp.defaultConfig {Cpp.configInputFile = path, Cpp.configMacros = M.mapKeys TE.encodeUtf8 (M.map TE.encodeUtf8 (cppMacrosFromOptions cppOptions))};
  drive includes step = case step of {
  Cpp.Done result -> pure (TE.decodeUtf8With lenientDecode (Cpp.resultOutput result), map cppDiagnosticValue (Cpp.resultDiagnostics result), includes);
  Cpp.NeedInclude req k -> do {
  resolved <- resolveInclude headerDir packageRoot (HackageCabal.fileInfoIncludeDirs fileInfo) path req;
  case resolved of {
  IncludeFromFile file content -> drive (M.insert (normalise (makeRelative packageRoot file)) (digestChunks [content]) includes) (k (Just content));
  IncludeFromCompiler content -> drive includes (k (Just content));
  IncludeMissing -> drive includes (k Nothing)}}}};
data ResolvedInclude = IncludeFromFile !FilePath !BS.ByteString | IncludeFromCompiler !BS.ByteString | IncludeMissing;
resolveInclude :: FilePath -> FilePath -> [FilePath] -> FilePath -> Cpp.IncludeRequest -> IO ResolvedInclude;
resolveInclude headerDir packageRoot includeDirs currentFile req = findFirst (includeCandidates packageRoot includeDirs currentFile req) where {
  compilerHeader = headerDir </> normalise (Cpp.includePath req);
  findFirst [] = do {
  exists <- doesFileExist compilerHeader;
  if exists then IncludeFromCompiler <$> BS.readFile compilerHeader else pure IncludeMissing};
  findFirst (candidate : rest) = do {
  exists <- doesFileExist candidate;
  if exists then IncludeFromFile candidate <$> BS.readFile candidate else findFirst rest}};
includeCandidates :: FilePath -> [FilePath] -> FilePath -> Cpp.IncludeRequest -> [FilePath];
includeCandidates packageRoot includeDirs currentFile req = map normalise $ nub [dir </> Cpp.includePath req | dir <- searchDirs] where {
  includeDir = takeDirectory (Cpp.includeFrom req);
  sourceRelDir = takeDirectory (makeRelative packageRoot currentFile);
  packageAncestors = ancestorDirs sourceRelDir;
  localRoots = [takeDirectory currentFile, packageRoot </> sourceRelDir, packageRoot </> includeDir];
  systemRoots = includeDirs <> [packageRoot </> "include", packageRoot </> "includes", packageRoot </> "cbits", packageRoot];
  searchDirs = case Cpp.includeKind req of {
  Cpp.IncludeLocal -> localRoots <> map (packageRoot </>) packageAncestors <> systemRoots;
  Cpp.IncludeSystem -> systemRoots <> localRoots <> map (packageRoot </>) packageAncestors}};
ancestorDirs :: FilePath -> [FilePath];
ancestorDirs path = case filter (not . null) (splitDirectories path) of {
  [] -> [];
  parts -> [foldl (</>) "." (take n parts) | n <- [length parts, length parts - 1 .. 1]]};
normalizeSource :: FilePath -> Text -> Text;
normalizeSource path source = let {
  withoutBom = T.dropWhile (== '\xfeff') source} in if takeExtension path == ".lhs" then T.unlines (unlitBird (T.lines withoutBom)) else withoutBom;
unlitBird :: [Text] -> [Text];
unlitBird = map $ \line -> case T.uncons line of {
  Just ('>', rest) -> rest;
  _ -> ""}
}
