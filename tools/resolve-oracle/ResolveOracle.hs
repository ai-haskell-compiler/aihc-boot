{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Resolve the vendored tree with aihc-resolve and print every resolution.
--
-- The boot compiler uses this program as its reference: a module counts as
-- resolved when aihc-boot's own resolver gives every identifier occurrence
-- the same target that aihc-resolve gives it.
--
-- The manifest comes on stdin, one item per line, in dependency order:
--
-- > package NAME          start a package; later lines belong to it
-- > language EDITION      the cabal default-language (default: GHC2021)
-- > extension NAME        one cabal default-extension (repeat)
-- > module PATH           one source file (repeat)
-- > builtin MODULE        a module whose exports are in scope everywhere
-- > report NAME           print the records of this package (default: all)
--
-- The output has one tab-separated record per line:
--
-- > module FILE ok
-- > module FILE error MESSAGE
-- > name FILE L1 C1 L2 C2 NS top PACKAGE MODULE NAME
-- > name FILE L1 C1 L2 C2 NS local ID
-- > name FILE L1 C1 L2 C2 NS syntax
-- > name FILE L1 C1 L2 C2 NS error MESSAGE
--
-- @NS@ is @term@, @type@ or @module@. @L1 C1@ is the start and @L2 C2@
-- the end of the identifier, 1-based, the end exclusive. A local id is
-- only unique inside its file.
module Main (main) where

import Aihc.Parser (ParserConfig (..), defaultConfig, parseModule)
import Aihc.Parser.Syntax
  ( Extension,
    ExtensionSetting (..),
    LanguageEdition (..),
    ModuleHeaderPragmas (..),
    Name (..),
    SourceSpan,
    effectiveExtensions,
    fromAnnotation,
    parseExtensionName,
    parseLanguageEdition,
    pattern SourceSpan,
  )
import Aihc.Parser.Token (readModuleHeaderPragmas)
import Aihc.Resolve
  ( ModuleExports,
    ModuleUnit (..),
    Package (..),
    PackageId (..),
    ResolutionAnnotation (..),
    ResolutionNamespace (..),
    ResolveError (..),
    ResolveResult (..),
    ResolvedName (..),
    Scope,
    collectModuleExportsWithDeps,
    emptyScope,
    lookupImportedModule,
    resolveUnit,
    unionScope,
  )
import Aihc.Resolve.Traverse (collectAnnotations)
import Data.ByteString qualified as BS
import Data.List (sortOn)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error (lenientDecode)
import Data.Text.IO qualified as TIO
import System.Exit (exitFailure)
import System.IO (hPutStrLn, hSetEncoding, stderr, stdout, utf8)

-- | One package of the manifest.
data ManifestPackage = ManifestPackage
  { manifestName :: Text,
    manifestEdition :: LanguageEdition,
    manifestExtensions :: [Extension],
    manifestModules :: [FilePath]
  }

data Manifest = Manifest
  { manifestPackages :: [ManifestPackage],
    manifestBuiltins :: [Text],
    -- | Empty: report every package.
    manifestReport :: [Text]
  }

main :: IO ()
main = do
  hSetEncoding stdout utf8
  input <- TIO.getContents
  manifest <- either die pure (readManifest input)
  records <- runManifest manifest
  mapM_ (TIO.putStrLn . T.intercalate "\t") records

die :: String -> IO a
die message = hPutStrLn stderr ("resolve-oracle: " <> message) >> exitFailure

-- --- Manifest ---------------------------------------------------------------

readManifest :: Text -> Either String Manifest
readManifest input = finish <$> foldl' step (Right (Manifest [] [] [])) (zip [1 :: Int ..] (T.lines input))
  where
    finish manifest = manifest {manifestPackages = reverse (map finishPackage (manifestPackages manifest))}
    finishPackage package = package {manifestModules = reverse (manifestModules package)}
    step (Left err) _ = Left err
    step (Right manifest) (lineNumber, line)
      | T.null (T.strip line) = Right manifest
      | otherwise =
          case T.words line of
            ["package", name] -> Right manifest {manifestPackages = ManifestPackage name GHC2021Edition [] [] : manifestPackages manifest}
            ["builtin", name] -> Right manifest {manifestBuiltins = manifestBuiltins manifest <> [name]}
            ["report", name] -> Right manifest {manifestReport = manifestReport manifest <> [name]}
            ["language", name] ->
              case parseLanguageEdition name of
                Just edition -> withPackage (\p -> p {manifestEdition = edition})
                Nothing -> Left (at "unknown language edition " <> T.unpack name)
            ["extension", name] ->
              case parseExtensionName name of
                Just ext -> withPackage (\p -> p {manifestExtensions = manifestExtensions p <> [ext]})
                Nothing -> Left (at "unknown extension " <> T.unpack name)
            ("module" : rest) | not (null rest) -> withPackage (\p -> p {manifestModules = T.unpack (T.unwords rest) : manifestModules p})
            _ -> Left (at "cannot read manifest line: " <> T.unpack line)
      where
        at message = "line " <> show lineNumber <> ": " <> message
        withPackage f =
          case manifestPackages manifest of
            [] -> Left (at "no package line before this line")
            package : rest -> Right manifest {manifestPackages = f package : rest}

-- --- Resolution -------------------------------------------------------------

data ParsedModule = ParsedModule
  { parsedPath :: FilePath,
    parsedErrors :: [String],
    parsedUnit :: ModuleUnit
  }

runManifest :: Manifest -> IO [[Text]]
runManifest manifest = go mempty (manifestPackages manifest)
  where
    reported name = null (manifestReport manifest) || name `elem` manifestReport manifest
    go _ [] = pure []
    go depExports (package : rest) = do
      let pkg = Package (manifestName package) (PackageId (manifestName package))
      parsed <- mapM (parseFile pkg package) (manifestModules package)
      let units = map parsedUnit parsed
          exports = collectModuleExportsWithDeps depExports units
          visible = exports <> depExports
          builtinScope = foldr (unionScope . builtin pkg visible) emptyScope (manifestBuiltins manifest)
          records
            | reported (manifestName package) = concatMap (moduleRecords builtinScope visible) parsed
            | otherwise = []
      -- The records of one package are complete before the next package
      -- starts, so a crash in a later package keeps the earlier output.
      mapM_ (TIO.putStrLn . T.intercalate "\t") records
      go visible rest

builtin :: Package -> ModuleExports -> Text -> Scope
builtin pkg visible name = lookupImportedModule pkg Nothing name visible

parseFile :: Package -> ManifestPackage -> FilePath -> IO ParsedModule
parseFile pkg package path = do
  bytes <- BS.readFile path
  let source = TE.decodeUtf8With lenientDecode bytes
      header = readModuleHeaderPragmas source
      edition = fromMaybe (manifestEdition package) (headerLanguageEdition header)
      settings = map EnableExtension (manifestExtensions package) <> headerExtensionSettings header
      extensions = effectiveExtensions edition settings
      config = defaultConfig {parserSourceName = path, parserExtensions = extensions}
      (errors, modu) = parseModule config source
  pure
    ParsedModule
      { parsedPath = path,
        parsedErrors = [renderSpan sp <> ": " <> T.unpack message | (sp, message) <- errors],
        parsedUnit = ModuleUnit pkg extensions modu
      }

-- | Resolve one module on its own. The exports of the whole package are
-- already known, so the module's siblings are in scope. A module of its
-- own also keeps the resolve errors of one module apart from the others.
moduleRecords :: Scope -> ModuleExports -> ParsedModule -> [[Text]]
moduleRecords builtinScope visible parsed =
  moduleRecord : nameRecords
  where
    file = T.pack (parsedPath parsed)
    ResolveResult {resolvedModules, resolveErrors} = resolveUnit builtinScope visible [parsedUnit parsed]
    errors = map ("parse: " <>) (parsedErrors parsed) <> map renderResolveError resolveErrors
    moduleRecord =
      case errors of
        [] -> ["module", file, "ok"]
        message : _ -> ["module", file, "error", T.pack message]
    annotations = concatMap (collectAnnotations fromAnnotation . moduleUnitAst) resolvedModules
    nameRecords = sortOn spanKey (mapMaybe (nameRecord file) annotations)
    spanKey record = map (read . T.unpack) (take 4 (drop 2 record)) :: [Int]

nameRecord :: Text -> ResolutionAnnotation -> Maybe [Text]
nameRecord file annotation = do
  sp <- resolutionSpan annotation
  pure (["name", file] <> spanFields sp <> [namespace (resolutionNamespace annotation)] <> target (resolutionTarget annotation))
  where
    namespace ns =
      case ns of
        ResolutionNamespaceTerm -> "term"
        ResolutionNamespaceType -> "type"
        ResolutionNamespaceModule -> "module"
    target resolved =
      case resolved of
        ResolvedTopLevel (PackageId package) modu name -> ["top", package, modu, nameText name]
        ResolvedLocal identifier _ -> ["local", T.pack (show identifier)]
        ResolvedSyntax -> ["syntax"]
        ResolvedError message -> ["error", T.pack message]

spanFields :: SourceSpan -> [Text]
spanFields (SourceSpan _ startLine startCol endLine endCol _ _) =
  map (T.pack . show) [startLine, startCol, endLine, endCol]

renderSpan :: SourceSpan -> String
renderSpan (SourceSpan _ startLine startCol _ _ _ _) = show startLine <> ":" <> show startCol

renderResolveError :: ResolveError -> String
renderResolveError err =
  case err of
    ResolveResolutionError {resolveErrorSpan, resolveErrorName, resolveErrorMessage} ->
      maybe "" ((<> ": ") . renderSpan) resolveErrorSpan <> resolveErrorMessage <> " (" <> T.unpack resolveErrorName <> ")"
    ResolveNotImplemented message -> "not implemented: " <> message
