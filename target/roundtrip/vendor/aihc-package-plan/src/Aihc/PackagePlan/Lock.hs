module Aihc.PackagePlan.Lock (LockFile(..), LockEntry(..), LockSource(..), lockFileName, lockFormatVersion, platformKey, readLockFile, writeLockFile, renderLockFile, parseLockFile, lockEntriesFromSolution, lockPreferences, lockRecorded) where {
import Aihc.PackagePlan.Solver (Assignment(..), CandidateSource(..), Preference(..), Solution);
import Data.Aeson ((.:), (.:?), (.=));
import Data.Aeson qualified as Aeson;
import Data.Aeson.Key qualified as Key;
import Data.Aeson.KeyMap qualified as KeyMap;
import Data.Aeson.Types qualified as AesonTypes;
import Data.ByteString qualified as BS;
import Data.ByteString.Lazy qualified as BL;
import Data.ByteString.Lazy.Char8 qualified as BLC;
import Data.List (sortOn);
import Data.Map.Strict (Map);
import Data.Map.Strict qualified as Map;
import Data.Text qualified as T;
import Distribution.Package (PackageName, mkPackageName, unPackageName);
import Distribution.Parsec (simpleParsec);
import Distribution.Pretty (prettyShow);
import Distribution.System (Arch, OS);
import Distribution.Types.Flag (FlagAssignment, mkFlagAssignment, mkFlagName, unFlagAssignment, unFlagName);
import Distribution.Types.Version (Version);
import System.Directory (doesFileExist);
lockFileName :: FilePath;
lockFileName = "aihc.lock";
lockFormatVersion :: Int;
lockFormatVersion = 1;
data LockSource = LockCore | LockHackage | LockLocal deriving (Eq, Ord, Show);
data LockEntry = LockEntry { lockName :: !PackageName, lockVersion :: !Version, lockSource :: !LockSource, lockRevision :: !(Maybe Int), lockFlags :: !FlagAssignment } deriving (Eq, Show);
data LockFile = LockFile { lockCompiler :: !String, lockIndexState :: !(Maybe String), lockPlatforms :: !(Map String [LockEntry]) } deriving (Eq, Show);
platformKey :: OS -> Arch -> String;
platformKey os arch = prettyShow os <> "-" <> prettyShow arch;
readLockFile :: FilePath -> IO (Maybe (Either String LockFile));
readLockFile path = do {
  exists <- doesFileExist path;
  if not exists then pure Nothing else Just . parseLockFile . BL.fromStrict <$> BS.readFile path};
writeLockFile :: FilePath -> LockFile -> IO ();
writeLockFile path = BL.writeFile path . renderLockFile;
renderLockFile :: LockFile -> BL.ByteString;
renderLockFile lock = BLC.unlines ([BLC.pack "{", field "format" (Aeson.encode lockFormatVersion) <> BLC.pack ",", field "compiler" (Aeson.encode (lockCompiler lock)) <> BLC.pack ",", field "index-state" (Aeson.encode (lockIndexState lock)) <> BLC.pack ",", BLC.pack "  \"platforms\": {"] <> concat (separatedGroups (map platform (Map.toAscList (lockPlatforms lock)))) <> [BLC.pack "  }", BLC.pack "}"]) where {
  field :: String -> BL.ByteString -> BL.ByteString;
  field name value = BLC.pack ("  " <> show name <> ": ") <> value;
  platform (key, entries) = [BLC.pack ("    " <> show key <> ": [")] <> separated (map ((BLC.pack "      " <>) . renderEntry) (sortOn lockName entries)) <> [BLC.pack "    ]"];
  separated [] = [];
  separated [x] = [x];
  separated (x : rest) = (x <> BLC.pack ",") : separated rest;
  separatedGroups [] = [];
  separatedGroups [group] = [group];
  separatedGroups (group : rest) = (init group <> [last group <> BLC.pack ","]) : separatedGroups rest};
renderEntry :: LockEntry -> BL.ByteString;
renderEntry entry = BLC.pack "{" <> BLC.intercalate (BLC.pack ", ") ([pair "name" (Aeson.encode (unPackageName (lockName entry))), pair "version" (Aeson.encode (prettyShow (lockVersion entry))), pair "source" (Aeson.encode (sourceText (lockSource entry)))] <> [pair "revision" (Aeson.encode revision) | Just revision <- [lockRevision entry]] <> [pair "flags" (Aeson.encode (Aeson.object [Key.fromString (unFlagName flag) .= value | (flag, value) <- sortOn fst (unFlagAssignment (lockFlags entry))])) | not (null (unFlagAssignment (lockFlags entry)))]) <> BLC.pack "}" where {
  pair :: String -> BL.ByteString -> BL.ByteString;
  pair name value = BLC.pack (show name <> ": ") <> value};
sourceText :: LockSource -> String;
sourceText source = case source of {
  LockCore -> "core";
  LockHackage -> "hackage";
  LockLocal -> "local"};
parseLockFile :: BL.ByteString -> Either String LockFile;
parseLockFile bytes = do {
  value <- Aeson.eitherDecode bytes;
  AesonTypes.parseEither parseLock value} where {
  parseLock = Aeson.withObject "lock file" $ \object -> do {
  format <- object .: "format";
  if format /= lockFormatVersion then fail ("unsupported lock format " <> show (format :: Int) <> "; this compiler writes format " <> show lockFormatVersion) else LockFile <$> object .: "compiler" <*> object .:? "index-state" <*> (object .: "platforms" >>= Aeson.withObject "platforms" (fmap Map.fromList . mapM parsePlatform . KeyMap.toList))};
  parsePlatform (key, value) = do {
  entries <- Aeson.parseJSON value >>= mapM parseEntry;
  pure (Key.toString key, entries)};
  parseEntry = Aeson.withObject "package" $ \object -> do {
  name <- object .: "name";
  versionText <- object .: "version";
  version <- maybe (fail ("invalid version " <> versionText)) pure (simpleParsec versionText);
  sourceName <- object .: "source";
  source <- case sourceName :: String of {
  "core" -> pure LockCore;
  "hackage" -> pure LockHackage;
  "local" -> pure LockLocal;
  other -> fail ("unknown package source " <> other)};
  revision <- object .:? "revision";
  flags <- object .:? "flags";
  pure LockEntry {lockName = mkPackageName name, lockVersion = version, lockSource = source, lockRevision = revision, lockFlags = mkFlagAssignment [(mkFlagName (T.unpack flag), value) | (flag, value) <- maybe [] Map.toList flags]}}};
lockEntriesFromSolution :: Solution -> [LockEntry];
lockEntriesFromSolution solution = [LockEntry {lockName = name, lockVersion = assignmentVersion assignment, lockSource = case assignmentSource assignment of {
  CandidateHackage -> LockHackage;
  CandidateLocal _ -> LockLocal;
  CandidateCore _ -> LockCore}, lockRevision = case assignmentSource assignment of {
  CandidateHackage -> Just (assignmentRevision assignment);
  _ -> Nothing}, lockFlags = assignmentFlags assignment} | (name, assignment) <- Map.toAscList solution];
lockPreferences :: [LockEntry] -> Map PackageName Preference;
lockPreferences entries = Map.fromList [(lockName entry, Preference (lockVersion entry) (lockRevision entry) (lockFlags entry)) | entry <- entries];
lockRecorded :: [LockEntry] -> Map PackageName (Version, Maybe Int, FlagAssignment);
lockRecorded entries = Map.fromList [(lockName entry, (lockVersion entry, lockRevision entry, lockFlags entry)) | entry <- entries]
}
