{-# LANGUAGE OverloadedStrings #-}
module Aihc.Cli.BuildStamp (FileStamp(..), ResolveStamp(..), UnitStamp(..), BackendStamp(..), PackageDigests(..), ModuleDigests(..), packageDigestsPath, readStamp, writeStamp, stampFiles, filesMatchStamps) where {
import Control.Exception (IOException, try);
import Data.Aeson ((.:), (.=));
import Data.Aeson qualified as Aeson;
import Data.ByteString.Lazy qualified as BL;
import Data.Map.Strict qualified as Map;
import Data.Text (Text);
import System.Directory (createDirectoryIfMissing, doesFileExist, getFileSize, getModificationTime, renameFile);
import System.FilePath (takeDirectory, (</>));
data FileStamp = FileStamp { fileStampPath :: !FilePath, fileStampSize :: !Integer, fileStampModified :: !String } deriving (Eq, Show);
instance Aeson.ToJSON FileStamp where {
  toJSON stamp = Aeson.object ["path" .= fileStampPath stamp, "size" .= fileStampSize stamp, "modified" .= fileStampModified stamp]};
instance Aeson.FromJSON FileStamp where {
  parseJSON = Aeson.withObject "FileStamp" $ \object -> FileStamp <$> object .: "path" <*> object .: "size" <*> object .: "modified"};
data ResolveStamp = ResolveStamp { resolveStampInputs :: ![(Text, Text)], resolveStampScopes :: !(Map.Map Text Text), resolveStampFiles :: ![FileStamp] } deriving (Eq, Show);
instance Aeson.ToJSON ResolveStamp where {
  toJSON stamp = Aeson.object ["schemaVersion" .= (1 :: Int), "inputs" .= resolveStampInputs stamp, "scopes" .= resolveStampScopes stamp, "files" .= resolveStampFiles stamp]};
instance Aeson.FromJSON ResolveStamp where {
  parseJSON = Aeson.withObject "ResolveStamp" $ \object -> do {
  schemaVersion <- object .: "schemaVersion";
  case schemaVersion :: Int of {
  1 -> ResolveStamp <$> object .: "inputs" <*> object .: "scopes" <*> object .: "files";
  _ -> fail "unsupported resolve stamp schema"}}};
data UnitStamp = UnitStamp { unitStampInputs :: ![(Text, Text)], unitStampTypes :: !(Map.Map Text Text), unitStampFacts :: !Text, unitStampFiles :: ![FileStamp], unitStampBackend :: !(Maybe BackendStamp) } deriving (Eq, Show);
data BackendStamp = BackendStamp { backendStampOptions :: !Text, backendStampFiles :: ![FileStamp], backendStampHeaders :: ![FileStamp] } deriving (Eq, Show);
instance Aeson.ToJSON BackendStamp where {
  toJSON stamp = Aeson.object ["options" .= backendStampOptions stamp, "files" .= backendStampFiles stamp, "headers" .= backendStampHeaders stamp]};
instance Aeson.FromJSON BackendStamp where {
  parseJSON = Aeson.withObject "BackendStamp" $ \object -> BackendStamp <$> object .: "options" <*> object .: "files" <*> object .: "headers"};
instance Aeson.ToJSON UnitStamp where {
  toJSON stamp = Aeson.object ["schemaVersion" .= (2 :: Int), "inputs" .= unitStampInputs stamp, "types" .= unitStampTypes stamp, "facts" .= unitStampFacts stamp, "files" .= unitStampFiles stamp, "backend" .= unitStampBackend stamp]};
instance Aeson.FromJSON UnitStamp where {
  parseJSON = Aeson.withObject "UnitStamp" $ \object -> do {
  schemaVersion <- object .: "schemaVersion";
  case schemaVersion :: Int of {
  2 -> UnitStamp <$> object .: "inputs" <*> object .: "types" <*> object .: "facts" <*> object .: "files" <*> object .: "backend";
  _ -> fail "unsupported unit stamp schema"}}};
data ModuleDigests = ModuleDigests { moduleScopeDigest :: !Text, moduleTypeDigest :: !Text } deriving (Eq, Show);
instance Aeson.ToJSON ModuleDigests where {
  toJSON digests = Aeson.object ["scope" .= moduleScopeDigest digests, "type" .= moduleTypeDigest digests]};
instance Aeson.FromJSON ModuleDigests where {
  parseJSON = Aeson.withObject "ModuleDigests" $ \object -> ModuleDigests <$> object .: "scope" <*> object .: "type"};
data PackageDigests = PackageDigests { packageDigestsModules :: !(Map.Map Text ModuleDigests), packageDigestsInstances :: !Text } deriving (Eq, Show);
instance Aeson.ToJSON PackageDigests where {
  toJSON digests = Aeson.object ["schemaVersion" .= (1 :: Int), "modules" .= packageDigestsModules digests, "instances" .= packageDigestsInstances digests]};
instance Aeson.FromJSON PackageDigests where {
  parseJSON = Aeson.withObject "PackageDigests" $ \object -> do {
  schemaVersion <- object .: "schemaVersion";
  case schemaVersion :: Int of {
  1 -> PackageDigests <$> object .: "modules" <*> object .: "instances";
  _ -> fail "unsupported package digests schema"}}};
packageDigestsPath :: FilePath -> FilePath;
packageDigestsPath packageRoot = packageRoot </> "digests.json";
readStamp :: (Aeson.FromJSON stamp) => FilePath -> IO (Maybe stamp);
readStamp path = do {
  exists <- doesFileExist path;
  if not exists then pure Nothing else do {
  result <- try (BL.readFile path) :: IO (Either IOException BL.ByteString);
  pure (either (const Nothing) (either (const Nothing) Just . Aeson.eitherDecode) result)}};
writeStamp :: (Aeson.ToJSON stamp) => FilePath -> stamp -> IO ();
writeStamp path stamp = do {
  createDirectoryIfMissing True (takeDirectory path);
  let {
  temporary = path <> ".tmp"};
  BL.writeFile temporary (Aeson.encode stamp);
  renameFile temporary path};
stampFiles :: FilePath -> [FilePath] -> IO [FileStamp];
stampFiles root = mapM stampFile where {
  stampFile path = do {
  size <- getFileSize (root </> path);
  modified <- getModificationTime (root </> path);
  pure (FileStamp path size (show modified))}};
filesMatchStamps :: FilePath -> [FileStamp] -> IO Bool;
filesMatchStamps root = fmap and . mapM matches where {
  matches stamp = do {
  let {
  path = root </> fileStampPath stamp};
  exists <- doesFileExist path;
  if not exists then pure False else do {
  size <- getFileSize path;
  modified <- getModificationTime path;
  pure (size == fileStampSize stamp && show modified == fileStampModified stamp)}}}
}
