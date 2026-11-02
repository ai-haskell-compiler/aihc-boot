{-# LANGUAGE OverloadedStrings #-}
module Aihc.Tc.Generate.Record (lookupRecordHead, orderRecordFields, recordUpdateHeads, synthesizedRecordLocal, recordFieldLabel, recordHeadNameSyntax) where {
import Aihc.Parser.Syntax (Name(..), NameType(..), RecordField(..), SourceSpan, UnqualifiedName(..), mkAnnotation, mkUnqualifiedName, nameText);
import Aihc.Resolve (Identifier(..), ResolutionAnnotation(..), ResolutionNamespace(..), ResolvedName(..));
import Aihc.Tc.Env (DataConFieldInfo(..), DataConInfo(..), DataTypeInfo(..), RecordHead(..), dataConRecordHead, patSynRecordHead);
import Aihc.Tc.Monad;
import Aihc.Tc.Types;
import Data.List (nub);
import Data.Maybe (catMaybes, mapMaybe);
import Data.Text (Text);
import Data.Text qualified as T;
recordFieldLabel :: RecordField a -> Text;
recordFieldLabel = nameText . recordFieldName;
lookupRecordHead :: Name -> TcM RecordHead;
lookupRecordHead conSyntax = do {
  target <- resolvedTermTarget conSyntax;
  case target of {
  ResolvedTopLevel packageId resolvedModule resolvedName -> do {
  let {
  conName = nameText resolvedName;
  origin = (packageId, resolvedModule)};
  dataTypes <- getDataTypes;
  let {
  matches = [con | dataType <- dataTypes, con <- dtiConstructors dataType, dciName con == conName, dciOrigin con == origin]};
  case matches of {
  con : _ -> pure (dataConRecordHead con);
  [] -> do {
  mPatSyn <- lookupPatSynTarget target;
  case mPatSyn of {
  Just info -> pure (patSynRecordHead info);
  Nothing -> abortTc ("record constructor missing from type environment: " <> show conName <> " resolved as " <> show target)}}}};
  _ -> abortTc ("record constructor is not a top-level name: " <> show (nameText conSyntax) <> " resolved as " <> show target)}};
orderRecordFields :: Maybe SourceSpan -> RecordHead -> [RecordField a] -> (Maybe Text -> TcM a) -> TcM [a];
orderRecordFields sp head' fields defaultValue = do {
  let {
  declared = rhFields head';
  labels = catMaybes declared;
  unknown = filter (`notElem` labels) (map recordFieldLabel fields);
  duplicates = duplicateLabels (map recordFieldLabel fields)};
  case unknown of {
  label : _ -> abortTc (T.unpack (rhName head') <> " has no field named " <> show label <> " at " <> show sp);
  [] -> pure ()};
  case duplicates of {
  label : _ -> abortTc ("record field " <> show label <> " occurs more than once at " <> show sp);
  [] -> pure ()};
  mapM pick declared} where {
  pick label = case [recordFieldValue occurrence | occurrence <- fields, Just (recordFieldLabel occurrence) == label] of {
  value : _ -> pure value;
  [] -> defaultValue label}};
duplicateLabels :: [Text] -> [Text];
duplicateLabels labels = nub [label | (index, label) <- zip [0 :: Int ..] labels, label `elem` take index labels];
recordUpdateHeads :: Maybe SourceSpan -> Maybe TcType -> [RecordField a] -> TcM [RecordHead];
recordUpdateHeads sp scrutineeType fields = do {
  dataTypes <- getDataTypes;
  let {
  byLabel = filter (\dataType -> any hasAllLabels (dtiConstructors dataType) && ownsSelectors dataType) dataTypes};
  candidates <- case scrutineeType of {
  Just (TcTyCon tyCon _) -> do {
  maybeDataType <- lookupDataType tyCon;
  pure (maybe byLabel (filter (any hasAllLabels . dtiConstructors) . pure) maybeDataType)};
  _ -> pure byLabel};
  case candidates of {
  [dataType] -> pure (map dataConRecordHead (filter hasAllLabels (dtiConstructors dataType)));
  [] -> do {
  patSyns <- filter hasAllFields <$> recordPatSynHeads;
  case patSyns of {
  [head'] -> pure [head'];
  [] -> abortTc ("no record type has the fields " <> show labels <> " at " <> show sp);
  _ -> abortTc ("record update with the fields " <> show labels <> " is ambiguous between the pattern synonyms " <> show (map rhName patSyns) <> " at " <> show sp)}};
  _ -> abortTc ("record update with the fields " <> show labels <> " is ambiguous between " <> show (map dtiName candidates) <> " at " <> show sp)}} where {
  labels = map recordFieldLabel fields;
  origins = mapMaybe (resolvedTermOrigin . recordFieldName) fields;
  ownsSelectors dataType = all (\origin -> any ((origin ==) . dciOrigin) (dtiConstructors dataType)) origins;
  hasAllLabels con = all (`elem` mapMaybe dcfiLabel (dciFields con)) labels;
  hasAllFields head' = all ((`elem` rhFields head') . Just) labels};
synthesizedRecordLocal :: Text -> TcM UnqualifiedName;
synthesizedRecordLocal text = do {
  Unique key <- freshUnique;
  let {
  unique = negate (1000 + key)};
  pure (UnqualifiedName NameVarId text [mkAnnotation (ResolutionAnnotation Nothing (IdentifierNamed text) ResolutionNamespaceTerm (ResolvedLocal unique (mkUnqualifiedName NameVarId text)))])};
recordHeadNameSyntax :: RecordHead -> Name;
recordHeadNameSyntax head' = Name (Just moduleName') nameType' text [mkAnnotation (ResolutionAnnotation Nothing (IdentifierNamed text) ResolutionNamespaceTerm (ResolvedTopLevel packageId moduleName' resolved))] where {
  (packageId, moduleName') = rhOrigin head';
  text = rhName head';
  nameType' = case T.uncons text of {
  Just (first, _) | first == ':' -> NameConSym;
  _ -> NameConId};
  resolved = Name Nothing nameType' text []}
}
