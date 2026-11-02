{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module Aihc.Parser.Syntax (ArithSeq(..), ArrowKind(..), ArrAppType(..), BangType(..), BinderName, BuiltinCon(..), BinderHead(..), CallConv(..), CaseAlt(..), ClassDecl(..), ClassDeclItem(..), Cmd(..), CompStmt(..), FunctionalDependency(..), TypeFamilyResultSig(..), TypeFamilyInjectivity(..), TypeHeadForm(..), DataConDecl(..), DataDecl(..), Decl(..), DerivingClause(..), DerivingStrategy(..), DoFlavor(..), DoStmt(..), Expr(..), Extension(..), ExtensionSet, ExtensionSetting(..), ExportSpec(..), IEEntityNamespace(..), IEBundledNamespace(..), IEBundledMember(..), FieldDecl(..), FixityAssoc(..), ForeignDecl(..), ForeignDirection(..), ForeignEntitySpec(..), ForeignSafety(..), GadtBody(..), GuardQualifier(..), GuardedRhs(..), ImportDecl(..), ImportLevel(..), ImportItem(..), ImportSpec(..), InstanceDecl(..), InstanceDeclItem(..), InstanceOverlapPragma(..), LanguageEdition(..), LambdaCaseAlt(..), Literal(..), Match(..), MatchHeadForm(..), MultiplicityTag(..), Module(..), ModuleHead(..), ModuleHeaderPragmas(..), Name(..), NameType(..), UnqualifiedName(..), InstanceHeadType, Annotation, NewtypeDecl(..), OperatorName, PatSynArgs(..), PatSynDecl(..), PatSynDir(..), Pattern(..), RecordField(..), Pragma(..), PragmaType(..), PragmaUnpackKind(..), Role(..), RoleAnnotation(..), Rhs(..), RuleActivation(..), RuleBinder(..), RuleDecl(..), SourceSpan, pattern SourceSpan, sourceSpanSourceName, sourceSpanStartLine, sourceSpanStartCol, sourceSpanEndLine, sourceSpanEndCol, sourceSpanStartOffset, sourceSpanEndOffset, StandaloneDerivingDecl(..), Type(..), TupleFlavor(..), TypeSyntaxForm(..), FloatType(..), NumericType(..), TypeLiteral(..), TypePromotion(..), ForallVis(..), ForallTelescope(..), TyVarBSpecificity(..), TyVarBVisibility(..), TyVarBinder(..), TypeSynDecl(..), TypeFamilyDecl(..), TypeFamilyEq(..), DataFamilyDecl(..), TypeFamilyInst(..), DataFamilyInst(..), ValueDecl(..), allKnownExtensions, applyExtensionSetting, applyImpliedExtensions, binderHeadForm, binderHeadName, binderHeadParams, effectiveExtensions, extensionName, memberExtension, mkExtensionSet, extensionSettingName, gadtBodyResultType, languageEditionExtensions, editionFromExtensionSettings, mergeSourceSpans, mkName, mkUnqualifiedName, nameFromText, parseExtensionName, parseExtensionSettingName, parseLanguageEdition, qualifyName, renderName, renderUnqualifiedName, unqualifiedNameFromText, moduleName, moduleWarningPragma, moduleExports, instanceHeadName, instanceHeadTypes, mkAnnotation, fromAnnotation, stripAnnotations, peelArithSeqAnn, peelClassDeclItemAnn, peelCmdAnn, peelCompStmtAnn, peelDataConAnn, peelDeclAnn, peelDoStmtAnn, peelExprAnn, peelGuardQualifierAnn, peelInstanceDeclItemAnn, peelPatternAnn, peelTypeAnn, peelTypeHead, literalAnnSpan, peelLiteralAnn, typeAnnSpan) where {
import Control.DeepSeq (NFData(..));
import Data.Bits (clearBit, countTrailingZeros, setBit, shiftL, shiftR, testBit, (.&.), (.|.));
import Data.Char (GeneralCategory(..), generalCategory);
import Data.Data (Constr, Data(..), DataType, Fixity(Prefix), mkConstr, mkDataType);
import Data.Dynamic (Dynamic, Typeable, fromDynamic, toDyn);
import Data.List qualified as List;
import Data.Map qualified as Map;
import Data.Maybe (fromMaybe);
import Data.String (IsString(..));
import Data.Text (Text);
import Data.Text qualified as T;
import Data.Typeable (cast);
import Data.Word (Word64);
import GHC.Generics (Generic);
data Extension = AllowAmbiguousTypes | AlternativeLayoutRule | AlternativeLayoutRuleTransitional | ApplicativeDo | Arrows | AutoDeriveTypeable | BangPatterns | BinaryLiterals | BlockArguments | CApiFFI | ConstrainedClassMethods | ConstraintKinds | CPP | CUSKs | DataKinds | DatatypeContexts | DeepSubsumption | DefaultSignatures | DeriveAnyClass | DeriveDataTypeable | DeriveFoldable | DeriveFunctor | DeriveGeneric | DeriveLift | DeriveTraversable | DerivingStrategies | DerivingViaExtension | DisambiguateRecordFields | DoAndIfThenElse | DoRec | DuplicateRecordFields | EmptyCase | EmptyDataDecls | EmptyDataDeriving | ExistentialQuantification | ExplicitForAll | ExplicitLevelImports | ExplicitNamespaces | ExtensibleRecords | ExtendedDefaultRules | ExtendedLiterals | FieldSelectors | FlexibleContexts | FlexibleInstances | ForeignFunctionInterface | FunctionalDependencies | GADTs | GADTSyntax | Generics | GeneralizedNewtypeDeriving | GHC2021 | GHC2024 | GHCForeignImportPrim | Haskell2010 | Haskell98 | HereDocuments | HexFloatLiterals | ImplicitParams | ImplicitPrelude | ImplicitStagePersistence | ImportQualifiedPost | ImpredicativeTypes | IncoherentInstances | InstanceSigs | InterruptibleFFI | JavaScriptFFI | KindSignatures | LambdaCase | LexicalNegation | LiberalTypeSynonyms | LinearTypes | ListTuplePuns | MagicHash | MonadComprehensions | MonadFailDesugaring | MonoLocalBinds | MonoPatBinds | MonomorphismRestriction | MultilineStrings | MultiParamTypeClasses | MultiWayIf | NamedDefaults | NamedFieldPuns | NamedWildCards | NewQualifiedOperators | NegativeLiterals | NondecreasingIndentation | NPlusKPatterns | NullaryTypeClasses | NumDecimals | NumericUnderscores | OrPatterns | OverlappingInstances | OverloadedLabels | OverloadedLists | OverloadedRecordDot | OverloadedRecordUpdate | OverloadedStrings | PackageImports | ParallelArrays | ParallelListComp | PartialTypeSignatures | PatternSignatures | PatternGuards | PatternSynonyms | PolymorphicComponents | PolyKinds | PostfixOperators | QualifiedDo | QualifiedStrings | QuantifiedConstraints | QuasiQuotes | Rank2Types | RankNTypes | RebindableSyntax | RecordPuns | RecordWildCards | RecursiveDo | RegularPatterns | RelaxedLayout | RelaxedPolyRec | RestrictedTypeSynonyms | RequiredTypeArguments | RoleAnnotations | SafeImports | SafeHaskell | ScopedTypeVariables | StandaloneDeriving | StandaloneKindSignatures | StarIsType | StaticPointers | Strict | StrictData | TemplateHaskell | TemplateHaskellQuotes | TraditionalRecordSyntax | TransformListComp | Trustworthy | TupleSections | TypeAbstractions | TypeApplications | TypeData | TypeFamilies | TypeFamilyDependencies | TypeInType | TypeOperators | TypeSynonymInstances | UnboxedSums | UnboxedTuples | UndecidableInstances | UndecidableSuperClasses | UnicodeSyntax | UnliftedDatatypes | UnliftedFFITypes | UnliftedNewtypes | UnsafeHaskell | ViewPatterns | XmlSyntax deriving (Data, Eq, Ord, Show, Read, Enum, Bounded, Generic, NFData);
data ExtensionSet = ExtensionSet !Word64 !Word64 !Word64 deriving stock (Eq, Show, Generic) deriving anyclass (NFData);
mkExtensionSet :: [Extension] -> ExtensionSet;
mkExtensionSet = List.foldl' (flip insertExtension) emptyExtensionSet;
emptyExtensionSet :: ExtensionSet;
emptyExtensionSet = ExtensionSet 0 0 0;
insertExtension :: Extension -> ExtensionSet -> ExtensionSet;
insertExtension ext (ExtensionSet low middle high) = case fromEnum ext `quotRem` 64 of {
  (0, bit) -> ExtensionSet (setBit low bit) middle high;
  (1, bit) -> ExtensionSet low (setBit middle bit) high;
  (2, bit) -> ExtensionSet low middle (setBit high bit);
  _ -> error "insertExtension: extension enum exceeds bitset capacity"};
deleteExtension :: Extension -> ExtensionSet -> ExtensionSet;
deleteExtension ext (ExtensionSet low middle high) = case fromEnum ext `quotRem` 64 of {
  (0, bit) -> ExtensionSet (clearBit low bit) middle high;
  (1, bit) -> ExtensionSet low (clearBit middle bit) high;
  (2, bit) -> ExtensionSet low middle (clearBit high bit);
  _ -> error "deleteExtension: extension enum exceeds bitset capacity"};
extensionSetToList :: ExtensionSet -> [Extension];
extensionSetToList (ExtensionSet low middle high) = wordMembers 0 low (wordMembers 64 middle (wordMembers 128 high [])) where {
  wordMembers base word rest | word == 0 = rest | otherwise = let {
  bit = countTrailingZeros word} in toEnum (base + bit) : wordMembers base (clearBit word bit) rest};
memberExtension :: Extension -> ExtensionSet -> Bool;
memberExtension ext (ExtensionSet low middle high) = case fromEnum ext `quotRem` 64 of {
  (0, bit) -> testBit low bit;
  (1, bit) -> testBit middle bit;
  (2, bit) -> testBit high bit;
  _ -> False};
data ExtensionSetting = EnableExtension Extension | DisableExtension Extension deriving (Data, Eq, Ord, Show, Read, Generic, NFData);
data LanguageEdition = Haskell98Edition | Haskell2010Edition | GHC2021Edition | GHC2024Edition deriving (Eq, Ord, Show, Read, Enum, Bounded, Generic, NFData);
data ModuleHeaderPragmas = ModuleHeaderPragmas { headerLanguageEdition :: Maybe LanguageEdition, headerExtensionSettings :: [ExtensionSetting] } deriving (Eq, Show, Generic, NFData);
allKnownExtensions :: [Extension];
allKnownExtensions = [minBound .. maxBound];
extensionName :: Extension -> Text;
extensionName ext = case ext of {
  DerivingViaExtension -> T.pack "DerivingVia";
  SafeHaskell -> T.pack "Safe";
  UnsafeHaskell -> T.pack "Unsafe";
  _ -> T.pack (show ext)};
extensionSettingName :: ExtensionSetting -> Text;
extensionSettingName setting = case setting of {
  EnableExtension ext -> extensionName ext;
  DisableExtension ext -> T.pack "No" <> extensionName ext};
parseExtensionName :: Text -> Maybe Extension;
parseExtensionName raw = Map.lookup trimmed extensionMap where {
  extensionMap :: Map.Map Text Extension;
  extensionMap = Map.fromList $ aliases ++ [(T.pack (show ext), ext) | ext <- [minBound .. maxBound]];
  trimmed = T.strip raw;
  aliases = [("Cpp", CPP), ("DerivingVia", DerivingViaExtension), ("GeneralisedNewtypeDeriving", GeneralizedNewtypeDeriving), ("Safe", SafeHaskell), ("Unsafe", UnsafeHaskell)]};
parseExtensionSettingName :: Text -> Maybe ExtensionSetting;
parseExtensionSettingName raw = case T.stripPrefix (T.pack "No") trimmed of {
  Just rest | not (T.null rest) -> case parseExtensionName rest of {
  Just ext -> Just (DisableExtension ext);
  Nothing -> EnableExtension <$> parseExtensionName trimmed};
  _ -> EnableExtension <$> parseExtensionName trimmed} where {
  trimmed = T.strip raw};
parseLanguageEdition :: Text -> Maybe LanguageEdition;
parseLanguageEdition raw | trimmed == T.pack "Haskell98" = Just Haskell98Edition | trimmed == T.pack "Haskell2010" = Just Haskell2010Edition | trimmed == T.pack "GHC2021" = Just GHC2021Edition | trimmed == T.pack "GHC2024" = Just GHC2024Edition | otherwise = Nothing where {
  trimmed = T.strip raw};
editionFromExtensionSettings :: [ExtensionSetting] -> Maybe LanguageEdition;
editionFromExtensionSettings = worker Nothing where {
  worker acc [] = acc;
  worker _ (EnableExtension Haskell98 : rest) = worker (Just Haskell98Edition) rest;
  worker _ (EnableExtension Haskell2010 : rest) = worker (Just Haskell2010Edition) rest;
  worker _ (EnableExtension GHC2021 : rest) = worker (Just GHC2021Edition) rest;
  worker _ (EnableExtension GHC2024 : rest) = worker (Just GHC2024Edition) rest;
  worker acc (_ : rest) = worker acc rest};
languageEditionExtensions :: LanguageEdition -> [Extension];
languageEditionExtensions edition = case edition of {
  Haskell98Edition -> [CUSKs, DeepSubsumption, DatatypeContexts, FieldSelectors, ImplicitPrelude, ImplicitStagePersistence, ListTuplePuns, MonomorphismRestriction, NondecreasingIndentation, NPlusKPatterns, StarIsType, TraditionalRecordSyntax];
  Haskell2010Edition -> [CUSKs, DeepSubsumption, DatatypeContexts, DoAndIfThenElse, EmptyDataDecls, FieldSelectors, ForeignFunctionInterface, ImplicitPrelude, ImplicitStagePersistence, ListTuplePuns, MonomorphismRestriction, PatternGuards, RelaxedPolyRec, StarIsType, TraditionalRecordSyntax];
  GHC2021Edition -> [BangPatterns, BinaryLiterals, ConstrainedClassMethods, ConstraintKinds, DeriveDataTypeable, DeriveFoldable, DeriveFunctor, DeriveGeneric, DeriveLift, DeriveTraversable, DoAndIfThenElse, EmptyCase, EmptyDataDecls, EmptyDataDeriving, ExistentialQuantification, ExplicitForAll, FieldSelectors, FlexibleContexts, FlexibleInstances, ForeignFunctionInterface, GADTSyntax, GeneralizedNewtypeDeriving, HexFloatLiterals, ImplicitPrelude, ImplicitStagePersistence, ImportQualifiedPost, InstanceSigs, KindSignatures, ListTuplePuns, MonomorphismRestriction, MultiParamTypeClasses, NamedFieldPuns, NamedWildCards, NumericUnderscores, PatternGuards, PolyKinds, PostfixOperators, RankNTypes, RelaxedPolyRec, ScopedTypeVariables, StandaloneDeriving, StandaloneKindSignatures, StarIsType, TraditionalRecordSyntax, TupleSections, TypeApplications, TypeOperators, TypeSynonymInstances];
  GHC2024Edition -> [BangPatterns, BinaryLiterals, ConstrainedClassMethods, ConstraintKinds, DataKinds, DeriveDataTypeable, DeriveFoldable, DeriveFunctor, DeriveGeneric, DeriveLift, DeriveTraversable, DerivingStrategies, DisambiguateRecordFields, DoAndIfThenElse, EmptyCase, EmptyDataDecls, EmptyDataDeriving, ExistentialQuantification, ExplicitForAll, ExplicitNamespaces, FieldSelectors, FlexibleContexts, FlexibleInstances, ForeignFunctionInterface, GADTs, GADTSyntax, GeneralizedNewtypeDeriving, HexFloatLiterals, ImplicitPrelude, ImplicitStagePersistence, ImportQualifiedPost, InstanceSigs, KindSignatures, LambdaCase, ListTuplePuns, MonoLocalBinds, MonomorphismRestriction, MultiParamTypeClasses, NamedFieldPuns, NamedWildCards, NumericUnderscores, PatternGuards, PolyKinds, PostfixOperators, RankNTypes, RelaxedPolyRec, RoleAnnotations, ScopedTypeVariables, StandaloneDeriving, StandaloneKindSignatures, StarIsType, TraditionalRecordSyntax, TupleSections, TypeApplications, TypeOperators, TypeSynonymInstances]};
impliedExtensions :: [(Extension, [ExtensionSetting])];
impliedExtensions = [(DeriveTraversable, [EnableExtension DeriveFunctor, EnableExtension DeriveFoldable]), (DerivingViaExtension, [EnableExtension DerivingStrategies]), (DuplicateRecordFields, [EnableExtension DisambiguateRecordFields]), (ExistentialQuantification, [EnableExtension ExplicitForAll]), (ExplicitLevelImports, [DisableExtension ImplicitStagePersistence]), (FlexibleInstances, [EnableExtension TypeSynonymInstances]), (FunctionalDependencies, [EnableExtension MultiParamTypeClasses]), (GADTs, [EnableExtension GADTSyntax, EnableExtension MonoLocalBinds]), (ImpredicativeTypes, [EnableExtension RankNTypes]), (IncoherentInstances, [EnableExtension OverlappingInstances]), (LiberalTypeSynonyms, [EnableExtension ExplicitForAll]), (LinearTypes, [EnableExtension MonoLocalBinds]), (MonadComprehensions, [EnableExtension ParallelListComp]), (MultiParamTypeClasses, [EnableExtension ConstrainedClassMethods]), (PolymorphicComponents, [EnableExtension RankNTypes]), (PolyKinds, [EnableExtension KindSignatures]), (QuantifiedConstraints, [EnableExtension ExplicitForAll]), (Rank2Types, [EnableExtension RankNTypes]), (RankNTypes, [EnableExtension ExplicitForAll]), (RebindableSyntax, [DisableExtension ImplicitPrelude]), (RecordWildCards, [EnableExtension DisambiguateRecordFields]), (ScopedTypeVariables, [EnableExtension ExplicitForAll]), (StandaloneKindSignatures, [DisableExtension CUSKs]), (Strict, [EnableExtension StrictData]), (TemplateHaskell, [EnableExtension TemplateHaskellQuotes]), (TypeFamilies, [EnableExtension ExplicitNamespaces, EnableExtension KindSignatures, EnableExtension MonoLocalBinds]), (TypeAbstractions, [EnableExtension TypeApplications]), (TypeFamilyDependencies, [EnableExtension TypeFamilies]), (TypeInType, [EnableExtension PolyKinds, EnableExtension DataKinds, EnableExtension KindSignatures]), (TypeOperators, [EnableExtension ExplicitNamespaces]), (RequiredTypeArguments, [EnableExtension TypeApplications]), (UnboxedTuples, [EnableExtension UnboxedSums]), (UnliftedDatatypes, [EnableExtension DataKinds, EnableExtension StandaloneKindSignatures])];
applyExtensionSetting :: ExtensionSetting -> [Extension] -> [Extension];
applyExtensionSetting setting extensions = case setting of {
  EnableExtension ext -> ext : filter (/= ext) extensions;
  DisableExtension ext -> filter (/= ext) extensions};
impliedExtensionMap :: Map.Map Extension [ExtensionSetting];
impliedExtensionMap = Map.fromList impliedExtensions;
applyImpliedExtensions :: [Extension] -> [Extension];
applyImpliedExtensions extensions | closure == initial = extensions | otherwise = extensionSetToList closure where {
  initial = mkExtensionSet extensions;
  closure = go initial;
  go current = let {
  settings = concatMap impliedBy (extensionSetToList current);
  next = foldr applySettingToSet current settings} in if next == current then current else go next;
  impliedBy ext = Map.findWithDefault [] ext impliedExtensionMap};
applySettingToSet :: ExtensionSetting -> ExtensionSet -> ExtensionSet;
applySettingToSet (EnableExtension ext) = insertExtension ext;
applySettingToSet (DisableExtension ext) = deleteExtension ext;
effectiveExtensions :: LanguageEdition -> [ExtensionSetting] -> [Extension];
effectiveExtensions edition = List.foldl' applyOne (languageEditionExtensions edition) where {
  applyOne extensions setting@(EnableExtension _) = applyImpliedExtensions (applyExtensionSetting setting extensions);
  applyOne extensions setting@(DisableExtension _) = applyExtensionSetting setting extensions};
data SourceSpan = PackedSourceSpan !Text !Word64 !Word64 !Word64 deriving (Data, Eq, Ord, Generic);
pattern SourceSpan :: Text -> Int -> Int -> Int -> Int -> Int -> Int -> SourceSpan;
pattern SourceSpan {sourceSpanSourceName, sourceSpanStartLine, sourceSpanStartCol, sourceSpanEndLine, sourceSpanEndCol, sourceSpanStartOffset, sourceSpanEndOffset} <- PackedSourceSpan sourceSpanSourceName (unpackPositions -> (sourceSpanStartLine, sourceSpanStartCol)) (unpackPositions -> (sourceSpanEndLine, sourceSpanEndCol)) (unpackPositions -> (sourceSpanStartOffset, sourceSpanEndOffset)) where {
  SourceSpan name startLine startCol endLine endCol startOffset endOffset = PackedSourceSpan name (packPositions startLine startCol) (packPositions endLine endCol) (packPositions startOffset endOffset)};
packPositions :: Int -> Int -> Word64;
packPositions high low = (fromIntegral high `shiftL` 32) .|. (fromIntegral low .&. 0xffffffff);
unpackPositions :: Word64 -> (Int, Int);
unpackPositions word = (highPosition word, lowPosition word);
highPosition :: Word64 -> Int;
highPosition word = fromIntegral (word `shiftR` 32);
lowPosition :: Word64 -> Int;
lowPosition word = fromIntegral (word .&. 0xffffffff);
instance NFData SourceSpan where {
  rnf span' = span' `seq` ()};
instance Show SourceSpan where {
  show SourceSpan {sourceSpanStartLine, sourceSpanStartCol, sourceSpanEndLine, sourceSpanEndCol} = "SourceSpan " ++ show sourceSpanStartLine ++ " " ++ show sourceSpanStartCol ++ " " ++ show sourceSpanEndLine ++ " " ++ show sourceSpanEndCol};
mergeSourceSpans :: SourceSpan -> SourceSpan -> SourceSpan;
mergeSourceSpans (PackedSourceSpan name start _ startOffsets) (PackedSourceSpan _ _ end endOffsets) = PackedSourceSpan name start end (packPositions (highPosition startOffsets) (lowPosition endOffsets));
data Name = Name { nameQualifier :: Maybe Text, nameType :: NameType, nameText :: Text, nameAnns :: [Annotation] } deriving (Eq, Show, Generic, NFData, Data);
data UnqualifiedName = UnqualifiedName { unqualifiedNameType :: NameType, unqualifiedNameText :: Text, unqualifiedNameAnns :: [Annotation] } deriving (Eq, Show, Generic, NFData, Data);
data NameType = NameVarId | NameConId | NameVarSym | NameConSym deriving (Eq, Show, Read, Generic, NFData, Enum, Bounded, Data);
mkName :: Maybe Text -> NameType -> Text -> Name;
mkName qualifier ty txt = Name qualifier ty txt [];
qualifyName :: Maybe Text -> UnqualifiedName -> Name;
qualifyName qualifier name = Name qualifier (unqualifiedNameType name) (unqualifiedNameText name) (unqualifiedNameAnns name);
mkUnqualifiedName :: NameType -> Text -> UnqualifiedName;
mkUnqualifiedName ty txt = UnqualifiedName ty txt [];
renderName :: Name -> Text;
renderName name = case nameQualifier name of {
  Just qualifier -> qualifier <> "." <> nameText name;
  Nothing -> nameText name};
renderUnqualifiedName :: UnqualifiedName -> Text;
renderUnqualifiedName = unqualifiedNameText;
instance IsString Name where {
  fromString = nameFromText . T.pack};
instance IsString UnqualifiedName where {
  fromString = unqualifiedNameFromText . T.pack};
nameFromText :: Text -> Name;
nameFromText txt = let {
  (qualifier, localName) = splitQualifiedIdentifierText txt} in Name qualifier (inferNameType localName) localName [] where {
  splitQualifiedIdentifierText fullName = case T.splitOn "." fullName of {
  [] -> (Nothing, fullName);
  [_] -> (Nothing, fullName);
  parts | all isModuleSegment (init parts) && isIdentifierSegment (last parts) -> (Just (T.intercalate "." (init parts)), last parts) | otherwise -> (Nothing, fullName)};
  isModuleSegment segment = case T.uncons segment of {
  Just (c, rest) -> isConIdentifierStartChar c && T.all isIdentChar rest;
  Nothing -> False};
  isIdentifierSegment segment = let {
  magicHashes = T.takeWhileEnd (== '#') segment;
  baseSegment = T.dropEnd (T.length magicHashes) segment} in case T.uncons baseSegment of {
  Just (c, rest) -> isIdentifierStartChar c && T.all isIdentChar rest;
  Nothing -> False}};
unqualifiedNameFromText :: Text -> UnqualifiedName;
unqualifiedNameFromText txt = UnqualifiedName (inferNameType txt) txt [];
inferNameType :: Text -> NameType;
inferNameType localName | isOperatorLikeText localName = if T.isPrefixOf ":" localName then NameConSym else NameVarSym | otherwise = case T.uncons localName of {
  Just (c, _) | isConIdentifierStartChar c -> NameConId;
  Just (c, _) | isIdentifierStartChar c -> NameVarId;
  _ -> NameConId};
isIdentChar :: Char -> Bool;
isIdentChar c = isIdentifierStartChar c || isIdentifierContinueChar c || c == '\'';
isIdentifierStartChar :: Char -> Bool;
isIdentifierStartChar c = c == '_' || generalCategory c == LowercaseLetter || isConIdentifierStartChar c;
isConIdentifierStartChar :: Char -> Bool;
isConIdentifierStartChar c = generalCategory c `elem` [UppercaseLetter, TitlecaseLetter];
isIdentifierNumberChar :: Char -> Bool;
isIdentifierNumberChar c = case generalCategory c of {
  DecimalNumber -> True;
  OtherNumber -> True;
  _ -> False};
isIdentifierContinueChar :: Char -> Bool;
isIdentifierContinueChar c = case generalCategory c of {
  LetterNumber -> True;
  ModifierLetter -> True;
  NonSpacingMark -> True;
  _ -> isIdentifierNumberChar c};
isOperatorLikeText :: Text -> Bool;
isOperatorLikeText op = not (T.null op) && T.all isOperatorChar op;
isOperatorChar :: Char -> Bool;
isOperatorChar c = c `elem` (":!#$%&*+./<=>?@\\^|-~" :: String) || isUnicodeOperatorChar c;
isUnicodeOperatorChar :: Char -> Bool;
isUnicodeOperatorChar c = case generalCategory c of {
  MathSymbol -> True;
  CurrencySymbol -> True;
  ModifierSymbol -> True;
  OtherSymbol -> True;
  ConnectorPunctuation -> c > '\x7f';
  DashPunctuation -> c > '\x7f';
  OtherPunctuation -> c > '\x7f';
  _ -> False};
type BinderName = UnqualifiedName;
type OperatorName = UnqualifiedName;
data PragmaUnpackKind = UnpackPragma | NoUnpackPragma deriving (Data, Eq, Ord, Show, Read, Generic, NFData);
data PragmaType = PragmaLanguage [ExtensionSetting] | PragmaInstanceOverlap InstanceOverlapPragma | PragmaWarning Text | PragmaDeprecated Text | PragmaInline Text Text | PragmaUnpack PragmaUnpackKind | PragmaSource Text Text | PragmaSCC Text | PragmaUnknown Text deriving (Data, Eq, Ord, Show, Read, Generic, NFData);
data Pragma = Pragma { pragmaType :: PragmaType, pragmaRawText :: Text } deriving (Data, Eq, Ord, Show, Read, Generic, NFData);
data Module = Module { moduleAnns :: [Annotation], moduleHead :: Maybe ModuleHead, moduleLanguagePragmas :: [ExtensionSetting], moduleImports :: [ImportDecl], moduleDecls :: [Decl] } deriving (Data, Eq, Show, Generic, NFData);
data ModuleHead = ModuleHead { moduleHeadAnns :: [Annotation], moduleHeadName :: Text, moduleHeadWarningPragma :: Maybe Pragma, moduleHeadExports :: Maybe [ExportSpec] } deriving (Data, Eq, Show, Generic, NFData);
moduleName :: Module -> Maybe Text;
moduleName modu = moduleHeadName <$> moduleHead modu;
moduleWarningPragma :: Module -> Maybe Pragma;
moduleWarningPragma modu = moduleHeadWarningPragma =<< moduleHead modu;
moduleExports :: Module -> Maybe [ExportSpec];
moduleExports modu = moduleHeadExports =<< moduleHead modu;
data IEEntityNamespace = IEEntityNamespaceType | IEEntityNamespacePattern | IEEntityNamespaceData deriving (Data, Eq, Show, Generic, NFData);
data IEBundledNamespace = IEBundledNamespaceType | IEBundledNamespaceData deriving (Data, Eq, Show, Generic, NFData);
data IEBundledMember = IEBundledMember { ieBundledMemberNamespace :: Maybe IEBundledNamespace, ieBundledMemberName :: Name } deriving (Data, Eq, Show, Generic, NFData);
data ExportSpec = ExportModule (Maybe Pragma) Text | ExportVar (Maybe Pragma) (Maybe IEEntityNamespace) Name | ExportAbs (Maybe Pragma) (Maybe IEEntityNamespace) Name | ExportAll (Maybe Pragma) (Maybe IEEntityNamespace) Name | ExportWith (Maybe Pragma) (Maybe IEEntityNamespace) Name [IEBundledMember] | ExportWithAll (Maybe Pragma) (Maybe IEEntityNamespace) Name Int [IEBundledMember] | ExportAnn Annotation ExportSpec deriving (Data, Eq, Show, Generic, NFData);
data ImportDecl = ImportDecl { importDeclAnns :: [Annotation], importDeclLevel :: Maybe ImportLevel, importDeclPackage :: Maybe Text, importDeclSourcePragma :: Maybe Pragma, importDeclSafe :: Bool, importDeclQualified :: Bool, importDeclQualifiedPost :: Bool, importDeclModule :: Text, importDeclAs :: Maybe Text, importDeclSpec :: Maybe ImportSpec } deriving (Data, Eq, Show, Generic, NFData);
data ImportLevel = ImportLevelQuote | ImportLevelSplice deriving (Data, Eq, Show, Generic, NFData);
data ImportSpec = ImportSpec { importSpecAnns :: [Annotation], importSpecHiding :: Bool, importSpecItems :: [ImportItem] } deriving (Data, Eq, Show, Generic, NFData);
data ImportItem = ImportItemVar (Maybe IEEntityNamespace) UnqualifiedName | ImportItemAbs (Maybe IEEntityNamespace) UnqualifiedName | ImportItemAll (Maybe IEEntityNamespace) UnqualifiedName | ImportItemWith (Maybe IEEntityNamespace) UnqualifiedName [IEBundledMember] | ImportItemAllWith (Maybe IEEntityNamespace) UnqualifiedName Int [IEBundledMember] | ImportAnn Annotation ImportItem deriving (Data, Eq, Show, Generic, NFData);
data Decl = DeclAnn Annotation Decl | DeclValue ValueDecl | DeclImplicitParam Text Expr (Maybe [Decl]) | DeclTypeSig [BinderName] Type | DeclPatSyn PatSynDecl | DeclPatSynSig [BinderName] Type | DeclStandaloneKindSig BinderName Type | DeclFixity FixityAssoc (Maybe IEEntityNamespace) (Maybe Int) [OperatorName] | DeclRoleAnnotation RoleAnnotation | DeclTypeSyn TypeSynDecl | DeclTypeData DataDecl | DeclData DataDecl | DeclNewtype NewtypeDecl | DeclClass ClassDecl | DeclInstance InstanceDecl | DeclStandaloneDeriving StandaloneDerivingDecl | DeclDefault [Type] | DeclSplice Expr | DeclForeign ForeignDecl | DeclTypeFamilyDecl TypeFamilyDecl | DeclDataFamilyDecl DataFamilyDecl | DeclTypeFamilyInst TypeFamilyInst | DeclDataFamilyInst DataFamilyInst | DeclPragma Pragma | DeclRules [RuleDecl] deriving (Data, Eq, Show, Generic, NFData);
data RuleDecl = RuleDecl { ruleAnns :: [Annotation], ruleName :: Text, ruleActivation :: Maybe RuleActivation, ruleTypeBinders :: [TyVarBinder], ruleBinders :: [RuleBinder], ruleLhs :: Expr, ruleRhs :: Expr } deriving (Data, Eq, Show, Generic, NFData);
data RuleActivation = RuleActiveAfter Int | RuleActiveBefore Int | RuleNeverActive deriving (Data, Eq, Show, Generic, NFData);
data RuleBinder = RuleBinder { ruleBinderAnns :: [Annotation], ruleBinderName :: UnqualifiedName, ruleBinderType :: Maybe Type } deriving (Data, Eq, Show, Generic, NFData);
peelDeclAnn :: Decl -> Decl;
peelDeclAnn (DeclAnn _ inner) = peelDeclAnn inner;
peelDeclAnn d = d;
data MultiplicityTag = NoMultiplicityTag | LinearMultiplicityTag | ExplicitMultiplicityTag Type deriving (Data, Eq, Show, Generic, NFData);
data ValueDecl = FunctionBind BinderName [Match] | PatternBind MultiplicityTag Pattern (Rhs Expr) deriving (Data, Eq, Show, Generic, NFData);
data Match = Match { matchAnns :: [Annotation], matchHeadForm :: MatchHeadForm, matchPats :: [Pattern], matchRhs :: Rhs Expr } deriving (Data, Eq, Show, Generic, NFData);
data MatchHeadForm = MatchHeadPrefix | MatchHeadInfix deriving (Data, Eq, Show, Generic, NFData);
data PatSynDir = PatSynUnidirectional | PatSynBidirectional | PatSynExplicitBidirectional [Match] deriving (Data, Eq, Show, Generic, NFData);
data PatSynArgs = PatSynPrefixArgs [Text] | PatSynInfixArgs Text Text | PatSynRecordArgs [Text] deriving (Data, Eq, Show, Generic, NFData);
data PatSynDecl = PatSynDecl { patSynDeclName :: UnqualifiedName, patSynDeclArgs :: PatSynArgs, patSynDeclPat :: Pattern, patSynDeclDir :: PatSynDir } deriving (Data, Eq, Show, Generic, NFData);
data Rhs body = UnguardedRhs [Annotation] body (Maybe [Decl]) | GuardedRhss [Annotation] [GuardedRhs body] (Maybe [Decl]) deriving (Data, Eq, Show, Generic, NFData);
data GuardedRhs body = GuardedRhs { guardedRhsAnns :: [Annotation], guardedRhsGuards :: [GuardQualifier], guardedRhsBody :: body } deriving (Data, Eq, Show, Generic, NFData);
data GuardQualifier = GuardAnn Annotation GuardQualifier | GuardExpr Expr | GuardPat Pattern Expr | GuardLet [Decl] deriving (Data, Eq, Show, Generic, NFData);
peelGuardQualifierAnn :: GuardQualifier -> GuardQualifier;
peelGuardQualifierAnn (GuardAnn _ inner) = peelGuardQualifierAnn inner;
peelGuardQualifierAnn q = q;
data Literal = LitAnn Annotation Literal | LitInt Integer NumericType Text | LitFloat Rational FloatType Text | LitChar Char Text | LitCharHash Char Text | LitString Text Text | LitStringHash Text Text deriving (Data, Eq, Show, Generic, NFData);
literalAnnSpan :: SourceSpan -> Literal -> Literal;
literalAnnSpan sp = LitAnn (mkAnnotation sp);
peelLiteralAnn :: Literal -> Literal;
peelLiteralAnn (LitAnn _ inner) = peelLiteralAnn inner;
peelLiteralAnn lit = lit;
data TupleFlavor = Boxed | Unboxed deriving (Data, Eq, Show, Generic, NFData);
data RecordField a = RecordField { recordFieldName :: Name, recordFieldValue :: a, recordFieldPun :: Bool } deriving (Data, Eq, Show, Generic, NFData);
data Pattern = PAnn Annotation Pattern | PVar UnqualifiedName | PTypeBinder TyVarBinder | PTypeSyntax TypeSyntaxForm Type | PWildcard | PLit Literal | PQuasiQuote Text Text | PTuple TupleFlavor [Pattern] | PUnboxedSum Int Int Pattern | PList [Pattern] | PCon Name [Type] [Pattern] | PBuiltinCon BuiltinCon [Type] [Pattern] | PInfix Pattern Name Pattern | PView Expr Pattern | PAs UnqualifiedName Pattern | PStrict Pattern | PIrrefutable Pattern | PNegLit Literal | PParen Pattern | PRecord Name [RecordField Pattern] Bool | PTypeSig Pattern Type | PSplice Expr deriving (Data, Eq, Show, Generic, NFData);
peelPatternAnn :: Pattern -> Pattern;
peelPatternAnn (PAnn _ inner) = peelPatternAnn inner;
peelPatternAnn p = p;
data TypeSyntaxForm = TypeSyntaxExplicitNamespace | TypeSyntaxInTerm deriving (Data, Eq, Show, Generic, NFData);
data ForallVis = ForallInvisible | ForallVisible deriving (Data, Eq, Show, Generic, NFData);
data ForallTelescope = ForallTelescope { forallTelescopeVisibility :: ForallVis, forallTelescopeBinders :: [TyVarBinder] } deriving (Data, Eq, Show, Generic, NFData);
data ArrowKind = ArrowUnrestricted | ArrowLinear | ArrowExplicit Type deriving (Data, Eq, Show, Generic, NFData);
data Type = TAnn Annotation Type | TVar UnqualifiedName | TCon Name TypePromotion | TBuiltinCon BuiltinCon TypePromotion | TImplicitParam Text Type | TTypeLit TypeLiteral | TStar Text | TQuasiQuote Text Text | TForall ForallTelescope Type | TApp Type Type | TTypeApp Type Type | TInfix Type Name TypePromotion Type | TFun ArrowKind Type Type | TTuple TupleFlavor TypePromotion [Type] | TUnboxedSum [Type] | TList TypePromotion [Type] | TParen Type | TKindSig Type Type | TContext [Type] Type | TSplice Expr | TWildcard deriving (Data, Eq, Show, Generic, NFData);
data BuiltinCon = BuiltinTuple TupleFlavor Int | BuiltinArrow | BuiltinList | BuiltinCons deriving (Data, Eq, Show, Generic, NFData);
typeAnnSpan :: SourceSpan -> Type -> Type;
typeAnnSpan sp = TAnn (mkAnnotation sp);
peelTypeAnn :: Type -> Type;
peelTypeAnn (TAnn _ inner) = peelTypeAnn inner;
peelTypeAnn t = t;
peelTypeHead :: Type -> Type;
peelTypeHead (TAnn _ inner) = peelTypeHead inner;
peelTypeHead (TParen inner) = peelTypeHead inner;
peelTypeHead ty = ty;
data TypeLiteral = TypeLitInteger Integer Text | TypeLitSymbol Text Text | TypeLitChar Char Text deriving (Data, Eq, Show, Generic, NFData);
data NumericType = TInteger | TIntHash | TWordHash | TInt8Hash | TInt16Hash | TInt32Hash | TInt64Hash | TWord8Hash | TWord16Hash | TWord32Hash | TWord64Hash deriving (Data, Eq, Ord, Show, Read, Generic, NFData);
data FloatType = TFractional | TFloatHash | TDoubleHash deriving (Data, Eq, Ord, Show, Read, Generic, NFData);
data TypePromotion = Unpromoted | Promoted deriving (Data, Eq, Show, Generic, NFData);
data TyVarBSpecificity = TyVarBInferred | TyVarBSpecified deriving (Data, Eq, Show, Generic, NFData);
data TyVarBVisibility = TyVarBVisible | TyVarBInvisible deriving (Data, Eq, Show, Generic, NFData);
data TyVarBinder = TyVarBinder { tyVarBinderAnns :: [Annotation], tyVarBinderName :: Text, tyVarBinderKind :: Maybe Type, tyVarBinderSpecificity :: TyVarBSpecificity, tyVarBinderVisibility :: TyVarBVisibility } deriving (Data, Eq, Show, Generic, NFData);
data TypeHeadForm = TypeHeadPrefix | TypeHeadInfix deriving (Data, Eq, Show, Generic, NFData);
data BinderHead name = PrefixBinderHead name [TyVarBinder] | InfixBinderHead TyVarBinder name TyVarBinder [TyVarBinder] deriving (Data, Eq, Show, Generic, NFData);
binderHeadForm :: BinderHead name -> TypeHeadForm;
binderHeadForm head' = case head' of {
  PrefixBinderHead {} -> TypeHeadPrefix;
  InfixBinderHead {} -> TypeHeadInfix};
binderHeadName :: BinderHead name -> name;
binderHeadName head' = case head' of {
  PrefixBinderHead name _ -> name;
  InfixBinderHead _ name _ _ -> name};
binderHeadParams :: BinderHead name -> [TyVarBinder];
binderHeadParams head' = case head' of {
  PrefixBinderHead _ params -> params;
  InfixBinderHead lhs _ rhs tailParams -> lhs : rhs : tailParams};
type InstanceHeadType = Type;
instanceHeadName :: Type -> Maybe Name;
instanceHeadName = go where {
  go (TAnn _ t) = go t;
  go (TApp f _) = go f;
  go (TTypeApp f _) = go f;
  go (TParen t) = go t;
  go (TCon name _) = Just name;
  go (TInfix _ name _ _) = Just name;
  go _ = Nothing};
instanceHeadTypes :: Type -> [Type];
instanceHeadTypes = go [] where {
  go acc (TAnn _ t) = go acc t;
  go acc (TApp f arg) = go (arg : acc) f;
  go acc (TTypeApp f _) = go acc f;
  go acc (TParen t) = go acc t;
  go _ (TInfix lhs _ _ rhs) = [lhs, rhs];
  go acc _ = acc};
data Role = RoleNominal | RoleRepresentational | RolePhantom | RoleInfer deriving (Data, Eq, Show, Generic, NFData);
data RoleAnnotation = RoleAnnotation { roleAnnotationName :: UnqualifiedName, roleAnnotationRoles :: [Role] } deriving (Data, Eq, Show, Generic, NFData);
data TypeSynDecl = TypeSynDecl { typeSynHead :: BinderHead UnqualifiedName, typeSynBody :: Type } deriving (Data, Eq, Show, Generic, NFData);
data TypeFamilyDecl = TypeFamilyDecl { typeFamilyDeclHeadForm :: TypeHeadForm, typeFamilyDeclExplicitFamilyKeyword :: Bool, typeFamilyDeclHead :: Type, typeFamilyDeclParams :: [TyVarBinder], typeFamilyDeclResultSig :: Maybe TypeFamilyResultSig, typeFamilyDeclEquations :: Maybe [TypeFamilyEq] } deriving (Data, Eq, Show, Generic, NFData);
data TypeFamilyResultSig = TypeFamilyKindSig Type | TypeFamilyTyVarSig TyVarBinder | TypeFamilyInjectiveSig TyVarBinder TypeFamilyInjectivity deriving (Data, Eq, Show, Generic, NFData);
data TypeFamilyInjectivity = TypeFamilyInjectivity { typeFamilyInjectivityAnns :: [Annotation], typeFamilyInjectivityResult :: Text, typeFamilyInjectivityDetermined :: [Text] } deriving (Data, Eq, Show, Generic, NFData);
data TypeFamilyEq = TypeFamilyEq { typeFamilyEqAnns :: [Annotation], typeFamilyEqForall :: [TyVarBinder], typeFamilyEqHeadForm :: TypeHeadForm, typeFamilyEqLhs :: Type, typeFamilyEqRhs :: Type } deriving (Data, Eq, Show, Generic, NFData);
data DataFamilyDecl = DataFamilyDecl { dataFamilyDeclHead :: BinderHead UnqualifiedName, dataFamilyDeclKind :: Maybe Type } deriving (Data, Eq, Show, Generic, NFData);
data TypeFamilyInst = TypeFamilyInst { typeFamilyInstForall :: [TyVarBinder], typeFamilyInstHeadForm :: TypeHeadForm, typeFamilyInstLhs :: Type, typeFamilyInstRhs :: Type } deriving (Data, Eq, Show, Generic, NFData);
data DataFamilyInst = DataFamilyInst { dataFamilyInstIsNewtype :: Bool, dataFamilyInstForall :: [TyVarBinder], dataFamilyInstHead :: Type, dataFamilyInstKind :: Maybe Type, dataFamilyInstConstructors :: [DataConDecl], dataFamilyInstDeriving :: [DerivingClause] } deriving (Data, Eq, Show, Generic, NFData);
data DataDecl = DataDecl { dataDeclCTypePragma :: Maybe Pragma, dataDeclHead :: BinderHead UnqualifiedName, dataDeclContext :: [Type], dataDeclKind :: Maybe Type, dataDeclConstructors :: [DataConDecl], dataDeclDeriving :: [DerivingClause] } deriving (Data, Eq, Show, Generic, NFData);
data NewtypeDecl = NewtypeDecl { newtypeDeclCTypePragma :: Maybe Pragma, newtypeDeclHead :: BinderHead UnqualifiedName, newtypeDeclContext :: [Type], newtypeDeclKind :: Maybe Type, newtypeDeclConstructor :: Maybe DataConDecl, newtypeDeclDeriving :: [DerivingClause] } deriving (Data, Eq, Show, Generic, NFData);
data DataConDecl = DataConAnn Annotation DataConDecl | PrefixCon [TyVarBinder] [Type] UnqualifiedName [BangType] | InfixCon [TyVarBinder] [Type] BangType UnqualifiedName BangType | RecordCon [TyVarBinder] [Type] UnqualifiedName [FieldDecl] | GadtCon [ForallTelescope] [Type] [UnqualifiedName] GadtBody | TupleCon [TyVarBinder] [Type] TupleFlavor [BangType] | UnboxedSumCon [TyVarBinder] [Type] Int Int BangType | ListCon [TyVarBinder] [Type] deriving (Data, Eq, Show, Generic, NFData);
peelDataConAnn :: DataConDecl -> DataConDecl;
peelDataConAnn (DataConAnn _ inner) = peelDataConAnn inner;
peelDataConAnn d = d;
data GadtBody = GadtPrefixBody [(BangType, ArrowKind)] Type | GadtRecordBody [FieldDecl] Type deriving (Data, Eq, Show, Generic, NFData);
gadtBodyResultType :: GadtBody -> Type;
gadtBodyResultType body = case body of {
  GadtPrefixBody _ ty -> ty;
  GadtRecordBody _ ty -> ty};
data BangType = BangType { bangAnns :: [Annotation], bangPragmas :: [Pragma], bangStrict :: Bool, bangLazy :: Bool, bangType :: Type } deriving (Data, Eq, Show, Generic, NFData);
data FieldDecl = FieldDecl { fieldAnns :: [Annotation], fieldNames :: [UnqualifiedName], fieldMultiplicity :: Maybe Type, fieldType :: BangType } deriving (Data, Eq, Show, Generic, NFData);
data DerivingClause = DerivingClause { derivingStrategy :: Maybe DerivingStrategy, derivingClasses :: Either Name [Type] } deriving (Data, Eq, Show, Generic, NFData);
data DerivingStrategy = DerivingStock | DerivingNewtype | DerivingAnyclass | DerivingVia Type deriving (Data, Eq, Show, Generic, NFData);
data StandaloneDerivingDecl = StandaloneDerivingDecl { standaloneDerivingStrategy :: Maybe DerivingStrategy, standaloneDerivingPragmas :: [Pragma], standaloneDerivingWarning :: Maybe Pragma, standaloneDerivingForall :: [TyVarBinder], standaloneDerivingContext :: [Type], standaloneDerivingHead :: InstanceHeadType } deriving (Data, Eq, Show, Generic, NFData);
data ClassDecl = ClassDecl { classDeclContext :: Maybe [Type], classDeclHead :: BinderHead UnqualifiedName, classDeclFundeps :: [FunctionalDependency], classDeclItems :: [ClassDeclItem] } deriving (Data, Eq, Show, Generic, NFData);
data FunctionalDependency = FunctionalDependency { functionalDependencyAnns :: [Annotation], functionalDependencyDeterminers :: [Text], functionalDependencyDetermined :: [Text] } deriving (Data, Eq, Show, Generic, NFData);
data ClassDeclItem = ClassItemAnn Annotation ClassDeclItem | ClassItemTypeSig [BinderName] Type | ClassItemDefaultSig BinderName Type | ClassItemFixity FixityAssoc (Maybe IEEntityNamespace) (Maybe Int) [OperatorName] | ClassItemDefault ValueDecl | ClassItemTypeFamilyDecl TypeFamilyDecl | ClassItemDataFamilyDecl DataFamilyDecl | ClassItemDefaultTypeInst TypeFamilyInst | ClassItemPragma Pragma deriving (Data, Eq, Show, Generic, NFData);
peelClassDeclItemAnn :: ClassDeclItem -> ClassDeclItem;
peelClassDeclItemAnn (ClassItemAnn _ inner) = peelClassDeclItemAnn inner;
peelClassDeclItemAnn item = item;
data InstanceDecl = InstanceDecl { instanceDeclPragmas :: [Pragma], instanceDeclWarning :: Maybe Pragma, instanceDeclForall :: [TyVarBinder], instanceDeclContext :: [Type], instanceDeclHead :: InstanceHeadType, instanceDeclItems :: [InstanceDeclItem] } deriving (Data, Eq, Show, Generic, NFData);
data InstanceOverlapPragma = Overlapping | Overlappable | Overlaps | Incoherent deriving (Data, Eq, Ord, Show, Read, Generic, NFData);
data InstanceDeclItem = InstanceItemAnn Annotation InstanceDeclItem | InstanceItemBind ValueDecl | InstanceItemTypeSig [BinderName] Type | InstanceItemFixity FixityAssoc (Maybe IEEntityNamespace) (Maybe Int) [OperatorName] | InstanceItemTypeFamilyInst TypeFamilyInst | InstanceItemDataFamilyInst DataFamilyInst | InstanceItemPragma Pragma deriving (Data, Eq, Show, Generic, NFData);
peelInstanceDeclItemAnn :: InstanceDeclItem -> InstanceDeclItem;
peelInstanceDeclItemAnn (InstanceItemAnn _ inner) = peelInstanceDeclItemAnn inner;
peelInstanceDeclItemAnn item = item;
data FixityAssoc = Infix | InfixL | InfixR deriving (Data, Eq, Show, Read, Generic, NFData);
data ForeignDecl = ForeignDecl { foreignDirection :: ForeignDirection, foreignCallConv :: CallConv, foreignSafety :: Maybe ForeignSafety, foreignEntity :: ForeignEntitySpec, foreignName :: UnqualifiedName, foreignType :: Type } deriving (Data, Eq, Show, Generic, NFData);
data ForeignEntitySpec = ForeignEntityDynamic | ForeignEntityWrapper | ForeignEntityStatic (Maybe Text) | ForeignEntityAddress (Maybe Text) | ForeignEntityNamed Text | ForeignEntityOmitted deriving (Data, Eq, Show, Generic, NFData);
data ForeignDirection = ForeignImport | ForeignExport deriving (Data, Eq, Show, Generic, NFData);
data CallConv = CCall | StdCall | CApi | CPrim | JavaScript deriving (Data, Eq, Show, Generic, NFData);
data ForeignSafety = Safe | Unsafe | Interruptible deriving (Data, Eq, Show, Generic, NFData);
data Annotation = SourceSpanAnnotation !SourceSpan | DynamicAnnotation Dynamic deriving (Show, Generic);
mkAnnotation :: (Typeable a) => a -> Annotation;
mkAnnotation value = case cast value of {
  Just span' -> SourceSpanAnnotation span';
  Nothing -> DynamicAnnotation (toDyn value)};
fromAnnotation :: (Typeable a) => Annotation -> Maybe a;
fromAnnotation (SourceSpanAnnotation span') = cast span';
fromAnnotation (DynamicAnnotation value) = fromDynamic value;
instance Data Annotation where {
  gfoldl _ z = z;
  gunfold _ z _ = z (DynamicAnnotation (toDyn ()));
  toConstr _ = annotationConstr;
  dataTypeOf _ = annotationDataType};
annotationConstr :: Constr;
annotationConstr = mkConstr annotationDataType "Annotation" [] Prefix;
annotationDataType :: DataType;
annotationDataType = mkDataType "Aihc.Parser.Syntax.Annotation" [annotationConstr];
instance Eq Annotation where {
  _ == _ = True};
instance NFData Annotation where {
  rnf (SourceSpanAnnotation span') = rnf span';
  rnf (DynamicAnnotation _) = ()};
data Expr = EAnn Annotation Expr | EVar Name | EImplicitParam Text | ETypeSyntax TypeSyntaxForm Type | EInt Integer NumericType Text | EFloat Rational FloatType Text | EChar Char Text | ECharHash Char Text | EString Text Text | EStringHash Text Text | EOverloadedLabel Text Text | EQuasiQuote Text Text | EIf Expr Expr Expr | EMultiWayIf [GuardedRhs Expr] | ELambdaPats [Pattern] Expr | ELambdaCase [CaseAlt Expr] | ELambdaCases [LambdaCaseAlt] | EInfix Expr Name Expr | EViewPat Expr Expr | ENegate Expr | ESectionL Expr Name | ESectionR Name Expr | ELetDecls [Decl] Expr | ECase Expr [CaseAlt Expr] | EDo [DoStmt Expr] DoFlavor | EListComp Expr [CompStmt] | EListCompParallel Expr [[CompStmt]] | EArithSeq ArithSeq | ERecordCon Name [RecordField Expr] Bool | ERecordUpd Expr [RecordField Expr] | EGetField Expr Name | EGetFieldProjection [Name] | ETypeSig Expr Type | EParen Expr | EList [Expr] | ETuple TupleFlavor [Maybe Expr] | EUnboxedSum Int Int Expr | ETypeApp Expr Type | EApp Expr Expr | ETHExpQuote Expr | ETHTypedQuote Expr | ETHDeclQuote [Decl] | ETHTypeQuote Type | ETHPatQuote Pattern | ETHNameQuote Expr | ETHTypeNameQuote Type | ETHSplice Expr | ETHTypedSplice Expr | EProc Pattern Cmd | EPragma Pragma Expr deriving (Data, Eq, Show, Generic, NFData);
peelExprAnn :: Expr -> Expr;
peelExprAnn (EAnn _ x) = peelExprAnn x;
peelExprAnn x = x;
data CaseAlt body = CaseAlt { caseAltAnns :: [Annotation], caseAltPattern :: Pattern, caseAltRhs :: Rhs body } deriving (Data, Eq, Show, Generic, NFData);
data LambdaCaseAlt = LambdaCaseAlt { lambdaCaseAltAnns :: [Annotation], lambdaCaseAltPats :: [Pattern], lambdaCaseAltRhs :: Rhs Expr } deriving (Data, Eq, Show, Generic, NFData);
data DoFlavor = DoPlain | DoMdo | DoQualified Text | DoQualifiedMdo Text deriving (Data, Eq, Show, Generic, NFData);
data DoStmt body = DoAnn Annotation (DoStmt body) | DoBind Pattern body | DoLetDecls [Decl] | DoExpr body | DoRecStmt [DoStmt body] deriving (Data, Eq, Show, Generic, NFData);
peelDoStmtAnn :: DoStmt body -> DoStmt body;
peelDoStmtAnn (DoAnn _ inner) = peelDoStmtAnn inner;
peelDoStmtAnn s = s;
data Cmd = CmdAnn Annotation Cmd | CmdArrApp Expr ArrAppType Expr | CmdInfix Cmd Name Cmd | CmdDo [DoStmt Cmd] | CmdIf Expr Cmd Cmd | CmdCase Expr [CaseAlt Cmd] | CmdLet [Decl] Cmd | CmdLam [Pattern] Cmd | CmdApp Cmd Expr | CmdPar Cmd deriving (Data, Eq, Show, Generic, NFData);
peelCmdAnn :: Cmd -> Cmd;
peelCmdAnn (CmdAnn _ inner) = peelCmdAnn inner;
peelCmdAnn c = c;
data ArrAppType = HsFirstOrderApp | HsHigherOrderApp deriving (Data, Eq, Show, Generic, NFData);
data CompStmt = CompAnn Annotation CompStmt | CompGen Pattern Expr | CompGuard Expr | CompLetDecls [Decl] | CompThen Expr | CompThenBy Expr Expr | CompGroupUsing Expr | CompGroupByUsing Expr Expr deriving (Data, Eq, Show, Generic, NFData);
peelCompStmtAnn :: CompStmt -> CompStmt;
peelCompStmtAnn (CompAnn _ inner) = peelCompStmtAnn inner;
peelCompStmtAnn s = s;
data ArithSeq = ArithSeqAnn Annotation ArithSeq | ArithSeqFrom Expr | ArithSeqFromThen Expr Expr | ArithSeqFromTo Expr Expr | ArithSeqFromThenTo Expr Expr Expr deriving (Data, Eq, Show, Generic, NFData);
peelArithSeqAnn :: ArithSeq -> ArithSeq;
peelArithSeqAnn (ArithSeqAnn _ inner) = peelArithSeqAnn inner;
peelArithSeqAnn s = s;
stripAnnotations :: (Data a) => a -> a;
stripAnnotations x = applyStrip (gmapT stripAnnotations x) where {
  applyStrip :: (Data c) => c -> c;
  applyStrip = id `extT` sExpr `extT` sType `extT` sPattern `extT` sDecl `extT` sDataConDecl `extT` sLiteral `extT` sGuardQualifier `extT` (sDoStmt :: DoStmt Expr -> DoStmt Expr) `extT` (sDoStmt :: DoStmt Cmd -> DoStmt Cmd) `extT` sCompStmt `extT` sArithSeq `extT` sClassDeclItem `extT` sInstanceDeclItem `extT` sCmd `extT` sName `extT` sUnqualifiedName `extT` sExportSpec `extT` sImportItem `extT` sAnnotations `extT` sPragma;
  extT :: (Typeable c, Typeable d) => (c -> c) -> (d -> d) -> c -> c;
  extT f g y = fromMaybe (f y) (cast . g =<< cast y);
  sExpr :: Expr -> Expr;
  sExpr (EAnn _ e) = e;
  sExpr e = e;
  sType :: Type -> Type;
  sType (TAnn _ t) = t;
  sType t = t;
  sPattern :: Pattern -> Pattern;
  sPattern (PAnn _ p) = p;
  sPattern p = p;
  sDecl :: Decl -> Decl;
  sDecl (DeclAnn _ d) = d;
  sDecl d = d;
  sDataConDecl :: DataConDecl -> DataConDecl;
  sDataConDecl (DataConAnn _ d) = d;
  sDataConDecl d = d;
  sLiteral :: Literal -> Literal;
  sLiteral (LitAnn _ l) = l;
  sLiteral l = l;
  sGuardQualifier :: GuardQualifier -> GuardQualifier;
  sGuardQualifier (GuardAnn _ q) = q;
  sGuardQualifier q = q;
  sDoStmt :: DoStmt body -> DoStmt body;
  sDoStmt (DoAnn _ s) = s;
  sDoStmt s = s;
  sCompStmt :: CompStmt -> CompStmt;
  sCompStmt (CompAnn _ s) = s;
  sCompStmt s = s;
  sArithSeq :: ArithSeq -> ArithSeq;
  sArithSeq (ArithSeqAnn _ s) = s;
  sArithSeq s = s;
  sClassDeclItem :: ClassDeclItem -> ClassDeclItem;
  sClassDeclItem (ClassItemAnn _ i) = i;
  sClassDeclItem i = i;
  sInstanceDeclItem :: InstanceDeclItem -> InstanceDeclItem;
  sInstanceDeclItem (InstanceItemAnn _ i) = i;
  sInstanceDeclItem i = i;
  sCmd :: Cmd -> Cmd;
  sCmd (CmdAnn _ c) = c;
  sCmd c = c;
  sName :: Name -> Name;
  sName name = name {nameAnns = []};
  sUnqualifiedName :: UnqualifiedName -> UnqualifiedName;
  sUnqualifiedName name = name {unqualifiedNameAnns = []};
  sExportSpec :: ExportSpec -> ExportSpec;
  sExportSpec (ExportAnn _ e) = e;
  sExportSpec e = e;
  sImportItem :: ImportItem -> ImportItem;
  sImportItem (ImportAnn _ i) = i;
  sImportItem i = i;
  sAnnotations :: [Annotation] -> [Annotation];
  sAnnotations _ = [];
  sPragma :: Pragma -> Pragma;
  sPragma p = p {pragmaRawText = ""}}
}
