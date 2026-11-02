{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module Aihc.Resolve.Types (pattern DeclResolution, pattern EResolution, pattern PResolution, pattern TResolution, ResolutionNamespace(..), Identifier(..), displayIdentifier, PackageId(..), Package(..), unnamedPackage, ModuleUnit(..), modulesInPackage, ResolvedName(..), ResolutionAnnotation(..), VisibleTermIdentities(..), ResolveError(..), resolutionError, ResolveResult(..)) where {
import Aihc.Parser.Syntax (Decl(..), Expr(..), Extension, Module(..), Name(..), Pattern(..), SourceSpan, TupleFlavor(..), Type(..), UnqualifiedName(..), fromAnnotation);
import Control.DeepSeq (NFData);
import Data.String (IsString(..));
import Data.Text (Text);
import Data.Text qualified as T;
import GHC.Generics (Generic);
newtype PackageId = PackageId { packageIdText :: Text } deriving (Eq, Ord, Show, Read, Generic);
instance IsString PackageId where {
  fromString = PackageId . T.pack};
data Package = Package { packageName :: !Text, packageId :: !PackageId } deriving (Eq, Ord, Show, Generic);
unnamedPackage :: Package;
unnamedPackage = Package "" (PackageId "main");
data ModuleUnit = ModuleUnit { moduleUnitPackage :: !Package, moduleUnitExtensions :: ![Extension], moduleUnitAst :: !Module } deriving (Show);
modulesInPackage :: Package -> [(Module, [Extension])] -> [ModuleUnit];
modulesInPackage package = map unitInPackage where {
  unitInPackage (modu, extensions) = ModuleUnit package extensions modu};
newtype VisibleTermIdentities = VisibleTermIdentities [(PackageId, Text, Text)] deriving (Eq, Show);
data ResolvedName = ResolvedTopLevel PackageId Text Name | ResolvedLocal Int UnqualifiedName | ResolvedSyntax | ResolvedError String deriving (Eq, Show, Generic);
data Identifier = IdentifierTuple !TupleFlavor !Int | IdentifierList | IdentifierNamed !Text deriving (Eq, Show, Generic);
displayIdentifier :: Identifier -> Text;
displayIdentifier identifier = case identifier of {
  IdentifierTuple flavor arity -> case flavor of {
  Boxed -> "(" <> T.replicate (max 0 (arity - 1)) "," <> ")";
  Unboxed -> "(#" <> T.replicate (max 0 (arity - 1)) "," <> "#)"};
  IdentifierList -> "[]";
  IdentifierNamed name -> name};
data ResolutionNamespace = ResolutionNamespaceTerm | ResolutionNamespaceType | ResolutionNamespaceModule deriving (Eq, Ord, Show, Read, Generic);
instance NFData PackageId where {};
instance NFData Package where {};
instance NFData ResolvedName where {};
instance NFData Identifier where {};
instance NFData ResolutionNamespace where {};
data ResolutionAnnotation = ResolutionAnnotation { resolutionSpan :: !(Maybe SourceSpan), resolutionIdentifier :: !Identifier, resolutionNamespace :: !ResolutionNamespace, resolutionTarget :: !ResolvedName } deriving (Eq, Show);
data ResolveError = ResolveResolutionError { resolveErrorSpan :: !(Maybe SourceSpan), resolveErrorName :: !Text, resolveErrorNamespace :: !ResolutionNamespace, resolveErrorMessage :: !String } | ResolveNotImplemented String deriving (Eq, Show);
resolutionError :: ResolutionAnnotation -> String -> ResolveError;
resolutionError annotation message = ResolveResolutionError {resolveErrorSpan = resolutionSpan annotation, resolveErrorName = displayIdentifier (resolutionIdentifier annotation), resolveErrorNamespace = resolutionNamespace annotation, resolveErrorMessage = message};
data ResolveResult = ResolveResult { resolvedModules :: [ModuleUnit], resolveErrors :: [ResolveError] } deriving (Show);
pattern DeclResolution :: ResolutionAnnotation -> Decl;
pattern DeclResolution resolution <- DeclAnn (fromAnnotation -> Just resolution) _;
pattern PResolution :: ResolutionAnnotation -> Pattern;
pattern PResolution resolution <- PAnn (fromAnnotation -> Just resolution) _;
pattern TResolution :: ResolutionAnnotation -> Type;
pattern TResolution resolution <- TAnn (fromAnnotation -> Just resolution) _;
pattern EResolution :: ResolutionAnnotation -> Expr;
pattern EResolution resolution <- EAnn (fromAnnotation -> Just resolution) _
}
