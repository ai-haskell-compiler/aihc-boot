{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
module Aihc.Fc.Name (Sort(..), NameClass(..), nameClass, Origin(..), Name(..), ScopeTable(..), emptyScopeTable, lookupScope, insertScope, scopeEntries, Vis(..)) where {
import Aihc.Resolve (PackageId);
import Aihc.Tc.Types (Unique(..));
import Control.DeepSeq (NFData(..));
import Data.Map.Strict (Map);
import Data.Map.Strict qualified as Map;
import Data.Text (Text);
import GHC.Generics (Generic);
data Sort = SortTypeConstructor | SortDataConstructor | SortValue | SortTypeVariable | SortAxiom | SortSynonym deriving stock (Eq, Ord, Show, Read, Generic) deriving anyclass (NFData);
data NameClass = NameClassType | NameClassValue | NameClassAxiom | NameClassTypeVar deriving stock (Eq, Ord, Show, Read, Generic) deriving anyclass (NFData);
nameClass :: Sort -> NameClass;
nameClass sort = case sort of {
  SortTypeConstructor -> NameClassType;
  SortSynonym -> NameClassType;
  SortValue -> NameClassValue;
  SortDataConstructor -> NameClassValue;
  SortAxiom -> NameClassAxiom;
  SortTypeVariable -> NameClassTypeVar};
data Origin = OriginLocal Unique | OriginTop PackageId Text deriving stock (Eq, Ord, Show, Read, Generic);
instance NFData Origin where {
  rnf origin = case origin of {
  OriginLocal unique -> unique `seq` ();
  OriginTop package name -> package `seq` rnf name}};
data Name = Name { nameText :: Text, nameSort :: Sort, nameOrigin :: Origin } deriving stock (Eq, Ord, Show, Read, Generic) deriving anyclass (NFData);
newtype ScopeTable = ScopeTable (Map Int (PackageId, Text)) deriving stock (Eq, Ord, Show, Read, Generic);
instance NFData ScopeTable where {
  rnf (ScopeTable table) = rnf [package `seq` rnf name | (package, name) <- Map.elems table]};
emptyScopeTable :: ScopeTable;
emptyScopeTable = ScopeTable Map.empty;
lookupScope :: Int -> ScopeTable -> Maybe (PackageId, Text);
lookupScope scopeId (ScopeTable table) = Map.lookup scopeId table;
insertScope :: Int -> PackageId -> Text -> ScopeTable -> ScopeTable;
insertScope scopeId package moduleName (ScopeTable table) = ScopeTable (Map.insert scopeId (package, moduleName) table);
scopeEntries :: ScopeTable -> [(Int, PackageId, Text)];
scopeEntries (ScopeTable table) = [(scopeId, package, moduleName) | (scopeId, (package, moduleName)) <- Map.toAscList table];
data Vis = Pub | Private deriving stock (Eq, Ord, Show, Read, Generic) deriving anyclass (NFData)
}
