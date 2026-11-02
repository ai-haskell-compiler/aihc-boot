{-# LANGUAGE OverloadedStrings #-}
module Aihc.Tc.Deriving.StockClass (StockClass(..), StockMethods(..), StockObligations(..), NewtypeDefaulting(..), stockClasses, lookupStockClass, stockClassRequirement, stockClassMethodsOf, stockClassObligationsOf, generatesStockMethods, newtypeDefaultingOf) where {
import Aihc.Parser.Syntax (Extension(..));
import Aihc.Tc.Deriving.References (DerivingReference, DerivingReferences(..), genericTermReferences);
import Data.List (find);
import Data.Maybe (isJust);
import Data.Text (Text);
data StockClass = StockClass { stockClassName :: !Text, stockClassExtension :: !(Maybe Extension), stockClassNewtypeDefaulting :: !NewtypeDefaulting, stockClassObligations :: !StockObligations, stockClassMethods :: !(Maybe StockMethods), stockClassReferences :: ![DerivingReferences -> DerivingReference] };
data StockMethods = StockEqMethods | StockOrdMethods | StockShowMethods | StockReadMethods | StockBoundedMethods | StockEnumMethods | StockLiftMethods | StockFunctorMethods | StockFoldableMethods | StockTraversableMethods | StockGenericMethods deriving (Eq, Show);
data StockObligations = FieldObligations | FunctorialObligations | NoObligations deriving (Eq, Show);
data NewtypeDefaulting = NewtypeAlways | NewtypeWithGnd | NewtypeNever deriving (Eq, Show);
stockClasses :: [StockClass];
stockClasses = [(report "Eq" NewtypeAlways) {stockClassMethods = Just StockEqMethods, stockClassReferences = [derivingTrue, derivingFalse]}, (report "Ord" NewtypeAlways) {stockClassMethods = Just StockOrdMethods, stockClassReferences = [derivingLT, derivingEQ, derivingGT]}, (report "Show" NewtypeNever) {stockClassMethods = Just StockShowMethods, stockClassReferences = [derivingIntCon, derivingGreaterOrEqual, derivingCons]}, (report "Read" NewtypeNever) {stockClassMethods = Just StockReadMethods, stockClassReferences = [derivingIntCon, derivingBind, derivingThen, derivingReturn, derivingReadParens, derivingReadPrecContext, derivingReadStep, derivingReadReset, derivingReadAlternative, derivingReadFail, derivingReadExpect, derivingReadField, derivingReadSymField, derivingLexemeIdent, derivingLexemeSymbol, derivingLexemePunc]}, (report "Bounded" NewtypeAlways) {stockClassMethods = Just StockBoundedMethods}, (report "Enum" NewtypeWithGnd) {stockClassMethods = Just StockEnumMethods, stockClassReferences = [derivingIntCon, derivingTrue, derivingFalse, derivingGreaterOrEqual]}, report "Ix" NewtypeAlways, (extension "Functor" DeriveFunctor NewtypeWithGnd) {stockClassObligations = FunctorialObligations, stockClassMethods = Just StockFunctorMethods}, (extension "Foldable" DeriveFoldable NewtypeWithGnd) {stockClassObligations = FunctorialObligations, stockClassMethods = Just StockFoldableMethods}, (extension "Traversable" DeriveTraversable NewtypeNever) {stockClassObligations = FunctorialObligations, stockClassMethods = Just StockTraversableMethods, stockClassReferences = [derivingPure, derivingApply]}, extension "Data" DeriveDataTypeable NewtypeNever, extension "Typeable" DeriveDataTypeable NewtypeNever, (extension "Generic" DeriveGeneric NewtypeNever) {stockClassObligations = NoObligations, stockClassMethods = Just StockGenericMethods, stockClassReferences = [select . derivingGeneric | select <- genericTermReferences]}, extension "Generic1" DeriveGeneric NewtypeNever, (extension "Lift" DeriveLift NewtypeNever) {stockClassMethods = Just StockLiftMethods, stockClassReferences = [derivingBind, derivingPure, derivingLiftConE, derivingLiftAppE, derivingLiftDataConName, derivingLiftCodeCoerce]}] where {
  report name defaulting = StockClass name Nothing defaulting FieldObligations Nothing [];
  extension name required defaulting = (report name defaulting) {stockClassExtension = Just required}};
lookupStockClass :: Text -> Maybe StockClass;
lookupStockClass className = find ((== className) . stockClassName) stockClasses;
stockClassRequirement :: Text -> Maybe (Maybe Extension);
stockClassRequirement className = stockClassExtension <$> lookupStockClass className;
stockClassMethodsOf :: Text -> Maybe StockMethods;
stockClassMethodsOf className = lookupStockClass className >>= stockClassMethods;
stockClassObligationsOf :: Text -> Maybe StockObligations;
stockClassObligationsOf className = stockClassObligations <$> lookupStockClass className;
generatesStockMethods :: Text -> Bool;
generatesStockMethods = isJust . stockClassMethodsOf;
newtypeDefaultingOf :: Text -> NewtypeDefaulting;
newtypeDefaultingOf className = maybe NewtypeNever stockClassNewtypeDefaulting (lookupStockClass className)
}
