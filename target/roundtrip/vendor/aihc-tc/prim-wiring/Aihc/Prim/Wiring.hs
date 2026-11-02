{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Aihc.Prim.Wiring (primTcConfig, primTcWiring, primDerivingReferences, boxedTupleTyConName, boxedTupleDataConName, unboxedTupleTyConName, unboxedSumTyConName, unboxedSumDataConName) where {
import Aihc.Parser.Syntax (NameType(..));
import Aihc.Resolve (PackageId(..), ResolutionNamespace(..));
import Aihc.Tc (DerivingReference(..), DerivingReferences(..), GenericReferences(..), ReferencePackage(..), StockClassLocation(..), TcConfig, TcWiring(..), TyCon, mkTcConfig, mkTyConWithNamespace);
import Aihc.Tc.TypeLitFamily (typeLitFamilyModules);
import Data.Set qualified as Set;
import Data.Text (Text);
import Data.Text qualified as T;
primTcConfig :: PackageId -> TcConfig;
primTcConfig prim = mkTcConfig prim (primDerivingReferences prim) (primTcWiring prim);
primTcWiring :: PackageId -> TcWiring;
primTcWiring prim = TcWiring {tcWiringBoxedTupleTyCon = \arity -> tyCon ResolutionNamespaceType "GHC.Tuple" (boxedTupleTyConName arity) arity, tcWiringBoxedTupleDataCon = \arity -> tyCon ResolutionNamespaceTerm "GHC.Tuple" (boxedTupleDataConName arity) arity, tcWiringUnboxedTupleTyCon = unboxedTuple ResolutionNamespaceType, tcWiringUnboxedTupleDataCon = unboxedTuple ResolutionNamespaceTerm, tcWiringUnboxedSumTyCon = \arity -> types ResolutionNamespaceType (unboxedSumTyConName arity) arity, tcWiringUnboxedSumDataCon = \alternative arity -> types ResolutionNamespaceTerm (unboxedSumDataConName alternative arity) 1, tcWiringListTyCon = types ResolutionNamespaceType "[]" 1, tcWiringListDeclaration = types ResolutionNamespaceType "List" 1, tcWiringNilDataCon = types ResolutionNamespaceTerm "[]" 0, tcWiringConsDataCon = types ResolutionNamespaceTerm ":" 2, tcWiringArrowTyCon = types ResolutionNamespaceType "(->)" 2, tcWiringTypeTyCon = types ResolutionNamespaceType "Type" 0, tcWiringConstraintTyCon = types ResolutionNamespaceType "Constraint" 0, tcWiringConstraintTupleTyCon = tyCon ResolutionNamespaceType "GHC.Classes" "CTuple0" 0, tcWiringBoolTyCon = types ResolutionNamespaceType "Bool" 0, tcWiringCharTyCon = types ResolutionNamespaceType "Char" 0, tcWiringNaturalTyCon = tyCon ResolutionNamespaceType "GHC.Prim.Natural" "Natural" 0, tcWiringSymbolTyCon = types ResolutionNamespaceType "Symbol" 0, tcWiringEqualityTyCon = types ResolutionNamespaceType "~" 2, tcWiringCoercibleTyCon = types ResolutionNamespaceType "Coercible" 2, tcWiringImplicitParamTyCon = \name -> tyCon ResolutionNamespaceType "GHC.Classes" name 1, tcWiringPrimitiveTyCon = \name -> tyCon ResolutionNamespaceType "GHC.Prim" name 0, tcWiringPrimitiveTerm = (prim,"GHC.Prim",), tcWiringRestrictedPrimitiveTerms = Set.singleton (prim, "GHC.Prim", "seq"), tcWiringKindTyCon = types ResolutionNamespaceType, tcWiringKindDataCon = types ResolutionNamespaceTerm, tcWiringTypeLitFamilyModules = typeLitFamilyModules, tcWiringTypeErrorFamily = ("GHC.TypeError", "TypeError"), tcWiringErrorMessageCons = ("Text", "ShowType", ":<>:", ":$$:"), tcWiringLiftClass = ("GHC.Internal.TH.Lift", "Lift")} where {
  unboxedTuple namespace arity = types namespace (unboxedTupleTyConName arity) arity;
  types namespace = tyCon namespace "GHC.Types";
  tyCon :: ResolutionNamespace -> Text -> Text -> Int -> TyCon;
  tyCon namespace = mkTyConWithNamespace namespace prim};
boxedTupleTyConName :: Int -> Text;
boxedTupleTyConName arity = case arity of {
  0 -> "Unit";
  1 -> "Solo";
  _ -> "Tuple" <> T.pack (show arity)};
boxedTupleDataConName :: Int -> Text;
boxedTupleDataConName arity = case arity of {
  1 -> "MkSolo";
  _ -> "(" <> T.replicate (max 0 (arity - 1)) "," <> ")"};
unboxedTupleTyConName :: Int -> Text;
unboxedTupleTyConName arity = "Tuple" <> T.pack (show arity) <> "#";
unboxedSumTyConName :: Int -> Text;
unboxedSumTyConName arity = "Sum" <> T.pack (show arity) <> "#";
unboxedSumDataConName :: Int -> Int -> Text;
unboxedSumDataConName alternative arity = "Sum" <> T.pack (show arity) <> "_" <> T.pack (show alternative) <> "#";
primDerivingReferences :: PackageId -> DerivingReferences;
primDerivingReferences prim = DerivingReferences {derivingTrue = term "GHC.Types" NameConId "True", derivingFalse = term "GHC.Types" NameConId "False", derivingLT = term "GHC.Types" NameConId "LT", derivingEQ = term "GHC.Types" NameConId "EQ", derivingGT = term "GHC.Types" NameConId "GT", derivingIntCon = term "GHC.Types" NameConId "I#", derivingIntPrimType = DerivingReference ReferencePrimPackage "GHC.Prim" "Int#" NameConId ResolutionNamespaceType, derivingGreaterOrEqual = term "GHC.Classes" NameVarSym ">=", derivingCons = term "GHC.Types" NameConSym ":", derivingBind = term "GHC.Prim.Base" NameVarSym ">>=", derivingThen = term "GHC.Prim.Base" NameVarSym ">>", derivingReturn = term "GHC.Prim.Base" NameVarId "return", derivingReadParens = term readModule NameVarId "parens", derivingReadPrecContext = term readModule NameVarId "prec", derivingReadStep = term readModule NameVarId "step", derivingReadReset = term readModule NameVarId "reset", derivingReadAlternative = term readModule NameVarSym "+++", derivingReadFail = term readModule NameVarId "pfail", derivingReadExpect = term readModule NameVarId "expectP", derivingReadField = term readModule NameVarId "readField", derivingReadSymField = term readModule NameVarId "readSymField", derivingLexemeIdent = term readModule NameConId "Ident", derivingLexemeSymbol = term readModule NameConId "Symbol", derivingLexemePunc = term readModule NameConId "Punc", derivingPure = term "GHC.Prim.Base" NameVarId "pure", derivingApply = term "GHC.Prim.Base" NameVarSym "<*>", derivingLiftConE = classTerm thSyntaxModule NameConId "ConE", derivingLiftAppE = classTerm thSyntaxModule NameConId "AppE", derivingLiftDataConName = classTerm thSyntaxModule NameVarId "mkNameG_d", derivingLiftCodeCoerce = classTerm thSyntaxModule NameVarId "unsafeCodeCoerce", derivingGeneric = genericReferences, derivingStockClasses = coreStockClasses prim, derivingRecognizedClasses = coreRecognizedClasses} where {
  readModule = "GHC.Prim.Read";
  thSyntaxModule = "GHC.Internal.TH.Syntax";
  term moduleName nameType name = DerivingReference ReferencePrimPackage moduleName name nameType ResolutionNamespaceTerm;
  classTerm moduleName nameType name = DerivingReference ReferenceClassPackage moduleName name nameType ResolutionNamespaceTerm};
genericReferences :: GenericReferences;
genericReferences = GenericReferences {genericM1 = constructor "M1", genericUnM1 = value "unM1", genericK1 = constructor "K1", genericUnK1 = value "unK1", genericU1 = constructor "U1", genericL1 = constructor "L1", genericR1 = constructor "R1", genericProduct = operator ":*:", genericV1Type = tyCon "V1", genericU1Type = tyCon "U1", genericSumType = tyOperator ":+:", genericProductType = tyOperator ":*:", genericD1Type = tyCon "D1", genericC1Type = tyCon "C1", genericS1Type = tyCon "S1", genericRec0Type = tyCon "Rec0", genericMetaData = constructor "MetaData", genericMetaCons = constructor "MetaCons", genericMetaSel = constructor "MetaSel", genericPrefixI = constructor "PrefixI", genericInfixI = constructor "InfixI", genericLeftAssociative = constructor "LeftAssociative", genericRightAssociative = constructor "RightAssociative", genericNotAssociative = constructor "NotAssociative", genericNoSourceUnpackedness = constructor "NoSourceUnpackedness", genericSourceNoUnpack = constructor "SourceNoUnpack", genericSourceUnpack = constructor "SourceUnpack", genericNoSourceStrictness = constructor "NoSourceStrictness", genericSourceLazy = constructor "SourceLazy", genericSourceStrict = constructor "SourceStrict", genericDecidedLazy = constructor "DecidedLazy", genericDecidedStrict = constructor "DecidedStrict", genericDecidedUnpack = constructor "DecidedUnpack"} where {
  genericsModule = "GHC.Generics";
  reference nameType namespace name = DerivingReference ReferenceClassPackage genericsModule name nameType namespace;
  constructor = reference NameConId ResolutionNamespaceTerm;
  operator = reference NameConSym ResolutionNamespaceTerm;
  value = reference NameVarId ResolutionNamespaceTerm;
  tyCon = reference NameConId ResolutionNamespaceType;
  tyOperator = reference NameConSym ResolutionNamespaceType};
coreStockClasses :: PackageId -> [StockClassLocation];
coreStockClasses prim = [primClass "GHC.Classes" "Eq", primClass "GHC.Classes" "Ord", primClass "GHC.Prim.Enum" "Enum", primClass "GHC.Prim.Enum" "Bounded", primClass "GHC.Prim.Show" "Show", primClass "GHC.Prim.Read" "Read", primClass "GHC.Prim.Base" "Functor", coreClass "GHC.Internal.Foldable" "Foldable", coreClass "GHC.Internal.Traversable" "Traversable", coreClass "GHC.Internal.TH.Lift" "Lift", coreClass "GHC.Generics" "Generic"] where {
  primClass = StockClassLocation (Just prim);
  coreClass = StockClassLocation Nothing};
coreRecognizedClasses :: [(Text, Text)];
coreRecognizedClasses = [("GHC.Ix", "Ix"), ("Data.Data", "Data"), ("Type.Reflection", "Typeable"), ("Type.Reflection.Internal", "Typeable"), ("GHC.Generics", "Generic1")]
}
