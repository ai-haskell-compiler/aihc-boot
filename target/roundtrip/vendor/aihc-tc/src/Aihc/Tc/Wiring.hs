module Aihc.Tc.Wiring (TcWiring(..), BuiltinDataCon(..), mkTcKinds, tupleTyCon, tupleDataCon, builtinDataCon) where {
import Aihc.Parser.Syntax (TupleFlavor(..));
import Aihc.Resolve (PackageId);
import Aihc.Tc.Types (TcKinds(..), TyCon);
import Data.Set (Set);
import Data.Text (Text);
data TcWiring = TcWiring { tcWiringBoxedTupleTyCon :: Int -> TyCon, tcWiringBoxedTupleDataCon :: Int -> TyCon, tcWiringUnboxedTupleTyCon :: Int -> TyCon, tcWiringUnboxedTupleDataCon :: Int -> TyCon, tcWiringUnboxedSumTyCon :: Int -> TyCon, tcWiringUnboxedSumDataCon :: Int -> Int -> TyCon, tcWiringListTyCon :: TyCon, tcWiringListDeclaration :: TyCon, tcWiringNilDataCon :: TyCon, tcWiringConsDataCon :: TyCon, tcWiringArrowTyCon :: TyCon, tcWiringTypeTyCon :: TyCon, tcWiringConstraintTyCon :: TyCon, tcWiringConstraintTupleTyCon :: TyCon, tcWiringBoolTyCon :: TyCon, tcWiringCharTyCon :: TyCon, tcWiringNaturalTyCon :: TyCon, tcWiringSymbolTyCon :: TyCon, tcWiringEqualityTyCon :: TyCon, tcWiringCoercibleTyCon :: TyCon, tcWiringImplicitParamTyCon :: Text -> TyCon, tcWiringPrimitiveTyCon :: Text -> TyCon, tcWiringPrimitiveTerm :: Text -> (PackageId, Text, Text), tcWiringRestrictedPrimitiveTerms :: Set (PackageId, Text, Text), tcWiringKindTyCon :: Text -> Int -> TyCon, tcWiringKindDataCon :: Text -> Int -> TyCon, tcWiringTypeLitFamilyModules :: [Text], tcWiringTypeErrorFamily :: (Text, Text), tcWiringErrorMessageCons :: (Text, Text, Text, Text), tcWiringLiftClass :: (Text, Text) };
instance Show TcWiring where {
  show _ = "TcWiring"};
mkTcKinds :: TcWiring -> TcKinds;
mkTcKinds wiring = TcKinds {kindsTyCon = tcWiringKindTyCon wiring, kindsDataCon = tcWiringKindDataCon wiring, kindsEqualityTyCon = tcWiringEqualityTyCon wiring, kindsArrowTyCon = tcWiringArrowTyCon wiring, kindsListTyCon = tcWiringListTyCon wiring, kindsListDeclaration = tcWiringListDeclaration wiring, kindsNilDataCon = tcWiringNilDataCon wiring, kindsConsDataCon = tcWiringConsDataCon wiring, kindsNaturalTyCon = tcWiringNaturalTyCon wiring, kindsSymbolTyCon = tcWiringSymbolTyCon wiring, kindsCharTyCon = tcWiringCharTyCon wiring};
tupleTyCon :: TcWiring -> TupleFlavor -> Int -> TyCon;
tupleTyCon wiring flavor = case flavor of {
  Boxed -> tcWiringBoxedTupleTyCon wiring;
  Unboxed -> tcWiringUnboxedTupleTyCon wiring};
tupleDataCon :: TcWiring -> TupleFlavor -> Int -> TyCon;
tupleDataCon wiring flavor = case flavor of {
  Boxed -> tcWiringBoxedTupleDataCon wiring;
  Unboxed -> tcWiringUnboxedTupleDataCon wiring};
data BuiltinDataCon = BuiltinTupleCon !TupleFlavor !Int | BuiltinUnboxedSumCon !Int !Int | BuiltinNilCon | BuiltinConsCon deriving (Eq, Show);
builtinDataCon :: TcWiring -> BuiltinDataCon -> TyCon;
builtinDataCon wiring builtin = case builtin of {
  BuiltinTupleCon flavor arity -> tupleDataCon wiring flavor arity;
  BuiltinUnboxedSumCon alternative arity -> tcWiringUnboxedSumDataCon wiring alternative arity;
  BuiltinNilCon -> tcWiringNilDataCon wiring;
  BuiltinConsCon -> tcWiringConsDataCon wiring}
}
