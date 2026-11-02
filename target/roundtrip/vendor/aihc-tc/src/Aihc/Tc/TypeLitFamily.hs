{-# LANGUAGE OverloadedStrings #-}
module Aihc.Tc.TypeLitFamily (typeLitFamilyModules, TypeLitValue(..), evaluateTypeLitFamily) where {
import Aihc.Tc.Types (TyLit(..));
import Data.Text (Text);
typeLitFamilyModules :: [Text];
typeLitFamilyModules = ["GHC.TypeNats", "GHC.TypeLits", "Data.Type.Ord"];
data TypeLitValue = TypeLitNatural !Integer | TypeLitOrdering !Ordering deriving (Eq, Show);
evaluateTypeLitFamily :: Text -> [TyLit] -> Maybe TypeLitValue;
evaluateTypeLitFamily family arguments = case (family, arguments) of {
  ("Compare", [left, right]) -> compareLiterals left right;
  ("CmpNat", [TyLitNat left, TyLitNat right]) -> ordering (compare left right);
  ("CmpSymbol", [TyLitSymbol left, TyLitSymbol right]) -> ordering (compare left right);
  ("CmpChar", [TyLitChar left, TyLitChar right]) -> ordering (compare left right);
  ("+", [TyLitNat left, TyLitNat right]) -> natural (left + right);
  ("*", [TyLitNat left, TyLitNat right]) -> natural (left * right);
  ("-", [TyLitNat left, TyLitNat right]) | left >= right -> natural (left - right);
  ("^", [TyLitNat left, TyLitNat right]) | right <= exponentLimit -> natural (left ^ right);
  ("Div", [TyLitNat left, TyLitNat right]) | right /= 0 -> natural (left `div` right);
  ("Mod", [TyLitNat left, TyLitNat right]) | right /= 0 -> natural (left `mod` right);
  ("Log2", [TyLitNat value]) | value > 0 -> natural (integerLog2 value);
  _ -> Nothing} where {
  compareLiterals left right = case (left, right) of {
  (TyLitNat a, TyLitNat b) -> ordering (compare a b);
  (TyLitSymbol a, TyLitSymbol b) -> ordering (compare a b);
  (TyLitChar a, TyLitChar b) -> ordering (compare a b);
  _ -> Nothing};
  natural = Just . TypeLitNatural;
  ordering = Just . TypeLitOrdering;
  exponentLimit = 10000;
  integerLog2 value = toInteger (length (takeWhile (<= value) (iterate (* 2) 2)))}
}
