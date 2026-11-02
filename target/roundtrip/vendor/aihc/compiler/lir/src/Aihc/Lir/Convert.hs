module Aihc.Lir.Convert (integerConversionBounds) where {
import Aihc.Lir.Syntax (Type(F32), typeBits);
integerConversionBounds :: Bool -> Type -> Type -> (Double, Bool, Double);
integerConversionBounds signed from to = (lower, excludeLower, upper) where {
  bits = typeBits to;
  precision = if from == F32 then 24 else 53;
  excludeLower = not signed || bits <= precision;
  lower | not signed = -1 | excludeLower = negate (2 ^^ (bits - 1)) - 1 | otherwise = negate (2 ^^ (bits - 1));
  upper = if signed then 2 ^^ (bits - 1) else 2 ^^ bits}
}
