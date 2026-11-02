module Aihc.Tc.Solve.Canonicalize (classifyConstraint, CanonResult(..)) where {
import Aihc.Tc.Constraint;
import Aihc.Tc.Types;
data CanonResult = CanonEqs ![Ct] | CanonDict !Ct | CanonSolved deriving (Show);
classifyConstraint :: Ct -> CanonResult;
classifyConstraint ct = case ctPred ct of {
  EqPred {} -> CanonEqs [ct];
  ClassPred {} -> CanonDict ct;
  QuantifiedPred {} -> CanonDict ct;
  IParamPred {} -> CanonDict ct;
  IrredPred {} -> CanonDict ct}
}
