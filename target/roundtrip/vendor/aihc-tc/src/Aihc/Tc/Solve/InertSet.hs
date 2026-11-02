module Aihc.Tc.Solve.InertSet (InertSet(..), emptyInertSet, addInertEq, addInertDict) where {
import Aihc.Tc.Constraint;
data InertSet = InertSet { inertEqs :: ![Ct], inertDicts :: ![Ct] } deriving (Show);
emptyInertSet :: InertSet;
emptyInertSet = InertSet [] [];
addInertEq :: Ct -> InertSet -> InertSet;
addInertEq ct is = is {inertEqs = ct : inertEqs is};
addInertDict :: Ct -> InertSet -> InertSet;
addInertDict ct is = is {inertDicts = ct : inertDicts is}
}
