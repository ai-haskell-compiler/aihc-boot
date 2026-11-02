module Aihc.Tc.Solve.Worklist (WorkList(..), emptyWorkList, addEq, addDict, addImpl, popWork) where {
import Aihc.Tc.Constraint;
data WorkList = WorkList { wlEqs :: ![Ct], wlDicts :: ![Ct], wlImpls :: ![Implication] } deriving (Show);
emptyWorkList :: WorkList;
emptyWorkList = WorkList [] [] [];
addEq :: Ct -> WorkList -> WorkList;
addEq ct wl = wl {wlEqs = ct : wlEqs wl};
addDict :: Ct -> WorkList -> WorkList;
addDict ct wl = wl {wlDicts = ct : wlDicts wl};
addImpl :: Implication -> WorkList -> WorkList;
addImpl impl wl = wl {wlImpls = impl : wlImpls wl};
popWork :: WorkList -> Maybe (Either Ct Implication, WorkList);
popWork wl = case wlEqs wl of {
  (ct : rest) -> Just (Left ct, wl {wlEqs = rest});
  [] -> case wlDicts wl of {
  (ct : rest) -> Just (Left ct, wl {wlDicts = rest});
  [] -> case wlImpls wl of {
  (impl : rest) -> Just (Right impl, wl {wlImpls = rest});
  [] -> Nothing}}}
}
