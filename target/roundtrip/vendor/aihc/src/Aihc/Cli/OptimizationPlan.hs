module Aihc.Cli.OptimizationPlan (OptimizationPlan(..), optimizationPlan) where {
import Aihc.Fc qualified as Fc;
import Aihc.Native (OptimizationLevel(..));
data OptimizationPlan = OptimizationPlan { planWholeProgram :: !Bool, planPasses :: ![Fc.Pass], planGrinPointsTo :: !Bool } deriving (Eq, Show);
optimizationPlan :: Bool -> OptimizationLevel -> OptimizationPlan;
optimizationPlan lto level = case level of {
  O0 -> OptimizationPlan {planWholeProgram = lto, planPasses = [], planGrinPointsTo = False};
  O1 -> OptimizationPlan {planWholeProgram = lto, planPasses = shrink <> grow, planGrinPointsTo = False};
  O2 -> OptimizationPlan {planWholeProgram = True, planPasses = shrink <> grow, planGrinPointsTo = True};
  Os -> OptimizationPlan {planWholeProgram = True, planPasses = shrink <> finish, planGrinPointsTo = True}} where {
  shrink = [Fc.PassEtaExpand, Fc.PassInline Fc.shrinkPolicy rounds 2];
  grow = [Fc.PassInline Fc.growPolicy rounds 1] <> finish;
  finish = [Fc.PassEtaExpand, Fc.PassSimplify 0];
  rounds = 4}
}
