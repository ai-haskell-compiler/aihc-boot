{-# LANGUAGE OverloadedStrings #-}
module Aihc.Fc.Pass (Pass(..), PassReport(..), passName, passPhase, runPass, runPasses) where {
import Aihc.Fc.Arity (EtaReport(..), etaExpandProgram);
import Aihc.Fc.Inline (InlineConfig(..), InlinePolicy(..), InlineReport(..), inlineProgram);
import Aihc.Fc.Name (Name);
import Aihc.Fc.Simplify (SimplifyReport(..), simplifyProgram);
import Aihc.Fc.Size (programSize);
import Aihc.Fc.Syntax (Program);
import Data.List qualified as List;
import Data.Text (Text);
import Data.Text qualified as T;
data Pass = PassEtaExpand | PassInline !InlinePolicy !Int !Int | PassSimplify !Int deriving (Eq, Show);
passPhase :: Pass -> Maybe Int;
passPhase pass = case pass of {
  PassEtaExpand -> Nothing;
  PassInline _ _ phase -> Just phase;
  PassSimplify phase -> Just phase};
data PassReport = PassReport { reportPass :: !Text, reportBefore :: !Int, reportAfter :: !Int, reportDetail :: !Text } deriving (Eq, Show);
passName :: Pass -> Text;
passName pass = case pass of {
  PassEtaExpand -> "eta expand";
  PassInline policy _ phase -> "inline " <> policyName policy <> " [" <> T.pack (show phase) <> "]";
  PassSimplify phase -> "simplify [" <> T.pack (show phase) <> "]"};
runPass :: Maybe [Name] -> Pass -> Program -> (Program, PassReport);
runPass roots pass program = case pass of {
  PassEtaExpand -> let {
  (expanded, report) = etaExpandProgram program} in (expanded, PassReport {reportPass = passName pass, reportBefore = programSize program, reportAfter = programSize expanded, reportDetail = count (reportExpandedValues report) "values" <> ", " <> count (reportAddedLambdas report) "lambdas added"});
  PassInline policy rounds phase -> let {
  config = InlineConfig {inlinePolicy = policy, inlineRoots = roots, inlineRounds = rounds, inlinePhase = phase};
  (inlined, report) = inlineProgram config program} in (inlined, PassReport {reportPass = passName pass, reportBefore = reportSizeBefore report, reportAfter = reportSizeAfter report, reportDetail = count (reportInlinedSites report) "sites" <> ", " <> count (reportDroppedValues report) "values dropped" <> ", " <> count (reportRulesFired report) "rules fired"});
  PassSimplify phase -> let {
  (simplified, report) = simplifyProgram phase program} in (simplified, PassReport {reportPass = passName pass, reportBefore = simplifySizeBefore report, reportAfter = simplifySizeAfter report, reportDetail = count (simplifyRulesFired report) "rules fired"})} where {
  count n what = T.pack (show n) <> " " <> what};
runPasses :: Maybe [Name] -> [Pass] -> Program -> (Program, [PassReport]);
runPasses roots passes program0 = let {
  (final, reports) = List.foldl' step (program0, []) passes} in (final, reverse reports) where {
  step (program, reports) pass = let {
  (program', report) = runPass roots pass program} in (program', report : reports)}
}
