module Aihc.Tc.Constraint (CtFlavor(..), CtOrigin(..), CtProvenance(..), EqProvenance(..), TypeOrigin(..), TypeRole(..), TypeTrace(..), Ct(..), mkWantedCt, mkWantedEqCt, ctEqProvenance, Implication(..)) where {
import Aihc.Parser.Syntax (SourceSpan);
import Aihc.Tc.Evidence (EvVar);
import Aihc.Tc.Types;
import Data.Text (Text);
data CtFlavor = Given | Wanted | Derived deriving (Eq, Show);
data CtOrigin = OccurrenceOf !Text | AppOrigin !(Maybe SourceSpan) | LambdaOrigin !(Maybe SourceSpan) | LetOrigin !(Maybe SourceSpan) | LitOrigin !(Maybe SourceSpan) | SigOrigin !(Maybe SourceSpan) | CaseBranchOrigin !(Maybe SourceSpan) | InstOrigin !Text | UnifyOrigin !(Maybe SourceSpan) | ImplicitParamOrigin !Text deriving (Eq, Show);
data TypeRole = ActualType | ExpectedType | RequiredType | InferredType deriving (Eq, Show);
data TypeOrigin = UnknownTypeOrigin | ExpressionTypeOrigin !(Maybe SourceSpan) | ListElementTypeOrigin !(Maybe SourceSpan) | TypeSignatureOrigin !Text !(Maybe SourceSpan) | ConstraintTypeOrigin !CtOrigin deriving (Eq, Show);
data TypeTrace = TypeTrace { typeTraceType :: !TcType, typeTraceRole :: !TypeRole, typeTraceOrigin :: !TypeOrigin } deriving (Eq, Show);
data EqProvenance = EqProvenance { eqActualTrace :: !TypeTrace, eqExpectedTrace :: !TypeTrace, eqContextOrigins :: ![TypeOrigin], eqPrimarySpan :: !(Maybe SourceSpan) } deriving (Eq, Show);
data CtProvenance = FromCtOrigin !CtOrigin | FromEqProvenance !EqProvenance deriving (Eq, Show);
data Ct = Ct { ctPred :: !Pred, ctFlavor :: !CtFlavor, ctEvVar :: !EvVar, ctOrigin :: !CtOrigin, ctProvenance :: !CtProvenance, ctLoc :: !(Maybe SourceSpan) } deriving (Show);
mkWantedCt :: Pred -> EvVar -> CtOrigin -> Maybe SourceSpan -> Ct;
mkWantedCt p ev orig loc = Ct {ctPred = p, ctFlavor = Wanted, ctEvVar = ev, ctOrigin = orig, ctProvenance = FromCtOrigin orig, ctLoc = loc};
mkWantedEqCt :: TypeTrace -> TypeTrace -> EvVar -> CtOrigin -> Maybe SourceSpan -> Ct;
mkWantedEqCt actual expected ev orig loc = Ct {ctPred = EqPred (typeTraceType actual) (typeTraceType expected), ctFlavor = Wanted, ctEvVar = ev, ctOrigin = orig, ctProvenance = FromEqProvenance EqProvenance {eqActualTrace = actual, eqExpectedTrace = expected, eqContextOrigins = [], eqPrimarySpan = loc}, ctLoc = loc};
ctEqProvenance :: Ct -> Maybe EqProvenance;
ctEqProvenance ct = case ctProvenance ct of {
  FromEqProvenance provenance -> Just provenance;
  FromCtOrigin {} -> Nothing};
data Implication = Implication { implSkols :: ![TyVarId], implGivenCts :: ![Ct], implWantedCts :: ![Ct] } deriving (Show)
}
