module Aihc.Tc.Error (TcDiagnostic(..), TcErrorKind(..), TcSeverity(..)) where {
import Aihc.Parser.Syntax (SourceSpan);
import Aihc.Tc.Constraint (CtOrigin, EqProvenance);
import Aihc.Tc.Types;
import Data.Text (Text);
data TcDiagnostic = TcDiagnostic { diagLoc :: !(Maybe SourceSpan), diagSeverity :: !TcSeverity, diagKind :: !TcErrorKind } deriving (Show);
data TcSeverity = TcError | TcWarning deriving (Eq, Show);
data TcErrorKind = UnificationError !TcType !TcType !CtOrigin !(Maybe EqProvenance) | OccursCheckError !TcType !TcType | UnboundVariable !String | KindMismatch !TcType !TcType | UnsolvedWanted !Pred !CtOrigin | TopLevelUnliftedBinding !Text !TcType | RepresentationPolymorphicFunctionArgument !Text !TcType | FunDepUnknownTyVar !Text !Text | InstanceFunDepCoverage !Pred ![Text] ![Text] | InstanceFunDepConflict !Pred !Pred ![Text] ![Text] | OtherError !String deriving (Show)
}
