module Aihc.Tc.Evidence (EvVar(..), EvTerm(..), CallSite(..), TypeableTyCon(..), TypeableKind(..), Coercion(..)) where {
import Aihc.Tc.Types;
import Data.Text (Text);
newtype EvVar = EvVar { evVarUnique :: Unique } deriving (Eq, Ord, Show, Read);
data EvTerm = EvVarTerm !EvVar | EvGiven !Pred | EvDict !(Text, Text) !Text ![TcType] ![EvTerm] | EvCoercible !TyCon !TcType !TcType | EvCoercion !Coercion | EvSuperClass !EvTerm !(Maybe (Text, Text)) !Pred ![TcType] !Int | EvCast !EvTerm !Coercion | EvTypeable !(Maybe (Text, Text)) !TcType !TypeableTyCon ![(TcType, EvTerm)] ![EvTerm] | EvTypeLit !(Maybe (Text, Text)) !TcType !TyLit | EvTypeLam !TyVarId !EvTerm | EvDictLam !Pred !TcType !EvTerm | EvTypeApp !EvTerm !TcType | EvDictApp !EvTerm !EvTerm | EvCallStackPush !(Text, Text) !Text !CallSite !EvTerm | EvCallStackEmpty !(Text, Text) deriving (Eq, Ord, Show, Read);
data TypeableTyCon = TypeableTyCon !TyCon !Int !TypeableKind deriving (Eq, Ord, Show, Read);
data TypeableKind = TypeableKindVar !Int | TypeableKindType !TcType | TypeableKindFun !TypeableKind !TypeableKind | TypeableKindCon !TypeableTyCon ![TypeableKind] | TypeableKindApp !TypeableKind !TypeableKind deriving (Eq, Ord, Show, Read);
data CallSite = CallSite { callSiteFile :: !Text, callSiteStartLine :: !Int, callSiteStartColumn :: !Int, callSiteEndLine :: !Int, callSiteEndColumn :: !Int } deriving (Eq, Ord, Show, Read);
data Coercion = CoVar !EvVar | GivenCo !Pred | Refl !TcType | Sym !Coercion | Trans !Coercion !Coercion | TyConAppCo !TyCon ![TcType] ![Coercion] | AppCo !Coercion !Coercion | FunCo !Coercion !Coercion | NthCo !Int !Coercion | EvidenceCo !Pred !EvTerm | AxiomInstCo !TcAxiomKey ![TcType] deriving (Eq, Ord, Show, Read)
}
