{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
module Aihc.Fc.Syntax (Type(..), TyLit(..), Binder(..), Expr(..), Bind(..), Alt(..), AltCon(..), Literal(..), Role(..), Coercion(..), Program(..), Imports(..), Decl(..), TypeDecl(..), ConDecl(..), ConRepresentation(..), SynonymDecl(..), AxiomDecl(..), ValDecl(..), InlineSpec(..), RuleDecl(..), RuleActivation(..), ForeignCall(..), ForeignImportDependency(..), CallingConvention(..), CCallSpec(..), CCallTarget(..), CAbiType(..), ForeignEffect(..), ForeignSafety(..)) where {
import Aihc.Fc.Name;
import Control.DeepSeq (NFData);
import Data.ByteString (ByteString);
import Data.Map.Strict (Map);
import Data.Text (Text);
import GHC.Generics (Generic);
data Type = TyVar Name | TyCon Name | TyApp Type Type | TyFun Type Type Type Type | TyForAll Binder Type | TyEq Type Type | TyLit Name TyLit deriving stock (Eq, Ord, Show, Read, Generic) deriving anyclass (NFData);
data TyLit = TyLitNat Integer | TyLitSymbol Text | TyLitChar Char deriving stock (Eq, Ord, Show, Read, Generic) deriving anyclass (NFData);
data Binder = Binder { binderName :: Name, binderType :: Type } deriving stock (Eq, Ord, Show, Read, Generic) deriving anyclass (NFData);
data Expr = ExVar Name | ExLit Literal | ExApp Expr Expr | ExTyApp Expr Type | ExLam Binder Expr | ExTyLam Binder Expr | ExLet Bind Expr | ExRec [Bind] Expr | ExCase Expr Binder Type [Alt] | ExCast Expr Coercion | ExCoercion Coercion | ExForeignCall ForeignCall [Type] [Expr] deriving stock (Eq, Ord, Show, Read, Generic) deriving anyclass (NFData);
data ForeignCall = ForeignCall { foreignCallName :: Name, foreignCallConvention :: CallingConvention, foreignCallDependencies :: [ForeignImportDependency], foreignCallType :: Type } deriving stock (Eq, Ord, Show, Read, Generic) deriving anyclass (NFData);
data Bind = Bind { bindBinder :: Binder, bindRhs :: Expr } deriving stock (Eq, Ord, Show, Read, Generic) deriving anyclass (NFData);
data Alt = Alt { altCon :: AltCon, altTypeBinders :: [Binder], altBinders :: [Binder], altRhs :: Expr } deriving stock (Eq, Ord, Show, Read, Generic) deriving anyclass (NFData);
data AltCon = AltData Name | AltLit Literal | AltDefault deriving stock (Eq, Ord, Show, Read, Generic) deriving anyclass (NFData);
data Literal = LitInt Type Integer | LitChar Type Char | LitAddr Type ByteString deriving stock (Eq, Ord, Show, Read, Generic) deriving anyclass (NFData);
data Role = Nominal | Representational | Phantom deriving stock (Eq, Ord, Show, Read, Generic) deriving anyclass (NFData);
data Coercion = CoVar Name | CoRefl Type | CoSym Coercion | CoTrans Coercion Coercion | CoApp Coercion Coercion | CoFun Coercion Coercion | CoNth Int Coercion | CoTyConApp Name [Coercion] | CoAxiom Name [Type] deriving stock (Eq, Ord, Show, Read, Generic) deriving anyclass (NFData);
data Program = Program { programScopes :: ScopeTable, programImports :: Imports, programDecls :: [Decl] } deriving stock (Eq, Ord, Show, Read, Generic) deriving anyclass (NFData);
data Imports = Imports { importHeaders :: Map Name Type, importSynonyms :: Map Name Type, importAxioms :: Map Name AxiomDecl, importBinders :: Map Name Type, importConRepresentations :: Map Name ConRepresentation } deriving stock (Eq, Ord, Show, Read, Generic) deriving anyclass (NFData);
data Decl = DeclType TypeDecl | DeclSynonym SynonymDecl | DeclAxiom AxiomDecl | DeclVal ValDecl | DeclRule RuleDecl deriving stock (Eq, Ord, Show, Read, Generic) deriving anyclass (NFData);
data RuleDecl = RuleDecl { ruleName :: Text, ruleActivation :: RuleActivation, ruleTypeBinders :: [Binder], ruleBinders :: [Binder], ruleType :: Type, ruleLhs :: Expr, ruleRhs :: Expr } deriving stock (Eq, Ord, Show, Read, Generic) deriving anyclass (NFData);
data RuleActivation = AlwaysActive | ActiveAfter Int | ActiveBefore Int | NeverActive deriving stock (Eq, Ord, Show, Read, Generic) deriving anyclass (NFData);
data TypeDecl = TypeDecl { typeVis :: Vis, typeName :: Name, typeBinders :: [Binder], typeResult :: Type, typeRoles :: [Role], typeCons :: [ConDecl] } deriving stock (Eq, Ord, Show, Read, Generic) deriving anyclass (NFData);
data ConRepresentation = HeapConstructor | UnboxedTupleConstructor | UnboxedSumConstructor Int Int deriving stock (Eq, Ord, Show, Read, Generic) deriving anyclass (NFData);
data ConDecl = ConDecl { conVis :: Vis, conName :: Name, conType :: Type, conRepresentation :: ConRepresentation } deriving stock (Eq, Ord, Show, Read, Generic) deriving anyclass (NFData);
data SynonymDecl = SynonymDecl { synVis :: Vis, synName :: Name, synBinders :: [Binder], synResult :: Type, synBody :: Type } deriving stock (Eq, Ord, Show, Read, Generic) deriving anyclass (NFData);
data AxiomDecl = AxiomDecl { axiomVis :: Vis, axiomName :: Name, axiomBinders :: [Binder], axiomRole :: Role, axiomLeft :: Type, axiomRight :: Type } deriving stock (Eq, Ord, Show, Read, Generic) deriving anyclass (NFData);
data ValDecl = ValDecl { valVis :: Vis, valName :: Name, valType :: Type, valBody :: Expr, valInline :: InlineSpec } deriving stock (Eq, Ord, Show, Read, Generic) deriving anyclass (NFData);
data InlineSpec = InlineDefault | InlineAlways RuleActivation | InlineWhenUseful RuleActivation | InlineNever RuleActivation deriving stock (Eq, Ord, Show, Read, Generic) deriving anyclass (NFData);
data ForeignImportDependency = ForeignAxiom Name | ForeignConstructor Name deriving stock (Eq, Ord, Show, Read, Generic) deriving anyclass (NFData);
data CallingConvention = Prim | CCall CCallSpec deriving stock (Eq, Ord, Show, Read, Generic) deriving anyclass (NFData);
data CCallSpec = CCallSpec { ccallSymbol :: Text, ccallTarget :: CCallTarget, ccallSafety :: ForeignSafety, ccallArgumentTypes :: [CAbiType], ccallResultType :: CAbiType, ccallEffect :: ForeignEffect } deriving stock (Eq, Ord, Show, Read, Generic) deriving anyclass (NFData);
data CCallTarget = CCallFunction | CCallAddress | CCallDynamic | CCallWrapper deriving stock (Eq, Ord, Show, Read, Generic) deriving anyclass (NFData);
data CAbiType = CAbiInt | CAbiInt8 | CAbiInt16 | CAbiInt32 | CAbiInt64 | CAbiWord | CAbiWord8 | CAbiWord16 | CAbiWord32 | CAbiWord64 | CAbiFloat | CAbiDouble | CAbiAddr | CAbiVoid deriving stock (Eq, Ord, Show, Read, Enum, Bounded, Generic) deriving anyclass (NFData);
data ForeignEffect = ForeignPure | ForeignRealWorld deriving stock (Eq, Ord, Show, Read, Generic) deriving anyclass (NFData);
data ForeignSafety = ForeignUnsafe | ForeignSafe | ForeignInterruptible deriving stock (Eq, Ord, Show, Read, Generic) deriving anyclass (NFData)
}
