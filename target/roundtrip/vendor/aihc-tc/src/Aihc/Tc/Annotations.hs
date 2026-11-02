{-# LANGUAGE PatternSynonyms #-}
module Aihc.Tc.Annotations (TcAnnotation(..), TcCastAnnotation(..), PendingTcCastAnnotation(..), CastDirection(..), annotateRhsCast, annotateExprCast, annotateFunCast, TcForeignImportAnnotation(..), TcForeignImportInfo(..), TcForeignSafety(..), TcForeignEffect(..), TcForeignTarget(..), TcForeignCApi(..), TcForeignCApiKind(..), TcForeignMarshal(..), TcForeignAbiType(..), PendingTcAnnotation(..), TcClassAnnotation(..), TcClassMethodAnnotation(..), TcDictBinderAnnotation(..), TcDerivingAnnotation(..), TcDerivingContext(..), TcDerivingPlan(..), TcDerivingStrategy(..), TcInstanceAnnotation(..), TcDerivedInstance(..), TcCoercedDeriving(..), TcCoercedInstance(..), TcCoercedMethod(..), TcPatSynAnnotation(..), TcInstanceMethodAnnotation(..), annotateDecl, pendingAnnotation, pendingTypeLambdaAnnotation, renderFunDepNames, renderPred, renderTcType, renderTyLit, renderTcTypeInModule, renderTcSignature) where {
import Aihc.Parser.Syntax (Decl(..), Expr(..), Match, Rhs(..), SourceSpan, mkAnnotation);
import Aihc.Resolve (ResolutionNamespace(..));
import Aihc.Tc.Env (AssociatedTypeInfo, CType, DataTypeInfo, FunDep, TypeFamilyInstanceInfo);
import Aihc.Tc.Evidence (Coercion, EvTerm, EvVar);
import Aihc.Tc.Types (Pred(..), TcType(..), TyCon(..), TyLit(..), TyVarId(..), Unique(..), tyConModuleName, tyConNamespace, pattern KType);
import Control.DeepSeq (NFData);
import Data.Text (Text);
import Data.Text qualified as T;
import GHC.Generics (Generic);
newtype TcCastAnnotation = TcCastAnnotation Coercion deriving (Eq, Show);
data PendingTcCastAnnotation = PendingTcCastAnnotation TcType EvVar CastDirection deriving (Eq, Show);
data CastDirection = CastToRight | CastToLeft deriving (Eq, Show);
annotateRhsCast :: TcType -> EvVar -> Rhs body -> Rhs body;
annotateRhsCast ty evidence rhs = let {
  annotation = mkAnnotation (PendingTcCastAnnotation ty evidence CastToRight)} in case rhs of {
  UnguardedRhs annotations body locals -> UnguardedRhs (annotation : annotations) body locals;
  GuardedRhss annotations bodies locals -> GuardedRhss (annotation : annotations) bodies locals};
annotateExprCast :: TcType -> EvVar -> Expr -> Expr;
annotateExprCast ty evidence = EAnn (mkAnnotation (PendingTcCastAnnotation ty evidence CastToLeft));
annotateFunCast :: TcType -> EvVar -> Expr -> Expr;
annotateFunCast ty evidence = EAnn (mkAnnotation (PendingTcCastAnnotation ty evidence CastToRight));
data TcAnnotation = TcAnnotation { tcAnnType :: !TcType, tcAnnTypeBinders :: ![TyVarId], tcAnnTypeArgs :: ![TcType], tcAnnEvidenceTerms :: ![EvTerm], tcAnnEvidenceBinders :: ![EvTerm], tcAnnTermArgTypes :: ![TcType] } deriving (Eq, Show);
data TcForeignImportAnnotation = TcForeignImportAnnotation { tcForeignArguments :: ![TcForeignMarshal], tcForeignResult :: !TcForeignMarshal, tcForeignEffect :: !TcForeignEffect, tcForeignSymbol :: !Text, tcForeignTarget :: !TcForeignTarget, tcForeignCApi :: !(Maybe TcForeignCApi) } deriving (Eq, Show, Read, Generic);
instance NFData TcForeignImportAnnotation where {};
data TcForeignCApi = TcForeignCApi { tcForeignCApiHeader :: !(Maybe Text), tcForeignCApiKind :: !TcForeignCApiKind } deriving (Eq, Show, Read, Generic);
instance NFData TcForeignCApi where {};
data TcForeignCApiKind = TcForeignCApiFunction | TcForeignCApiValue deriving (Eq, Show, Read, Generic);
instance NFData TcForeignCApiKind where {};
data TcForeignImportInfo = TcForeignPrimImport | TcForeignCCallImport !TcForeignSafety !TcForeignImportAnnotation deriving (Eq, Show, Read, Generic);
instance NFData TcForeignImportInfo where {};
data TcForeignSafety = TcForeignSafe | TcForeignUnsafe | TcForeignInterruptible deriving (Eq, Show, Read, Generic);
instance NFData TcForeignSafety where {};
data TcForeignTarget = TcForeignCall | TcForeignAddress | TcForeignDynamic | TcForeignWrapper !TcForeignMarshal deriving (Eq, Show, Read, Generic);
instance NFData TcForeignTarget where {};
data TcForeignEffect = TcForeignPure | TcForeignRealWorld deriving (Eq, Show, Read, Generic);
instance NFData TcForeignEffect where {};
data TcForeignMarshal = TcForeignMarshal { tcForeignSourceType :: !TcType, tcForeignPrimitiveType :: !TcType, tcForeignConstructors :: ![Text], tcForeignAbiType :: !TcForeignAbiType, tcForeignCType :: !(Maybe CType) } deriving (Eq, Show, Read, Generic);
instance NFData TcForeignMarshal where {};
data TcForeignAbiType = TcForeignInt | TcForeignInt8 | TcForeignInt16 | TcForeignInt32 | TcForeignInt64 | TcForeignWord | TcForeignWord8 | TcForeignWord16 | TcForeignWord32 | TcForeignWord64 | TcForeignFloat | TcForeignDouble | TcForeignAddr | TcForeignVoid deriving (Eq, Show, Read, Generic);
instance NFData TcForeignAbiType where {};
data PendingTcAnnotation = PendingTcAnnotation { pendingTcAnnType :: !TcType, pendingTcAnnTypeBinders :: ![TyVarId], pendingTcAnnTypeArgs :: ![TcType], pendingTcAnnInferredTypeArgs :: !Int, pendingTcAnnEvidenceVars :: ![EvVar], pendingTcAnnEvidenceBinders :: ![EvVar], pendingTcAnnTermArgTypes :: ![TcType] } deriving (Eq, Show);
data TcDictBinderAnnotation = TcDictBinderAnnotation { tcDictBinderClassName :: !Text, tcDictBinderArgs :: ![TcType], tcDictBinderType :: !TcType } deriving (Eq, Show);
data TcClassMethodAnnotation = TcClassMethodAnnotation { tcClassMethodName :: !Text, tcClassMethodType :: !TcType, tcClassMethodTyVars :: ![TyVarId], tcClassMethodDictType :: !TcType, tcClassMethodIndex :: !Int } deriving (Eq, Show);
data TcClassAnnotation = TcClassAnnotation { tcClassTyCon :: !TyCon, tcClassKindTyVars :: ![TyVarId], tcClassTyVars :: ![TyVarId], tcClassSuperClasses :: ![TcDictBinderAnnotation], tcClassMethods :: ![TcClassMethodAnnotation], tcClassDefaultMethods :: ![Text], tcClassDefaultSignatures :: ![(Text, TcType)], tcClassAssociatedTypes :: ![AssociatedTypeInfo], tcClassFunDeps :: ![FunDep] } deriving (Eq, Show);
data TcDerivingStrategy = TcDerivingStock | TcDerivingNewtype | TcDerivingAnyclass | TcDerivingVia !TcType deriving (Eq, Show);
data TcDerivingContext = TcDerivingInferContext | TcDerivingExplicitContext ![Pred] deriving (Eq, Show);
data TcDerivingPlan = TcDerivingPlan { tcDerivingSourceSpan :: !(Maybe SourceSpan), tcDerivingStrategy :: !TcDerivingStrategy, tcDerivingStockFallback :: !Bool, tcDerivingClassName :: !Text, tcDerivingClassTyCon :: !TyCon, tcDerivingClassOrigin :: !(Maybe (Text, Text)), tcDerivingTyVars :: ![TyVarId], tcDerivingHeadTypes :: ![TcType], tcDerivingDataType :: !(Maybe DataTypeInfo), tcDerivingContext :: !TcDerivingContext, tcDerivingClassTyVars :: ![TyVarId], tcDerivingClassSuperClasses :: ![TcDictBinderAnnotation], tcDerivingClassMethods :: ![TcClassMethodAnnotation], tcDerivingDefaultMethods :: ![Text], tcDerivingDefaultSignatures :: ![(Text, [Pred])] } deriving (Eq, Show);
newtype TcDerivingAnnotation = TcDerivingAnnotation { tcDerivingPlans :: [TcDerivingPlan] } deriving (Eq, Show);
data TcPatSynAnnotation = TcPatSynAnnotation { tcPatSynMatcher :: !Match, tcPatSynBuilder :: !(Maybe [Match]), tcPatSynSelectors :: ![(Text, Match)] } deriving (Eq, Show);
newtype TcCoercedDeriving = TcCoercedDeriving TcDerivingPlan deriving (Eq, Show);
data TcDerivedInstance = TcDerivedInstance deriving (Eq, Show);
data TcCoercedInstance = TcCoercedInstance { tcCoercedHeadTypes :: ![TcType], tcCoercedEvidence :: !(Maybe EvTerm), tcCoercedFieldTypes :: ![TcType], tcCoercedDictionaryCast :: !(Maybe Coercion), tcCoercedMethods :: ![TcCoercedMethod] } deriving (Eq, Show);
data TcCoercedMethod = TcCoercedMethod { tcCoercedMethodName :: !Text, tcCoercedMethodIndex :: !Int, tcCoercedMethodTyVars :: ![TyVarId], tcCoercedMethodPredicates :: ![Pred], tcCoercedMethodCoercion :: !Coercion } deriving (Eq, Show);
data TcInstanceAnnotation = TcInstanceAnnotation { tcInstanceDictName :: !Text, tcInstanceDictType :: !TcType, tcInstanceClassTyCon :: !TyCon, tcInstanceTyVars :: ![TyVarId], tcInstanceHeadTypes :: ![TcType], tcInstanceClassTyVars :: ![TyVarId], tcInstanceClassOrigin :: !(Maybe (Text, Text)), tcInstanceClassSuperClasses :: ![TcDictBinderAnnotation], tcInstanceClassMethods :: ![TcClassMethodAnnotation], tcInstanceContextDicts :: ![TcDictBinderAnnotation], tcInstanceSuperClasses :: ![(TcDictBinderAnnotation, EvTerm)], tcInstanceMethodOrder :: ![Text], tcInstanceDefaultMethods :: ![Text], tcInstanceDefaultMethodEvidence :: ![(Text, [EvTerm])], tcInstanceDefaultMethodTypes :: ![(Text, [TcType])], tcInstanceAssociatedTypes :: ![TypeFamilyInstanceInfo], tcInstanceCoerced :: !(Maybe TcCoercedInstance) } deriving (Eq, Show);
data TcInstanceMethodAnnotation = TcInstanceMethodAnnotation { tcInstanceMethodName :: !Text, tcInstanceMethodType :: !TcType } deriving (Eq, Show);
annotateDecl :: TcAnnotation -> Decl -> Decl;
annotateDecl ann = DeclAnn (mkAnnotation ann);
pendingAnnotation :: TcType -> [TcType] -> [EvVar] -> [TcType] -> PendingTcAnnotation;
pendingAnnotation ty typeArgs evidenceVars = PendingTcAnnotation ty [] typeArgs 0 evidenceVars [];
pendingTypeLambdaAnnotation :: TcType -> [TyVarId] -> [EvVar] -> PendingTcAnnotation;
pendingTypeLambdaAnnotation ty binders evidenceBinders = PendingTcAnnotation ty binders [] 0 [] evidenceBinders [];
renderTcSignature :: Text -> TcType -> String;
renderTcSignature name ty = T.unpack name ++ " ∷ " ++ renderTcType ty;
renderFunDepNames :: [Text] -> [Text] -> String;
renderFunDepNames determiners determined = unwords (map T.unpack determiners) ++ " → " ++ unwords (map T.unpack determined);
renderPred :: Pred -> String;
renderPred pred' = case pred' of {
  ClassPred classTyCon args -> renderTcType (TcTyCon classTyCon args);
  EqPred left right -> renderTcType left ++ " ~ " ++ renderTcType right;
  IParamPred name payload -> T.unpack name ++ " ∷ " ++ renderTcType payload;
  IrredPred constraint -> renderTcType constraint;
  QuantifiedPred variables antecedents consequent -> "∀ " ++ unwords (map (T.unpack . tvName) variables) ++ ". " ++ (if null antecedents then "" else "(" ++ commaSep (map renderPred antecedents) ++ ") ⇒ ") ++ renderPred consequent} where {
  commaSep = T.unpack . T.intercalate (T.pack ", ") . map T.pack};
renderTyLit :: TyLit -> String;
renderTyLit literal = case literal of {
  TyLitNat value -> show value;
  TyLitSymbol value -> show (T.unpack value);
  TyLitChar value -> show value};
renderTcType :: TcType -> String;
renderTcType = renderTcTypeInModule Nothing;
renderTcTypeInModule :: Maybe Text -> TcType -> String;
renderTcTypeInModule currentModule = go 0 where {
  go :: Int -> TcType -> String;
  go _ (TcTyVar tv) = T.unpack (tvName tv);
  go _ (TcMetaTv (Unique u)) = "?" ++ show u;
  go _ KType = "Type";
  go _ TcArrowTy = "(->)";
  go _ (TcTyLit literal) = renderTyLit literal;
  go _ (TcTyCon (TyCon name 1) [arg]) | name == T.pack "[]" = "[" ++ go 0 arg ++ "]";
  go _ (TcTyCon tc@(TyCon name arity) args) | tyConNamespace tc == ResolutionNamespaceType, isBoxedTupleCon name arity, arity == length args = "(" ++ commaSep (map (go 0) args) ++ ")";
  go _ (TcTyCon tc []) = T.unpack (renderTyConName tc);
  go p (TcTyCon tc args) = parenIf (p >= 2) $ unwords (T.unpack (renderTyConName tc) : map (go 2) args);
  go p (TcFunTy a b) = parenIf (p >= 1) $ go 1 a ++ " → " ++ go 0 b;
  go p (TcForAllTy tv body) = let {
  (tvs, inner) = collectForAlls body} in parenIf (p >= 1) $ "∀ " ++ unwords (map (T.unpack . tvName) (tv : tvs)) ++ ". " ++ go 0 inner;
  go p (TcQualTy preds body) = parenIf (p >= 1) $ "(" ++ commaSep (map showPred preds) ++ ") ⇒ " ++ go 0 body;
  go p (TcAppTy f a) = parenIf (p >= 2) $ go 1 f ++ " " ++ go 2 a;
  showPred (ClassPred cls args) = T.unpack (renderTyConName cls) ++ " " ++ unwords (map (go 2) args);
  showPred (EqPred t1 t2) = go 2 t1 ++ " ~ " ++ go 2 t2;
  showPred (IParamPred name payload) = T.unpack name ++ " ∷ " ++ go 0 payload;
  showPred (IrredPred constraint) = go 0 constraint;
  showPred predicate@QuantifiedPred {} = renderPred predicate;
  parenIf False s = s;
  parenIf True s = "(" ++ s ++ ")";
  commaSep = T.unpack . T.intercalate (T.pack ", ") . map T.pack;
  isBoxedTupleCon name arity = arity /= 1 && name == tupleDisplayName arity;
  tupleDisplayName arity = case arity of {
  0 -> T.pack "Unit";
  1 -> T.pack "Solo";
  _ -> T.pack ("Tuple" <> show arity)};
  renderTyConName tyCon = namespacePrefix <> qualifiedName where {
  definingModule = tyConModuleName tyCon;
  namespacePrefix | tyConNamespace tyCon == ResolutionNamespaceTerm = T.pack "'" | otherwise = T.empty;
  qualifiedName | Nothing <- currentModule = tyConName tyCon | Just definingModule == currentModule = tyConName tyCon | otherwise = definingModule <> T.pack "." <> tyConName tyCon}};
collectForAlls :: TcType -> ([TyVarId], TcType);
collectForAlls (TcForAllTy tv body) = let {
  (tvs, inner) = collectForAlls body} in (tv : tvs, inner);
collectForAlls ty = ([], ty)
}
