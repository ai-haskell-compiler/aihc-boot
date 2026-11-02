module Aihc.Tc.Instantiate (Instantiation(..), instantiate, instantiateWithArgs) where {
import Aihc.Tc.Monad;
import Aihc.Tc.Types;
import Control.Monad (foldM);
import Data.Map.Strict qualified as Map;
data Instantiation = Instantiation { instType :: !TcType, instTypeArgs :: ![TcType], instSpecifiedArgs :: ![TcType], instPreds :: ![Pred] } deriving (Eq, Show);
instantiate :: TypeScheme -> TcM (TcType, [Pred]);
instantiate scheme = do {
  inst <- instantiateWithArgs scheme;
  pure (instType inst, instPreds inst)};
instantiateWithArgs :: TypeScheme -> TcM Instantiation;
instantiateWithArgs (Scheme inferred specified preds body) = do {
  let {
  tvs = inferred <> specified};
  subst <- foldM extendSubst Map.empty tvs;
  let {
  substTy = applySubst subst;
  body' = substTy body;
  preds' = map (applySubstPred subst) preds;
  typeArgs = map (substTy . TcTyVar) tvs};
  pure Instantiation {instType = body', instTypeArgs = typeArgs, instSpecifiedArgs = drop (length inferred) typeArgs, instPreds = preds'}} where {
  extendSubst subst tv = do {
  meta <- freshMetaTvOfKind (applySubst subst (tvKind tv));
  pure (Map.insert (tvUnique tv) meta subst)}}
}
