{-# LANGUAGE OverloadedStrings #-}
module Aihc.Tc.Deriving.Functorial (FieldUse(..), fieldUse, fieldUseObligations) where {
import Aihc.Tc.Annotations (renderTcType);
import Aihc.Tc.Types;
import Data.Text qualified as T;
data FieldUse = FieldAbsent | FieldParameter | FieldContainer !TcType !FieldUse deriving (Eq, Show);
fieldUse :: String -> TyVarId -> TcType -> Either String FieldUse;
fieldUse mechanism parameter = go where {
  go ty | not (mentions ty) = Right FieldAbsent | TcTyVar tyVar <- ty, tvUnique tyVar == tvUnique parameter = Right FieldParameter | Just (function, argument) <- splitLastArgument ty, not (mentions function) = FieldContainer function <$> go argument | otherwise = Left (positionError mechanism parameter ty);
  mentions = typeMentionsTyVar parameter};
splitLastArgument :: TcType -> Maybe (TcType, TcType);
splitLastArgument ty = case ty of {
  TcTyCon _ [] -> Nothing;
  TcTyCon tyCon arguments -> Just (TcTyCon tyCon (init arguments), last arguments);
  TcAppTy function argument -> Just (function, argument);
  TcFunTy domain result -> Just (TcAppTy TcArrowTy domain, result);
  _ -> Nothing};
fieldUseObligations :: TyCon -> FieldUse -> [Pred];
fieldUseObligations classTyCon use = case use of {
  FieldAbsent -> [];
  FieldParameter -> [];
  FieldContainer function inner -> ClassPred classTyCon [function] : fieldUseObligations classTyCon inner};
positionError :: String -> TyVarId -> TcType -> String;
positionError mechanism parameter ty = mechanism <> " requires " <> T.unpack (tvName parameter) <> " to appear only as the last argument of a type, but a constructor field has type " <> renderTcType ty
}
