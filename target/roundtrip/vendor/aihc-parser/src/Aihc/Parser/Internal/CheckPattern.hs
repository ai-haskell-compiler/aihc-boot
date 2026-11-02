{-# LANGUAGE OverloadedStrings #-}
module Aihc.Parser.Internal.CheckPattern (checkPattern, checkPatterns) where {
import Aihc.Parser.Internal.Common (isConLikeName, nameToUnqualified);
import Aihc.Parser.Syntax;
import Data.Maybe (isJust, isNothing);
import Data.Text (Text);
checkPattern :: Expr -> Either Text Pattern;
checkPattern expr = case expr of {
  EAnn ann sub -> fmap (PAnn ann) (checkPattern sub);
  EPragma pragma _ -> checkPragmaPattern pragma;
  EVar name | nameText name == "_" -> Right PWildcard | isConLikeName name -> Right (PCon name [] []) | isJust (nameQualifier name) -> Left "unexpected qualified name in pattern" | otherwise -> Right (PVar (nameToUnqualified name));
  EImplicitParam {} -> Left "unexpected implicit parameter in pattern";
  ETypeSyntax form ty -> Right (PTypeSyntax form ty);
  EParen inner | Just vp <- asViewPat inner -> Right (PParen vp) | otherwise -> PParen <$> checkPattern inner;
  ETuple fl elems | Just tupleCon <- tupleConstructorPattern fl elems -> Right tupleCon | otherwise -> do {
  pats <- traverse checkTupleElement elems;
  Right (PTuple fl pats)};
  EList elems -> PList <$> traverse checkPattern elems;
  EUnboxedSum i n e -> PUnboxedSum i n <$> checkPattern e;
  EViewPat l r -> do {
  rPat <- checkPattern r;
  Right (PView l rPat)};
  EInfix l op r | isConLikeOp op -> do {
  lPat <- checkPattern l;
  rPat <- checkPattern r;
  Right (PInfix lPat op rPat)} | otherwise -> Left ("unexpected variable operator '" <> renderName op <> "' in pattern");
  ETypeSig e ty -> do {
  pat <- checkPattern e;
  Right (PTypeSig pat ty)};
  ENegate inner -> checkNegLitPattern inner;
  EApp f x -> do {
  fPat <- checkPattern f;
  xPat <- checkPattern x;
  case peelPatternAnn fPat of {
  PCon name typeArgs args -> Right (PCon name typeArgs (args ++ [xPat]));
  PBuiltinCon con typeArgs args -> Right (PBuiltinCon con typeArgs (args ++ [xPat]));
  _ -> Left "invalid pattern: application of non-constructor"}};
  ERecordCon name fields wc -> do {
  patFields <- traverse (\field -> do {
  pat <- checkPattern (recordFieldValue field);
  pure field {recordFieldValue = pat}}) fields;
  Right (PRecord name patFields wc)};
  EInt n nt repr -> Right (PLit (LitInt n nt repr));
  EFloat x ft repr -> Right (PLit (LitFloat x ft repr));
  EChar c repr -> Right (PLit (LitChar c repr));
  ECharHash c repr -> Right (PLit (LitCharHash c repr));
  EString s repr -> Right (PLit (LitString s repr));
  EStringHash s repr -> Right (PLit (LitStringHash s repr));
  EOverloadedLabel {} -> Left "unexpected overloaded label in pattern";
  ETHSplice body -> Right (PSplice body);
  EQuasiQuote q b -> Right (PQuasiQuote q b);
  EIf {} -> Left "unexpected if-then-else in pattern";
  EMultiWayIf {} -> Left "unexpected multi-way if in pattern";
  ECase {} -> Left "unexpected case expression in pattern";
  EDo {} -> Left "unexpected do expression in pattern";
  ELambdaPats {} -> Left "unexpected lambda in pattern";
  ELambdaCase {} -> Left "unexpected lambda-case in pattern";
  ELambdaCases {} -> Left "unexpected lambda-cases in pattern";
  ELetDecls {} -> Left "unexpected let expression in pattern";
  EArithSeq {} -> Left "unexpected arithmetic sequence in pattern";
  EListComp {} -> Left "unexpected list comprehension in pattern";
  EListCompParallel {} -> Left "unexpected parallel list comprehension in pattern";
  ESectionL {} -> Left "unexpected left section in pattern";
  ESectionR {} -> Left "unexpected right section in pattern";
  ERecordUpd {} -> Left "unexpected record update in pattern";
  EGetField {} -> Left "unexpected record field access in pattern";
  EGetFieldProjection {} -> Left "unexpected projection section in pattern";
  ETypeApp fun ty -> do {
  funPat <- checkPattern fun;
  case peelPatternAnn funPat of {
  PCon name typeArgs args -> Right (PCon name (typeArgs ++ [ty]) args);
  PBuiltinCon con typeArgs args -> Right (PBuiltinCon con (typeArgs ++ [ty]) args);
  _ -> Left "unexpected type application in pattern"}};
  ETHExpQuote {} -> Left "unexpected Template Haskell expression quote in pattern";
  ETHTypedQuote {} -> Left "unexpected Template Haskell typed quote in pattern";
  ETHDeclQuote {} -> Left "unexpected Template Haskell declaration quote in pattern";
  ETHTypeQuote {} -> Left "unexpected Template Haskell type quote in pattern";
  ETHPatQuote {} -> Left "unexpected Template Haskell pattern quote in pattern";
  ETHNameQuote {} -> Left "unexpected Template Haskell name quote in pattern";
  ETHTypeNameQuote {} -> Left "unexpected Template Haskell type name quote in pattern";
  ETHTypedSplice {} -> Left "unexpected typed Template Haskell splice in pattern";
  EProc {} -> Left "unexpected proc expression in pattern"};
checkPragmaPattern :: Pragma -> Either Text Pattern;
checkPragmaPattern pragma = case pragmaType pragma of {
  PragmaSCC {} -> Left "unexpected SCC pragma in pattern";
  _ -> Left "unexpected pragma in pattern"};
checkPatterns :: [Expr] -> Either Text [Pattern];
checkPatterns = traverse checkPattern;
checkTupleElement :: Maybe Expr -> Either Text Pattern;
checkTupleElement Nothing = Left "unexpected tuple section in pattern";
checkTupleElement (Just e) = checkPattern e;
tupleConstructorPattern :: TupleFlavor -> [Maybe Expr] -> Maybe Pattern;
tupleConstructorPattern fl elems | null elems = Nothing | all isNothing elems = Just (PBuiltinCon (BuiltinTuple fl (length elems)) [] []) | otherwise = Nothing;
checkNegLitPattern :: Expr -> Either Text Pattern;
checkNegLitPattern inner = case inner of {
  EInt n nt repr -> Right (PNegLit (LitInt n nt repr));
  EFloat x ft repr -> Right (PNegLit (LitFloat x ft repr));
  EAnn ann sub -> fmap (PAnn ann) (checkNegLitPattern sub);
  _ -> Left "negation in pattern requires a numeric literal"};
isConLikeOp :: Name -> Bool;
isConLikeOp = isConLikeName;
asViewPat :: Expr -> Maybe Pattern;
asViewPat (EViewPat l r) = case checkPattern r of {
  Right rPat -> Just (PView l rPat);
  Left _ -> Nothing};
asViewPat _ = Nothing
}
