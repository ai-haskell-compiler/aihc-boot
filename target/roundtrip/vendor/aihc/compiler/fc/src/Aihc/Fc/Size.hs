{-# LANGUAGE OverloadedStrings #-}
module Aihc.Fc.Size (programSize, exprSize, tailLeaves, isStrictBinder, isLiftedBinder) where {
import Aihc.Fc.Name;
import Aihc.Fc.Syntax;
import Aihc.Fc.TypeOf (TypeEnv, extendBinder, reduceType, repOf, typeEnvFromProgram);
import Aihc.Fc.Wired (primPackageFromScopes);
import Aihc.Resolve (PackageId(..));
import Data.List qualified as List;
import Data.Maybe (fromMaybe);
programSize :: Program -> Int;
programSize program = sum [1 + exprSize env (valBody declaration) | DeclVal declaration <- programDecls program] where {
  env = typeEnvFromProgram (fromMaybe (PackageId "aihc-prim") (primPackageFromScopes (programScopes program))) program};
exprSize :: TypeEnv -> Expr -> Int;
exprSize env expr = case expr of {
  ExVar {} -> 1;
  ExLit {} -> 1;
  ExCoercion {} -> 1;
  ExApp function argument -> 1 + exprSize env function + exprSize env argument;
  ExTyApp function _ -> exprSize env function;
  ExLam _ body -> 1 + exprSize env body;
  ExTyLam binder body -> exprSize (extendBinder env binder) body;
  ExLet bind body | isStrictBinder env (bindBinder bind) -> 1 + exprSize env (bindRhs bind) + tailLeaves env (bindRhs bind) * exprSize env body | otherwise -> 1 + exprSize env (bindRhs bind) + exprSize env body;
  ExRec binds body -> 1 + sum (map (exprSize env . bindRhs) binds) + exprSize env body;
  ExCase scrutinee _ _ alternatives -> exprSize env scrutinee + tailLeaves env scrutinee * sum [1 + altSize alternative | alternative <- alternatives];
  ExCast body _ -> exprSize env body;
  ExForeignCall _ _ arguments -> 1 + sum (map (exprSize env) arguments)} where {
  altSize alternative = exprSize (List.foldl' extendBinder env (altTypeBinders alternative)) (altRhs alternative)};
tailLeaves :: TypeEnv -> Expr -> Int;
tailLeaves env expr = case expr of {
  ExCase _ _ _ alternatives -> max 1 (sum [tailLeaves (List.foldl' extendBinder env (altTypeBinders alternative)) (altRhs alternative) | alternative <- alternatives]);
  ExLet bind body | isStrictBinder env (bindBinder bind) -> tailLeaves env (bindRhs bind) * tailLeaves env body | otherwise -> tailLeaves env body;
  ExRec _ body -> tailLeaves env body;
  ExCast body _ -> tailLeaves env body;
  ExTyLam binder body -> tailLeaves (extendBinder env binder) body;
  _ -> 1};
isStrictBinder :: TypeEnv -> Binder -> Bool;
isStrictBinder env = not . isLiftedBinder env;
isLiftedBinder :: TypeEnv -> Binder -> Bool;
isLiftedBinder env binder = case reduceType env <$> repOf env (binderType binder) of {
  Just (TyCon name) -> nameText name == "LiftedRep";
  Just (TyApp (TyCon boxed) (TyCon levity)) -> nameText boxed == "BoxedRep" && nameText levity == "Lifted";
  _ -> False}
}
