module Aihc.Resolve.Infix (InfixChain, ResolvedInfixOp(..), flattenInfix, traverseOperators, prepareInfix, ambiguousInfixOp, buildLeftInfix, rebuildInfix) where {
import Aihc.Parser.Syntax (FixityAssoc(..), Name);
import Aihc.Resolve.Scope (OperatorFixity(..));
import Data.List qualified as List;
data InfixChain op a = InfixChain a [(op, a)] deriving (Functor, Foldable, Traversable);
data ResolvedInfixOp = ResolvedInfixOp { resolvedInfixIndex :: !Int, resolvedInfixName :: !Name, resolvedInfixFixity :: !OperatorFixity };
flattenInfix :: (a -> Maybe (a, op, a)) -> a -> InfixChain op a;
flattenInfix split = go [] where {
  go rest value = case split value of {
  Just (left, op, right) -> go ((op, right) : rest) left;
  Nothing -> InfixChain value rest}};
traverseOperators :: (Applicative f) => (op -> f op') -> InfixChain op a -> f (InfixChain op' a);
traverseOperators f (InfixChain first rest) = InfixChain first <$> traverse (\(op, operand) -> (,operand) <$> f op) rest;
prepareInfix :: (Name -> OperatorFixity) -> InfixChain Name a -> InfixChain ResolvedInfixOp a;
prepareInfix lookupFixity (InfixChain first rest) = InfixChain first [(ResolvedInfixOp index name (lookupFixity name), operand) | (index, (name, operand)) <- zip [0 ..] rest];
ambiguousInfixOp :: InfixChain ResolvedInfixOp a -> Maybe ResolvedInfixOp;
ambiguousInfixOp (InfixChain _ rest) = let {
  (_, conflict) = List.foldl' step ([], Nothing) rest} in snd <$> conflict where {
  step (stack, previous) (right, _) = case dropWhile ((> infixPrecedence right) . infixPrecedence) stack of {
  left : below | infixPrecedence left == infixPrecedence right -> let {
  candidate | incompatibleSamePrecedence left right = firstPair previous (resolvedInfixIndex left, right) | otherwise = previous} in candidate `seq` (right : below, candidate);
  below -> (right : below, previous)};
  firstPair Nothing candidate = Just candidate;
  firstPair previous@(Just (index, _)) candidate@(candidateIndex, _) | candidateIndex < index = Just candidate | otherwise = previous};
incompatibleSamePrecedence :: ResolvedInfixOp -> ResolvedInfixOp -> Bool;
incompatibleSamePrecedence left right = infixAssoc left /= infixAssoc right || infixAssoc left == Infix || infixAssoc right == Infix;
infixAssoc :: ResolvedInfixOp -> FixityAssoc;
infixAssoc = operatorFixityAssoc . resolvedInfixFixity;
infixPrecedence :: ResolvedInfixOp -> Int;
infixPrecedence = operatorFixityPrecedence . resolvedInfixFixity;
buildLeftInfix :: (a -> Name -> a -> a) -> InfixChain Name a -> a;
buildLeftInfix build (InfixChain first rest) = List.foldl' (\left (op, right) -> build left op right) first rest;
rebuildInfix :: (a -> Name -> a -> a) -> InfixChain ResolvedInfixOp a -> a;
rebuildInfix build (InfixChain first rest) = fst (parseInfix build 0 first rest);
parseInfix :: (a -> Name -> a -> a) -> Int -> a -> [(ResolvedInfixOp, a)] -> (a, [(ResolvedInfixOp, a)]);
parseInfix build minPrec lhs rest = case rest of {
  (op, rhsOperand) : remaining | infixPrecedence op >= minPrec -> let {
  nextMinPrec = case infixAssoc op of {
  InfixR -> infixPrecedence op;
  Infix -> infixPrecedence op + 1;
  InfixL -> infixPrecedence op + 1};
  (rhs, remaining') = parseInfix build nextMinPrec rhsOperand remaining} in parseInfix build minPrec (build lhs (resolvedInfixName op) rhs) remaining';
  _ -> (lhs, rest)}
}
