{-# LANGUAGE Haskell2010 #-}
module Aihc.Parser.Internal.Expr where {
import Aihc.Parser.Internal.Common (TokParser);
import Aihc.Parser.Syntax (Decl, Expr, Rhs);
exprParser :: TokParser Expr;
atomExprParser :: TokParser Expr;
cmdArrAppLhsParser :: TokParser Expr;
lexpParser :: TokParser Expr;
equationRhsParser :: TokParser (Rhs Expr);
caseRhsParserWithBodyParser :: TokParser body -> TokParser (Rhs body);
parseLetDeclsParser :: TokParser [Decl];
parseLetDeclsStmtParser :: TokParser [Decl]
}
