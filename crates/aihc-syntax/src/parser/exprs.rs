//! Expressions, statements, alternatives and right-hand sides.
//!
//! ```text
//! exp      ::= infixexp :: type | infixexp
//! infixexp ::= [-] lexp (qop [-] lexp)*       -- operators in source order
//! lexp     ::= \ apat+ -> exp | \case { alts } | let { decls } in exp
//!            | if exp [;] then exp [;] else exp | case exp of { alts }
//!            | do { stmts } | fexp
//! fexp     ::= aexp (aexp | @ atype)*
//! aexp     ::= qvar | gcon | literal | _ | ( ... ) | [ ... ]
//!            | qcon { fbinds } | aexp { fbinds }
//! ```
//!
//! Operator chains stay flat and in source order. Fixity resolution is a
//! later pass, after imports are known. The printer reproduces the chain
//! as written, so any parser resolves the printed source the same way as
//! the original.

use super::decls::DeclContext;
use super::{Parser, Result};
use crate::ast::{Alt, Decl, Expr, FieldUpdate, GuardedRhs, QName, Rhs, RhsBody, Stmt};
use crate::token::{Keyword, ReservedOp, TokenKind};

impl Parser {
    /// A full expression, with an optional type signature.
    pub(super) fn exp(&mut self) -> Result<Expr> {
        let e = self.infixexp()?;
        if self.eat(&TokenKind::ReservedOp(ReservedOp::DoubleColon)) {
            let ty = self.ty()?;
            return Ok(Expr::Sig(Box::new(e), ty));
        }
        Ok(e)
    }

    /// An operator chain. `stop_at_section` makes the chain stop before an
    /// operator that is followed by `)`, and reports that operator, so
    /// the caller can build a left section.
    fn infixexp(&mut self) -> Result<Expr> {
        let (e, section) = self.infixexp_or_section(false)?;
        debug_assert!(section.is_none());
        Ok(e)
    }

    /// The first element of the result is the chain. The second is the
    /// operator of a left section, when the chain stopped before `op )`.
    fn infixexp_or_section(&mut self, allow_section: bool) -> Result<(Expr, Option<QName>)> {
        let first = self.operand()?;
        let mut rest = Vec::new();
        while let Some(op) = self.operator_ahead() {
            if allow_section
                && matches!(
                    self.tokens.get(self.idx + op.1).map(|t| &t.kind),
                    Some(TokenKind::Special(')'))
                )
            {
                self.idx += op.1;
                let e = build_chain(first, rest);
                return Ok((e, Some(op.0)));
            }
            self.idx += op.1;
            rest.push((op.0, self.operand()?));
        }
        Ok((build_chain(first, rest), None))
    }

    /// One operand of an operator chain: a `lexp`, with optional prefix
    /// negation.
    fn operand(&mut self) -> Result<Expr> {
        if self.at_varsym("-") {
            self.bump();
            return Ok(Expr::Neg(Box::new(self.lexp()?)));
        }
        self.lexp()
    }

    /// An operator at the current position, not consumed, with the number
    /// of tokens it takes: `+`, `M.+`, `:`, `:|`, `` `f` ``, `` `M.f` ``.
    pub(super) fn operator_ahead(&self) -> Option<(QName, usize)> {
        match self.kind() {
            TokenKind::VarSym { qual, name } | TokenKind::ConSym { qual, name } => Some((
                QName {
                    qual: qual.clone(),
                    name: name.clone(),
                },
                1,
            )),
            TokenKind::ReservedOp(ReservedOp::Colon) => Some((QName::unqualified(":"), 1)),
            TokenKind::Special('`') => {
                let name = match self.tokens.get(self.idx + 1).map(|t| &t.kind) {
                    Some(TokenKind::VarId { qual, name } | TokenKind::ConId { qual, name }) => {
                        QName {
                            qual: qual.clone(),
                            name: name.clone(),
                        }
                    }
                    _ => return None,
                };
                if !matches!(
                    self.tokens.get(self.idx + 2).map(|t| &t.kind),
                    Some(TokenKind::Special('`'))
                ) {
                    return None;
                }
                Some((name, 3))
            }
            _ => None,
        }
    }

    /// An expression with a keyword at the front, or an application.
    fn lexp(&mut self) -> Result<Expr> {
        match self.kind() {
            TokenKind::ReservedOp(ReservedOp::Backslash) => {
                self.bump();
                if self.at_keyword(Keyword::Case) {
                    self.bump();
                    let alts = self.alts()?;
                    return Ok(Expr::LambdaCase(alts));
                }
                let mut pats = Vec::new();
                while !self.at(&TokenKind::ReservedOp(ReservedOp::RightArrow)) {
                    pats.push(self.apat()?);
                }
                self.bump();
                let body = self.exp()?;
                Ok(Expr::Lambda(pats, Box::new(body)))
            }
            TokenKind::Keyword(Keyword::Let) => {
                self.bump();
                let decls = self.local_decls()?;
                self.expect_keyword(Keyword::In)?;
                let body = self.exp()?;
                Ok(Expr::Let(decls, Box::new(body)))
            }
            TokenKind::Keyword(Keyword::If) => {
                self.bump();
                let c = self.exp()?;
                self.semis_before(Keyword::Then);
                self.expect_keyword(Keyword::Then)?;
                let t = self.exp()?;
                self.semis_before(Keyword::Else);
                self.expect_keyword(Keyword::Else)?;
                let e = self.exp()?;
                Ok(Expr::If(Box::new(c), Box::new(t), Box::new(e)))
            }
            TokenKind::Keyword(Keyword::Case) => {
                self.bump();
                let scrutinee = self.exp()?;
                self.expect_keyword(Keyword::Of)?;
                let alts = self.alts()?;
                Ok(Expr::Case(Box::new(scrutinee), alts))
            }
            TokenKind::Keyword(Keyword::Do) => {
                self.bump();
                Ok(Expr::Do(self.stmts()?))
            }
            _ => self.fexp(),
        }
    }

    /// `DoAndIfThenElse`: a `;` may stand before `then` and `else`.
    fn semis_before(&mut self, k: Keyword) {
        if matches!(self.kind(), TokenKind::Special(';') | TokenKind::VSemi)
            && matches!(
                self.tokens.get(self.idx + 1).map(|t| &t.kind),
                Some(TokenKind::Keyword(k2)) if *k2 == k
            )
        {
            self.bump();
        }
    }

    /// The declarations of a `let` or `where`, in a block.
    pub(super) fn local_decls(&mut self) -> Result<Vec<Decl>> {
        self.block(|p| p.decl(DeclContext::Local))
    }

    /// A function application.
    fn fexp(&mut self) -> Result<Expr> {
        let mut e = self.aexp()?;
        loop {
            if self.at(&TokenKind::ReservedOp(ReservedOp::At)) {
                self.bump();
                let ty = self.atype()?;
                e = Expr::TypeApp(Box::new(e), ty);
            } else if self.at_aexp_start() {
                let arg = self.aexp()?;
                e = Expr::App(Box::new(e), Box::new(arg));
            } else {
                return Ok(e);
            }
        }
    }

    fn at_aexp_start(&self) -> bool {
        matches!(
            self.kind(),
            TokenKind::VarId { .. }
                | TokenKind::ConId { .. }
                | TokenKind::Integer(_)
                | TokenKind::Float(_)
                | TokenKind::Char { .. }
                | TokenKind::String { .. }
                | TokenKind::Special('(' | '[')
                | TokenKind::Keyword(Keyword::Underscore)
        )
    }

    /// An atomic expression, with any record braces that follow it.
    fn aexp(&mut self) -> Result<Expr> {
        let mut e = self.aexp1()?;
        while self.at_special('{') {
            let (fields, wildcard) = self.field_updates()?;
            e = match e {
                Expr::Con(name) => Expr::RecordCon {
                    name,
                    fields,
                    wildcard,
                },
                e => {
                    if wildcard {
                        return self.error("`..` in a record update");
                    }
                    Expr::RecordUpdate(Box::new(e), fields)
                }
            };
        }
        Ok(e)
    }

    fn aexp1(&mut self) -> Result<Expr> {
        match self.kind() {
            TokenKind::VarId { qual, name } => {
                let name = QName {
                    qual: qual.clone(),
                    name: name.clone(),
                };
                self.bump();
                Ok(Expr::Var(name))
            }
            TokenKind::Keyword(Keyword::Underscore) => {
                self.bump();
                Ok(Expr::Hole)
            }
            TokenKind::Integer(_)
            | TokenKind::Float(_)
            | TokenKind::Char { .. }
            | TokenKind::String { .. } => Ok(Expr::Lit(self.literal()?)),
            TokenKind::ConId { .. } => {
                let name = self.gcon_ahead().unwrap();
                self.bump();
                Ok(Expr::Con(name))
            }
            TokenKind::Special('(') => {
                if let Some(name) = self.gcon_ahead() {
                    self.bump_gcon(&name);
                    return Ok(Expr::Con(name));
                }
                if let Some(name) = self.parenthesized_name() {
                    return Ok(Expr::Var(name));
                }
                self.paren_exp()
            }
            TokenKind::Special('[') => {
                if let Some(name) = self.gcon_ahead() {
                    self.bump_gcon(&name);
                    return Ok(Expr::Con(name));
                }
                self.bracket_exp()
            }
            _ => self.unexpected("an expression"),
        }
    }

    /// After `(`: a parenthesized expression, a tuple, a tuple section,
    /// or an operator section.
    fn paren_exp(&mut self) -> Result<Expr> {
        self.expect_special('(')?;
        // A right section: `(op e)`. `(- e)` is negation, not a section.
        if let Some((op, len)) = self.operator_ahead() {
            if !(op.qual.is_empty() && op.name == "-") {
                self.idx += len;
                let e = self.infixexp()?;
                self.expect_special(')')?;
                return Ok(Expr::RightSection(op, Box::new(e)));
            }
        }
        let mut items: Vec<Option<Expr>> = Vec::new();
        loop {
            if self.at_special(',') || self.at_special(')') {
                items.push(None);
            } else {
                let (e, section) = self.infixexp_or_section(true)?;
                if let Some(op) = section {
                    self.expect_special(')')?;
                    return Ok(Expr::LeftSection(Box::new(e), op));
                }
                let e = if self.eat(&TokenKind::ReservedOp(ReservedOp::DoubleColon)) {
                    let ty = self.ty()?;
                    Expr::Sig(Box::new(e), ty)
                } else {
                    e
                };
                items.push(Some(e));
            }
            if !self.eat_special(',') {
                self.expect_special(')')?;
                break;
            }
        }
        if items.len() == 1 {
            return match items.pop().unwrap() {
                Some(e) => Ok(Expr::Paren(Box::new(e))),
                None => self.error("empty parentheses"),
            };
        }
        if items.iter().all(Option::is_some) {
            Ok(Expr::Tuple(items.into_iter().map(Option::unwrap).collect()))
        } else {
            Ok(Expr::TupleSection(items))
        }
    }

    /// After `[`: a list, an arithmetic sequence or a list comprehension.
    fn bracket_exp(&mut self) -> Result<Expr> {
        self.expect_special('[')?;
        let first = self.exp()?;
        if self.eat(&TokenKind::ReservedOp(ReservedOp::DotDot)) {
            let to = if self.at_special(']') {
                None
            } else {
                Some(Box::new(self.exp()?))
            };
            self.expect_special(']')?;
            return Ok(Expr::ArithSeq {
                from: Box::new(first),
                then: None,
                to,
            });
        }
        if self.eat(&TokenKind::ReservedOp(ReservedOp::Bar)) {
            let mut stmts = Vec::new();
            loop {
                stmts.push(self.stmt()?);
                if !self.eat_special(',') {
                    break;
                }
            }
            self.expect_special(']')?;
            return Ok(Expr::ListComp(Box::new(first), stmts));
        }
        let mut items = vec![first];
        if self.eat_special(',') {
            let second = self.exp()?;
            if self.eat(&TokenKind::ReservedOp(ReservedOp::DotDot)) {
                let to = if self.at_special(']') {
                    None
                } else {
                    Some(Box::new(self.exp()?))
                };
                self.expect_special(']')?;
                return Ok(Expr::ArithSeq {
                    from: Box::new(items.pop().unwrap()),
                    then: Some(Box::new(second)),
                    to,
                });
            }
            items.push(second);
            while self.eat_special(',') {
                items.push(self.exp()?);
            }
        }
        self.expect_special(']')?;
        Ok(Expr::List(items))
    }

    /// `{ f = e, g, .. }`
    fn field_updates(&mut self) -> Result<(Vec<FieldUpdate>, bool)> {
        self.expect_special('{')?;
        let mut fields = Vec::new();
        let mut wildcard = false;
        if !self.at_special('}') {
            loop {
                if self.eat(&TokenKind::ReservedOp(ReservedOp::DotDot)) {
                    wildcard = true;
                    break;
                }
                let name = self.qname()?;
                let value = if self.eat(&TokenKind::ReservedOp(ReservedOp::Equals)) {
                    Some(self.exp()?)
                } else {
                    None
                };
                fields.push(FieldUpdate { name, value });
                if !self.eat_special(',') {
                    break;
                }
            }
        }
        self.expect_special('}')?;
        Ok((fields, wildcard))
    }

    // --- Statements ---------------------------------------------------------

    /// The statements of a `do` block.
    fn stmts(&mut self) -> Result<Vec<Stmt>> {
        self.block(Self::stmt)
    }

    /// One statement of a `do` block, a comprehension or a guard.
    pub(super) fn stmt(&mut self) -> Result<Stmt> {
        if self.at_keyword(Keyword::Let) {
            self.bump();
            let decls = self.local_decls()?;
            if self.eat_keyword(Keyword::In) {
                // `let ... in e` as an expression statement, possibly the
                // first operand of an operator chain.
                let body = self.exp()?;
                let first = Expr::Let(decls, Box::new(body));
                let mut rest = Vec::new();
                while let Some((op, len)) = self.operator_ahead() {
                    self.idx += len;
                    rest.push((op, self.operand()?));
                }
                return Ok(Stmt::Expr(build_chain(first, rest)));
            }
            return Ok(Stmt::Let(decls));
        }
        if self.bind_arrow_ahead() {
            let pat = self.pat()?;
            self.expect(&TokenKind::ReservedOp(ReservedOp::LeftArrow), "`<-`")?;
            let e = self.exp()?;
            return Ok(Stmt::Bind(pat, e));
        }
        Ok(Stmt::Expr(self.exp()?))
    }

    /// Whether a `<-` follows at bracket depth zero before the end of
    /// this statement.
    fn bind_arrow_ahead(&self) -> bool {
        let mut i = self.idx;
        let mut depth = 0u32;
        while let Some(tok) = self.tokens.get(i) {
            match &tok.kind {
                TokenKind::Special('(' | '[' | '{') | TokenKind::VOpen => depth += 1,
                TokenKind::Special(')' | ']' | '}') | TokenKind::VClose => {
                    if depth == 0 {
                        return false;
                    }
                    depth -= 1;
                }
                TokenKind::ReservedOp(ReservedOp::LeftArrow) if depth == 0 => return true,
                TokenKind::Special(';' | ',') | TokenKind::VSemi | TokenKind::Eof if depth == 0 => {
                    return false
                }
                TokenKind::ReservedOp(
                    ReservedOp::Equals | ReservedOp::RightArrow | ReservedOp::Bar,
                ) if depth == 0 => return false,
                _ => {}
            }
            i += 1;
        }
        false
    }

    // --- Alternatives and right-hand sides ------------------------------------

    fn alts(&mut self) -> Result<Vec<Alt>> {
        self.block(|p| {
            let pos = p.pos();
            let pat = p.pat()?;
            let rhs = p.rhs(ReservedOp::RightArrow)?;
            Ok(Alt { pos, pat, rhs })
        })
    }

    /// `= e` or `| g = e ...`, then an optional `where`. `sep` is `=` in
    /// a binding and `->` in an alternative.
    pub(super) fn rhs(&mut self, sep: ReservedOp) -> Result<Rhs> {
        let body = if self.at(&TokenKind::ReservedOp(ReservedOp::Bar)) {
            let mut guarded = Vec::new();
            while self.eat(&TokenKind::ReservedOp(ReservedOp::Bar)) {
                let mut guards = Vec::new();
                loop {
                    guards.push(self.stmt()?);
                    if !self.eat_special(',') {
                        break;
                    }
                }
                self.expect(&TokenKind::ReservedOp(sep), &format!("`{}`", sep.as_str()))?;
                let body = self.exp()?;
                guarded.push(GuardedRhs { guards, body });
            }
            RhsBody::Guarded(guarded)
        } else {
            self.expect(&TokenKind::ReservedOp(sep), &format!("`{}`", sep.as_str()))?;
            RhsBody::Plain(self.exp()?)
        };
        let wheres = if self.eat_keyword(Keyword::Where) {
            self.local_decls()?
        } else {
            Vec::new()
        };
        Ok(Rhs { body, wheres })
    }
}

fn build_chain(first: Expr, rest: Vec<(QName, Expr)>) -> Expr {
    if rest.is_empty() {
        first
    } else {
        Expr::Infix {
            first: Box::new(first),
            rest,
        }
    }
}
