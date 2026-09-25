//! Types and kinds.
//!
//! The grammar, from loose to tight:
//!
//! ```text
//! type   ::= forall tvs . type
//!          | btype => type
//!          | btype -> type
//!          | btype
//! btype  ::= optype (typeop optype)*        -- infix type operators
//! optype ::= atype+                         -- application, with @k
//! atype  ::= tyvar | qtycon | ( ) | ( -> ) | (,) | [ ]
//!          | ( type ) | ( type , type ... ) | ( type :: kind )
//!          | [ type ] | ' atype | _ | literal
//! ```
//!
//! Type operators stay in source order. The tree keeps every parenthesis,
//! so the printer reproduces the tokens and a reader sees the same fixity.

use super::{Parser, Result};
use crate::ast::{BangType, Ident, QName, Span, TyVarBind, Type};
use crate::token::{Keyword, ReservedOp, TokenKind};

impl Parser {
    /// A type with optional `forall` and context.
    pub(super) fn ty(&mut self) -> Result<Type> {
        if self.at_varid("forall") {
            self.bump();
            let binders = self.ty_var_binds()?;
            self.expect_varsym(".")?;
            return Ok(Type::Forall(binders, Box::new(self.ty()?)));
        }
        let left = self.btype()?;
        if self.eat(&TokenKind::ReservedOp(ReservedOp::DoubleArrow)) {
            return Ok(Type::Qual(Box::new(left), Box::new(self.ty()?)));
        }
        if self.eat(&TokenKind::ReservedOp(ReservedOp::RightArrow)) {
            return Ok(Type::Fun(Box::new(left), Box::new(self.ty()?)));
        }
        Ok(left)
    }

    /// Type variable binders up to the `.` of a `forall`, or the end of a
    /// declaration head: `a`, `(a :: k)`.
    pub(super) fn ty_var_binds(&mut self) -> Result<Vec<TyVarBind>> {
        let mut binders = Vec::new();
        loop {
            match self.kind() {
                TokenKind::VarId { qual, name } if qual.is_empty() => {
                    binders.push(TyVarBind {
                        name: Ident::new(name.clone(), self.token_span()),
                        kind: None,
                    });
                    self.bump();
                }
                TokenKind::Special('(') if self.next_is_varid() => {
                    self.bump();
                    let name = self.varid()?;
                    self.expect(
                        &TokenKind::ReservedOp(ReservedOp::DoubleColon),
                        "`::` in a kinded binder",
                    )?;
                    let kind = self.ty()?;
                    self.expect_special(')')?;
                    binders.push(TyVarBind {
                        name,
                        kind: Some(kind),
                    });
                }
                _ => return Ok(binders),
            }
        }
    }

    fn next_is_varid(&self) -> bool {
        matches!(
            self.tokens.get(self.idx + 1).map(|t| &t.kind),
            Some(TokenKind::VarId { qual, .. }) if qual.is_empty()
        )
    }

    /// An unqualified variable name.
    pub(super) fn varid(&mut self) -> Result<Ident> {
        match self.kind() {
            TokenKind::VarId { qual, name } if qual.is_empty() => {
                let name = Ident::new(name.clone(), self.token_span());
                self.bump();
                Ok(name)
            }
            _ => self.unexpected("a variable"),
        }
    }

    pub(super) fn at_varsym(&self, sym: &str) -> bool {
        matches!(self.kind(), TokenKind::VarSym { qual, name } if qual.is_empty() && name == sym)
    }

    pub(super) fn expect_varsym(&mut self, sym: &str) -> Result<()> {
        if self.at_varsym(sym) {
            self.bump();
            Ok(())
        } else {
            self.unexpected(&format!("`{sym}`"))
        }
    }

    /// Applications joined by infix type operators.
    pub(super) fn btype(&mut self) -> Result<Type> {
        let mut left = self.optype()?;
        while let Some(op) = self.type_operator() {
            let right = self.optype()?;
            left = Type::Op(Box::new(left), op, Box::new(right));
        }
        Ok(left)
    }

    /// A type operator, consumed: `:+:`, `~`, `` `Op` `` or a symbol
    /// such as `+`.
    fn type_operator(&mut self) -> Option<QName> {
        let span = self.token_span();
        let op = match self.kind() {
            TokenKind::ConSym { qual, name } | TokenKind::VarSym { qual, name } => QName {
                qual: qual.clone(),
                name: name.clone(),
                span,
            },
            TokenKind::ReservedOp(ReservedOp::Tilde) => QName {
                span,
                ..QName::unqualified("~")
            },
            TokenKind::Special('`') => {
                let inner = self.tokens.get(self.idx + 1)?;
                let name = match &inner.kind {
                    TokenKind::ConId { qual, name } | TokenKind::VarId { qual, name } => QName {
                        qual: qual.clone(),
                        name: name.clone(),
                        span: Span {
                            start: inner.pos,
                            end: inner.end,
                        },
                    },
                    _ => return None,
                };
                if !matches!(
                    self.tokens.get(self.idx + 2).map(|t| &t.kind),
                    Some(TokenKind::Special('`'))
                ) {
                    return None;
                }
                self.idx += 3;
                return Some(name);
            }
            _ => return None,
        };
        self.bump();
        Some(op)
    }

    /// One or more atypes applied to each other.
    pub(super) fn optype(&mut self) -> Result<Type> {
        let mut ty = self.atype()?;
        loop {
            if self.at(&TokenKind::ReservedOp(ReservedOp::At)) {
                self.bump();
                let kind = self.atype()?;
                ty = Type::AppKind(Box::new(ty), Box::new(kind));
            } else if self.at_atype_start() {
                let arg = self.atype()?;
                ty = Type::App(Box::new(ty), Box::new(arg));
            } else {
                return Ok(ty);
            }
        }
    }

    pub(super) fn at_atype_start(&self) -> bool {
        match self.kind() {
            TokenKind::VarId { qual, name } => {
                // `forall` starts a nested type, never an argument.
                !(qual.is_empty() && name == "forall")
            }
            TokenKind::ConId { .. }
            | TokenKind::Special('(' | '[')
            | TokenKind::Tick
            | TokenKind::Keyword(Keyword::Underscore)
            | TokenKind::String { .. }
            | TokenKind::Integer(_)
            | TokenKind::Char { .. } => true,
            _ => false,
        }
    }

    /// An atomic type.
    pub(super) fn atype(&mut self) -> Result<Type> {
        match self.kind() {
            TokenKind::VarId { qual, name } if qual.is_empty() => {
                let name = Ident::new(name.clone(), self.token_span());
                self.bump();
                Ok(Type::Var(name))
            }
            TokenKind::ConId { qual, name } => {
                let name = QName {
                    qual: qual.clone(),
                    name: name.clone(),
                    span: self.token_span(),
                };
                self.bump();
                Ok(Type::Con(name))
            }
            TokenKind::Keyword(Keyword::Underscore) => {
                self.bump();
                Ok(Type::Wildcard)
            }
            TokenKind::Tick => {
                self.bump();
                if self.at_special('[') {
                    let items = self.bracketed_types()?;
                    return Ok(Type::PromotedList(items));
                }
                Ok(Type::Promoted(Box::new(self.atype()?)))
            }
            TokenKind::String { .. } | TokenKind::Integer(_) | TokenKind::Char { .. } => {
                Ok(Type::Lit(self.literal()?))
            }
            TokenKind::Special('[') => {
                let start = self.pos();
                let mut items = self.bracketed_types()?;
                let span = self.span_from(start);
                Ok(match items.len() {
                    0 => Type::Con(QName {
                        span,
                        ..QName::unqualified("[]")
                    }),
                    1 => Type::List(Box::new(items.pop().unwrap()), span),
                    _ => Type::PromotedList(items),
                })
            }
            TokenKind::Special('(') => self.paren_type(),
            _ => self.unexpected("a type"),
        }
    }

    /// `[ ]`, `[ t ]` or `[ t, t ]`.
    fn bracketed_types(&mut self) -> Result<Vec<Type>> {
        self.expect_special('[')?;
        let mut items = Vec::new();
        if self.eat_special(']') {
            return Ok(items);
        }
        loop {
            items.push(self.ty()?);
            if !self.eat_special(',') {
                self.expect_special(']')?;
                return Ok(items);
            }
        }
    }

    /// Everything that starts with `(`: unit, tuple constructors, the
    /// function arrow, parenthesized types, tuples and kind signatures.
    fn paren_type(&mut self) -> Result<Type> {
        let start = self.pos();
        let con = |name: &str, span| {
            Type::Con(QName {
                span,
                ..QName::unqualified(name)
            })
        };
        self.expect_special('(')?;
        if self.eat_special(')') {
            return Ok(con("()", self.span_from(start)));
        }
        if self.at(&TokenKind::ReservedOp(ReservedOp::RightArrow)) {
            self.bump();
            self.expect_special(')')?;
            return Ok(con("->", self.span_from(start)));
        }
        if self.at_special(',') {
            let mut name = String::from("(");
            while self.eat_special(',') {
                name.push(',');
            }
            name.push(')');
            self.expect_special(')')?;
            return Ok(con(&name, self.span_from(start)));
        }
        // An operator in parentheses, such as `(:+:)` or `(~)`.
        if let Some(mut name) = self.parenthesized_name_after_open() {
            name.span = self.span_from(start);
            return Ok(Type::Con(name));
        }
        let first = self.ty()?;
        if self.eat(&TokenKind::ReservedOp(ReservedOp::DoubleColon)) {
            let kind = self.ty()?;
            self.expect_special(')')?;
            return Ok(Type::Paren(
                Box::new(Type::KindSig(Box::new(first), Box::new(kind))),
                self.span_from(start),
            ));
        }
        if self.eat_special(')') {
            return Ok(Type::Paren(Box::new(first), self.span_from(start)));
        }
        let mut items = vec![first];
        while self.eat_special(',') {
            items.push(self.ty()?);
        }
        self.expect_special(')')?;
        Ok(Type::Tuple(items))
    }

    /// After a consumed `(`: an operator followed by `)`.
    fn parenthesized_name_after_open(&mut self) -> Option<QName> {
        let name = match self.kind() {
            TokenKind::VarSym { qual, name } | TokenKind::ConSym { qual, name } => QName {
                qual: qual.clone(),
                name: name.clone(),
                span: Span::default(),
            },
            TokenKind::ReservedOp(ReservedOp::Tilde) => QName::unqualified("~"),
            _ => return None,
        };
        // The caller sets the span, because it knows where `(` is.
        if !matches!(
            self.tokens.get(self.idx + 1).map(|t| &t.kind),
            Some(TokenKind::Special(')'))
        ) {
            return None;
        }
        self.idx += 2;
        Some(name)
    }

    /// A constructor argument or field type: optional `!` or `~`, then an
    /// atype (or a full type after `::`, which the caller chooses with
    /// `full`).
    pub(super) fn bang_type(&mut self, full: bool) -> Result<BangType> {
        let strict = if self.at_varsym("!") {
            self.bump();
            Some(true)
        } else if self.eat(&TokenKind::ReservedOp(ReservedOp::Tilde)) {
            Some(false)
        } else {
            None
        };
        let ty = if full { self.ty()? } else { self.atype()? };
        Ok(BangType { strict, ty })
    }
}
