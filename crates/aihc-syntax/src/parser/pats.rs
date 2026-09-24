//! Patterns.
//!
//! ```text
//! pat  ::= lpat (qconop lpat)*                -- infix constructors
//! lpat ::= apat | - literal | gcon apat+
//! apat ::= var [@ apat] | gcon | qcon { fpats } | literal | _
//!        | ( pat ) | ( pat , pat ... ) | ( exp -> pat ) | ( pat :: type )
//!        | [ pat , ... ] | ~ apat | ! apat
//! ```

use super::{Parser, Result};
use crate::ast::{FieldPat, Literal, Pat, QName};
use crate::token::{Keyword, ReservedOp, TokenKind};

impl Parser {
    /// A pattern, with infix constructor operators.
    pub(super) fn pat(&mut self) -> Result<Pat> {
        let first = self.lpat()?;
        let mut rest = Vec::new();
        while let Some(op) = self.con_operator() {
            rest.push((op, self.lpat()?));
        }
        if rest.is_empty() {
            Ok(first)
        } else {
            Ok(Pat::Infix {
                first: Box::new(first),
                rest,
            })
        }
    }

    /// A constructor operator, consumed: `:`, `:|`, `` `Con` ``.
    fn con_operator(&mut self) -> Option<QName> {
        match self.kind() {
            TokenKind::ConSym { qual, name } => {
                let op = QName {
                    qual: qual.clone(),
                    name: name.clone(),
                };
                self.bump();
                Some(op)
            }
            TokenKind::ReservedOp(ReservedOp::Colon) => {
                self.bump();
                Some(QName::unqualified(":"))
            }
            TokenKind::Special('`') => {
                let Some(TokenKind::ConId { qual, name }) =
                    self.tokens.get(self.idx + 1).map(|t| &t.kind)
                else {
                    return None;
                };
                if !matches!(
                    self.tokens.get(self.idx + 2).map(|t| &t.kind),
                    Some(TokenKind::Special('`'))
                ) {
                    return None;
                }
                let op = QName {
                    qual: qual.clone(),
                    name: name.clone(),
                };
                self.idx += 3;
                Some(op)
            }
            _ => None,
        }
    }

    /// A pattern without infix operators at the top: a constructor
    /// application, a negative literal, or an atomic pattern.
    fn lpat(&mut self) -> Result<Pat> {
        if self.at_varsym("-") {
            self.bump();
            let lit = self.literal()?;
            return Ok(Pat::Lit {
                lit,
                negative: true,
            });
        }
        if let Some(name) = self.gcon_ahead() {
            self.bump_gcon(&name);
            if self.at_special('{') {
                return self.record_pat(name);
            }
            let mut args = Vec::new();
            while self.at_apat_start() {
                args.push(self.apat()?);
            }
            return Ok(Pat::Con { name, args });
        }
        self.apat()
    }

    pub(super) fn literal(&mut self) -> Result<Literal> {
        let lit = match self.kind() {
            TokenKind::Integer(s) => Literal::Integer(s.clone()),
            TokenKind::Float(s) => Literal::Float(s.clone()),
            TokenKind::Char { value, raw } => Literal::Char {
                value: *value,
                raw: raw.clone(),
            },
            TokenKind::String { value, raw } => Literal::String {
                value: value.clone(),
                raw: raw.clone(),
            },
            _ => return self.unexpected("a literal"),
        };
        self.bump();
        Ok(lit)
    }

    /// A constructor name at the current position, without consuming it:
    /// `C`, `M.C`, `()`, `[]`, `(,)`, `(:|)`.
    pub(super) fn gcon_ahead(&self) -> Option<QName> {
        match self.kind() {
            TokenKind::ConId { qual, name } => Some(QName {
                qual: qual.clone(),
                name: name.clone(),
            }),
            TokenKind::Special('(') => {
                let mut i = self.idx + 1;
                match self.tokens.get(i).map(|t| &t.kind) {
                    Some(TokenKind::Special(')')) => Some(QName::unqualified("()")),
                    Some(TokenKind::Special(',')) => {
                        let mut name = String::from("(");
                        while let Some(TokenKind::Special(',')) =
                            self.tokens.get(i).map(|t| &t.kind)
                        {
                            name.push(',');
                            i += 1;
                        }
                        match self.tokens.get(i).map(|t| &t.kind) {
                            Some(TokenKind::Special(')')) => {
                                name.push(')');
                                Some(QName::unqualified(name))
                            }
                            _ => None,
                        }
                    }
                    Some(TokenKind::ConSym { qual, name }) => {
                        if matches!(
                            self.tokens.get(i + 1).map(|t| &t.kind),
                            Some(TokenKind::Special(')'))
                        ) {
                            Some(QName {
                                qual: qual.clone(),
                                name: name.clone(),
                            })
                        } else {
                            None
                        }
                    }
                    Some(TokenKind::ReservedOp(ReservedOp::Colon)) => {
                        if matches!(
                            self.tokens.get(i + 1).map(|t| &t.kind),
                            Some(TokenKind::Special(')'))
                        ) {
                            Some(QName::unqualified(":"))
                        } else {
                            None
                        }
                    }
                    _ => None,
                }
            }
            TokenKind::Special('[') => {
                if matches!(
                    self.tokens.get(self.idx + 1).map(|t| &t.kind),
                    Some(TokenKind::Special(']'))
                ) {
                    Some(QName::unqualified("[]"))
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    /// Consume the tokens of a constructor name that `gcon_ahead` found.
    pub(super) fn bump_gcon(&mut self, name: &QName) {
        match self.kind() {
            TokenKind::ConId { .. } => {
                self.bump();
            }
            TokenKind::Special('(') => {
                if name.name == "()" || name.name.starts_with("(,") {
                    // `(` `,`* `)`
                    self.idx += name.name.len();
                } else {
                    // `(` op `)`
                    self.idx += 3;
                }
            }
            _ => {
                // `[]`
                self.idx += 2;
            }
        }
    }

    pub(super) fn at_apat_start(&self) -> bool {
        match self.kind() {
            TokenKind::VarId { qual, .. } => qual.is_empty(),
            TokenKind::ConId { .. }
            | TokenKind::Special('(' | '[')
            | TokenKind::Keyword(Keyword::Underscore)
            | TokenKind::Integer(_)
            | TokenKind::Float(_)
            | TokenKind::Char { .. }
            | TokenKind::String { .. }
            | TokenKind::ReservedOp(ReservedOp::Tilde) => true,
            TokenKind::VarSym { qual, name } => qual.is_empty() && name == "!",
            _ => false,
        }
    }

    /// An atomic pattern.
    pub(super) fn apat(&mut self) -> Result<Pat> {
        match self.kind() {
            TokenKind::VarId { qual, name } if qual.is_empty() => {
                let name = name.clone();
                self.bump();
                if self.eat(&TokenKind::ReservedOp(ReservedOp::At)) {
                    let inner = self.apat()?;
                    return Ok(Pat::As(name, Box::new(inner)));
                }
                Ok(Pat::Var(name))
            }
            TokenKind::Keyword(Keyword::Underscore) => {
                self.bump();
                Ok(Pat::Wildcard)
            }
            TokenKind::ReservedOp(ReservedOp::Tilde) => {
                self.bump();
                Ok(Pat::Lazy(Box::new(self.apat()?)))
            }
            TokenKind::VarSym { .. } if self.at_varsym("!") => {
                self.bump();
                Ok(Pat::Bang(Box::new(self.apat()?)))
            }
            TokenKind::Integer(_)
            | TokenKind::Float(_)
            | TokenKind::Char { .. }
            | TokenKind::String { .. } => {
                let lit = self.literal()?;
                Ok(Pat::Lit {
                    lit,
                    negative: false,
                })
            }
            TokenKind::Special('[') => {
                if let Some(name) = self.gcon_ahead() {
                    self.bump_gcon(&name);
                    return Ok(Pat::Con {
                        name,
                        args: Vec::new(),
                    });
                }
                self.bump();
                let mut items = Vec::new();
                loop {
                    items.push(self.pat()?);
                    if !self.eat_special(',') {
                        self.expect_special(']')?;
                        return Ok(Pat::List(items));
                    }
                }
            }
            TokenKind::Special('(') => {
                if let Some(name) = self.gcon_ahead() {
                    self.bump_gcon(&name);
                    if self.at_special('{') {
                        return self.record_pat(name);
                    }
                    return Ok(Pat::Con {
                        name,
                        args: Vec::new(),
                    });
                }
                self.paren_pat()
            }
            TokenKind::ConId { .. } => {
                let name = self.gcon_ahead().unwrap();
                self.bump();
                if self.at_special('{') {
                    return self.record_pat(name);
                }
                Ok(Pat::Con {
                    name,
                    args: Vec::new(),
                })
            }
            _ => self.unexpected("a pattern"),
        }
    }

    /// After `(`: a parenthesized pattern, a tuple, a view pattern or a
    /// pattern with a type signature.
    fn paren_pat(&mut self) -> Result<Pat> {
        self.expect_special('(')?;
        let mut items = Vec::new();
        loop {
            let item = if self.view_arrow_ahead() {
                let e = self.exp()?;
                self.expect(&TokenKind::ReservedOp(ReservedOp::RightArrow), "`->`")?;
                let p = self.pat()?;
                Pat::View(Box::new(e), Box::new(p))
            } else {
                let p = self.pat()?;
                if self.eat(&TokenKind::ReservedOp(ReservedOp::DoubleColon)) {
                    let ty = self.ty()?;
                    Pat::Sig(Box::new(p), ty)
                } else {
                    p
                }
            };
            items.push(item);
            if !self.eat_special(',') {
                self.expect_special(')')?;
                break;
            }
        }
        if items.len() == 1 {
            Ok(Pat::Paren(Box::new(items.pop().unwrap())))
        } else {
            Ok(Pat::Tuple(items))
        }
    }

    /// Whether a `->` follows at bracket depth zero before the `,` or `)`
    /// that ends this parenthesized item: the mark of a view pattern.
    fn view_arrow_ahead(&self) -> bool {
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
                TokenKind::Special(',') if depth == 0 => return false,
                TokenKind::ReservedOp(ReservedOp::RightArrow) if depth == 0 => return true,
                // A lambda or case inside the item would need parentheses
                // to be an expression here, so `->` at depth zero is the
                // view arrow.
                TokenKind::Eof => return false,
                _ => {}
            }
            i += 1;
        }
        false
    }

    /// `C { f = p, g, .. }` after the constructor name.
    fn record_pat(&mut self, name: QName) -> Result<Pat> {
        self.expect_special('{')?;
        let mut fields = Vec::new();
        let mut wildcard = false;
        if !self.at_special('}') {
            loop {
                if self.eat(&TokenKind::ReservedOp(ReservedOp::DotDot)) {
                    wildcard = true;
                    break;
                }
                let field = self.qname()?;
                let pat = if self.eat(&TokenKind::ReservedOp(ReservedOp::Equals)) {
                    Some(self.pat()?)
                } else {
                    None
                };
                fields.push(FieldPat { name: field, pat });
                if !self.eat_special(',') {
                    break;
                }
            }
        }
        self.expect_special('}')?;
        Ok(Pat::Record {
            name,
            fields,
            wildcard,
        })
    }
}
