//! Declarations: type signatures, fixity, `data`, `newtype`, `type`,
//! `class`, `instance`, bindings and pattern synonyms.

use super::{Parser, Result};
use crate::ast::{
    Assoc, BangType, ConBody, ConDecl, Decl, DerivStrategy, Deriving, Field, Lhs, Pat, PatSynDir,
    PatSynLhs, Type,
};
use crate::token::{Keyword, ReservedOp, TokenKind};

/// Where a declaration appears. Class bodies permit `default` signatures.
#[derive(Clone, Copy, PartialEq, Eq)]
pub(super) enum DeclContext {
    TopLevel,
    Class,
    Instance,
    /// A `let` or `where` block.
    Local,
}

impl Parser {
    /// A `where` block of declarations, which may be empty. The `where`
    /// itself is consumed here.
    fn where_decls(&mut self, ctx: DeclContext) -> Result<Vec<Decl>> {
        if !self.eat_keyword(Keyword::Where) {
            return Ok(Vec::new());
        }
        self.block(|p| p.decl(ctx))
    }

    pub(super) fn decl(&mut self, ctx: DeclContext) -> Result<Decl> {
        let pos = self.pos();
        match self.kind() {
            TokenKind::Keyword(Keyword::Data) => self.data_decl(false),
            TokenKind::Keyword(Keyword::Newtype) => self.data_decl(true),
            TokenKind::Keyword(Keyword::Type) => self.type_decl(ctx),
            TokenKind::Keyword(Keyword::Class) => self.class_decl(),
            TokenKind::Keyword(Keyword::Instance) => self.instance_decl(),
            TokenKind::Keyword(Keyword::Infix | Keyword::Infixl | Keyword::Infixr) => self.fixity(),
            TokenKind::Keyword(Keyword::Deriving) => {
                self.error("standalone deriving is not supported yet")
            }
            TokenKind::Keyword(Keyword::Foreign) => {
                self.error("foreign declarations are not supported yet")
            }
            TokenKind::Keyword(Keyword::Default)
                if ctx == DeclContext::Class && self.next_is_varid_or_paren() =>
            {
                self.bump();
                let name = self.sig_name()?;
                self.expect(&TokenKind::ReservedOp(ReservedOp::DoubleColon), "`::`")?;
                let ty = self.ty()?;
                Ok(Decl::DefaultSig { pos, name, ty })
            }
            TokenKind::Keyword(Keyword::Default) => {
                self.error("default declarations are not supported yet")
            }
            _ if ctx == DeclContext::TopLevel
                && self.at_varid("pattern")
                && self.pattern_decl_ahead() =>
            {
                self.pat_syn()
            }
            _ => self.sig_or_binding(),
        }
    }

    /// `pattern` starts a pattern synonym when a constructor name, or a
    /// variable followed by a constructor operator, comes next.
    fn pattern_decl_ahead(&self) -> bool {
        match self.tokens.get(self.idx + 1).map(|t| &t.kind) {
            Some(TokenKind::ConId { .. }) => true,
            Some(TokenKind::VarId { .. }) => matches!(
                self.tokens.get(self.idx + 2).map(|t| &t.kind),
                Some(TokenKind::ConSym { .. } | TokenKind::Special('`'))
            ),
            _ => false,
        }
    }

    fn next_is_varid_or_paren(&self) -> bool {
        matches!(
            self.tokens.get(self.idx + 1).map(|t| &t.kind),
            Some(TokenKind::VarId { .. } | TokenKind::Special('('))
        )
    }

    /// `f, g :: t`, or a binding.
    fn sig_or_binding(&mut self) -> Result<Decl> {
        let pos = self.pos();
        let start = self.idx;
        let mut names = Vec::new();
        while let Ok(name) = self.sig_name() {
            names.push(name);
            if self.eat_special(',') {
                continue;
            }
            if self.eat(&TokenKind::ReservedOp(ReservedOp::DoubleColon)) {
                let ty = self.ty()?;
                return Ok(Decl::TypeSig { pos, names, ty });
            }
            break;
        }
        self.idx = start;
        let lhs = self.lhs()?;
        let rhs = self.rhs(ReservedOp::Equals)?;
        Ok(Decl::Bind { pos, lhs, rhs })
    }

    /// The left-hand side of a binding: `f p1 p2`, `(op) p1`, `p1 op p2`
    /// or a pattern.
    fn lhs(&mut self) -> Result<Lhs> {
        let start = self.idx;
        if let Ok(name) = self.sig_name() {
            let mut args = Vec::new();
            while self.at_apat_start() {
                args.push(self.apat()?);
            }
            if self.at_lhs_end() {
                return Ok(Lhs::Fun { name, args });
            }
            self.idx = start;
        }
        let mut left: Pat = self.pat()?;
        if self.eat(&TokenKind::ReservedOp(ReservedOp::DoubleColon)) {
            // A pattern binding with a signature: `p :: t = e`.
            let ty = self.ty()?;
            left = Pat::Sig(Box::new(left), ty);
        }
        if let Some((op, len)) = self.var_operator_ahead() {
            self.idx += len;
            let right = self.pat()?;
            return Ok(Lhs::Infix {
                left,
                op,
                right,
                args: Vec::new(),
            });
        }
        if self.at_lhs_end() {
            return Ok(Lhs::Pat(left));
        }
        self.unexpected("`=` or `|`")
    }

    fn at_lhs_end(&self) -> bool {
        matches!(
            self.kind(),
            TokenKind::ReservedOp(ReservedOp::Equals | ReservedOp::Bar)
        )
    }

    /// A variable operator, not consumed, with its token count: `<+>` or
    /// `` `f` ``. Constructor operators belong to patterns.
    fn var_operator_ahead(&self) -> Option<(String, usize)> {
        match self.kind() {
            TokenKind::VarSym { qual, name } if qual.is_empty() => Some((name.clone(), 1)),
            TokenKind::Special('`') => {
                let Some(TokenKind::VarId { qual, name }) =
                    self.tokens.get(self.idx + 1).map(|t| &t.kind)
                else {
                    return None;
                };
                if !qual.is_empty()
                    || !matches!(
                        self.tokens.get(self.idx + 2).map(|t| &t.kind),
                        Some(TokenKind::Special('`'))
                    )
                {
                    return None;
                }
                Some((format!("`{name}`"), 3))
            }
            _ => None,
        }
    }

    /// A name in a signature: `f` or `(+)`.
    fn sig_name(&mut self) -> Result<String> {
        if let Some(name) = self.parenthesized_name() {
            return Ok(name.name);
        }
        self.varid()
    }

    /// `pattern P, Q :: t`, or a pattern synonym definition.
    fn pat_syn(&mut self) -> Result<Decl> {
        let pos = self.pos();
        self.bump();
        let start = self.idx;
        let mut names = Vec::new();
        while let Ok(name) = self.conid() {
            names.push(name);
            if !self.eat_special(',') {
                break;
            }
        }
        if !names.is_empty() && self.eat(&TokenKind::ReservedOp(ReservedOp::DoubleColon)) {
            let ty = self.ty()?;
            return Ok(Decl::PatSynSig { pos, names, ty });
        }
        self.idx = start;
        let lhs = if let Ok(name) = self.conid() {
            if self.eat_special('{') {
                let mut fields = Vec::new();
                if !self.at_special('}') {
                    loop {
                        fields.push(self.varid()?);
                        if !self.eat_special(',') {
                            break;
                        }
                    }
                }
                self.expect_special('}')?;
                PatSynLhs::Record { name, fields }
            } else {
                let mut args = Vec::new();
                while let TokenKind::VarId { qual, .. } = self.kind() {
                    if !qual.is_empty() {
                        break;
                    }
                    args.push(self.varid()?);
                }
                PatSynLhs::Prefix { name, args }
            }
        } else {
            let left = self.varid()?;
            let op = match self.kind() {
                TokenKind::ConSym { qual, name } if qual.is_empty() => {
                    let name = name.clone();
                    self.bump();
                    name
                }
                TokenKind::ReservedOp(ReservedOp::Colon) => {
                    self.bump();
                    ":".to_string()
                }
                TokenKind::Special('`') => {
                    self.bump();
                    let name = self.conid()?;
                    self.expect_special('`')?;
                    format!("`{name}`")
                }
                _ => return self.unexpected("a constructor operator"),
            };
            let right = self.varid()?;
            PatSynLhs::Infix { left, op, right }
        };
        let (dir, pat) = if self.eat(&TokenKind::ReservedOp(ReservedOp::Equals)) {
            (PatSynDir::Implicit, self.pat()?)
        } else {
            self.expect(&TokenKind::ReservedOp(ReservedOp::LeftArrow), "`=` or `<-`")?;
            let pat = self.pat()?;
            if self.eat_keyword(Keyword::Where) {
                (PatSynDir::Explicit(self.local_decls()?), pat)
            } else {
                (PatSynDir::Unidirectional, pat)
            }
        };
        Ok(Decl::PatSyn { pos, lhs, dir, pat })
    }

    /// An unqualified constructor name.
    fn conid(&mut self) -> Result<String> {
        match self.kind() {
            TokenKind::ConId { qual, name } if qual.is_empty() => {
                let name = name.clone();
                self.bump();
                Ok(name)
            }
            _ => self.unexpected("a constructor name"),
        }
    }

    fn fixity(&mut self) -> Result<Decl> {
        let pos = self.pos();
        let assoc = match self.kind() {
            TokenKind::Keyword(Keyword::Infixl) => Assoc::Left,
            TokenKind::Keyword(Keyword::Infixr) => Assoc::Right,
            _ => Assoc::None,
        };
        self.bump();
        let prec = match self.kind() {
            TokenKind::Integer(s) => {
                let s = s.clone();
                self.bump();
                Some(s)
            }
            _ => None,
        };
        let mut ops = Vec::new();
        loop {
            ops.push(self.operator_name()?);
            if !self.eat_special(',') {
                break;
            }
        }
        Ok(Decl::Fixity {
            pos,
            assoc,
            prec,
            ops,
        })
    }

    /// An operator as it appears in a fixity declaration: a symbol, or a
    /// name in backquotes.
    fn operator_name(&mut self) -> Result<String> {
        match self.kind() {
            TokenKind::VarSym { qual, name } | TokenKind::ConSym { qual, name }
                if qual.is_empty() =>
            {
                let name = name.clone();
                self.bump();
                Ok(name)
            }
            TokenKind::ReservedOp(ReservedOp::Colon) => {
                self.bump();
                Ok(":".to_string())
            }
            TokenKind::Special('`') => {
                self.bump();
                let name = match self.kind() {
                    TokenKind::VarId { qual, name } | TokenKind::ConId { qual, name }
                        if qual.is_empty() =>
                    {
                        name.clone()
                    }
                    _ => return self.unexpected("a name in backquotes"),
                };
                self.bump();
                self.expect_special('`')?;
                Ok(format!("`{name}`"))
            }
            _ => self.unexpected("an operator"),
        }
    }

    // --- data / newtype ---------------------------------------------------

    fn data_decl(&mut self, newtype: bool) -> Result<Decl> {
        let pos = self.pos();
        self.bump();
        // A type operator in prefix form: `data (:+:) f g p`.
        let name = match self.parenthesized_name() {
            Some(name) if name.qual.is_empty() => name.name,
            Some(_) => return self.unexpected("an unqualified type constructor"),
            None => self.conid()?,
        };
        let vars = self.ty_var_binds()?;
        let kind = if self.eat(&TokenKind::ReservedOp(ReservedOp::DoubleColon)) {
            Some(self.ty()?)
        } else {
            None
        };
        let mut cons = Vec::new();
        if self.eat(&TokenKind::ReservedOp(ReservedOp::Equals)) {
            loop {
                cons.push(self.con_decl()?);
                if !self.eat(&TokenKind::ReservedOp(ReservedOp::Bar)) {
                    break;
                }
            }
        }
        let deriving = self.deriving_clauses()?;
        Ok(Decl::Data {
            pos,
            newtype,
            name,
            vars,
            kind,
            cons,
            deriving,
        })
    }

    fn con_decl(&mut self) -> Result<ConDecl> {
        let pos = self.pos();
        let forall = if self.at_varid("forall") {
            self.bump();
            let binders = self.ty_var_binds()?;
            self.expect_varsym(".")?;
            binders
        } else {
            Vec::new()
        };
        let ctx = if self.con_has_context() {
            let ctx = self.btype()?;
            self.expect(&TokenKind::ReservedOp(ReservedOp::DoubleArrow), "`=>`")?;
            Some(ctx)
        } else {
            None
        };
        let body = if self.con_is_infix() {
            let left = self.infix_con_arg()?;
            let op = match self.kind() {
                TokenKind::ConSym { qual, name } if qual.is_empty() => {
                    let name = name.clone();
                    self.bump();
                    name
                }
                TokenKind::ReservedOp(ReservedOp::Colon) => {
                    self.bump();
                    ":".to_string()
                }
                TokenKind::Special('`') => {
                    self.bump();
                    let name = self.conid()?;
                    self.expect_special('`')?;
                    format!("`{name}`")
                }
                _ => return self.unexpected("a constructor operator"),
            };
            let right = self.infix_con_arg()?;
            ConBody::Infix { left, op, right }
        } else if self.at_special('[')
            && matches!(
                self.tokens.get(self.idx + 1).map(|t| &t.kind),
                Some(TokenKind::Special(']'))
            )
        {
            // The empty list constructor, in the declaration of the list
            // type: `data List a = [] | a : List a`.
            self.idx += 2;
            ConBody::Prefix {
                name: "[]".to_string(),
                args: Vec::new(),
            }
        } else {
            let name = self.conid()?;
            if self.at_special('{') {
                let fields = self.record_fields()?;
                ConBody::Record { name, fields }
            } else {
                let mut args = Vec::new();
                while self.at_bang_type_start() {
                    args.push(self.bang_type(false)?);
                }
                ConBody::Prefix { name, args }
            }
        };
        Ok(ConDecl {
            pos,
            forall,
            ctx,
            body,
        })
    }

    /// An argument of an infix constructor: a type application such as
    /// `List a`, or a strictness mark and an atype.
    fn infix_con_arg(&mut self) -> Result<BangType> {
        if self.at_varsym("!") || self.at(&TokenKind::ReservedOp(ReservedOp::Tilde)) {
            return self.bang_type(false);
        }
        Ok(BangType {
            strict: None,
            ty: self.optype()?,
        })
    }

    fn at_bang_type_start(&self) -> bool {
        self.at_atype_start()
            || self.at_varsym("!")
            || self.at(&TokenKind::ReservedOp(ReservedOp::Tilde))
    }

    /// Whether a `=>` follows before the end of this constructor, at
    /// bracket depth zero.
    fn con_has_context(&self) -> bool {
        self.scan_constructor(|k| matches!(k, TokenKind::ReservedOp(ReservedOp::DoubleArrow)))
    }

    /// Whether a constructor operator follows before the end of this
    /// constructor, at bracket depth zero.
    fn con_is_infix(&self) -> bool {
        let mut i = self.idx;
        let mut depth = 0u32;
        while let Some(tok) = self.tokens.get(i) {
            match &tok.kind {
                TokenKind::Special('(' | '[' | '{') => depth += 1,
                TokenKind::Special(')' | ']' | '}') => {
                    if depth == 0 {
                        return false;
                    }
                    depth -= 1;
                }
                TokenKind::ConSym { .. } | TokenKind::ReservedOp(ReservedOp::Colon)
                    if depth == 0 =>
                {
                    return true;
                }
                TokenKind::Special('`') if depth == 0 => {
                    return matches!(
                        self.tokens.get(i + 1).map(|t| &t.kind),
                        Some(TokenKind::ConId { .. })
                    );
                }
                k if depth == 0 && Self::ends_constructor(k) => return false,
                _ => {}
            }
            i += 1;
        }
        false
    }

    fn scan_constructor(&self, target: impl Fn(&TokenKind) -> bool) -> bool {
        let mut i = self.idx;
        let mut depth = 0u32;
        while let Some(tok) = self.tokens.get(i) {
            match &tok.kind {
                TokenKind::Special('(' | '[' | '{') => depth += 1,
                TokenKind::Special(')' | ']' | '}') => {
                    if depth == 0 {
                        return false;
                    }
                    depth -= 1;
                }
                k if depth == 0 && target(k) => return true,
                k if depth == 0 && Self::ends_constructor(k) => return false,
                _ => {}
            }
            i += 1;
        }
        false
    }

    fn ends_constructor(k: &TokenKind) -> bool {
        matches!(
            k,
            TokenKind::ReservedOp(ReservedOp::Bar | ReservedOp::Equals)
                | TokenKind::Keyword(Keyword::Deriving | Keyword::Where)
                | TokenKind::Special(';')
                | TokenKind::VSemi
                | TokenKind::VClose
                | TokenKind::Eof
        )
    }

    fn record_fields(&mut self) -> Result<Vec<Field>> {
        self.expect_special('{')?;
        let mut fields = Vec::new();
        if self.eat_special('}') {
            return Ok(fields);
        }
        loop {
            let mut names = Vec::new();
            loop {
                names.push(self.sig_name()?);
                if !self.eat_special(',') {
                    break;
                }
            }
            self.expect(&TokenKind::ReservedOp(ReservedOp::DoubleColon), "`::`")?;
            let ty: BangType = self.bang_type(true)?;
            fields.push(Field { names, ty });
            if !self.eat_special(',') {
                self.expect_special('}')?;
                return Ok(fields);
            }
        }
    }

    fn deriving_clauses(&mut self) -> Result<Vec<Deriving>> {
        let mut clauses = Vec::new();
        while self.eat_keyword(Keyword::Deriving) {
            let strategy = if self.eat_varid("stock") {
                Some(DerivStrategy::Stock)
            } else if self.eat_varid("anyclass") {
                Some(DerivStrategy::Anyclass)
            } else if self.eat_keyword(Keyword::Newtype) {
                Some(DerivStrategy::Newtype)
            } else {
                None
            };
            let mut class_list = Vec::new();
            let parens = self.at_special('(');
            if parens {
                self.bump();
                if !self.eat_special(')') {
                    loop {
                        class_list.push(self.ty()?);
                        if !self.eat_special(',') {
                            self.expect_special(')')?;
                            break;
                        }
                    }
                }
            } else {
                class_list.push(self.atype()?);
            }
            let via = if self.eat_varid("via") {
                Some(self.atype()?)
            } else {
                None
            };
            clauses.push(Deriving {
                strategy,
                classes: class_list,
                parens,
                via,
            });
        }
        Ok(clauses)
    }

    // --- type ---------------------------------------------------------------

    /// `type T a = t`, `type family F a :: k`, `type instance F Int = t`,
    /// and the associated forms in class and instance bodies.
    fn type_decl(&mut self, ctx: DeclContext) -> Result<Decl> {
        let pos = self.pos();
        self.bump();
        let family = self.eat_varid("family") || ctx == DeclContext::Class;
        let instance = self.eat_keyword(Keyword::Instance) || ctx == DeclContext::Instance;
        if instance {
            let lhs = self.btype()?;
            self.expect(&TokenKind::ReservedOp(ReservedOp::Equals), "`=`")?;
            let rhs = self.ty()?;
            return Ok(Decl::TypeInstance { pos, lhs, rhs });
        }
        let name = self.conid()?;
        let vars = self.ty_var_binds()?;
        if family {
            let kind = if self.eat(&TokenKind::ReservedOp(ReservedOp::DoubleColon)) {
                Some(self.ty()?)
            } else {
                None
            };
            return Ok(Decl::TypeFamily {
                pos,
                name,
                vars,
                kind,
            });
        }
        self.expect(&TokenKind::ReservedOp(ReservedOp::Equals), "`=`")?;
        let rhs = self.ty()?;
        Ok(Decl::TypeSyn {
            pos,
            name,
            vars,
            rhs,
        })
    }

    // --- class / instance ---------------------------------------------------

    fn class_decl(&mut self) -> Result<Decl> {
        let pos = self.pos();
        self.bump();
        let ctx = if self.scan_head_for_double_arrow() {
            let ctx = self.btype()?;
            self.expect(&TokenKind::ReservedOp(ReservedOp::DoubleArrow), "`=>`")?;
            Some(ctx)
        } else {
            None
        };
        let name = self.conid()?;
        let vars = self.ty_var_binds()?;
        let body = self.where_decls(DeclContext::Class)?;
        Ok(Decl::Class {
            pos,
            ctx,
            name,
            vars,
            body,
        })
    }

    /// Whether a `=>` follows before `where` or the end of the
    /// declaration, at bracket depth zero.
    fn scan_head_for_double_arrow(&self) -> bool {
        self.scan_constructor(|k| matches!(k, TokenKind::ReservedOp(ReservedOp::DoubleArrow)))
    }

    fn instance_decl(&mut self) -> Result<Decl> {
        let pos = self.pos();
        self.bump();
        let head: Type = self.ty()?;
        let body = self.where_decls(DeclContext::Instance)?;
        Ok(Decl::Instance { pos, head, body })
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::*;
    use crate::parse;
    use crate::Pos;

    fn decls(src: &str) -> Vec<Decl> {
        parse(&format!("module M where\n{src}\n")).unwrap().decls
    }

    fn con(name: &str) -> Type {
        Type::Con(QName::unqualified(name))
    }

    #[test]
    fn signatures_and_fixity() {
        let d = decls("f, (+) :: a -> Int\ninfixl 6 +, `plus`\ninfix -");
        assert_eq!(
            d[0],
            Decl::TypeSig {
                pos: Pos { line: 2, col: 1 },
                names: vec!["f".into(), "+".into()],
                ty: Type::Fun(Box::new(Type::Var("a".into())), Box::new(con("Int"))),
            }
        );
        assert_eq!(
            d[1],
            Decl::Fixity {
                pos: Pos { line: 3, col: 1 },
                assoc: Assoc::Left,
                prec: Some("6".into()),
                ops: vec!["+".into(), "`plus`".into()],
            }
        );
        assert!(matches!(
            &d[2],
            Decl::Fixity {
                assoc: Assoc::None,
                prec: None,
                ..
            }
        ));
    }

    #[test]
    fn list_and_operator_declarations() {
        let d = decls("infixr 5 :\ndata List a = [] | a : List a\ndata (:+:) f g p = L1 (f p)");
        assert!(matches!(&d[0], Decl::Fixity { ops, .. } if ops == &[":"]));
        let Decl::Data { cons, .. } = &d[1] else {
            panic!("{:?}", d[1]);
        };
        assert!(
            matches!(&cons[0].body, ConBody::Prefix { name, args } if name == "[]" && args.is_empty())
        );
        assert!(matches!(&cons[1].body, ConBody::Infix { op, .. } if op == ":"));
        assert!(matches!(&d[2], Decl::Data { name, vars, .. } if name == ":+:" && vars.len() == 3));
    }

    #[test]
    fn data_declarations() {
        let d = decls(
            "data T a = C !Int (Maybe a) | a :+ a | R { f, g :: {-# UNPACK #-} !a, h :: Int }\n  deriving (Show, Eq)\n  deriving stock Ord\nnewtype N = N Int deriving newtype (Num)",
        );
        let Decl::Data { cons, deriving, .. } = &d[0] else {
            panic!("{:?}", d[0]);
        };
        assert_eq!(cons.len(), 3);
        assert!(
            matches!(&cons[0].body, ConBody::Prefix { name, args } if name == "C" && args.len() == 2 && args[0].strict == Some(true))
        );
        assert!(matches!(&cons[1].body, ConBody::Infix { op, .. } if op == ":+"));
        let ConBody::Record { fields, .. } = &cons[2].body else {
            panic!()
        };
        assert_eq!(fields[0].names, vec!["f", "g"]);
        assert_eq!(fields[0].ty.strict, Some(true));
        assert_eq!(deriving.len(), 2);
        assert!(deriving[0].parens && deriving[0].classes.len() == 2);
        assert_eq!(deriving[1].strategy, Some(DerivStrategy::Stock));
        assert!(!deriving[1].parens);
        assert!(
            matches!(&d[1], Decl::Data { newtype: true, deriving, .. } if deriving[0].strategy == Some(DerivStrategy::Newtype))
        );
    }

    #[test]
    fn existential_constructor() {
        let d = decls("data T = forall a. Show a => C a");
        let Decl::Data { cons, .. } = &d[0] else {
            panic!()
        };
        assert_eq!(cons[0].forall.len(), 1);
        assert!(cons[0].ctx.is_some());
    }

    #[test]
    fn class_and_instance() {
        let d = decls(
            "class Eq a => C a where\n  m :: a -> Int\n  default m :: Show a => a -> Int\n  {-# MINIMAL m #-}\ninstance C Int where\ninstance forall a. Show a => C [a]",
        );
        let Decl::Class {
            ctx, name, body, ..
        } = &d[0]
        else {
            panic!()
        };
        assert!(ctx.is_some());
        assert_eq!(name, "C");
        assert_eq!(body.len(), 2);
        assert!(matches!(&body[1], Decl::DefaultSig { name, .. } if name == "m"));
        assert!(matches!(&d[1], Decl::Instance { body, .. } if body.is_empty()));
        assert!(matches!(
            &d[2],
            Decl::Instance {
                head: Type::Forall(..),
                ..
            }
        ));
    }

    #[test]
    fn types() {
        let d = decls("type F = forall a. (Eq a, Show a) => [a] -> (a, Maybe a) -> M.T @k '[a] (t :: k) _ \"s\" (a `E` b) (a ~ b)");
        let Decl::TypeSyn { rhs, .. } = &d[0] else {
            panic!()
        };
        let Type::Forall(binders, inner) = rhs else {
            panic!()
        };
        assert_eq!(binders[0].name, "a");
        let Type::Qual(ctx, _) = &**inner else {
            panic!()
        };
        assert!(matches!(&**ctx, Type::Tuple(items) if items.len() == 2));
    }

    #[test]
    fn bindings() {
        let d = decls(
            "f x (Just y) = x\n\
             (<+>) a b = a\n\
             x `plus` y = x\n\
             (a, b) = (1, 2)\n\
             !z = 3\n\
             g x\n  | x > 0, Just y <- h x = y\n  | otherwise = 0\n  where h = Just\n",
        );
        assert!(
            matches!(&d[0], Decl::Bind { lhs: Lhs::Fun { name, args }, .. } if name == "f" && args.len() == 2)
        );
        assert!(
            matches!(&d[1], Decl::Bind { lhs: Lhs::Fun { name, args }, .. } if name == "<+>" && args.len() == 2)
        );
        assert!(matches!(&d[2], Decl::Bind { lhs: Lhs::Infix { op, .. }, .. } if op == "`plus`"));
        assert!(matches!(
            &d[3],
            Decl::Bind {
                lhs: Lhs::Pat(Pat::Tuple(_)),
                ..
            }
        ));
        assert!(matches!(
            &d[4],
            Decl::Bind {
                lhs: Lhs::Pat(Pat::Bang(_)),
                ..
            }
        ));
        let Decl::Bind { rhs, .. } = &d[5] else {
            panic!()
        };
        let RhsBody::Guarded(guards) = &rhs.body else {
            panic!()
        };
        assert_eq!(guards.len(), 2);
        assert_eq!(guards[0].guards.len(), 2);
        assert_eq!(rhs.wheres.len(), 1);
    }

    #[test]
    fn pattern_synonyms() {
        let d = decls(
            "pattern P x = Just x\n\
             pattern x :< xs <- (uncons -> Just (x, xs))\n\
             pattern Q {a, b} <- (a, b) where\n  Q a b = (a, b)\n",
        );
        assert!(matches!(
            &d[0],
            Decl::PatSyn {
                lhs: PatSynLhs::Prefix { .. },
                dir: PatSynDir::Implicit,
                ..
            }
        ));
        assert!(
            matches!(&d[1], Decl::PatSyn { lhs: PatSynLhs::Infix { op, .. }, dir: PatSynDir::Unidirectional, pat: Pat::Paren(_), .. } if op == ":<")
        );
        assert!(
            matches!(&d[2], Decl::PatSyn { lhs: PatSynLhs::Record { fields, .. }, dir: PatSynDir::Explicit(ds), .. } if fields.len() == 2 && ds.len() == 1)
        );
    }
}
