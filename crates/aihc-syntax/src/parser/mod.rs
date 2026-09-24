//! The parser: tokens (after layout) to a syntax tree.
//!
//! This is a recursive descent parser over the token stream of
//! [`crate::tokenize`]. This module handles the module header, the export
//! list, the imports and the shared machinery. The `types`, `decls`,
//! `pats` and `exprs` modules handle the rest. Nothing is skipped: a
//! construct the parser does not know is an error.
//!
//! The parser completes the layout rule. The `layout` pass inserts the
//! virtual braces and semicolons that indentation determines. The report
//! adds one rule it cannot apply: an implicit block also closes where the
//! next token would be a parse error. [`Parser::block`] applies it: when
//! an item of an implicit block ends and the next token is neither `;`
//! nor the block's `}`, or when an item cannot start, the block closes
//! there and its virtual `}` is removed from the stream.

use crate::ast::{Export, Import, ImportItem, Module, QName, Qualified, Subs};
use crate::token::{Keyword, Pos, ReservedOp, Token, TokenKind};
use std::fmt;

/// A parse error with the position of the token the parser stopped at.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ParseError {
    pub pos: Pos,
    pub message: String,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.pos, self.message)
    }
}

impl std::error::Error for ParseError {}

type Result<T> = std::result::Result<T, ParseError>;

/// Parse a module from its tokens. The tokens must come from
/// [`crate::tokenize`], so the layout rule has run and the stream ends
/// with `Eof`.
pub fn parse_module(tokens: Vec<Token>) -> Result<Module> {
    let mut p = Parser { tokens, idx: 0 };
    let module = p.module()?;
    p.expect_eof()?;
    Ok(module)
}

pub(crate) struct Parser {
    tokens: Vec<Token>,
    idx: usize,
}

impl Parser {
    // --- Token access -----------------------------------------------------

    fn peek(&self) -> &Token {
        // The stream ends with `Eof`, and the parser never moves past it.
        &self.tokens[self.idx.min(self.tokens.len() - 1)]
    }

    fn kind(&self) -> &TokenKind {
        &self.peek().kind
    }

    fn pos(&self) -> Pos {
        self.peek().pos
    }

    fn bump(&mut self) -> &Token {
        let idx = self.idx.min(self.tokens.len() - 1);
        if self.tokens[idx].kind != TokenKind::Eof {
            self.idx += 1;
        }
        &self.tokens[idx]
    }

    fn at(&self, kind: &TokenKind) -> bool {
        self.kind() == kind
    }

    fn at_keyword(&self, k: Keyword) -> bool {
        matches!(self.kind(), TokenKind::Keyword(k2) if *k2 == k)
    }

    fn at_special(&self, c: char) -> bool {
        matches!(self.kind(), TokenKind::Special(c2) if *c2 == c)
    }

    /// An unqualified variable identifier with the given spelling. Used
    /// for the words that are not reserved, such as `qualified`.
    fn at_varid(&self, name: &str) -> bool {
        matches!(self.kind(), TokenKind::VarId { qual, name: n } if qual.is_empty() && n == name)
    }

    fn eat(&mut self, kind: &TokenKind) -> bool {
        if self.at(kind) {
            self.bump();
            true
        } else {
            false
        }
    }

    fn eat_keyword(&mut self, k: Keyword) -> bool {
        if self.at_keyword(k) {
            self.bump();
            true
        } else {
            false
        }
    }

    fn eat_special(&mut self, c: char) -> bool {
        if self.at_special(c) {
            self.bump();
            true
        } else {
            false
        }
    }

    fn eat_varid(&mut self, name: &str) -> bool {
        if self.at_varid(name) {
            self.bump();
            true
        } else {
            false
        }
    }

    fn error<T>(&self, message: impl Into<String>) -> Result<T> {
        Err(ParseError {
            pos: self.pos(),
            message: message.into(),
        })
    }

    fn unexpected<T>(&self, expected: &str) -> Result<T> {
        self.error(format!("expected {expected}, found `{}`", self.kind()))
    }

    fn expect(&mut self, kind: &TokenKind, what: &str) -> Result<()> {
        if self.at(kind) {
            self.bump();
            Ok(())
        } else {
            self.unexpected(what)
        }
    }

    fn expect_keyword(&mut self, k: Keyword) -> Result<()> {
        self.expect(&TokenKind::Keyword(k), &format!("`{}`", k.as_str()))?;
        Ok(())
    }

    fn expect_special(&mut self, c: char) -> Result<()> {
        self.expect(&TokenKind::Special(c), &format!("`{c}`"))?;
        Ok(())
    }

    fn expect_eof(&self) -> Result<()> {
        if self.at(&TokenKind::Eof) {
            Ok(())
        } else {
            self.unexpected("end of input")
        }
    }

    // --- Blocks -----------------------------------------------------------

    /// `{` or a virtual `{`. Returns whether the brace was explicit.
    fn open_block(&mut self) -> Result<bool> {
        if self.eat_special('{') {
            Ok(true)
        } else if self.eat(&TokenKind::VOpen) {
            Ok(false)
        } else {
            self.unexpected("`{`")
        }
    }

    fn at_close_block(&self, explicit: bool) -> bool {
        if explicit {
            self.at_special('}')
        } else {
            self.at(&TokenKind::VClose)
        }
    }

    /// Close a block. An implicit block closes at any token: that is the
    /// parse-error rule of the layout algorithm. Its virtual `}` further
    /// on in the stream goes away, so the enclosing blocks stay balanced.
    fn close_block(&mut self, explicit: bool) -> Result<()> {
        if self.at_close_block(explicit) {
            self.bump();
            return Ok(());
        }
        if explicit {
            return self.unexpected("`}`");
        }
        // The block's own virtual `;` tokens further on are stale too:
        // a layout-aware lexer stops producing them once the block is closed.
        let mut depth = 0u32;
        let mut i = self.idx;
        while i < self.tokens.len() {
            match self.tokens[i].kind {
                TokenKind::VOpen => depth += 1,
                TokenKind::VSemi if depth == 0 => {
                    self.tokens.remove(i);
                    continue;
                }
                TokenKind::VClose if depth == 0 => {
                    self.tokens.remove(i);
                    return Ok(());
                }
                TokenKind::VClose => depth -= 1,
                _ => {}
            }
            i += 1;
        }
        self.unexpected("`}`")
    }

    /// A block of items: `{` or a virtual `{`, items separated by `;`,
    /// then `}`. See the module documentation for how an implicit block
    /// closes early.
    fn block<T>(&mut self, mut item: impl FnMut(&mut Self) -> Result<T>) -> Result<Vec<T>> {
        let explicit = self.open_block()?;
        let mut items = Vec::new();
        self.semis();
        while !self.at_close_block(explicit) {
            let start = self.idx;
            let pos = self.pos();
            match item(self) {
                Ok(x) => items.push(x),
                // An item that cannot start closes an implicit block.
                Err(e) if !explicit && e.pos == pos => {
                    self.idx = start;
                    break;
                }
                Err(e) => return Err(e),
            }
            if !self.semis() {
                break;
            }
        }
        self.close_block(explicit)?;
        Ok(items)
    }

    /// One or more `;` or virtual `;`. Returns whether any was found.
    fn semis(&mut self) -> bool {
        let mut found = false;
        while self.eat_special(';') || self.eat(&TokenKind::VSemi) {
            found = true;
        }
        found
    }

    // --- Names ------------------------------------------------------------

    fn modid(&mut self) -> Result<String> {
        match self.kind() {
            TokenKind::ConId { qual, name } => {
                let modid = if qual.is_empty() {
                    name.clone()
                } else {
                    format!("{qual}.{name}")
                };
                self.bump();
                Ok(modid)
            }
            _ => self.unexpected("a module name"),
        }
    }

    /// `(varsym)` or `(consym)`, possibly qualified. Returns `None` and
    /// consumes nothing when the parenthesis starts something else.
    fn parenthesized_name(&mut self) -> Option<QName> {
        if !self.at_special('(') {
            return None;
        }
        let name = match &self.tokens.get(self.idx + 1).map(|t| &t.kind) {
            Some(TokenKind::VarSym { qual, name } | TokenKind::ConSym { qual, name }) => QName {
                qual: qual.clone(),
                name: name.clone(),
            },
            Some(TokenKind::ReservedOp(op @ (ReservedOp::Colon | ReservedOp::Tilde))) => {
                QName::unqualified(op.as_str())
            }
            _ => return None,
        };
        if !matches!(
            self.tokens.get(self.idx + 2).map(|t| &t.kind),
            Some(TokenKind::Special(')'))
        ) {
            return None;
        }
        self.idx += 3;
        Some(name)
    }

    // --- Module -----------------------------------------------------------

    fn module(&mut self) -> Result<Module> {
        let pos = self.pos();
        let mut pragmas = Vec::new();
        while let TokenKind::Pragma(text) = self.kind() {
            pragmas.push(text.trim().to_string());
            self.bump();
        }
        let (name, exports) = if self.eat_keyword(Keyword::Module) {
            let name = self.modid()?;
            let exports = if self.at_special('(') {
                Some(self.export_list()?)
            } else {
                None
            };
            self.expect_keyword(Keyword::Where)?;
            (name, exports)
        } else {
            ("Main".to_string(), None)
        };
        let mut imports = Vec::new();
        let mut decls_started = false;
        let decls = self.block(|p| {
            if p.at_keyword(Keyword::Import) && !decls_started {
                imports.push(p.import()?);
                return Ok(None);
            }
            decls_started = true;
            p.decl(decls::DeclContext::TopLevel).map(Some)
        })?;
        let decls = decls.into_iter().flatten().collect();
        Ok(Module {
            pos,
            pragmas,
            name,
            exports,
            imports,
            decls,
        })
    }

    fn export_list(&mut self) -> Result<Vec<Export>> {
        self.expect_special('(')?;
        let mut items = Vec::new();
        loop {
            // Trailing and repeated commas are permitted.
            while self.eat_special(',') {}
            if self.eat_special(')') {
                return Ok(items);
            }
            items.push(self.export()?);
            if !self.at_special(',') {
                self.expect_special(')')?;
                return Ok(items);
            }
        }
    }

    fn export(&mut self) -> Result<Export> {
        if self.eat_keyword(Keyword::Module) {
            return Ok(Export::Module(self.modid()?));
        }
        if self.at_varid("pattern") && self.next_starts_name() {
            self.bump();
            return Ok(Export::Pattern(self.qname()?));
        }
        let explicit_type = self.at_keyword(Keyword::Type) && self.next_starts_name();
        if explicit_type {
            self.bump();
        }
        match self.kind() {
            TokenKind::VarId { .. } if !explicit_type => Ok(Export::Var(self.qname()?)),
            TokenKind::ConId { .. } | TokenKind::Special('(') => {
                let name = self.qname()?;
                let is_var = !explicit_type
                    && name
                        .name
                        .starts_with(|c: char| !c.is_uppercase() && c != ':');
                if is_var {
                    return Ok(Export::Var(name));
                }
                Ok(Export::Thing {
                    name,
                    subs: self.subs()?,
                    explicit_type,
                })
            }
            _ => self.unexpected("an export item"),
        }
    }

    /// The token after the current one starts a name, so the current
    /// `pattern` or `type` is a namespace keyword and not a name itself.
    fn next_starts_name(&self) -> bool {
        matches!(
            self.tokens.get(self.idx + 1).map(|t| &t.kind),
            Some(TokenKind::VarId { .. } | TokenKind::ConId { .. } | TokenKind::Special('('))
        )
    }

    /// A possibly qualified variable, constructor or parenthesized
    /// operator.
    fn qname(&mut self) -> Result<QName> {
        if let Some(name) = self.parenthesized_name() {
            return Ok(name);
        }
        match self.kind() {
            TokenKind::VarId { qual, name } | TokenKind::ConId { qual, name } => {
                let qname = QName {
                    qual: qual.clone(),
                    name: name.clone(),
                };
                self.bump();
                Ok(qname)
            }
            _ => self.unexpected("a name"),
        }
    }

    /// The optional `(..)`, `(a, B, (+))` or `(.., P)` after a type or
    /// class.
    fn subs(&mut self) -> Result<Subs> {
        if !self.at_special('(') {
            return Ok(Subs::None);
        }
        self.bump();
        let all = self.eat(&TokenKind::ReservedOp(ReservedOp::DotDot));
        let mut names = Vec::new();
        loop {
            while self.eat_special(',') {}
            if self.eat_special(')') {
                break;
            }
            names.push(self.qname()?.name);
            if !self.at_special(',') {
                self.expect_special(')')?;
                break;
            }
        }
        Ok(match (all, names.is_empty()) {
            (true, true) => Subs::All,
            (true, false) => Subs::AllAnd(names),
            (false, _) => Subs::Some(names),
        })
    }

    fn import(&mut self) -> Result<Import> {
        let pos = self.pos();
        self.expect_keyword(Keyword::Import)?;
        let mut qualified = if self.eat_varid("qualified") {
            Qualified::Pre
        } else {
            Qualified::No
        };
        let package = match self.kind() {
            TokenKind::String { value, .. } => {
                let s = value.clone();
                self.bump();
                Some(s)
            }
            _ => None,
        };
        let module = self.modid()?;
        // `ImportQualifiedPost`
        if self.eat_varid("qualified") {
            qualified = Qualified::Post;
        }
        let alias = if self.eat_varid("as") {
            Some(self.modid()?)
        } else {
            None
        };
        let hiding = self.eat_varid("hiding");
        let items = if self.at_special('(') {
            Some(self.import_list()?)
        } else {
            if hiding {
                return self.unexpected("`(` after `hiding`");
            }
            None
        };
        Ok(Import {
            pos,
            module,
            qualified,
            package,
            alias,
            hiding,
            items,
        })
    }

    fn import_list(&mut self) -> Result<Vec<ImportItem>> {
        self.expect_special('(')?;
        let mut items = Vec::new();
        loop {
            while self.eat_special(',') {}
            if self.eat_special(')') {
                return Ok(items);
            }
            items.push(self.import_item()?);
            if !self.at_special(',') {
                self.expect_special(')')?;
                return Ok(items);
            }
        }
    }

    fn import_item(&mut self) -> Result<ImportItem> {
        if self.at_varid("pattern") && self.next_starts_name() {
            self.bump();
            return Ok(ImportItem::Pattern(self.qname()?.name));
        }
        let explicit_type = self.at_keyword(Keyword::Type) && self.next_starts_name();
        if explicit_type {
            self.bump();
        }
        match self.kind() {
            TokenKind::VarId { .. } if !explicit_type => Ok(ImportItem::Var(self.qname()?.name)),
            TokenKind::ConId { .. } | TokenKind::Special('(') => {
                let name = self.qname()?.name;
                let is_var =
                    !explicit_type && name.starts_with(|c: char| !c.is_uppercase() && c != ':');
                if is_var {
                    return Ok(ImportItem::Var(name));
                }
                Ok(ImportItem::Thing {
                    name,
                    subs: self.subs()?,
                    explicit_type,
                })
            }
            _ => self.unexpected("an import item"),
        }
    }
}

mod decls;
mod exprs;
mod pats;
mod types;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tokenize;

    fn parse(src: &str) -> Module {
        parse_module(tokenize(src).unwrap()).unwrap()
    }

    fn import(module: &str) -> Import {
        Import {
            pos: Pos { line: 1, col: 1 },
            module: module.into(),
            qualified: Qualified::No,
            package: None,
            alias: None,
            hiding: false,
            items: None,
        }
    }

    #[test]
    fn header() {
        let m = parse("{-# LANGUAGE CPP #-}\nmodule Data.Foo.Bar (a, B, C(..), D(E, f), (+), module M, pattern P, type (:+), S(.., P)) where\n");
        assert_eq!(m.pragmas, vec!["LANGUAGE CPP"]);
        assert_eq!(m.name, "Data.Foo.Bar");
        assert_eq!(
            m.exports,
            Some(vec![
                Export::Var(QName::unqualified("a")),
                Export::Thing {
                    name: QName::unqualified("B"),
                    subs: Subs::None,
                    explicit_type: false
                },
                Export::Thing {
                    name: QName::unqualified("C"),
                    subs: Subs::All,
                    explicit_type: false
                },
                Export::Thing {
                    name: QName::unqualified("D"),
                    subs: Subs::Some(vec!["E".into(), "f".into()]),
                    explicit_type: false
                },
                Export::Var(QName::unqualified("+")),
                Export::Module("M".into()),
                Export::Pattern(QName::unqualified("P")),
                Export::Thing {
                    name: QName::unqualified(":+"),
                    subs: Subs::None,
                    explicit_type: true
                },
                Export::Thing {
                    name: QName::unqualified("S"),
                    subs: Subs::AllAnd(vec!["P".into()]),
                    explicit_type: false
                },
            ])
        );
        assert!(m.imports.is_empty());
        assert!(m.decls.is_empty());
    }

    #[test]
    fn no_header() {
        let m = parse("import A\nx :: Int\n");
        assert_eq!(m.name, "Main");
        assert_eq!(m.exports, None);
        assert_eq!(m.imports, vec![import("A")]);
        assert_eq!(m.decls.len(), 1);
    }

    #[test]
    fn empty_export_list_and_trailing_comma() {
        assert_eq!(parse("module M () where").exports, Some(vec![]));
        assert_eq!(
            parse("module M (\n  a,\n  ) where").exports,
            Some(vec![Export::Var(QName::unqualified("a"))])
        );
    }

    #[test]
    fn imports() {
        let src = "module M where\n\
                   import A\n\
                   import qualified B as C\n\
                   import D qualified as E\n\
                   import F (x, Y(..), Z(a, B), (<|>), pattern P, type (+))\n\
                   import G hiding (null)\n\
                   import {-# SOURCE #-} H\n\
                   import \"base\" Prelude\n";
        let m = parse(src);
        let at = |line, i: Import| Import {
            pos: Pos { line, col: 1 },
            ..i
        };
        assert_eq!(
            m.imports,
            vec![
                at(2, import("A")),
                at(
                    3,
                    Import {
                        qualified: Qualified::Pre,
                        alias: Some("C".into()),
                        ..import("B")
                    }
                ),
                at(
                    4,
                    Import {
                        qualified: Qualified::Post,
                        alias: Some("E".into()),
                        ..import("D")
                    }
                ),
                at(
                    5,
                    Import {
                        items: Some(vec![
                            ImportItem::Var("x".into()),
                            ImportItem::Thing {
                                name: "Y".into(),
                                subs: Subs::All,
                                explicit_type: false
                            },
                            ImportItem::Thing {
                                name: "Z".into(),
                                subs: Subs::Some(vec!["a".into(), "B".into()]),
                                explicit_type: false
                            },
                            ImportItem::Var("<|>".into()),
                            ImportItem::Pattern("P".into()),
                            ImportItem::Thing {
                                name: "+".into(),
                                subs: Subs::None,
                                explicit_type: true
                            },
                        ]),
                        ..import("F")
                    }
                ),
                at(
                    6,
                    Import {
                        hiding: true,
                        items: Some(vec![ImportItem::Var("null".into())]),
                        ..import("G")
                    }
                ),
                at(7, import("H")),
                at(
                    8,
                    Import {
                        package: Some("base".into()),
                        ..import("Prelude")
                    }
                ),
            ]
        );
    }

    #[test]
    fn names_that_are_not_keywords() {
        // `as`, `hiding`, `qualified`, `pattern` and `type` used as
        // variables in an import list.
        let m = parse("import A (as, hiding, qualified, pattern)");
        assert_eq!(
            m.imports[0].items,
            Some(vec![
                ImportItem::Var("as".into()),
                ImportItem::Var("hiding".into()),
                ImportItem::Var("qualified".into()),
                ImportItem::Var("pattern".into()),
            ])
        );
        // `type` alone is a keyword, so this is an error.
        assert!(parse_module(tokenize("import B (type)").unwrap()).is_err());
    }

    #[test]
    fn explicit_braces() {
        let m = parse("module M where { import A; import B; x :: Int; y :: Int }");
        assert_eq!(m.imports.len(), 2);
        assert_eq!(m.decls.len(), 2);
    }

    #[test]
    fn errors_have_positions() {
        let err = parse_module(tokenize("module where").unwrap()).unwrap_err();
        assert_eq!(err.pos, Pos { line: 1, col: 8 });
        let err =
            parse_module(tokenize("module M where\nimport A hiding\nx = 1").unwrap()).unwrap_err();
        assert_eq!(err.pos.line, 3);
    }
}
