//! Syntax of the Haskell subset that aihc and its vendored dependencies
//! use.
//!
//! The crate has three stages. Each stage takes the output of the one
//! before it:
//!
//! 1. [`lexer`]: source text to tokens.
//! 2. [`layout`]: virtual braces and semicolons from indentation.
//! 3. [`parser`]: tokens to a syntax tree ([`ast`]).
//!
//! [`print`] goes the other way, from a syntax tree to source text. The
//! boot compiler uses it to check the parser against aihc-parser, the
//! parser aihc itself uses: a module counts as parsed only when
//! aihc-parser reads the printed source as the same module.

pub mod ast;
pub mod layout;
pub mod lexer;
pub mod parser;
pub mod print;
pub mod token;

use std::fmt;

pub use layout::LayoutError;
pub use lexer::{LexError, LexOptions};
pub use parser::ParseError;
pub use print::print_module;
pub use token::{Pos, Token, TokenKind};

/// An error from any syntax stage, with a position.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SyntaxError {
    pub stage: &'static str,
    pub pos: Pos,
    pub message: String,
}

impl fmt::Display for SyntaxError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}: {}", self.pos, self.stage, self.message)
    }
}

impl std::error::Error for SyntaxError {}

impl From<LexError> for SyntaxError {
    fn from(e: LexError) -> SyntaxError {
        SyntaxError {
            stage: "lex",
            pos: e.pos,
            message: e.message,
        }
    }
}

impl From<LayoutError> for SyntaxError {
    fn from(e: LayoutError) -> SyntaxError {
        SyntaxError {
            stage: "layout",
            pos: e.pos,
            message: e.message,
        }
    }
}

impl From<ParseError> for SyntaxError {
    fn from(e: ParseError) -> SyntaxError {
        SyntaxError {
            stage: "parse",
            pos: e.pos,
            message: e.message,
        }
    }
}

/// Lex a source file and apply the layout rule. The lexer options come
/// from the file's `LANGUAGE` pragmas.
pub fn tokenize(src: &str) -> Result<Vec<Token>, SyntaxError> {
    let opts = LexOptions::from_source(src);
    let tokens = lexer::lex(src, opts)?;
    Ok(layout::layout(tokens)?)
}

/// Parse a source file into a module.
pub fn parse(src: &str) -> Result<ast::Module, SyntaxError> {
    Ok(parser::parse_module(tokenize(src)?)?)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::{Path, PathBuf};

    fn haskell_files(dir: &Path, out: &mut Vec<PathBuf>) {
        let Ok(entries) = std::fs::read_dir(dir) else {
            return;
        };
        for entry in entries.flatten() {
            let path = entry.path();
            if path.is_dir() {
                haskell_files(&path, out);
            } else if path
                .extension()
                .is_some_and(|e| e == "hs" || e == "hs-boot")
            {
                out.push(path);
            }
        }
    }

    fn at(line: u32, col: u32) -> Pos {
        Pos { line, col }
    }

    /// Names carry the span of the name as written, and bracketed types
    /// the span of their brackets.
    #[test]
    fn names_have_spans() {
        use ast::{Decl, Expr, Lhs, Pat, RhsBody, Type};
        let module =
            parse("module M where\n(.) p = (.) x `k` M.y\ng :: [a] -> (a :+: b)\n").unwrap();
        let Decl::Bind { lhs, rhs, .. } = &module.decls[0] else {
            panic!("{:?}", module.decls[0]);
        };
        let Lhs::Fun { name, args } = lhs else {
            panic!("{lhs:?}");
        };
        assert_eq!((name.span.start, name.span.end), (at(2, 1), at(2, 4)));
        let Pat::Var(arg) = &args[0] else {
            panic!("{:?}", args[0]);
        };
        assert_eq!((arg.span.start, arg.span.end), (at(2, 5), at(2, 6)));
        let RhsBody::Plain(Expr::Infix { first, rest }) = &rhs.body else {
            panic!("{rhs:?}");
        };
        let Expr::App(fun, _) = &**first else {
            panic!("{first:?}");
        };
        let Expr::Var(op) = &**fun else {
            panic!("{fun:?}");
        };
        // `(.)` with its parentheses, `k` without its backquotes, and a
        // qualified name with its qualifier.
        assert_eq!((op.span.start, op.span.end), (at(2, 9), at(2, 12)));
        assert_eq!(
            (rest[0].0.span.start, rest[0].0.span.end),
            (at(2, 16), at(2, 17))
        );
        let Expr::Var(y) = &rest[0].1 else {
            panic!("{:?}", rest[0].1);
        };
        assert_eq!((y.span.start, y.span.end), (at(2, 19), at(2, 22)));
        let Decl::TypeSig { ty, .. } = &module.decls[1] else {
            panic!("{:?}", module.decls[1]);
        };
        let Type::Fun(list, paren) = ty else {
            panic!("{ty:?}");
        };
        let (Type::List(_, list), Type::Paren(_, paren)) = (&**list, &**paren) else {
            panic!("{ty:?}");
        };
        assert_eq!((list.start, list.end), (at(3, 6), at(3, 9)));
        assert_eq!((paren.start, paren.end), (at(3, 13), at(3, 22)));
    }

    /// Every vendored module that parses must survive a print and a
    /// second parse with the same tree. Modules that do not parse yet are
    /// skipped here; the tracker counts them.
    #[test]
    fn vendored_tree_roundtrips() {
        let vendor = Path::new(env!("CARGO_MANIFEST_DIR")).join("../../vendor");
        let mut files = Vec::new();
        haskell_files(&vendor, &mut files);
        assert!(
            !files.is_empty(),
            "no vendored sources under {}",
            vendor.display()
        );
        let mut failures = Vec::new();
        let mut parsed = 0;
        for path in &files {
            let src = std::fs::read_to_string(path).unwrap();
            let Ok(module) = parse(&src) else { continue };
            parsed += 1;
            let printed = print_module(&module);
            match parse(&printed) {
                Ok(again) if print_module(&again) == printed => {}
                Ok(_) => failures.push(format!("{}: the second parse differs", path.display())),
                Err(e) => failures.push(format!("{}: printed source: {e}", path.display())),
            }
        }
        eprintln!("{parsed} of {} vendored modules parse", files.len());
        assert!(failures.is_empty(), "{}", failures.join("\n"));
    }
}
