//! Syntax of the Haskell subset that aihc and its vendored dependencies
//! use.
//!
//! The crate has three stages. Each stage takes the output of the one
//! before it:
//!
//! 1. [`lexer`]: source text to tokens.
//! 2. [`layout`]: virtual braces and semicolons from indentation.
//! 3. [`parser`]: tokens to a syntax tree ([`ast`]).

pub mod ast;
pub mod layout;
pub mod lexer;
pub mod parser;
pub mod token;

use std::fmt;

pub use layout::LayoutError;
pub use lexer::{LexError, LexOptions};
pub use parser::ParseError;
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
    Ok(parser::parse_module(&tokenize(src)?)?)
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

    /// Every vendored module must parse. The vendored tree is the target;
    /// a module the parser rejects is a bug in the parser.
    #[test]
    fn vendored_tree_parses() {
        let vendor = Path::new(env!("CARGO_MANIFEST_DIR")).join("../../vendor");
        let mut files = Vec::new();
        haskell_files(&vendor, &mut files);
        assert!(
            !files.is_empty(),
            "no vendored sources under {}",
            vendor.display()
        );
        let mut failures = Vec::new();
        for path in &files {
            let src = std::fs::read_to_string(path).unwrap();
            if let Err(e) = parse(&src) {
                failures.push(format!("{}: {e}", path.display()));
            }
        }
        assert!(failures.is_empty(), "{}", failures.join("\n"));
    }
}
