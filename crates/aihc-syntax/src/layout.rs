//! The layout rule: insert virtual braces and semicolons.
//!
//! This is the algorithm `L` from section 10.3 of the Haskell 2010 report,
//! run as one pass over the token stream. The report leaves one rule to the
//! parser: an implicit block closes when the next token would otherwise be
//! a parse error. This pass approximates that rule with three cases that
//! cover the vendored tree:
//!
//! - `in` closes the block a `let` opened.
//! - A closing bracket (`)`, `]` or a non-layout `}`) closes every implicit
//!   block that opened inside the bracket.
//! - A comma closes an implicit block that opened inside a bracket, as in
//!   a list comprehension `let` or a record field. Blocks that `of` opened
//!   stay open, because guards in case alternatives contain commas.
//! - `where` at the column of a block closes the block.
//!
//! The parser can move the rule to where it belongs later; the interface
//! here is one token stream, so the change is local.

use crate::token::{Keyword, Pos, ReservedOp, Token, TokenKind};
use std::fmt;

/// A layout error with the position of the token that caused it.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LayoutError {
    pub pos: Pos,
    pub message: String,
}

impl fmt::Display for LayoutError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.pos, self.message)
    }
}

impl std::error::Error for LayoutError {}

#[derive(Clone, Copy, Debug)]
enum Context {
    /// An implicit block with the column of its first token.
    Implicit {
        col: u32,
        /// The keyword that opened the block.
        opener: Keyword,
        /// The bracket depth when the block opened.
        depth: u32,
    },
    /// A block between explicit braces.
    Explicit { depth: u32 },
}

/// Apply the layout rule. The input must end with an `Eof` token; the
/// output does too.
#[allow(clippy::too_many_lines)]
pub fn layout(tokens: Vec<Token>) -> Result<Vec<Token>, LayoutError> {
    let mut out = Vec::with_capacity(tokens.len() + tokens.len() / 4);
    let mut stack: Vec<Context> = Vec::new();
    let mut depth: u32 = 0;
    // The keyword that opened a block, if the previous token was one.
    let mut pending_open: Option<Keyword> = None;
    let mut prev_line: u32 = 0;
    // The previous token was `\`, so a following `case` opens a block
    // (`LambdaCase`).
    let mut after_backslash = false;

    // The module body is a block unless it starts with `module` or `{`.
    let first_is_module = tokens
        .iter()
        .find(|t| !matches!(t.kind, TokenKind::Pragma(_)))
        .is_some_and(|t| {
            matches!(
                t.kind,
                TokenKind::Keyword(Keyword::Module) | TokenKind::Special('{')
            )
        });
    if !first_is_module {
        pending_open = Some(Keyword::Where);
    }

    for tok in tokens {
        let pos = tok.pos;
        let col = pos.col;
        let is_eof = tok.kind == TokenKind::Eof;

        if let TokenKind::Pragma(_) = tok.kind {
            // Pragmas are comments for the layout rule.
            out.push(tok);
            continue;
        }

        if let Some(keyword) = pending_open.take() {
            if tok.kind == TokenKind::Special('{') {
                stack.push(Context::Explicit { depth });
                out.push(tok);
                prev_line = pos.line;
                after_backslash = false;
                continue;
            }
            let enclosing = enclosing_col(&stack);
            let n = if is_eof { 0 } else { col };
            if n > enclosing || (stack.is_empty() && n > 0) {
                stack.push(Context::Implicit {
                    col: n,
                    opener: keyword,
                    depth,
                });
                out.push(Token {
                    kind: TokenKind::VOpen,
                    pos,
                });
                // The token that opened the block is not the first token on
                // a new line for the purpose of the `<n>` rule.
                prev_line = pos.line;
            } else {
                // An empty block: `{}`. The token then starts a new line
                // as usual.
                out.push(Token {
                    kind: TokenKind::VOpen,
                    pos,
                });
                out.push(Token {
                    kind: TokenKind::VClose,
                    pos,
                });
            }
        }

        if is_eof {
            while let Some(ctx) = stack.pop() {
                match ctx {
                    Context::Implicit { .. } => out.push(Token {
                        kind: TokenKind::VClose,
                        pos,
                    }),
                    Context::Explicit { .. } => {
                        return Err(LayoutError {
                            pos,
                            message: "missing `}` at end of input".into(),
                        })
                    }
                }
            }
            out.push(tok);
            break;
        }

        // The `<n>` rule: the first token on a line.
        if pos.line > prev_line {
            loop {
                match stack.last() {
                    Some(Context::Implicit { col: m, .. }) if col < *m => {
                        stack.pop();
                        out.push(Token {
                            kind: TokenKind::VClose,
                            pos,
                        });
                    }
                    Some(Context::Implicit { col: m, .. }) if col == *m => {
                        // `where` cannot start a block item. The parse-error
                        // rule closes the block instead, so a `where` at the
                        // column of a `do` block belongs to the equation.
                        if tok.kind == TokenKind::Keyword(Keyword::Where) {
                            stack.pop();
                            out.push(Token {
                                kind: TokenKind::VClose,
                                pos,
                            });
                            continue;
                        }
                        out.push(Token {
                            kind: TokenKind::VSemi,
                            pos,
                        });
                        break;
                    }
                    _ => break,
                }
            }
        }
        prev_line = pos.line;

        // The parse-error rule, approximated.
        match tok.kind {
            TokenKind::Keyword(Keyword::In) => {
                if let Some(Context::Implicit {
                    opener: Keyword::Let,
                    ..
                }) = stack.last()
                {
                    stack.pop();
                    out.push(Token {
                        kind: TokenKind::VClose,
                        pos,
                    });
                }
            }
            TokenKind::Special(')' | ']') => {
                close_implicit_at(&mut stack, &mut out, depth, pos, false);
                depth = depth.saturating_sub(1);
            }
            TokenKind::Special('}') => {
                close_implicit_at(&mut stack, &mut out, depth, pos, false);
                match stack.last() {
                    Some(Context::Explicit { depth: d }) if *d == depth => {
                        stack.pop();
                    }
                    _ => depth = depth.saturating_sub(1),
                }
            }
            TokenKind::Special(',') => {
                if depth > 0 {
                    close_implicit_at(&mut stack, &mut out, depth, pos, true);
                }
            }
            TokenKind::Special('(' | '[' | '{') => depth += 1,
            _ => {}
        }

        // Keywords that open a block.
        match tok.kind {
            TokenKind::Keyword(k @ (Keyword::Let | Keyword::Where | Keyword::Do | Keyword::Of)) => {
                pending_open = Some(k);
            }
            TokenKind::Keyword(Keyword::Case) if after_backslash => {
                pending_open = Some(Keyword::Case);
            }
            _ => {}
        }
        after_backslash = tok.kind == TokenKind::ReservedOp(ReservedOp::Backslash);

        out.push(tok);
    }
    Ok(out)
}

/// The column of the innermost implicit block, or 0 inside explicit braces
/// or at the top.
fn enclosing_col(stack: &[Context]) -> u32 {
    match stack.last() {
        Some(Context::Implicit { col, .. }) => *col,
        _ => 0,
    }
}

/// Close the implicit blocks that opened at bracket depth `depth`. With
/// `skip_of`, stop at a block that `of` opened.
fn close_implicit_at(
    stack: &mut Vec<Context>,
    out: &mut Vec<Token>,
    depth: u32,
    pos: Pos,
    skip_of: bool,
) {
    while let Some(Context::Implicit {
        depth: d, opener, ..
    }) = stack.last()
    {
        if *d != depth || (skip_of && *opener == Keyword::Of) {
            break;
        }
        stack.pop();
        out.push(Token {
            kind: TokenKind::VClose,
            pos,
        });
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::{lex, LexOptions};

    /// Render the token stream with `{`, `}` and `;` for the virtual
    /// tokens, so tests read like the report's examples.
    fn render(src: &str) -> String {
        let toks = layout(lex(src, LexOptions::default()).unwrap()).unwrap();
        let mut s = String::new();
        for t in toks {
            let piece = match t.kind {
                TokenKind::VOpen => "{".to_string(),
                TokenKind::VClose => "}".to_string(),
                TokenKind::VSemi => ";".to_string(),
                TokenKind::Eof => continue,
                k => k.to_string(),
            };
            if !s.is_empty() {
                s.push(' ');
            }
            s.push_str(&piece);
        }
        s
    }

    #[test]
    fn module_body() {
        assert_eq!(
            render("module M where\nx = 1\ny = 2\n"),
            "module M where { x = 1 ; y = 2 }"
        );
        assert_eq!(render("x = 1\ny = 2\n"), "{ x = 1 ; y = 2 }");
        assert_eq!(
            render("{-# LANGUAGE CPP #-}\nmodule M where\nx = 1"),
            "{-# LANGUAGE CPP #-} module M where { x = 1 }"
        );
    }

    #[test]
    fn explicit_braces() {
        assert_eq!(
            render("module M where { x = 1; y = 2 }"),
            "module M where { x = 1 ; y = 2 }"
        );
        assert_eq!(
            render("x = let { a = 1 } in a"),
            "{ x = let { a = 1 } in a }"
        );
    }

    #[test]
    fn nested_blocks() {
        let src = "f x = do\n  a\n  b\n  where\n    a = 1\n    b = 2\ng = 3\n";
        assert_eq!(
            render(src),
            "{ f x = do { a ; b } where { a = 1 ; b = 2 } ; g = 3 }"
        );
    }

    #[test]
    fn let_in_on_one_line() {
        assert_eq!(render("x = let a = 1 in a"), "{ x = let { a = 1 } in a }");
        assert_eq!(
            render("x = let a = 1\n        b = 2\n    in a"),
            "{ x = let { a = 1 ; b = 2 } in a }"
        );
    }

    #[test]
    fn brackets_close_blocks() {
        assert_eq!(
            render("x = (do a) + [case y of A -> 1]"),
            "{ x = ( do { a } ) + [ case y of { A -> 1 } ] }"
        );
        assert_eq!(
            render("xs = [y | x <- zs, let y = x, y > 0]"),
            "{ xs = [ y | x <- zs , let { y = x } , y > 0 ] }"
        );
        assert_eq!(
            render("r = Rec { a = do b, c = 1 }"),
            "{ r = Rec { a = do { b } , c = 1 } }"
        );
        assert_eq!(
            render("f = g where { h = Rec { a = 1 } }"),
            "{ f = g where { h = Rec { a = 1 } } }"
        );
    }

    #[test]
    fn guards_with_commas() {
        assert_eq!(
            render("f x\n  | a, b = 1\n  | otherwise = 2\n"),
            "{ f x | a , b = 1 | otherwise = 2 }"
        );
        assert_eq!(
            render("f = (case x of A | a, b -> 1)"),
            "{ f = ( case x of { A | a , b -> 1 } ) }"
        );
    }

    #[test]
    fn where_at_block_column() {
        assert_eq!(
            render("f = case x of\n  A -> 1\n  where y = 2\n"),
            "{ f = case x of { A -> 1 } where { y = 2 } }"
        );
    }

    #[test]
    fn empty_block() {
        assert_eq!(
            render("f = x where\ng = 1\n"),
            "{ f = x where { } ; g = 1 }"
        );
        assert_eq!(render("class C a where"), "{ class C a where { } }");
    }

    #[test]
    fn lambda_case() {
        assert_eq!(
            render("f = \\case\n  A -> 1\n  B -> 2\n"),
            "{ f = \\ case { A -> 1 ; B -> 2 } }"
        );
    }

    #[test]
    fn if_then_else_in_do() {
        // Haskell 2010 permits the semicolons before `then` and `else`.
        assert_eq!(
            render("f = do\n  if a\n  then b\n  else c\n"),
            "{ f = do { if a ; then b ; else c } }"
        );
    }

    #[test]
    fn unbalanced_explicit_brace() {
        let toks = lex("module M where {", LexOptions::default()).unwrap();
        assert!(layout(toks).is_err());
    }
}
