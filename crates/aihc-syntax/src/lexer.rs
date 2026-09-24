//! The lexer: source text to tokens.
//!
//! The lexer follows chapter 2 of the Haskell 2010 report, plus the
//! extensions the vendored tree uses: `MagicHash` names and the `'` tick
//! of `DataKinds`. Comments go away. So do pragmas, except the three that
//! change how a module is read: `LANGUAGE`, `OPTIONS_GHC` and `SOURCE`.
//! Every other pragma is an optimization hint, and aihc-boot ignores
//! those. The lexer does not apply the layout rule; see the `layout`
//! module for that.

use crate::token::{Keyword, Pos, ReservedOp, Token, TokenKind};
use std::fmt;

/// A lexical error with the position where the lexer stopped.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LexError {
    pub pos: Pos,
    pub message: String,
}

impl fmt::Display for LexError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.pos, self.message)
    }
}

impl std::error::Error for LexError {}

/// Options that change what the lexer accepts.
#[derive(Clone, Copy, Debug, Default)]
pub struct LexOptions {
    /// Permit `#` at the end of names, as in `Int#` and `+#`.
    pub magic_hash: bool,
}

impl LexOptions {
    /// Options for a source file, from the `LANGUAGE` pragmas before the
    /// module header.
    pub fn from_source(src: &str) -> LexOptions {
        let mut opts = LexOptions::default();
        for ext in language_pragmas(src) {
            if ext == "MagicHash" {
                opts.magic_hash = true;
            }
        }
        opts
    }
}

/// The extension names in every `{-# LANGUAGE ... #-}` pragma of a file.
///
/// The scan is textual and stops at the first `module` keyword outside a
/// comment, so it also works on files the lexer rejects later.
pub fn language_pragmas(src: &str) -> Vec<String> {
    let mut exts = Vec::new();
    let mut rest = src;
    while let Some(start) = rest.find("{-#") {
        let after = &rest[start + 3..];
        let Some(end) = after.find("#-}") else { break };
        let body = after[..end].trim();
        if let Some(list) = body.strip_prefix("LANGUAGE") {
            for ext in list.split(',') {
                let ext = ext.trim();
                if !ext.is_empty() {
                    exts.push(ext.to_string());
                }
            }
        }
        rest = &after[end + 3..];
    }
    exts
}

/// Lex a whole file. Returns the tokens with an `Eof` token at the end.
pub fn lex(src: &str, opts: LexOptions) -> Result<Vec<Token>, LexError> {
    let mut lexer = Lexer::new(src, opts);
    let mut tokens = Vec::new();
    loop {
        let tok = lexer.next_token()?;
        let done = tok.kind == TokenKind::Eof;
        tokens.push(tok);
        if done {
            return Ok(tokens);
        }
    }
}

struct Lexer {
    chars: Vec<char>,
    idx: usize,
    pos: Pos,
    opts: LexOptions,
}

fn is_symbol(c: char) -> bool {
    match c {
        '!' | '#' | '$' | '%' | '&' | '*' | '+' | '.' | '/' | '<' | '=' | '>' | '?' | '@'
        | '\\' | '^' | '|' | '-' | '~' | ':' => true,
        '(' | ')' | ',' | ';' | '[' | ']' | '`' | '{' | '}' | '_' | '"' | '\'' => false,
        c if c.is_ascii() => false,
        c => {
            // Unicode symbols and punctuation, but not letters or digits.
            !c.is_alphanumeric() && !c.is_whitespace() && !c.is_control()
        }
    }
}

fn is_ident_start(c: char) -> bool {
    c == '_' || c.is_alphabetic()
}

fn is_ident_char(c: char) -> bool {
    c == '_' || c == '\'' || c.is_alphanumeric()
}

impl Lexer {
    fn new(src: &str, opts: LexOptions) -> Lexer {
        Lexer {
            chars: src.chars().collect(),
            idx: 0,
            pos: Pos { line: 1, col: 1 },
            opts,
        }
    }

    fn peek(&self) -> Option<char> {
        self.chars.get(self.idx).copied()
    }

    fn peek_at(&self, n: usize) -> Option<char> {
        self.chars.get(self.idx + n).copied()
    }

    fn starts_with(&self, s: &str) -> bool {
        s.chars()
            .enumerate()
            .all(|(i, c)| self.peek_at(i) == Some(c))
    }

    fn bump(&mut self) -> Option<char> {
        let c = self.peek()?;
        self.idx += 1;
        match c {
            '\n' => {
                self.pos.line += 1;
                self.pos.col = 1;
            }
            '\t' => {
                self.pos.col = (self.pos.col - 1) / 8 * 8 + 9;
            }
            '\r' => {}
            _ => self.pos.col += 1,
        }
        Some(c)
    }

    fn bump_n(&mut self, n: usize) {
        for _ in 0..n {
            self.bump();
        }
    }

    #[allow(clippy::unused_self)]
    fn error<T>(&self, pos: Pos, message: impl Into<String>) -> Result<T, LexError> {
        Err(LexError {
            pos,
            message: message.into(),
        })
    }

    /// Skip whitespace and comments. Returns a pragma token if one is
    /// found, because pragmas are the one kind of comment that stays.
    fn skip_trivia(&mut self) -> Result<Option<Token>, LexError> {
        loop {
            match self.peek() {
                Some(c) if c.is_whitespace() => {
                    self.bump();
                }
                Some('-') if self.is_line_comment() => {
                    while let Some(c) = self.peek() {
                        if c == '\n' {
                            break;
                        }
                        self.bump();
                    }
                }
                Some('{') if self.starts_with("{-#") => {
                    if let Some(pragma) = self.pragma()? {
                        return Ok(Some(pragma));
                    }
                }
                Some('{') if self.starts_with("{-") => {
                    self.block_comment()?;
                }
                _ => return Ok(None),
            }
        }
    }

    /// `--` starts a line comment only when the dashes are not part of a
    /// longer operator, so `-->` is an operator and `---` is a comment.
    fn is_line_comment(&self) -> bool {
        let mut n = 0;
        while self.peek_at(n) == Some('-') {
            n += 1;
        }
        n >= 2 && !self.peek_at(n).is_some_and(is_symbol)
    }

    fn block_comment(&mut self) -> Result<(), LexError> {
        let start = self.pos;
        self.bump_n(2);
        let mut depth = 1;
        while depth > 0 {
            if self.starts_with("{-") {
                self.bump_n(2);
                depth += 1;
            } else if self.starts_with("-}") {
                self.bump_n(2);
                depth -= 1;
            } else if self.bump().is_none() {
                return self.error(start, "unterminated block comment");
            }
        }
        Ok(())
    }

    /// A pragma. Returns a token for the pragmas that matter and `None`
    /// for the ones the compiler ignores.
    fn pragma(&mut self) -> Result<Option<Token>, LexError> {
        let start = self.pos;
        self.bump_n(3);
        let mut text = String::new();
        loop {
            if self.starts_with("#-}") {
                self.bump_n(3);
                break;
            }
            match self.bump() {
                Some(c) => text.push(c),
                None => return self.error(start, "unterminated pragma"),
            }
        }
        let keep = matches!(
            text.split_whitespace().next(),
            Some("LANGUAGE" | "OPTIONS_GHC" | "OPTIONS" | "SOURCE")
        );
        Ok(keep.then_some(Token {
            kind: TokenKind::Pragma(text),
            pos: start,
        }))
    }

    fn next_token(&mut self) -> Result<Token, LexError> {
        if let Some(pragma) = self.skip_trivia()? {
            return Ok(pragma);
        }
        let start = self.pos;
        let Some(c) = self.peek() else {
            return Ok(Token {
                kind: TokenKind::Eof,
                pos: start,
            });
        };
        let kind = match c {
            '(' | ')' | ',' | ';' | '[' | ']' | '`' | '{' | '}' => {
                self.bump();
                TokenKind::Special(c)
            }
            '"' => self.string()?,
            '\'' => self.char_or_tick()?,
            c if c.is_ascii_digit() => self.number()?,
            c if is_ident_start(c) => self.name()?,
            c if is_symbol(c) => {
                let sym = self.symbol();
                match ReservedOp::parse(&sym) {
                    Some(op) => TokenKind::ReservedOp(op),
                    None if sym.starts_with(':') => TokenKind::ConSym {
                        qual: String::new(),
                        name: sym,
                    },
                    None => TokenKind::VarSym {
                        qual: String::new(),
                        name: sym,
                    },
                }
            }
            c => return self.error(start, format!("unexpected character {c:?}")),
        };
        Ok(Token { kind, pos: start })
    }

    /// An identifier segment: letters, digits, `_`, `'`, and with
    /// `MagicHash` any number of trailing `#`.
    fn ident(&mut self) -> String {
        let mut s = String::new();
        while let Some(c) = self.peek() {
            if is_ident_char(c) {
                s.push(c);
                self.bump();
            } else {
                break;
            }
        }
        if self.opts.magic_hash {
            while self.peek() == Some('#') {
                s.push('#');
                self.bump();
            }
        }
        s
    }

    fn symbol(&mut self) -> String {
        let mut s = String::new();
        while let Some(c) = self.peek() {
            if is_symbol(c) {
                s.push(c);
                self.bump();
            } else {
                break;
            }
        }
        s
    }

    /// A name, possibly qualified: `x`, `Con`, `M.N.x`, `M.N.Con`, `M.+`,
    /// `M.:+`.
    fn name(&mut self) -> Result<TokenKind, LexError> {
        let mut qual = String::new();
        loop {
            let seg = self.ident();
            let is_con = seg.chars().next().is_some_and(char::is_uppercase);
            if is_con && self.peek() == Some('.') {
                // A qualifier, if a name follows the dot.
                match self.peek_at(1) {
                    Some(c) if is_ident_start(c) => {
                        self.bump();
                        if !qual.is_empty() {
                            qual.push('.');
                        }
                        qual.push_str(&seg);
                        continue;
                    }
                    Some(c) if is_symbol(c) => {
                        self.bump();
                        if !qual.is_empty() {
                            qual.push('.');
                        }
                        qual.push_str(&seg);
                        let sym = self.symbol();
                        return Ok(if sym.starts_with(':') {
                            TokenKind::ConSym { qual, name: sym }
                        } else {
                            TokenKind::VarSym { qual, name: sym }
                        });
                    }
                    _ => {}
                }
            }
            return Ok(if is_con {
                TokenKind::ConId { qual, name: seg }
            } else if qual.is_empty() {
                match Keyword::parse(&seg) {
                    Some(k) => TokenKind::Keyword(k),
                    None => TokenKind::VarId { qual, name: seg },
                }
            } else {
                TokenKind::VarId { qual, name: seg }
            });
        }
    }

    fn digits(&mut self, s: &mut String, radix: u32) -> usize {
        let mut n = 0;
        while let Some(c) = self.peek() {
            if c.is_digit(radix) {
                s.push(c);
                self.bump();
                n += 1;
            } else if c == '_' && self.peek_at(1).is_some_and(|d| d.is_digit(radix)) {
                // NumericUnderscores
                s.push(c);
                self.bump();
            } else {
                break;
            }
        }
        n
    }

    fn number(&mut self) -> Result<TokenKind, LexError> {
        let start = self.pos;
        let mut s = String::new();
        if self.peek() == Some('0') {
            let radix = match self.peek_at(1) {
                Some('x' | 'X') => Some(16),
                Some('o' | 'O') => Some(8),
                Some('b' | 'B') => Some(2),
                _ => None,
            };
            if let Some(radix) = radix {
                if self.peek_at(2).is_some_and(|c| c.is_digit(radix)) {
                    s.push(self.bump().unwrap());
                    s.push(self.bump().unwrap());
                    self.digits(&mut s, radix);
                    self.magic_hash_suffix(&mut s);
                    return Ok(TokenKind::Integer(s));
                }
            }
        }
        self.digits(&mut s, 10);
        let mut is_float = false;
        if self.peek() == Some('.') && self.peek_at(1).is_some_and(|c| c.is_ascii_digit()) {
            is_float = true;
            s.push('.');
            self.bump();
            self.digits(&mut s, 10);
        }
        if matches!(self.peek(), Some('e' | 'E')) {
            let sign = matches!(self.peek_at(1), Some('+' | '-'));
            let digit_at = if sign { 2 } else { 1 };
            if self.peek_at(digit_at).is_some_and(|c| c.is_ascii_digit()) {
                is_float = true;
                s.push(self.bump().unwrap());
                if sign {
                    s.push(self.bump().unwrap());
                }
                if self.digits(&mut s, 10) == 0 {
                    return self.error(start, "malformed exponent");
                }
            }
        }
        self.magic_hash_suffix(&mut s);
        Ok(if is_float {
            TokenKind::Float(s)
        } else {
            TokenKind::Integer(s)
        })
    }

    fn magic_hash_suffix(&mut self, s: &mut String) {
        if self.opts.magic_hash {
            while self.peek() == Some('#') {
                s.push('#');
                self.bump();
            }
        }
    }

    fn char_or_tick(&mut self) -> Result<TokenKind, LexError> {
        let start = self.pos;
        // Look ahead without consuming: a char literal is `'c'` or
        // `'\escape'`. Anything else is a tick.
        let save = (self.idx, self.pos);
        self.bump();
        let c = match self.peek() {
            Some('\\') => {
                self.bump();
                match self.escape(start)? {
                    Some(c) => Some(c),
                    None => {
                        return self.error(start, "\\& is not permitted in a character literal")
                    }
                }
            }
            Some(c) if c != '\'' && c != '\n' => {
                self.bump();
                Some(c)
            }
            _ => None,
        };
        if let Some(c) = c {
            if self.peek() == Some('\'') {
                self.bump();
                let raw = self.chars[save.0..self.idx].iter().collect();
                return Ok(TokenKind::Char { value: c, raw });
            }
        }
        (self.idx, self.pos) = save;
        self.bump();
        Ok(TokenKind::Tick)
    }

    fn string(&mut self) -> Result<TokenKind, LexError> {
        let start = self.pos;
        let start_idx = self.idx;
        self.bump();
        let mut s = String::new();
        loop {
            match self.bump() {
                None | Some('\n') => return self.error(start, "unterminated string literal"),
                Some('"') => {
                    let raw = self.chars[start_idx..self.idx].iter().collect();
                    return Ok(TokenKind::String { value: s, raw });
                }
                Some('\\') => match self.peek() {
                    Some(c) if c.is_whitespace() => {
                        // A string gap: `\` whitespace `\`.
                        while self.peek().is_some_and(char::is_whitespace) {
                            self.bump();
                        }
                        if self.bump() != Some('\\') {
                            return self.error(start, "malformed string gap");
                        }
                    }
                    _ => {
                        if let Some(c) = self.escape(start)? {
                            s.push(c);
                        }
                    }
                },
                Some(c) => s.push(c),
            }
        }
    }

    /// An escape after the backslash. Returns `None` for `\&`, the empty
    /// escape.
    fn escape(&mut self, start: Pos) -> Result<Option<char>, LexError> {
        let Some(c) = self.bump() else {
            return self.error(start, "unterminated escape");
        };
        let ch = match c {
            'a' => '\x07',
            'b' => '\x08',
            'f' => '\x0c',
            'n' => '\n',
            'r' => '\r',
            't' => '\t',
            'v' => '\x0b',
            '\\' => '\\',
            '"' => '"',
            '\'' => '\'',
            '&' => return Ok(None),
            '^' => {
                let Some(c) = self.bump() else {
                    return self.error(start, "unterminated escape");
                };
                match c {
                    '@'..='_' => char::from(c as u8 - b'@'),
                    _ => return self.error(start, format!("bad control escape \\^{c}")),
                }
            }
            'x' => self.numeric_escape(start, 16)?,
            'o' => self.numeric_escape(start, 8)?,
            c if c.is_ascii_digit() => {
                self.idx -= 1;
                self.pos.col -= 1;
                self.numeric_escape(start, 10)?
            }
            c if c.is_ascii_uppercase() => {
                // A named ASCII control character, such as `\NUL`. The
                // lexer matches the longest name, so `\SOH` is one escape
                // and not `\SO` followed by `H`.
                self.idx -= 1;
                self.pos.col -= 1;
                let Some((name, ch)) = ASCII_NAMES
                    .iter()
                    .filter(|(name, _)| self.starts_with(name))
                    .max_by_key(|(name, _)| name.len())
                else {
                    return self.error(start, format!("unknown escape \\{c}"));
                };
                self.bump_n(name.len());
                *ch
            }
            c => return self.error(start, format!("unknown escape \\{c}")),
        };
        Ok(Some(ch))
    }

    fn numeric_escape(&mut self, start: Pos, radix: u32) -> Result<char, LexError> {
        let mut value: u32 = 0;
        let mut n = 0;
        while let Some(c) = self.peek() {
            let Some(d) = c.to_digit(radix) else { break };
            value = match value.checked_mul(radix).and_then(|v| v.checked_add(d)) {
                Some(v) => v,
                None => return self.error(start, "numeric escape out of range"),
            };
            self.bump();
            n += 1;
        }
        if n == 0 {
            return self.error(start, "numeric escape without digits");
        }
        match char::from_u32(value) {
            Some(c) => Ok(c),
            None => self.error(start, "numeric escape out of range"),
        }
    }
}

/// The named ASCII escapes of the Haskell report.
const ASCII_NAMES: [(&str, char); 34] = [
    ("NUL", '\x00'),
    ("SOH", '\x01'),
    ("STX", '\x02'),
    ("ETX", '\x03'),
    ("EOT", '\x04'),
    ("ENQ", '\x05'),
    ("ACK", '\x06'),
    ("BEL", '\x07'),
    ("BS", '\x08'),
    ("HT", '\x09'),
    ("LF", '\x0a'),
    ("VT", '\x0b'),
    ("FF", '\x0c'),
    ("CR", '\x0d'),
    ("SO", '\x0e'),
    ("SI", '\x0f'),
    ("DLE", '\x10'),
    ("DC1", '\x11'),
    ("DC2", '\x12'),
    ("DC3", '\x13'),
    ("DC4", '\x14'),
    ("NAK", '\x15'),
    ("SYN", '\x16'),
    ("ETB", '\x17'),
    ("CAN", '\x18'),
    ("EM", '\x19'),
    ("SUB", '\x1a'),
    ("ESC", '\x1b'),
    ("FS", '\x1c'),
    ("GS", '\x1d'),
    ("RS", '\x1e'),
    ("US", '\x1f'),
    ("SP", '\x20'),
    ("DEL", '\x7f'),
];

#[cfg(test)]
mod tests {
    use super::*;

    fn kinds(src: &str) -> Vec<TokenKind> {
        let mut toks: Vec<_> = lex(src, LexOptions::default())
            .unwrap()
            .into_iter()
            .map(|t| t.kind)
            .collect();
        assert_eq!(toks.pop(), Some(TokenKind::Eof));
        toks
    }

    fn var(name: &str) -> TokenKind {
        TokenKind::VarId {
            qual: String::new(),
            name: name.into(),
        }
    }

    fn chr(value: char, raw: &str) -> TokenKind {
        TokenKind::Char {
            value,
            raw: raw.into(),
        }
    }

    fn string(value: &str, raw: &str) -> TokenKind {
        TokenKind::String {
            value: value.into(),
            raw: raw.into(),
        }
    }

    fn sym(name: &str) -> TokenKind {
        TokenKind::VarSym {
            qual: String::new(),
            name: name.into(),
        }
    }

    #[test]
    fn names_and_keywords() {
        assert_eq!(
            kinds("let x' = M.N.y"),
            vec![
                TokenKind::Keyword(Keyword::Let),
                var("x'"),
                TokenKind::ReservedOp(ReservedOp::Equals),
                TokenKind::VarId {
                    qual: "M.N".into(),
                    name: "y".into()
                },
            ]
        );
        assert_eq!(
            kinds("Data.Map.Map"),
            vec![TokenKind::ConId {
                qual: "Data.Map".into(),
                name: "Map".into()
            }]
        );
        assert_eq!(kinds("as qualified"), vec![var("as"), var("qualified")]);
    }

    #[test]
    fn operators() {
        assert_eq!(
            kinds("a -> b <$> M.+ x --> y"),
            vec![
                var("a"),
                TokenKind::ReservedOp(ReservedOp::RightArrow),
                var("b"),
                sym("<$>"),
                TokenKind::VarSym {
                    qual: "M".into(),
                    name: "+".into()
                },
                var("x"),
                sym("-->"),
                var("y"),
            ]
        );
        assert_eq!(
            kinds("M.. Prelude.."),
            vec![
                TokenKind::VarSym {
                    qual: "M".into(),
                    name: ".".into()
                },
                TokenKind::VarSym {
                    qual: "Prelude".into(),
                    name: ".".into()
                },
            ]
        );
        assert_eq!(
            kinds("x :| xs"),
            vec![
                var("x"),
                TokenKind::ConSym {
                    qual: String::new(),
                    name: ":|".into()
                },
                var("xs")
            ]
        );
    }

    #[test]
    fn comments() {
        assert_eq!(kinds("x -- comment\n--- also\ny"), vec![var("x"), var("y")]);
        assert_eq!(
            kinds("x {- a {- nested -} b -} y"),
            vec![var("x"), var("y")]
        );
        assert_eq!(
            kinds("{-# LANGUAGE CPP #-} x {-# INLINE x #-}"),
            vec![TokenKind::Pragma(" LANGUAGE CPP ".into()), var("x")]
        );
    }

    #[test]
    fn numbers() {
        assert_eq!(
            kinds("0 42 0x1F 0o17 0b101 1.5 2e10 3.0e-2 1_000"),
            vec![
                TokenKind::Integer("0".into()),
                TokenKind::Integer("42".into()),
                TokenKind::Integer("0x1F".into()),
                TokenKind::Integer("0o17".into()),
                TokenKind::Integer("0b101".into()),
                TokenKind::Float("1.5".into()),
                TokenKind::Float("2e10".into()),
                TokenKind::Float("3.0e-2".into()),
                TokenKind::Integer("1_000".into()),
            ]
        );
        // `1.f` is `1` `.` `f`, not a float.
        assert_eq!(
            kinds("1.f"),
            vec![TokenKind::Integer("1".into()), sym("."), var("f")]
        );
    }

    #[test]
    fn chars_and_strings() {
        assert_eq!(
            kinds(r"'a' '\n' '\'' '\\' '\x41' '\65' '\NUL' '\^A'"),
            vec![
                chr('a', "'a'"),
                chr('\n', r"'\n'"),
                chr('\'', r"'\''"),
                chr('\\', r"'\\'"),
                chr('A', r"'\x41'"),
                chr('A', r"'\65'"),
                chr('\0', r"'\NUL'"),
                chr('\x01', r"'\^A'"),
            ]
        );
        assert_eq!(
            kinds(r#""a\tb\"c" "\SOH" "\SO\&H" "ab\   \cd" "\1234\&5" "\DC1\DEL""#),
            vec![
                string("a\tb\"c", r#""a\tb\"c""#),
                string("\x01", r#""\SOH""#),
                string("\x0eH", r#""\SO\&H""#),
                string("abcd", r#""ab\   \cd""#),
                string("\u{4d2}5", r#""\1234\&5""#),
                string("\x11\x7f", r#""\DC1\DEL""#),
            ]
        );
    }

    #[test]
    fn ticks() {
        assert_eq!(
            kinds("'Just '[] x'"),
            vec![
                TokenKind::Tick,
                TokenKind::ConId {
                    qual: String::new(),
                    name: "Just".into()
                },
                TokenKind::Tick,
                TokenKind::Special('['),
                TokenKind::Special(']'),
                var("x'"),
            ]
        );
    }

    #[test]
    fn magic_hash() {
        let opts = LexOptions { magic_hash: true };
        let toks: Vec<_> = lex("I# 3# +#", opts)
            .unwrap()
            .into_iter()
            .map(|t| t.kind)
            .collect();
        assert_eq!(
            toks,
            vec![
                TokenKind::ConId {
                    qual: String::new(),
                    name: "I#".into()
                },
                TokenKind::Integer("3#".into()),
                sym("+#"),
                TokenKind::Eof,
            ]
        );
        assert!(
            LexOptions::from_source("{-# LANGUAGE BangPatterns, MagicHash #-}\nmodule M where")
                .magic_hash
        );
    }

    #[test]
    fn positions() {
        let toks = lex("a\n\tb c", LexOptions::default()).unwrap();
        assert_eq!(toks[0].pos, Pos { line: 1, col: 1 });
        assert_eq!(toks[1].pos, Pos { line: 2, col: 9 });
        assert_eq!(toks[2].pos, Pos { line: 2, col: 11 });
    }

    #[test]
    fn errors() {
        let err = lex("x \"abc", LexOptions::default()).unwrap_err();
        assert_eq!(err.pos, Pos { line: 1, col: 3 });
        assert!(lex("{- open", LexOptions::default()).is_err());
    }
}
