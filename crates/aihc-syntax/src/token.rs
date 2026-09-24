//! Tokens and source positions.

use std::fmt;

/// A position in a source file. Lines and columns start at 1. A tab moves
/// the column to the next multiple of 8 plus 1, as the Haskell report
/// specifies for the layout rule.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Pos {
    pub line: u32,
    pub col: u32,
}

impl fmt::Display for Pos {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.col)
    }
}

/// A token with the position of its first character.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
    pub pos: Pos,
}

/// The kinds of token the lexer and the layout pass produce.
///
/// Names carry their qualifier as a separate string. The qualifier is empty
/// for an unqualified name.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TokenKind {
    /// A variable identifier, such as `map` or `M.map`.
    VarId { qual: String, name: String },
    /// A constructor identifier, such as `Just` or `M.Just`.
    ConId { qual: String, name: String },
    /// A variable operator, such as `+` or `M.+`.
    VarSym { qual: String, name: String },
    /// A constructor operator, such as `:|` or `M.:|`.
    ConSym { qual: String, name: String },

    /// An integer literal. The text is the source spelling.
    Integer(String),
    /// A floating point literal. The text is the source spelling.
    Float(String),
    /// A character literal, decoded.
    Char(char),
    /// A string literal, decoded.
    String(String),

    /// A reserved word, such as `where`.
    Keyword(Keyword),
    /// A reserved operator, such as `->`.
    ReservedOp(ReservedOp),
    /// A special character: one of `(`, `)`, `,`, `;`, `[`, `]`, `` ` ``, `{` or `}`.
    Special(char),

    /// A pragma, such as `{-# LANGUAGE CPP #-}`. The text excludes the
    /// `{-#` and `#-}` brackets.
    Pragma(String),

    /// A lone `'` that does not start a character literal. `DataKinds`
    /// uses it to promote constructors, as in `'Just`.
    Tick,

    /// A virtual `{` from the layout rule.
    VOpen,
    /// A virtual `}` from the layout rule.
    VClose,
    /// A virtual `;` from the layout rule.
    VSemi,

    /// The end of the input.
    Eof,
}

/// Reserved words. `As`, `Hiding` and `Qualified` are not reserved in
/// Haskell. The lexer returns them as `VarId`; the parser recognizes them
/// by name.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Keyword {
    Case,
    Class,
    Data,
    Default,
    Deriving,
    Do,
    Else,
    Foreign,
    If,
    Import,
    In,
    Infix,
    Infixl,
    Infixr,
    Instance,
    Let,
    Module,
    Newtype,
    Of,
    Then,
    Type,
    Where,
    /// The wildcard `_`.
    Underscore,
}

impl Keyword {
    pub fn parse(s: &str) -> Option<Keyword> {
        Some(match s {
            "case" => Keyword::Case,
            "class" => Keyword::Class,
            "data" => Keyword::Data,
            "default" => Keyword::Default,
            "deriving" => Keyword::Deriving,
            "do" => Keyword::Do,
            "else" => Keyword::Else,
            "foreign" => Keyword::Foreign,
            "if" => Keyword::If,
            "import" => Keyword::Import,
            "in" => Keyword::In,
            "infix" => Keyword::Infix,
            "infixl" => Keyword::Infixl,
            "infixr" => Keyword::Infixr,
            "instance" => Keyword::Instance,
            "let" => Keyword::Let,
            "module" => Keyword::Module,
            "newtype" => Keyword::Newtype,
            "of" => Keyword::Of,
            "then" => Keyword::Then,
            "type" => Keyword::Type,
            "where" => Keyword::Where,
            "_" => Keyword::Underscore,
            _ => return None,
        })
    }

    pub fn as_str(self) -> &'static str {
        match self {
            Keyword::Case => "case",
            Keyword::Class => "class",
            Keyword::Data => "data",
            Keyword::Default => "default",
            Keyword::Deriving => "deriving",
            Keyword::Do => "do",
            Keyword::Else => "else",
            Keyword::Foreign => "foreign",
            Keyword::If => "if",
            Keyword::Import => "import",
            Keyword::In => "in",
            Keyword::Infix => "infix",
            Keyword::Infixl => "infixl",
            Keyword::Infixr => "infixr",
            Keyword::Instance => "instance",
            Keyword::Let => "let",
            Keyword::Module => "module",
            Keyword::Newtype => "newtype",
            Keyword::Of => "of",
            Keyword::Then => "then",
            Keyword::Type => "type",
            Keyword::Where => "where",
            Keyword::Underscore => "_",
        }
    }
}

/// Reserved operators.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum ReservedOp {
    /// `..`
    DotDot,
    /// `:`
    Colon,
    /// `::`
    DoubleColon,
    /// `=`
    Equals,
    /// `\`
    Backslash,
    /// `|`
    Bar,
    /// `<-`
    LeftArrow,
    /// `->`
    RightArrow,
    /// `@`
    At,
    /// `~`
    Tilde,
    /// `=>`
    DoubleArrow,
}

impl ReservedOp {
    pub fn parse(s: &str) -> Option<ReservedOp> {
        Some(match s {
            ".." => ReservedOp::DotDot,
            ":" => ReservedOp::Colon,
            "::" => ReservedOp::DoubleColon,
            "=" => ReservedOp::Equals,
            "\\" => ReservedOp::Backslash,
            "|" => ReservedOp::Bar,
            "<-" => ReservedOp::LeftArrow,
            "->" => ReservedOp::RightArrow,
            "@" => ReservedOp::At,
            "~" => ReservedOp::Tilde,
            "=>" => ReservedOp::DoubleArrow,
            _ => return None,
        })
    }

    pub fn as_str(self) -> &'static str {
        match self {
            ReservedOp::DotDot => "..",
            ReservedOp::Colon => ":",
            ReservedOp::DoubleColon => "::",
            ReservedOp::Equals => "=",
            ReservedOp::Backslash => "\\",
            ReservedOp::Bar => "|",
            ReservedOp::LeftArrow => "<-",
            ReservedOp::RightArrow => "->",
            ReservedOp::At => "@",
            ReservedOp::Tilde => "~",
            ReservedOp::DoubleArrow => "=>",
        }
    }
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TokenKind::VarId { qual, name }
            | TokenKind::ConId { qual, name }
            | TokenKind::VarSym { qual, name }
            | TokenKind::ConSym { qual, name } => {
                if qual.is_empty() {
                    write!(f, "{name}")
                } else {
                    write!(f, "{qual}.{name}")
                }
            }
            TokenKind::Integer(s) | TokenKind::Float(s) => write!(f, "{s}"),
            TokenKind::Char(c) => write!(f, "{c:?}"),
            TokenKind::String(s) => write!(f, "{s:?}"),
            TokenKind::Keyword(k) => write!(f, "{}", k.as_str()),
            TokenKind::ReservedOp(op) => write!(f, "{}", op.as_str()),
            TokenKind::Special(c) => write!(f, "{c}"),
            TokenKind::Pragma(s) => write!(f, "{{-#{s}#-}}"),
            TokenKind::Tick => write!(f, "'"),
            TokenKind::VOpen => write!(f, "{{(virtual)"),
            TokenKind::VClose => write!(f, "}}(virtual)"),
            TokenKind::VSemi => write!(f, ";(virtual)"),
            TokenKind::Eof => write!(f, "<end of input>"),
        }
    }
}
