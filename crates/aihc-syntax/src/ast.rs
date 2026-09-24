//! The syntax tree.
//!
//! The tree covers the module header, exports and imports. Declarations
//! are still opaque token runs; later work replaces `Decl::Unparsed` with
//! real declarations.

use crate::token::Pos;

/// A possibly qualified name. The qualifier is empty for an unqualified
/// name. Operators keep their symbol spelling, without parentheses.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct QName {
    pub qual: String,
    pub name: String,
}

impl QName {
    pub fn unqualified(name: impl Into<String>) -> QName {
        QName {
            qual: String::new(),
            name: name.into(),
        }
    }
}

/// A whole module.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Module {
    pub pos: Pos,
    /// The pragmas before the module header, such as `LANGUAGE`.
    pub pragmas: Vec<String>,
    /// `Main` when the module has no header.
    pub name: String,
    /// `None` exports everything.
    pub exports: Option<Vec<Export>>,
    pub imports: Vec<Import>,
    pub decls: Vec<Decl>,
}

/// One item of an export list.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Export {
    /// A variable or an operator, such as `map` or `(+)`.
    Var(QName),
    /// A type or class, with the constructors or methods it takes along.
    /// The `type` namespace keyword (`ExplicitNamespaces`) is recorded in
    /// `explicit_type`.
    Thing {
        name: QName,
        subs: Subs,
        explicit_type: bool,
    },
    /// A pattern synonym, `pattern P` (`PatternSynonyms`).
    Pattern(QName),
    /// `module M`: everything in scope from `M`.
    Module(String),
}

/// One item of an import list. The same shapes as an export, except
/// `module M`.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ImportItem {
    Var(String),
    Thing {
        name: String,
        subs: Subs,
        explicit_type: bool,
    },
    Pattern(String),
}

/// The constructors, fields or methods that come with a type or class.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Subs {
    /// `T`
    None,
    /// `T(..)`
    All,
    /// `T(A, b)`
    Some(Vec<String>),
    /// `T(.., P)`: everything, plus bundled pattern synonyms.
    AllAnd(Vec<String>),
}

/// An import declaration.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Import {
    pub pos: Pos,
    pub module: String,
    /// `{-# SOURCE #-}`: import the `.hs-boot` file.
    pub source: bool,
    pub qualified: bool,
    /// The package qualifier of `PackageImports`, such as `"base"`.
    pub package: Option<String>,
    /// `as M`
    pub alias: Option<String>,
    pub hiding: bool,
    /// `None` imports everything.
    pub items: Option<Vec<ImportItem>>,
}

/// A top-level declaration.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Decl {
    /// A declaration the parser does not handle yet: the position of its
    /// first token and the number of tokens up to the next declaration.
    Unparsed { pos: Pos, tokens: usize },
}

impl Decl {
    pub fn pos(&self) -> Pos {
        match self {
            Decl::Unparsed { pos, .. } => *pos,
        }
    }
}
