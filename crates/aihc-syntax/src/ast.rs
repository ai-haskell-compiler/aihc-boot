//! The syntax tree.
//!
//! The tree covers the module header, exports, imports, types and the
//! declarations that have no expressions. The parser reports an error
//! for value bindings, so no declaration is ever skipped: a module either
//! has a complete tree or a parse error.

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
    pub qualified: Qualified,
    /// The package qualifier of `PackageImports`, such as `"base"`.
    pub package: Option<String>,
    /// `as M`
    pub alias: Option<String>,
    pub hiding: bool,
    /// `None` imports everything.
    pub items: Option<Vec<ImportItem>>,
}

// --- Types -----------------------------------------------------------------

/// A type, or a kind. The tree keeps the source structure, including
/// parentheses, so that a printer reproduces the tokens the parser saw.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Type {
    /// A type variable, such as `a`.
    Var(String),
    /// A type constructor, such as `Maybe`, `M.T`, `()`, `[]`, `(->)` or
    /// `(,)`. The name keeps its source spelling.
    Con(QName),
    /// `f a`
    App(Box<Type>, Box<Type>),
    /// `f @k` (visible kind application)
    AppKind(Box<Type>, Box<Type>),
    /// `a -> b`
    Fun(Box<Type>, Box<Type>),
    /// `a `Op` b`, `a :+: b` or `a ~ b`
    Op(Box<Type>, QName, Box<Type>),
    /// `[a]`
    List(Box<Type>),
    /// `(a, b)`. The empty tuple `()` is `Con`.
    Tuple(Vec<Type>),
    /// `(t)`
    Paren(Box<Type>),
    /// `forall a b. t`
    Forall(Vec<TyVarBind>, Box<Type>),
    /// `ctx => t`. The context is a type: one constraint, or a tuple of
    /// constraints.
    Qual(Box<Type>, Box<Type>),
    /// `(t :: k)`, without the parentheses.
    KindSig(Box<Type>, Box<Type>),
    /// `'T` or `'[a, b]` (`DataKinds`)
    Promoted(Box<Type>),
    /// `'[a, b]` or `[a, b]` as a type-level list.
    PromotedList(Vec<Type>),
    /// A type-level literal, such as `"sym"` or `3`.
    Lit(Literal),
    /// `_`
    Wildcard,
}

/// A literal in a type or an expression.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Literal {
    /// The source spelling.
    Integer(String),
    /// The source spelling.
    Float(String),
    Char(char),
    String(String),
}

/// A type variable binder: `a` or `(a :: k)`.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TyVarBind {
    pub name: String,
    pub kind: Option<Type>,
}

/// A constructor argument or record field type, with its strictness.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BangType {
    /// `{-# UNPACK #-}` (`Some(true)`) or `{-# NOUNPACK #-}`
    /// (`Some(false)`).
    pub unpack: Option<bool>,
    /// `!` (`Some(true)`) or `~` (`Some(false)`).
    pub strict: Option<bool>,
    pub ty: Type,
}

// --- Declarations ----------------------------------------------------------

/// Where the `qualified` keyword of an import stands. GHC prints the two
/// positions differently, so the tree keeps the distinction.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Qualified {
    No,
    /// `import qualified M`
    Pre,
    /// `import M qualified` (`ImportQualifiedPost`)
    Post,
}

/// A top-level declaration, or a declaration in a class or instance body.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Decl {
    /// `f, g :: t`
    TypeSig {
        pos: Pos,
        names: Vec<String>,
        ty: Type,
    },
    /// `default f :: t` in a class body (`DefaultSignatures`).
    DefaultSig { pos: Pos, name: String, ty: Type },
    /// `pattern P :: t`
    PatSynSig {
        pos: Pos,
        names: Vec<String>,
        ty: Type,
    },
    /// `infixl 5 +, -`
    Fixity {
        pos: Pos,
        assoc: Assoc,
        prec: Option<String>,
        ops: Vec<String>,
    },
    /// `data T a = ...` or `newtype T a = ...`
    Data {
        pos: Pos,
        newtype: bool,
        name: String,
        vars: Vec<TyVarBind>,
        /// `data T :: k`
        kind: Option<Type>,
        cons: Vec<ConDecl>,
        deriving: Vec<Deriving>,
    },
    /// `type T a = t`
    TypeSyn {
        pos: Pos,
        name: String,
        vars: Vec<TyVarBind>,
        rhs: Type,
    },
    /// `class ctx => C a where { ... }`
    Class {
        pos: Pos,
        ctx: Option<Type>,
        name: String,
        vars: Vec<TyVarBind>,
        body: Vec<Decl>,
    },
    /// `instance ctx => C t where { ... }`. The head is a type, so it
    /// carries any `forall` and context.
    Instance {
        pos: Pos,
        head: Type,
        body: Vec<Decl>,
    },
    /// A pragma that stands as a declaration, such as `{-# INLINE f #-}`.
    /// The text is kept as is.
    Pragma { pos: Pos, text: String },
}

impl Decl {
    pub fn pos(&self) -> Pos {
        match self {
            Decl::TypeSig { pos, .. }
            | Decl::DefaultSig { pos, .. }
            | Decl::PatSynSig { pos, .. }
            | Decl::Fixity { pos, .. }
            | Decl::Data { pos, .. }
            | Decl::TypeSyn { pos, .. }
            | Decl::Class { pos, .. }
            | Decl::Instance { pos, .. }
            | Decl::Pragma { pos, .. } => *pos,
        }
    }
}

/// The associativity of a fixity declaration.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Assoc {
    Left,
    Right,
    None,
}

/// One constructor of a data declaration.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ConDecl {
    pub pos: Pos,
    /// `forall a.` before the constructor (`ExistentialQuantification`).
    pub forall: Vec<TyVarBind>,
    /// `ctx =>` before the constructor.
    pub ctx: Option<Type>,
    pub body: ConBody,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ConBody {
    /// `C a b`
    Prefix { name: String, args: Vec<BangType> },
    /// `a :+ b`
    Infix {
        left: BangType,
        op: String,
        right: BangType,
    },
    /// `C { f :: a, g, h :: b }`
    Record { name: String, fields: Vec<Field> },
}

/// One record field group: `f, g :: t`.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Field {
    pub names: Vec<String>,
    pub ty: BangType,
}

/// A `deriving` clause.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Deriving {
    pub strategy: Option<DerivStrategy>,
    /// The classes, as types. `deriving Show` is one class without
    /// parentheses; `parens` records whether the source had them.
    pub classes: Vec<Type>,
    pub parens: bool,
    /// `via t`
    pub via: Option<Type>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum DerivStrategy {
    Stock,
    Anyclass,
    Newtype,
}
