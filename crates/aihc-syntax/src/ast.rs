//! The syntax tree.
//!
//! The tree keeps the source structure, including parentheses and the
//! order of infix operators, so that a printer reproduces the tokens the
//! parser saw. It has no pragmas: the lexer drops them, because aihc-boot
//! ignores optimization hints.

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

/// A literal in a type, a pattern or an expression. Every variant keeps
/// the source spelling, because GHC prints literals from the source and
/// the round trip compares the printed forms.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Literal {
    Integer(String),
    Float(String),
    Char { value: char, raw: String },
    String { value: String, raw: String },
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
    /// `type F a :: k` in a class body, or `type family F a :: k`.
    TypeFamily {
        pos: Pos,
        name: String,
        vars: Vec<TyVarBind>,
        kind: Option<Type>,
    },
    /// `type F Int = t` in an instance body, or `type instance F Int = t`.
    TypeInstance { pos: Pos, lhs: Type, rhs: Type },
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
    /// One clause of a function, or a pattern binding: `f x = e`,
    /// `x <+> y = e`, `(a, b) = e`.
    Bind { pos: Pos, lhs: Lhs, rhs: Rhs },
    /// A pattern synonym definition.
    PatSyn {
        pos: Pos,
        lhs: PatSynLhs,
        dir: PatSynDir,
        pat: Pat,
    },
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
            | Decl::TypeFamily { pos, .. }
            | Decl::TypeInstance { pos, .. }
            | Decl::Class { pos, .. }
            | Decl::Instance { pos, .. }
            | Decl::Bind { pos, .. }
            | Decl::PatSyn { pos, .. } => *pos,
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

// --- Bindings --------------------------------------------------------------

/// The left-hand side of a binding.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Lhs {
    /// `f p1 p2`
    Fun { name: String, args: Vec<Pat> },
    /// `p1 op p2`, and `(p1 op p2) p3 ...` with `args`.
    Infix {
        left: Pat,
        op: String,
        right: Pat,
        args: Vec<Pat>,
    },
    /// A pattern binding, such as `(a, b)` or `!x`.
    Pat(Pat),
}

/// The right-hand side of a binding or a case alternative.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Rhs {
    pub body: RhsBody,
    /// `where` declarations.
    pub wheres: Vec<Decl>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum RhsBody {
    /// `= e` (or `-> e` in an alternative)
    Plain(Expr),
    /// `| g = e | g = e`
    Guarded(Vec<GuardedRhs>),
}

/// One guarded alternative: `| g1, g2 = e`.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct GuardedRhs {
    pub guards: Vec<Stmt>,
    pub body: Expr,
}

/// The left-hand side of a pattern synonym: `P a b`, `a :< b` or
/// `P {a, b}`.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum PatSynLhs {
    Prefix {
        name: String,
        args: Vec<String>,
    },
    Infix {
        left: String,
        op: String,
        right: String,
    },
    Record {
        name: String,
        fields: Vec<String>,
    },
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum PatSynDir {
    /// `pattern P <- p`
    Unidirectional,
    /// `pattern P = p`
    Implicit,
    /// `pattern P <- p where P = e`
    Explicit(Vec<Decl>),
}

// --- Patterns --------------------------------------------------------------

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Pat {
    Var(String),
    Wildcard,
    /// A literal; `negative` for `-1`.
    Lit {
        lit: Literal,
        negative: bool,
    },
    /// `C p1 p2`; a bare constructor has no arguments.
    Con {
        name: QName,
        args: Vec<Pat>,
    },
    /// `p1 op p2 op p3`, in source order.
    Infix {
        first: Box<Pat>,
        rest: Vec<(QName, Pat)>,
    },
    /// `C { f = p, g }`; `wildcard` for `..`.
    Record {
        name: QName,
        fields: Vec<FieldPat>,
        wildcard: bool,
    },
    Tuple(Vec<Pat>),
    List(Vec<Pat>),
    Paren(Box<Pat>),
    /// `x@p`
    As(String, Box<Pat>),
    /// `~p`
    Lazy(Box<Pat>),
    /// `!p`
    Bang(Box<Pat>),
    /// `(e -> p)`, without the parentheses.
    View(Box<Expr>, Box<Pat>),
    /// `p :: t`, without the parentheses.
    Sig(Box<Pat>, Type),
    /// `@t` in a constructor pattern (`TypeAbstractions`).
    TypeArg(Type),
}

/// A field in a record pattern: `f = p`, or a pun `f`.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FieldPat {
    pub name: QName,
    pub pat: Option<Pat>,
}

// --- Expressions -----------------------------------------------------------

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Expr {
    Var(QName),
    Con(QName),
    Lit(Literal),
    /// `f x`
    App(Box<Expr>, Box<Expr>),
    /// `f @t`
    TypeApp(Box<Expr>, Type),
    /// `e1 op e2 op e3`, in source order. Operators keep their spelling;
    /// the printer adds backquotes to alphanumeric ones.
    Infix {
        first: Box<Expr>,
        rest: Vec<(QName, Expr)>,
    },
    /// `-e`
    Neg(Box<Expr>),
    /// `\p1 p2 -> e`
    Lambda(Vec<Pat>, Box<Expr>),
    /// `\case { alts }`
    LambdaCase(Vec<Alt>),
    /// `let decls in e`
    Let(Vec<Decl>, Box<Expr>),
    /// `if c then t else e`
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    /// `case e of { alts }`
    Case(Box<Expr>, Vec<Alt>),
    /// `do { stmts }`
    Do(Vec<Stmt>),
    Tuple(Vec<Expr>),
    /// `(, e)` or `(e ,)`: a missing component is `None`.
    TupleSection(Vec<Option<Expr>>),
    List(Vec<Expr>),
    /// `[from, then .. to]`
    ArithSeq {
        from: Box<Expr>,
        then: Option<Box<Expr>>,
        to: Option<Box<Expr>>,
    },
    /// `[e | stmts]`
    ListComp(Box<Expr>, Vec<Stmt>),
    Paren(Box<Expr>),
    /// `(e op)`, without the parentheses.
    LeftSection(Box<Expr>, QName),
    /// `(op e)`, without the parentheses.
    RightSection(QName, Box<Expr>),
    /// `C { f = e, g }`
    RecordCon {
        name: QName,
        fields: Vec<FieldUpdate>,
        wildcard: bool,
    },
    /// `e { f = e }`
    RecordUpdate(Box<Expr>, Vec<FieldUpdate>),
    /// `e :: t`
    Sig(Box<Expr>, Type),
    /// `_`, a typed hole.
    Hole,
}

/// A field in a record construction or update: `f = e`, or a pun `f`.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FieldUpdate {
    pub name: QName,
    pub value: Option<Expr>,
}

/// A case alternative.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Alt {
    pub pos: Pos,
    pub pat: Pat,
    pub rhs: Rhs,
}

/// A statement in a `do` block, a list comprehension or a guard.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Stmt {
    /// `p <- e`
    Bind(Pat, Expr),
    /// `let decls`
    Let(Vec<Decl>),
    /// `e`
    Expr(Expr),
}
