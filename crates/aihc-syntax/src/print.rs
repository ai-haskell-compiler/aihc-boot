//! The printer: a syntax tree back to Haskell source.
//!
//! The output uses explicit braces and semicolons, so the layout rule
//! plays no part in reading it back. The printer keeps every token of the
//! tree in source order, which is what the round trip through GHC checks:
//! GHC's tree for the printed source must equal GHC's tree for the
//! original.

use crate::ast::{
    Assoc, BangType, ConBody, ConDecl, Decl, DerivStrategy, Deriving, Export, Import, ImportItem,
    Literal, Module, QName, Qualified, Subs, TyVarBind, Type,
};
use std::fmt::Write;

/// Print a module as source text.
pub fn print_module(module: &Module) -> String {
    let mut p = Printer { out: String::new() };
    p.module(module);
    p.out
}

struct Printer {
    out: String,
}

/// Whether a name is an operator, which needs parentheses where a
/// variable is expected and none where an operator is expected.
fn is_symbolic(name: &str) -> bool {
    name.chars()
        .next()
        .is_some_and(|c| !c.is_alphanumeric() && c != '_' && c != '(' && c != '[')
}

/// A name in a position that expects a variable or constructor.
fn var_name(name: &QName) -> String {
    let n = if is_symbolic(&name.name) {
        format!("({})", name.name)
    } else {
        name.name.clone()
    };
    qualify(&name.qual, &n)
}

/// A name in a position that expects an operator.
fn op_name(name: &QName) -> String {
    let n = if is_symbolic(&name.name) {
        name.name.clone()
    } else {
        format!("`{}`", name.name)
    };
    qualify(&name.qual, &n)
}

fn qualify(qual: &str, name: &str) -> String {
    if qual.is_empty() {
        name.to_string()
    } else if let Some(op) = name.strip_prefix('(') {
        format!("({qual}.{op}")
    } else if let Some(op) = name.strip_prefix('`') {
        format!("`{qual}.{op}")
    } else {
        format!("{qual}.{name}")
    }
}

/// A string literal in Haskell syntax.
pub fn string_literal(s: &str) -> String {
    let mut out = String::from("\"");
    let mut prev_numeric = false;
    for c in s.chars() {
        let numeric = escape_char(c, '"', &mut out, prev_numeric);
        prev_numeric = numeric;
    }
    out.push('"');
    out
}

/// A character literal in Haskell syntax.
pub fn char_literal(c: char) -> String {
    let mut out = String::from("'");
    escape_char(c, '\'', &mut out, false);
    out.push('\'');
    out
}

/// Append one character, escaped for a literal delimited by `quote`.
/// Returns whether the escape was numeric, so that a following digit
/// needs `\&`.
fn escape_char(c: char, quote: char, out: &mut String, prev_numeric: bool) -> bool {
    if prev_numeric && c.is_ascii_digit() {
        out.push_str("\\&");
    }
    match c {
        '\\' => out.push_str("\\\\"),
        '\n' => out.push_str("\\n"),
        '\t' => out.push_str("\\t"),
        '\r' => out.push_str("\\r"),
        c if c == quote => {
            out.push('\\');
            out.push(c);
        }
        c if (' '..='~').contains(&c) => out.push(c),
        c => {
            write!(out, "\\{}", c as u32).unwrap();
            return true;
        }
    }
    false
}

impl Printer {
    fn push(&mut self, s: &str) {
        self.out.push_str(s);
    }

    fn module(&mut self, m: &Module) {
        for pragma in &m.pragmas {
            writeln!(self.out, "{{-# {pragma} #-}}").unwrap();
        }
        self.push("module ");
        self.push(&m.name);
        if let Some(exports) = &m.exports {
            self.push(" (");
            for (i, e) in exports.iter().enumerate() {
                if i > 0 {
                    self.push(", ");
                }
                self.export(e);
            }
            self.push(")");
        }
        self.push(" where {\n");
        let mut first = true;
        for import in &m.imports {
            if !first {
                self.push(";\n");
            }
            first = false;
            self.import(import);
        }
        for decl in &m.decls {
            if !first {
                self.push(";\n");
            }
            first = false;
            self.decl(decl);
        }
        self.push("\n}\n");
    }

    fn export(&mut self, e: &Export) {
        match e {
            Export::Var(name) => self.push(&var_name(name)),
            Export::Thing {
                name,
                subs,
                explicit_type,
            } => {
                if *explicit_type {
                    self.push("type ");
                }
                self.push(&var_name(name));
                self.subs(subs);
            }
            Export::Pattern(name) => {
                self.push("pattern ");
                self.push(&var_name(name));
            }
            Export::Module(name) => {
                self.push("module ");
                self.push(name);
            }
        }
    }

    fn subs(&mut self, subs: &Subs) {
        match subs {
            Subs::None => {}
            Subs::All => self.push("(..)"),
            Subs::Some(names) => {
                self.push("(");
                self.names(names);
                self.push(")");
            }
            Subs::AllAnd(names) => {
                self.push("(.., ");
                self.names(names);
                self.push(")");
            }
        }
    }

    fn names(&mut self, names: &[String]) {
        for (i, n) in names.iter().enumerate() {
            if i > 0 {
                self.push(", ");
            }
            self.push(&var_name(&QName::unqualified(n.clone())));
        }
    }

    fn import(&mut self, i: &Import) {
        self.push("import ");
        if i.source {
            self.push("{-# SOURCE #-} ");
        }
        if i.qualified == Qualified::Pre {
            self.push("qualified ");
        }
        if let Some(pkg) = &i.package {
            self.push(&string_literal(pkg));
            self.push(" ");
        }
        self.push(&i.module);
        if i.qualified == Qualified::Post {
            self.push(" qualified");
        }
        if let Some(alias) = &i.alias {
            self.push(" as ");
            self.push(alias);
        }
        if i.hiding {
            self.push(" hiding");
        }
        if let Some(items) = &i.items {
            self.push(" (");
            for (k, item) in items.iter().enumerate() {
                if k > 0 {
                    self.push(", ");
                }
                self.import_item(item);
            }
            self.push(")");
        }
    }

    fn import_item(&mut self, item: &ImportItem) {
        match item {
            ImportItem::Var(name) => self.push(&var_name(&QName::unqualified(name.clone()))),
            ImportItem::Thing {
                name,
                subs,
                explicit_type,
            } => {
                if *explicit_type {
                    self.push("type ");
                }
                self.push(&var_name(&QName::unqualified(name.clone())));
                self.subs(subs);
            }
            ImportItem::Pattern(name) => {
                self.push("pattern ");
                self.push(name);
            }
        }
    }

    // --- Types ------------------------------------------------------------

    fn ty(&mut self, t: &Type) {
        match t {
            Type::Var(v) => self.push(v),
            Type::Con(name) => self.push(&con_name(name)),
            Type::App(f, a) => {
                self.ty(f);
                self.push(" ");
                self.ty(a);
            }
            Type::AppKind(f, k) => {
                self.ty(f);
                self.push(" @");
                self.ty(k);
            }
            Type::Fun(a, b) => {
                self.ty(a);
                self.push(" -> ");
                self.ty(b);
            }
            Type::Op(a, op, b) => {
                self.ty(a);
                self.push(" ");
                self.push(&op_name(op));
                self.push(" ");
                self.ty(b);
            }
            Type::List(t) => {
                self.push("[");
                self.ty(t);
                self.push("]");
            }
            Type::Tuple(items) => {
                self.push("(");
                self.types(items, ", ");
                self.push(")");
            }
            Type::Paren(t) => {
                self.push("(");
                self.ty(t);
                self.push(")");
            }
            Type::Forall(binders, t) => {
                self.push("forall");
                self.binders(binders);
                self.push(". ");
                self.ty(t);
            }
            Type::Qual(ctx, t) => {
                self.ty(ctx);
                self.push(" => ");
                self.ty(t);
            }
            Type::KindSig(t, k) => {
                self.ty(t);
                self.push(" :: ");
                self.ty(k);
            }
            Type::Promoted(t) => {
                self.push("'");
                self.ty(t);
            }
            Type::PromotedList(items) => {
                self.push("'[");
                self.types(items, ", ");
                self.push("]");
            }
            Type::Lit(lit) => self.literal(lit),
            Type::Wildcard => self.push("_"),
        }
    }

    fn types(&mut self, items: &[Type], sep: &str) {
        for (i, t) in items.iter().enumerate() {
            if i > 0 {
                self.push(sep);
            }
            self.ty(t);
        }
    }

    fn literal(&mut self, lit: &Literal) {
        match lit {
            Literal::Integer(s) | Literal::Float(s) => self.push(s),
            Literal::Char(c) => self.push(&char_literal(*c)),
            Literal::String(s) => self.push(&string_literal(s)),
        }
    }

    /// Binders with a leading space each, so they follow a keyword.
    fn binders(&mut self, binders: &[TyVarBind]) {
        for b in binders {
            self.push(" ");
            match &b.kind {
                None => self.push(&b.name),
                Some(k) => {
                    self.push("(");
                    self.push(&b.name);
                    self.push(" :: ");
                    self.ty(k);
                    self.push(")");
                }
            }
        }
    }

    fn bang_type(&mut self, bt: &BangType) {
        match bt.unpack {
            Some(true) => self.push("{-# UNPACK #-} "),
            Some(false) => self.push("{-# NOUNPACK #-} "),
            None => {}
        }
        match bt.strict {
            Some(true) => self.push("!"),
            Some(false) => self.push("~"),
            None => {}
        }
        self.ty(&bt.ty);
    }

    // --- Declarations -----------------------------------------------------

    fn decl(&mut self, d: &Decl) {
        match d {
            Decl::TypeSig { names, ty, .. } => {
                self.names(names);
                self.push(" :: ");
                self.ty(ty);
            }
            Decl::DefaultSig { name, ty, .. } => {
                self.push("default ");
                self.push(&var_name(&QName::unqualified(name.clone())));
                self.push(" :: ");
                self.ty(ty);
            }
            Decl::PatSynSig { names, ty, .. } => {
                self.push("pattern ");
                self.push(&names.join(", "));
                self.push(" :: ");
                self.ty(ty);
            }
            Decl::Fixity {
                assoc, prec, ops, ..
            } => {
                self.push(match assoc {
                    Assoc::Left => "infixl",
                    Assoc::Right => "infixr",
                    Assoc::None => "infix",
                });
                if let Some(p) = prec {
                    self.push(" ");
                    self.push(p);
                }
                self.push(" ");
                self.push(&ops.join(", "));
            }
            Decl::Data {
                newtype,
                name,
                vars,
                kind,
                cons,
                deriving,
                ..
            } => {
                self.push(if *newtype { "newtype " } else { "data " });
                self.push(name);
                self.binders(vars);
                if let Some(k) = kind {
                    self.push(" :: ");
                    self.ty(k);
                }
                for (i, c) in cons.iter().enumerate() {
                    self.push(if i == 0 { " = " } else { " | " });
                    self.con_decl(c);
                }
                for d in deriving {
                    self.deriving(d);
                }
            }
            Decl::TypeSyn {
                name, vars, rhs, ..
            } => {
                self.push("type ");
                self.push(name);
                self.binders(vars);
                self.push(" = ");
                self.ty(rhs);
            }
            Decl::Class {
                ctx,
                name,
                vars,
                body,
                ..
            } => {
                self.push("class ");
                if let Some(ctx) = ctx {
                    self.ty(ctx);
                    self.push(" => ");
                }
                self.push(name);
                self.binders(vars);
                self.body(body);
            }
            Decl::Instance { head, body, .. } => {
                self.push("instance ");
                self.ty(head);
                self.body(body);
            }
            Decl::Pragma { text, .. } => {
                self.push("{-# ");
                self.push(text);
                self.push(" #-}");
            }
        }
    }

    /// A `where` block with explicit braces.
    fn body(&mut self, decls: &[Decl]) {
        self.push(" where {");
        for (i, d) in decls.iter().enumerate() {
            if i > 0 {
                self.push(";");
            }
            self.push("\n  ");
            self.decl(d);
        }
        self.push("}");
    }

    fn con_decl(&mut self, c: &ConDecl) {
        if !c.forall.is_empty() {
            self.push("forall");
            self.binders(&c.forall);
            self.push(". ");
        }
        if let Some(ctx) = &c.ctx {
            self.ty(ctx);
            self.push(" => ");
        }
        match &c.body {
            ConBody::Prefix { name, args } => {
                self.push(name);
                for a in args {
                    self.push(" ");
                    self.bang_type(a);
                }
            }
            ConBody::Infix { left, op, right } => {
                self.bang_type(left);
                self.push(" ");
                self.push(op);
                self.push(" ");
                self.bang_type(right);
            }
            ConBody::Record { name, fields } => {
                self.push(name);
                self.push(" { ");
                for (i, f) in fields.iter().enumerate() {
                    if i > 0 {
                        self.push(", ");
                    }
                    self.names(&f.names);
                    self.push(" :: ");
                    self.bang_type(&f.ty);
                }
                self.push(" }");
            }
        }
    }

    fn deriving(&mut self, d: &Deriving) {
        self.push(" deriving ");
        match d.strategy {
            Some(DerivStrategy::Stock) => self.push("stock "),
            Some(DerivStrategy::Anyclass) => self.push("anyclass "),
            Some(DerivStrategy::Newtype) => self.push("newtype "),
            None => {}
        }
        if d.parens {
            self.push("(");
            self.types(&d.classes, ", ");
            self.push(")");
        } else {
            self.types(&d.classes, ", ");
        }
        if let Some(via) = &d.via {
            self.push(" via ");
            self.ty(via);
        }
    }
}

/// A type constructor name. The special names `()`, `[]`, `(,)` and
/// `(->)` keep their spelling; operators get parentheses.
fn con_name(name: &QName) -> String {
    match name.name.as_str() {
        "()" | "[]" => name.name.clone(),
        "->" => "(->)".to_string(),
        n if n.starts_with("(,") => n.to_string(),
        _ => var_name(name),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse;

    /// Parse, print, parse again: the trees must be equal.
    fn roundtrip(src: &str) -> String {
        let m1 = parse(src).unwrap();
        let printed = print_module(&m1);
        let m2 = parse(&printed).unwrap_or_else(|e| panic!("{e}\n{printed}"));
        // Positions differ, so compare the printed forms.
        assert_eq!(print_module(&m2), printed);
        printed
    }

    #[test]
    fn header_and_imports() {
        let printed = roundtrip(
            "{-# LANGUAGE CPP #-}\nmodule M (a, (+), T(..), U(V, w), pattern P, type (:+), module N, S(.., Q)) where\n\
             import qualified A as B\nimport C qualified\nimport D hiding (x, Y(..))\nimport {-# SOURCE #-} \"pkg\" E\n",
        );
        assert_eq!(
            printed,
            "{-# LANGUAGE CPP #-}\n\
             module M (a, (+), T(..), U(V, w), pattern P, type (:+), module N, S(.., Q)) where {\n\
             import qualified A as B;\n\
             import C qualified;\n\
             import D hiding (x, Y(..));\n\
             import {-# SOURCE #-} \"pkg\" E\n}\n"
        );
    }

    #[test]
    fn declarations() {
        let printed = roundtrip(
            "module M where\n\
             f, (<+>) :: forall a. (Eq a, Show a) => [a] -> (a, M.T @k) -> 'Just '[a] -> (t :: k) -> \"s\" -> a `E` b -> (a ~ b) -> (->) a b\n\
             infixl 6 <+>, `plus`\n\
             data T a = C !Int (Maybe a) | a :+ a | R { f, g :: {-# UNPACK #-} !a, h :: Int -> Int }\n  deriving (Show, Eq)\n  deriving stock Ord\n\
             newtype N = N Int deriving newtype (Num)\n\
             type S a = (a, [a])\n\
             class Eq a => C a where\n  m :: a -> Int\n  default m :: Show a => a -> Int\n  {-# MINIMAL m #-}\n\
             instance C Int\n\
             {-# INLINE f #-}\n\
             pattern P :: Int\n",
        );
        assert_eq!(
            printed,
            "module M where {\n\
             f, (<+>) :: forall a. (Eq a, Show a) => [a] -> (a, M.T @k) -> 'Just '[a] -> (t :: k) -> \"s\" -> a `E` b -> (a ~ b) -> (->) a b;\n\
             infixl 6 <+>, `plus`;\n\
             data T a = C !Int (Maybe a) | a :+ a | R { f, g :: {-# UNPACK #-} !a, h :: Int -> Int } deriving (Show, Eq) deriving stock Ord;\n\
             newtype N = N Int deriving newtype (Num);\n\
             type S a = (a, [a]);\n\
             class Eq a => C a where {\n  m :: a -> Int;\n  default m :: Show a => a -> Int;\n  {-# MINIMAL m #-}};\n\
             instance C Int where {};\n\
             {-# INLINE f #-};\n\
             pattern P :: Int\n}\n"
        );
    }

    #[test]
    fn literals() {
        assert_eq!(
            string_literal("a\"b\\c\n\u{4d2}5\u{7f}"),
            "\"a\\\"b\\\\c\\n\\1234\\&5\\127\""
        );
        assert_eq!(char_literal('\''), "'\\''");
        assert_eq!(char_literal('x'), "'x'");
    }
}
