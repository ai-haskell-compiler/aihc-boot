//! The printer: a syntax tree back to Haskell source.
//!
//! The output uses explicit braces and semicolons, so the layout rule
//! plays no part in reading it back. The printer keeps every token of the
//! tree in source order, which is what the round trip through aihc-parser
//! checks: its tree for the printed source must equal its tree for the
//! original.

// The printer is one `match` arm per tree node. Long functions are the
// natural shape here.
#![allow(clippy::too_many_lines)]

use crate::ast::{
    Alt, Assoc, BangType, ConBody, ConDecl, Decl, DerivStrategy, Deriving, Export, Expr, FieldPat,
    FieldUpdate, GuardedRhs, Import, ImportItem, Lhs, Literal, Module, Pat, PatSynDir, PatSynLhs,
    QName, Qualified, Rhs, RhsBody, Stmt, Subs, TyVarBind, Type,
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
                self.push(&var_name(&QName::unqualified(name.clone())));
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
            Literal::Char { raw, .. } | Literal::String { raw, .. } => self.push(raw),
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
            Decl::TypeFamily {
                name, vars, kind, ..
            } => {
                self.push("type ");
                self.push(name);
                self.binders(vars);
                if let Some(k) = kind {
                    self.push(" :: ");
                    self.ty(k);
                }
            }
            Decl::TypeInstance { lhs, rhs, .. } => {
                self.push("type ");
                self.ty(lhs);
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
            Decl::Bind { lhs, rhs, .. } => {
                self.lhs(lhs);
                self.rhs(rhs, "=");
            }
            Decl::PatSyn { lhs, dir, pat, .. } => {
                self.push("pattern ");
                match lhs {
                    PatSynLhs::Prefix { name, args } => {
                        self.push(name);
                        for a in args {
                            self.push(" ");
                            self.push(a);
                        }
                    }
                    PatSynLhs::Infix { left, op, right } => {
                        self.push(left);
                        self.push(" ");
                        self.push(op);
                        self.push(" ");
                        self.push(right);
                    }
                    PatSynLhs::Record { name, fields } => {
                        self.push(name);
                        self.push(" {");
                        self.push(&fields.join(", "));
                        self.push("}");
                    }
                }
                match dir {
                    PatSynDir::Implicit => {
                        self.push(" = ");
                        self.pat(pat);
                    }
                    PatSynDir::Unidirectional => {
                        self.push(" <- ");
                        self.pat(pat);
                    }
                    PatSynDir::Explicit(decls) => {
                        self.push(" <- ");
                        self.pat(pat);
                        self.push(" where");
                        self.decl_block(decls);
                    }
                }
            }
        }
    }

    /// A `where` block with explicit braces.
    fn body(&mut self, decls: &[Decl]) {
        self.push(" where");
        self.decl_block(decls);
    }

    /// ` { d; d }`
    fn decl_block(&mut self, decls: &[Decl]) {
        self.push(" {");
        for (i, d) in decls.iter().enumerate() {
            if i > 0 {
                self.push(";");
            }
            self.push("\n  ");
            self.decl(d);
        }
        self.push("}");
    }

    // --- Bindings ---------------------------------------------------------

    fn lhs(&mut self, lhs: &Lhs) {
        match lhs {
            Lhs::Fun { name, args } => {
                self.push(&var_name(&QName::unqualified(name.clone())));
                for a in args {
                    self.push(" ");
                    self.pat(a);
                }
            }
            Lhs::Infix {
                left,
                op,
                right,
                args,
            } => {
                if !args.is_empty() {
                    self.push("(");
                }
                self.pat(left);
                self.push(" ");
                self.push(op);
                self.push(" ");
                self.pat(right);
                if !args.is_empty() {
                    self.push(")");
                    for a in args {
                        self.push(" ");
                        self.pat(a);
                    }
                }
            }
            Lhs::Pat(p) => self.pat(p),
        }
    }

    /// `= e` or guards, then `where`. `sep` is `=` or `->`.
    fn rhs(&mut self, rhs: &Rhs, sep: &str) {
        match &rhs.body {
            RhsBody::Plain(e) => {
                self.push(" ");
                self.push(sep);
                self.push(" ");
                self.exp(e);
            }
            RhsBody::Guarded(guards) => {
                for GuardedRhs { guards, body } in guards {
                    self.push(" | ");
                    self.stmts(guards, ", ");
                    self.push(" ");
                    self.push(sep);
                    self.push(" ");
                    self.exp(body);
                }
            }
        }
        if !rhs.wheres.is_empty() {
            self.push(" where");
            self.decl_block(&rhs.wheres);
        }
    }

    fn stmts(&mut self, stmts: &[Stmt], sep: &str) {
        for (i, s) in stmts.iter().enumerate() {
            if i > 0 {
                self.push(sep);
            }
            self.stmt(s);
        }
    }

    fn stmt(&mut self, s: &Stmt) {
        match s {
            Stmt::Bind(p, e) => {
                self.pat(p);
                self.push(" <- ");
                self.exp(e);
            }
            Stmt::Let(decls) => {
                self.push("let");
                self.decl_block(decls);
            }
            Stmt::Expr(e) => self.exp(e),
        }
    }

    // --- Patterns ---------------------------------------------------------

    fn pat(&mut self, p: &Pat) {
        match p {
            Pat::Var(v) => self.push(&var_name(&QName::unqualified(v.clone()))),
            Pat::Wildcard => self.push("_"),
            Pat::Lit { lit, negative } => {
                if *negative {
                    self.push("-");
                }
                self.literal(lit);
            }
            Pat::Con { name, args } => {
                self.push(&con_name(name));
                for a in args {
                    self.push(" ");
                    self.pat(a);
                }
            }
            Pat::Infix { first, rest } => {
                self.pat(first);
                for (op, p) in rest {
                    self.push(" ");
                    self.push(&op_name(op));
                    self.push(" ");
                    self.pat(p);
                }
            }
            Pat::Record {
                name,
                fields,
                wildcard,
            } => {
                self.push(&con_name(name));
                self.push(" {");
                for (i, FieldPat { name, pat }) in fields.iter().enumerate() {
                    if i > 0 {
                        self.push(", ");
                    }
                    self.push(&var_name(name));
                    if let Some(p) = pat {
                        self.push(" = ");
                        self.pat(p);
                    }
                }
                if *wildcard {
                    if !fields.is_empty() {
                        self.push(", ");
                    }
                    self.push("..");
                }
                self.push("}");
            }
            Pat::Tuple(items) => {
                self.push("(");
                for (i, p) in items.iter().enumerate() {
                    if i > 0 {
                        self.push(", ");
                    }
                    self.pat(p);
                }
                self.push(")");
            }
            Pat::List(items) => {
                self.push("[");
                for (i, p) in items.iter().enumerate() {
                    if i > 0 {
                        self.push(", ");
                    }
                    self.pat(p);
                }
                self.push("]");
            }
            Pat::Paren(p) => {
                self.push("(");
                self.pat(p);
                self.push(")");
            }
            Pat::As(v, p) => {
                self.push(v);
                self.push("@");
                self.pat(p);
            }
            Pat::Lazy(p) => {
                self.push("~");
                self.pat(p);
            }
            Pat::Bang(p) => {
                self.push("!");
                self.pat(p);
            }
            Pat::View(e, p) => {
                self.exp(e);
                self.push(" -> ");
                self.pat(p);
            }
            Pat::Sig(p, t) => {
                self.pat(p);
                self.push(" :: ");
                self.ty(t);
            }
            Pat::TypeArg(t) => {
                self.push("@");
                self.ty(t);
            }
        }
    }

    // --- Expressions ------------------------------------------------------

    fn exp(&mut self, e: &Expr) {
        match e {
            Expr::Var(name) => self.push(&var_name(name)),
            Expr::Con(name) => self.push(&con_name(name)),
            Expr::Lit(lit) => self.literal(lit),
            Expr::Hole => self.push("_"),
            Expr::App(f, a) => {
                self.exp(f);
                self.push(" ");
                self.exp(a);
            }
            Expr::TypeApp(f, t) => {
                self.exp(f);
                self.push(" @");
                self.ty(t);
            }
            Expr::Infix { first, rest } => {
                self.exp(first);
                for (op, e) in rest {
                    self.push(" ");
                    self.push(&op_name(op));
                    self.push(" ");
                    self.exp(e);
                }
            }
            Expr::Neg(e) => {
                self.push("-");
                self.exp(e);
            }
            Expr::Lambda(pats, body) => {
                self.push("\\");
                for p in pats {
                    self.pat(p);
                    self.push(" ");
                }
                self.push("-> ");
                self.exp(body);
            }
            Expr::LambdaCase(alts) => {
                self.push("\\case");
                self.alts(alts);
            }
            Expr::Let(decls, body) => {
                self.push("let");
                self.decl_block(decls);
                self.push(" in ");
                self.exp(body);
            }
            Expr::If(c, t, e) => {
                self.push("if ");
                self.exp(c);
                self.push(" then ");
                self.exp(t);
                self.push(" else ");
                self.exp(e);
            }
            Expr::Case(scrutinee, alts) => {
                self.push("case ");
                self.exp(scrutinee);
                self.push(" of");
                self.alts(alts);
            }
            Expr::Do(stmts) => {
                self.push("do {");
                for (i, s) in stmts.iter().enumerate() {
                    if i > 0 {
                        self.push(";");
                    }
                    self.push("\n  ");
                    self.stmt(s);
                }
                self.push("}");
            }
            Expr::Tuple(items) => {
                self.push("(");
                self.exps(items);
                self.push(")");
            }
            Expr::TupleSection(items) => {
                self.push("(");
                for (i, e) in items.iter().enumerate() {
                    if i > 0 {
                        self.push(",");
                    }
                    if let Some(e) = e {
                        self.exp(e);
                    }
                }
                self.push(")");
            }
            Expr::List(items) => {
                self.push("[");
                self.exps(items);
                self.push("]");
            }
            Expr::ArithSeq { from, then, to } => {
                self.push("[");
                self.exp(from);
                if let Some(t) = then {
                    self.push(", ");
                    self.exp(t);
                }
                self.push(" ..");
                if let Some(t) = to {
                    self.push(" ");
                    self.exp(t);
                }
                self.push("]");
            }
            Expr::ListComp(e, stmts) => {
                self.push("[");
                self.exp(e);
                self.push(" | ");
                self.stmts(stmts, ", ");
                self.push("]");
            }
            Expr::Paren(e) => {
                self.push("(");
                self.exp(e);
                self.push(")");
            }
            Expr::LeftSection(e, op) => {
                self.push("(");
                self.exp(e);
                self.push(" ");
                self.push(&op_name(op));
                self.push(")");
            }
            Expr::RightSection(op, e) => {
                self.push("(");
                self.push(&op_name(op));
                self.push(" ");
                self.exp(e);
                self.push(")");
            }
            Expr::RecordCon {
                name,
                fields,
                wildcard,
            } => {
                self.push(&con_name(name));
                self.field_updates(fields, *wildcard);
            }
            Expr::RecordUpdate(e, fields) => {
                self.exp(e);
                self.field_updates(fields, false);
            }
            Expr::Sig(e, t) => {
                self.exp(e);
                self.push(" :: ");
                self.ty(t);
            }
        }
    }

    fn exps(&mut self, items: &[Expr]) {
        for (i, e) in items.iter().enumerate() {
            if i > 0 {
                self.push(", ");
            }
            self.exp(e);
        }
    }

    fn field_updates(&mut self, fields: &[FieldUpdate], wildcard: bool) {
        self.push(" {");
        for (i, FieldUpdate { name, value }) in fields.iter().enumerate() {
            if i > 0 {
                self.push(", ");
            }
            self.push(&var_name(name));
            if let Some(e) = value {
                self.push(" = ");
                self.exp(e);
            }
        }
        if wildcard {
            if !fields.is_empty() {
                self.push(", ");
            }
            self.push("..");
        }
        self.push("}");
    }

    fn alts(&mut self, alts: &[Alt]) {
        self.push(" {");
        for (i, alt) in alts.iter().enumerate() {
            if i > 0 {
                self.push(";");
            }
            self.push("\n  ");
            self.pat(&alt.pat);
            self.rhs(&alt.rhs, "->");
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
             import \"pkg\" E\n}\n"
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
             data T a = C !Int (Maybe a) | a :+ a | R { f, g :: !a, h :: Int -> Int } deriving (Show, Eq) deriving stock Ord;\n\
             newtype N = N Int deriving newtype (Num);\n\
             type S a = (a, [a]);\n\
             class Eq a => C a where {\n  m :: a -> Int;\n  default m :: Show a => a -> Int};\n\
             instance C Int where {};\n\
             pattern P :: Int\n}\n"
        );
    }

    #[test]
    fn expressions() {
        let printed = roundtrip(
            "module M where\n\
             f x (Just y) = g x + -1 `div` z where { z = 2 }\n\
             g = \\x -> case x of\n  A | x > 0, Just q <- h -> q\n    | otherwise -> 0\n  _ -> do\n    a <- m\n    let b = a\n    if b then pure b else pure a\n\
             h = [y | x <- xs, let y = x, odd y] ++ [1, 3 .. 9] ++ [1 ..]\n\
             i = (subtract 1) . (`div` 2) . (+ 1) $ r { a = 1 } :: Int\n\
             j = (,1) (1,) (\\case { A -> 1 }) f @Int (- 1) M.x (M.+) ((:)) []\n",
        );
        assert_eq!(
            printed,
            "module M where {\n\
             f x (Just y) = g x + -1 `div` z where {\n  z = 2};\n\
             g = \\x -> case x of {\n  A | x > 0, Just q <- h -> q | otherwise -> 0;\n  _ -> do {\n  a <- m;\n  let {\n  b = a};\n  if b then pure b else pure a}};\n\
             h = [y | x <- xs, let {\n  y = x}, odd y] ++ [1, 3 .. 9] ++ [1 ..];\n\
             i = (subtract 1) . (`div` 2) . (+ 1) $ r {a = 1} :: Int;\n\
             j = (,1) (1,) (\\case {\n  A -> 1}) f @Int (-1) M.x (M.+) ((:)) []\n}\n"
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
