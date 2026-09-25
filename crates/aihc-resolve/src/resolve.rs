//! The resolution of one module: a record for every identifier
//! occurrence.
//!
//! The records must agree with aihc-resolve, so the spans follow its rules:
//!
//! - A use of a name, and a variable that a pattern binds, has the span of
//!   the name as written.
//! - A name that a declaration defines starts at the start of the
//!   declaration, or after its keyword (`data `, `newtype `, `class `), and
//!   is as wide as the name without parentheses. A constructor starts at
//!   the start of its constructor declaration.
//! - A type operator in parentheses has the span of the parentheses.
//! - Built-in syntax, such as `()` or `[a]`, is a `syntax` record with the
//!   span of the whole form.
//! - Export lists, import lists, fixity declarations, type variables, the
//!   field names of a declaration, and `[]` and tuples in patterns have no
//!   records.
//!
//! The resolver refuses the constructs that it does not handle yet, with
//! the position of the construct, rather than give records that can be
//! wrong.

use aihc_syntax::ast::{
    Alt, ConBody, Decl, Expr, Ident, Lhs, Literal, Pat, QName, Rhs, RhsBody, Stmt, Type,
};
use aihc_syntax::Pos;

use crate::record::{Namespace, Occurrence, Span, Target};
use crate::scope::{bare, unsupported, Entity, Program, Scope, Space};
use crate::ResolveError;

type Result<T> = std::result::Result<T, ResolveError>;

/// Every identifier occurrence of a module, sorted by span.
pub fn resolve(program: &Program, path: &str) -> Result<Vec<Occurrence>> {
    let Some((_, module)) = program.module_at(path) else {
        return Err(ResolveError {
            message: format!("resolve: no module at {path}"),
            pos: None,
        });
    };
    let scope = program.scope(&module.name)?;
    let mut resolver = Resolver {
        program,
        scope,
        locals: Vec::new(),
        next_local: 0,
        out: Vec::new(),
    };
    for decl in &module.decls {
        resolver.top_decl(decl)?;
    }
    let mut out = resolver.out;
    out.sort_by_key(|o| o.span);
    Ok(out)
}

struct Resolver<'a> {
    program: &'a Program,
    /// The top-level scope of the module.
    scope: &'a Scope,
    /// The local term names in scope, innermost last.
    locals: Vec<(String, u32)>,
    next_local: u32,
    out: Vec<Occurrence>,
}

/// The span of a name as wide as `name`, from `start`.
fn narrow(start: Pos, name: &str) -> Span {
    let width = u32::try_from(name.chars().count()).unwrap_or(u32::MAX);
    Span {
        start_line: start.line,
        start_col: start.col,
        end_line: start.line,
        end_col: start.col.saturating_add(width),
    }
}

/// `start` moved to the right by the width of `keyword`.
fn after(start: Pos, keyword: &str) -> Pos {
    let width = u32::try_from(keyword.chars().count()).unwrap_or(u32::MAX);
    Pos {
        line: start.line,
        col: start.col.saturating_add(width),
    }
}

fn span(s: aihc_syntax::ast::Span) -> Span {
    Span {
        start_line: s.start.line,
        start_col: s.start.col,
        end_line: s.end.line,
        end_col: s.end.col,
    }
}

/// Whether a constructor name is built-in syntax: `()`, `[]` or a tuple.
fn is_builtin_con(name: &str) -> bool {
    name == "()" || name == "[]" || name.starts_with("(,")
}

impl Resolver<'_> {
    fn emit(&mut self, span: Span, namespace: Namespace, target: Target) {
        self.out.push(Occurrence {
            span,
            namespace,
            target,
        });
    }

    fn lookup(&self, space: Space, name: &QName) -> Target {
        if space == Space::Term && name.qual.is_empty() {
            if let Some((_, id)) = self.locals.iter().rev().find(|(n, _)| *n == name.name) {
                return Target::Local(*id);
            }
        }
        let found = self
            .scope
            .qualifier(&name.qual)
            .and_then(|scope| match space {
                Space::Term => scope.terms.get(&name.name),
                Space::Type => scope.types.get(&name.name),
            });
        match found {
            Some(entity) => entity.target(),
            None => Target::Error("unbound".into()),
        }
    }

    fn lookup_top(&self, space: Space, name: &str) -> Target {
        self.lookup(space, &QName::unqualified(name))
    }

    /// A new local binder for `name`, in scope until the caller truncates
    /// the local scope.
    fn bind_local(&mut self, name: &str) -> u32 {
        let id = self.next_local;
        self.next_local += 1;
        self.locals.push((name.to_string(), id));
        id
    }

    // --- Declarations ------------------------------------------------------

    fn top_decl(&mut self, decl: &Decl) -> Result<()> {
        match decl {
            Decl::TypeSig { pos, names, ty } => {
                for name in names {
                    let target = self.lookup_top(Space::Term, name);
                    self.emit(narrow(*pos, name), Namespace::Term, target);
                }
                self.ty(ty)
            }
            Decl::Fixity { .. } => Ok(()),
            Decl::Bind { pos, lhs, rhs } => self.top_bind(*pos, lhs, rhs, None, false),
            Decl::Data { .. } => self.data_decl(decl),
            Decl::TypeSyn {
                pos,
                name,
                vars,
                rhs,
            } => {
                let target = self.lookup_top(Space::Type, name);
                self.emit(narrow(after(*pos, "type "), name), Namespace::Type, target);
                self.binder_kinds(vars)?;
                self.ty(rhs)
            }
            Decl::TypeFamily {
                pos,
                name,
                vars,
                kind,
            } => {
                // The record spans the head: the name and its variables.
                let mut end = name.span.end;
                for var in vars {
                    if var.kind.is_some() {
                        return Err(unsupported("a kinded type family parameter", Some(*pos)));
                    }
                    end = var.name.span.end;
                }
                let head = aihc_syntax::ast::Span {
                    start: name.span.start,
                    end,
                };
                let target = self.lookup_top(Space::Type, name);
                self.emit(span(head), Namespace::Type, target);
                if let Some(kind) = kind {
                    self.ty(kind)?;
                }
                Ok(())
            }
            Decl::Class {
                pos,
                ctx,
                name,
                vars,
                body,
            } => {
                let target = self.lookup_top(Space::Type, name);
                self.emit(narrow(after(*pos, "class "), name), Namespace::Type, target);
                if let Some(ctx) = ctx {
                    self.context(ctx)?;
                }
                self.binder_kinds(vars)?;
                for item in body {
                    match item {
                        Decl::TypeSig { .. } | Decl::TypeFamily { .. } => self.top_decl(item)?,
                        Decl::DefaultSig { ty, .. } => self.ty(ty)?,
                        Decl::Bind { pos, lhs, rhs } => {
                            self.top_bind(*pos, lhs, rhs, None, true)?;
                        }
                        _ => return Err(unsupported("this class item", Some(item.pos()))),
                    }
                }
                Ok(())
            }
            Decl::Instance { pos, head, body } => {
                let class = self.instance_head(head, *pos)?;
                for item in body {
                    match item {
                        Decl::Bind { pos, lhs, rhs } => {
                            self.top_bind(*pos, lhs, rhs, class.as_ref(), true)?;
                        }
                        Decl::TypeSig { ty, .. } => self.ty(ty)?,
                        _ => return Err(unsupported("this instance item", Some(item.pos()))),
                    }
                }
                Ok(())
            }
            _ => Err(unsupported("this declaration", Some(decl.pos()))),
        }
    }

    /// A `data` or `newtype` declaration.
    fn data_decl(&mut self, decl: &Decl) -> Result<()> {
        let Decl::Data {
            pos,
            newtype,
            name,
            vars,
            kind,
            cons,
            deriving,
        } = decl
        else {
            return Ok(());
        };
        let keyword = if *newtype { "newtype " } else { "data " };
        let target = self.lookup_top(Space::Type, name);
        self.emit(narrow(after(*pos, keyword), name), Namespace::Type, target);
        self.binder_kinds(vars)?;
        if let Some(kind) = kind {
            self.ty(kind)?;
        }
        for con in cons {
            if !con.forall.is_empty() || con.ctx.is_some() {
                return Err(unsupported("an existential constructor", Some(con.pos)));
            }
            match &con.body {
                ConBody::Prefix { name, args } if name.name == "[]" => {
                    self.emit(span(name.span), Namespace::Term, Target::Syntax);
                    for arg in args {
                        self.ty(&arg.ty)?;
                    }
                }
                ConBody::Prefix { name, args } => {
                    let target = self.lookup_top(Space::Term, name);
                    self.emit(narrow(con.pos, name), Namespace::Term, target);
                    for arg in args {
                        self.ty(&arg.ty)?;
                    }
                }
                ConBody::Infix { left, op, right } => {
                    let op = bare(op);
                    let target = self.lookup_top(Space::Term, op);
                    self.emit(narrow(con.pos, op), Namespace::Term, target);
                    self.ty(&left.ty)?;
                    self.ty(&right.ty)?;
                }
                ConBody::Record { name, fields } => {
                    let target = self.lookup_top(Space::Term, name);
                    self.emit(narrow(con.pos, name), Namespace::Term, target);
                    for field in fields {
                        self.ty(&field.ty.ty)?;
                    }
                }
            }
        }
        for clause in deriving {
            if clause.via.is_some() {
                return Err(unsupported("deriving via", Some(*pos)));
            }
            for class in &clause.classes {
                self.ty(class)?;
            }
        }
        Ok(())
    }

    /// The context and the head of an instance. Returns the class, for the
    /// method bindings.
    fn instance_head(&mut self, head: &Type, pos: Pos) -> Result<Option<Entity>> {
        let mut head = head;
        if let Type::Qual(ctx, inner) = head {
            self.context(ctx)?;
            head = inner;
        }
        self.ty(head)?;
        let mut class = head;
        while let Type::App(fun, _) = class {
            class = fun;
        }
        match class {
            Type::Con(name) => match self.lookup(Space::Type, name) {
                Target::Top {
                    package,
                    module,
                    name,
                } => Ok(Some(Entity {
                    package,
                    module,
                    name,
                })),
                _ => Ok(None),
            },
            _ => Err(unsupported("this instance head", Some(pos))),
        }
    }

    /// A top-level binding, a class default or an instance method.
    ///
    /// The local binders of a file are numbered in one sequence, but as in
    /// aihc-resolve, each class default and instance method (`reset`)
    /// numbers its binders from zero, and the sequence continues after it.
    fn top_bind(
        &mut self,
        pos: Pos,
        lhs: &Lhs,
        rhs: &Rhs,
        class: Option<&Entity>,
        reset: bool,
    ) -> Result<()> {
        let (name, pats): (&str, Vec<&Pat>) = match lhs {
            Lhs::Fun { name, args } => (name, args.iter().collect()),
            Lhs::Infix {
                left,
                op,
                right,
                args,
            } if args.is_empty() => (bare(op), vec![left, right]),
            _ => return Err(unsupported("this binding", Some(pos))),
        };
        let mut target = self.lookup_top(Space::Term, name);
        if let (Target::Error(_), Some(class)) = (&target, class) {
            // An instance method of a class that is in scope only by its
            // class name.
            if self
                .program
                .children(class)
                .contains(&(Space::Term, name.to_string()))
            {
                target = Entity {
                    name: name.to_string(),
                    ..class.clone()
                }
                .target();
            }
        }
        self.emit(narrow(pos, name), Namespace::Term, target);
        let saved = self.next_local;
        if reset {
            self.next_local = 0;
        }
        let mark = self.locals.len();
        let result = self.pats(&pats).and_then(|()| self.rhs(rhs));
        self.locals.truncate(mark);
        if reset {
            self.next_local = saved;
        }
        result
    }

    /// The kinds of the type variables of a declaration head.
    fn binder_kinds(&mut self, vars: &[aihc_syntax::ast::TyVarBind]) -> Result<()> {
        for var in vars {
            if let Some(kind) = &var.kind {
                self.ty(kind)?;
            }
        }
        Ok(())
    }

    /// The binders of a group of local declarations, of a `where` or a
    /// `let`. They are in scope for the whole group and its body; the
    /// caller truncates the scope. Returns where the group starts in the
    /// local scope.
    ///
    /// As in aihc-resolve, every clause and every signature of one name
    /// gets a new binder, and the last binder of a name wins.
    fn bind_group(&mut self, decls: &[Decl]) -> Result<usize> {
        let group = self.locals.len();
        for decl in decls {
            match decl {
                Decl::Bind {
                    lhs: Lhs::Fun { name, .. },
                    ..
                } => {
                    self.bind_local(name);
                }
                Decl::Bind {
                    lhs: Lhs::Infix { op, .. },
                    ..
                } => {
                    self.bind_local(bare(op));
                }
                Decl::Bind { pos, .. } => {
                    return Err(unsupported("a local pattern binding", Some(*pos)))
                }
                Decl::TypeSig { names, .. } => {
                    if let [name] = names.as_slice() {
                        self.bind_local(name);
                    }
                }
                Decl::Fixity { .. } => {}
                _ => return Err(unsupported("this local declaration", Some(decl.pos()))),
            }
        }
        Ok(group)
    }

    /// The declarations of a local group that `bind_group` bound at
    /// `group`.
    fn group_decls(&mut self, group: usize, decls: &[Decl]) -> Result<()> {
        for decl in decls {
            match decl {
                Decl::TypeSig { pos, names, ty } => {
                    for name in names {
                        // A name that the group does not bind has no record.
                        let bound = self.locals[group..]
                            .iter()
                            .rev()
                            .find(|(n, _)| *n == name.name);
                        if let Some((_, id)) = bound {
                            let id = *id;
                            self.emit(narrow(*pos, name), Namespace::Term, Target::Local(id));
                        }
                    }
                    self.ty(ty)?;
                }
                Decl::Bind { pos, lhs, rhs } => {
                    let (name, pats): (&str, Vec<&Pat>) = match lhs {
                        Lhs::Fun { name, args } => (name, args.iter().collect()),
                        Lhs::Infix {
                            left,
                            op,
                            right,
                            args,
                        } if args.is_empty() => (bare(op), vec![left, right]),
                        _ => return Err(unsupported("this binding", Some(*pos))),
                    };
                    let target = self.lookup_top(Space::Term, name);
                    self.emit(narrow(*pos, name), Namespace::Term, target);
                    let mark = self.locals.len();
                    let result = self.pats(&pats).and_then(|()| self.rhs(rhs));
                    self.locals.truncate(mark);
                    result?;
                }
                _ => {}
            }
        }
        Ok(())
    }

    /// A right-hand side. As in aihc-resolve, the body comes before the
    /// `where` declarations, which matters for the numbering of binders.
    fn rhs(&mut self, rhs: &Rhs) -> Result<()> {
        let mark = self.locals.len();
        let result = self.rhs_body(rhs);
        self.locals.truncate(mark);
        result
    }

    fn rhs_body(&mut self, rhs: &Rhs) -> Result<()> {
        let group = self.bind_group(&rhs.wheres)?;
        match &rhs.body {
            RhsBody::Plain(e) => self.expr(e)?,
            RhsBody::Guarded(guards) => {
                for guarded in guards {
                    let inner = self.locals.len();
                    let result = self
                        .stmts(&guarded.guards)
                        .and_then(|()| self.expr(&guarded.body));
                    self.locals.truncate(inner);
                    result?;
                }
            }
        }
        self.group_decls(group, &rhs.wheres)
    }

    /// Guards: each one sees the binders of the ones before it. The caller
    /// truncates the scope.
    fn stmts(&mut self, stmts: &[Stmt]) -> Result<()> {
        for stmt in stmts {
            match stmt {
                Stmt::Expr(e) => self.expr(e)?,
                Stmt::Bind(pat, e) => {
                    self.expr(e)?;
                    self.pats(&[pat])?;
                }
                Stmt::Let(decls) => {
                    let group = self.bind_group(decls)?;
                    self.group_decls(group, decls)?;
                }
            }
        }
        Ok(())
    }

    // --- Patterns ----------------------------------------------------------

    /// Bind patterns left to right. The binders stay in scope; the caller
    /// truncates the scope.
    fn pats(&mut self, pats: &[&Pat]) -> Result<()> {
        let mut bound = Vec::new();
        for pat in pats {
            self.pat(pat, &mut bound)?;
        }
        self.locals.extend(bound);
        Ok(())
    }

    fn pat(&mut self, pat: &Pat, bound: &mut Vec<(String, u32)>) -> Result<()> {
        match pat {
            Pat::Var(name) => {
                self.pat_binder(name, bound);
                Ok(())
            }
            Pat::Wildcard => Ok(()),
            Pat::Con { name, args } => {
                if !is_builtin_con(&name.name) {
                    let target = self.lookup(Space::Term, name);
                    self.emit(span(name.span), Namespace::Term, target);
                }
                args.iter().try_for_each(|p| self.pat(p, bound))
            }
            Pat::Infix { first, rest } => {
                self.pat(first, bound)?;
                for (op, p) in rest {
                    let target = self.lookup(Space::Term, op);
                    self.emit(span(op.span), Namespace::Term, target);
                    self.pat(p, bound)?;
                }
                Ok(())
            }
            Pat::Tuple(ps) | Pat::List(ps) => ps.iter().try_for_each(|p| self.pat(p, bound)),
            Pat::Paren(p) | Pat::Lazy(p) | Pat::Bang(p) => self.pat(p, bound),
            Pat::As(name, p) => {
                self.pat_binder(name, bound);
                self.pat(p, bound)
            }
            Pat::Sig(p, ty) => {
                self.pat(p, bound)?;
                self.ty(ty)
            }
            _ => Err(unsupported("this pattern", None)),
        }
    }

    fn pat_binder(&mut self, name: &Ident, bound: &mut Vec<(String, u32)>) {
        let id = self.next_local;
        self.next_local += 1;
        self.emit(span(name.span), Namespace::Term, Target::Local(id));
        bound.push((name.to_string(), id));
    }

    // --- Expressions -------------------------------------------------------

    fn expr(&mut self, e: &Expr) -> Result<()> {
        match e {
            Expr::Var(name) | Expr::Con(name) => {
                let target = if is_builtin_con(&name.name) {
                    Target::Syntax
                } else {
                    self.lookup(Space::Term, name)
                };
                self.emit(span(name.span), Namespace::Term, target);
                Ok(())
            }
            Expr::Lit(Literal::Char { .. }) => Ok(()),
            Expr::App(f, x) => {
                self.expr(f)?;
                self.expr(x)
            }
            Expr::Paren(e) => self.expr(e),
            Expr::Sig(e, ty) | Expr::TypeApp(e, ty) => {
                self.expr(e)?;
                self.ty(ty)
            }
            Expr::Infix { first, rest } => {
                self.expr(first)?;
                for (op, operand) in rest {
                    self.op(op);
                    self.expr(operand)?;
                }
                Ok(())
            }
            Expr::LeftSection(e, op) => {
                self.expr(e)?;
                self.op(op);
                Ok(())
            }
            Expr::RightSection(op, e) => {
                self.op(op);
                self.expr(e)
            }
            Expr::Lambda(pats, body) => {
                let mark = self.locals.len();
                let pats: Vec<&Pat> = pats.iter().collect();
                let result = self.pats(&pats).and_then(|()| self.expr(body));
                self.locals.truncate(mark);
                result
            }
            Expr::Let(decls, body) => {
                let mark = self.locals.len();
                let result = self
                    .bind_group(decls)
                    .and_then(|group| self.group_decls(group, decls))
                    .and_then(|()| self.expr(body));
                self.locals.truncate(mark);
                result
            }
            Expr::Case(scrutinee, alts) => {
                self.expr(scrutinee)?;
                self.alts(alts)
            }
            Expr::LambdaCase(alts) => self.alts(alts),
            _ => Err(unsupported("this expression", None)),
        }
    }

    fn op(&mut self, op: &QName) {
        let target = self.lookup(Space::Term, op);
        self.emit(span(op.span), Namespace::Term, target);
    }

    fn alts(&mut self, alts: &[Alt]) -> Result<()> {
        for alt in alts {
            let mark = self.locals.len();
            let result = self.pats(&[&alt.pat]).and_then(|()| self.rhs(&alt.rhs));
            self.locals.truncate(mark);
            result?;
        }
        Ok(())
    }

    // --- Types -------------------------------------------------------------

    fn ty(&mut self, ty: &Type) -> Result<()> {
        match ty {
            Type::Var(_) | Type::Wildcard => Ok(()),
            Type::Con(name) if name.name == "()" => {
                self.emit(span(name.span), Namespace::Type, Target::Syntax);
                Ok(())
            }
            Type::Con(name)
                if name.name == "->" || name.name == "[]" || name.name.starts_with("(,") =>
            {
                Err(unsupported(
                    "this built-in type constructor",
                    Some(name.span.start),
                ))
            }
            Type::Con(name) => {
                let target = self.lookup(Space::Type, name);
                self.emit(span(name.span), Namespace::Type, target);
                Ok(())
            }
            Type::App(f, x) | Type::Fun(f, x) => {
                self.ty(f)?;
                self.ty(x)
            }
            Type::List(t, s) => {
                self.emit(span(*s), Namespace::Type, Target::Syntax);
                self.ty(t)
            }
            Type::Paren(inner, s) => match &**inner {
                // The operator of an infix type has the span of the
                // parentheses around it.
                Type::Op(left, op, right) => {
                    let target = self.lookup(Space::Type, op);
                    self.emit(span(*s), Namespace::Type, target);
                    self.ty(left)?;
                    self.ty(right)
                }
                inner => self.ty(inner),
            },
            Type::Qual(ctx, t) => {
                self.context(ctx)?;
                self.ty(t)
            }
            Type::KindSig(t, k) => {
                self.ty(t)?;
                self.ty(k)
            }
            Type::Op(_, op, _) => Err(unsupported(
                "a type operator without parentheses",
                Some(op.span.start),
            )),
            _ => Err(unsupported("this type", None)),
        }
    }

    /// A context: one constraint, or a tuple of constraints. The tuple is
    /// not a tuple type, so it has no record.
    fn context(&mut self, ctx: &Type) -> Result<()> {
        match ctx {
            Type::Tuple(items) => items.iter().try_for_each(|t| self.ty(t)),
            t => self.ty(t),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::scope::{Package, Program, SourceModule};

    /// A package for a test: its name, its extensions, and its modules as
    /// paths and sources.
    type TestPackage<'a> = (&'a str, &'a [&'a str], &'a [(&'a str, &'a str)]);

    /// Resolve the module at `path`, and render its records as
    /// `L:C-L:C namespace target`.
    fn records(packages: &[TestPackage], path: &str) -> Vec<String> {
        let names: Vec<&str> = packages.iter().map(|(name, _, _)| *name).collect();
        let mut program = Program::new(&crate::builtin_modules(names));
        for (name, extensions, modules) in packages {
            program.add_package(&Package {
                name: (*name).to_string(),
                extensions: extensions.iter().map(|e| (*e).to_string()).collect(),
                modules: modules
                    .iter()
                    .map(|(path, src)| SourceModule {
                        path: (*path).to_string(),
                        module: aihc_syntax::parse(src).unwrap(),
                    })
                    .collect(),
            });
        }
        crate::resolve_module(&program, path)
            .unwrap()
            .into_iter()
            .map(|o| format!("{} {} {}", o.span, o.namespace, o.target).replace('\t', " "))
            .collect()
    }

    const TYPES: (&str, &str) = (
        "GHC/Types.hs",
        "module GHC.Types (List (..), Int) where\ninfixr 5 :\ndata List a = [] | a : List a\ndata Int\n",
    );

    #[test]
    fn locals_where_and_operators() {
        let src = "module M where\nk a b = a\nf x = go x `k` x\n  where\n    go [] = x\n    go (y : ys) = go ys\n";
        let base: &[(&str, &str)] = &[TYPES, ("M.hs", src)];
        let got = records(&[("base", &["NoImplicitPrelude"], base)], "M.hs");
        assert_eq!(
            got,
            [
                "2:1-2:2 term top base M k",
                "2:3-2:4 term local 0",
                "2:5-2:6 term local 1",
                "2:9-2:10 term local 0",
                "3:1-3:2 term top base M f",
                // The binders continue the numbering of the file.
                "3:3-3:4 term local 2",
                // Both clauses of `go` share the last binder of the group.
                "3:7-3:9 term local 4",
                "3:10-3:11 term local 2",
                "3:13-3:14 term top base M k",
                "3:16-3:17 term local 2",
                "5:5-5:7 term local 4",
                "5:13-5:14 term local 2",
                "6:5-6:7 term local 4",
                "6:9-6:10 term local 5",
                // `:` comes from the builtin module GHC.Types.
                "6:11-6:12 term top base GHC.Types :",
                "6:13-6:15 term local 6",
                "6:19-6:21 term local 4",
                "6:22-6:24 term local 6",
            ]
        );
    }

    #[test]
    fn declarations_and_types() {
        let src = "module M where\nimport GHC.Types\ndata (:+:) f g = L (f ()) | f :+: g\nclass C a where\n  type R a :: List Int\n  m :: a -> [R a]\ninstance C Int where\n  m = m\n";
        let base: &[(&str, &str)] = &[TYPES, ("M.hs", src)];
        let got = records(&[("base", &["NoImplicitPrelude"], base)], "M.hs");
        assert_eq!(
            got,
            [
                // The name after `data ` has the width of the operator.
                "3:6-3:9 type top base M :+:",
                // A constructor starts at its constructor declaration.
                "3:18-3:19 term top base M L",
                "3:23-3:25 type syntax",
                "3:29-3:32 term top base M :+:",
                "4:7-4:8 type top base M C",
                // The head of an associated type: the name and its variables.
                "5:8-5:11 type top base M R",
                "5:15-5:19 type top base GHC.Types List",
                "5:20-5:23 type top base GHC.Types Int",
                "6:3-6:4 term top base M m",
                "6:13-6:18 type syntax",
                "6:14-6:15 type top base M R",
                "7:10-7:11 type top base M C",
                "7:12-7:15 type top base GHC.Types Int",
                "8:3-8:4 term top base M m",
                "8:7-8:8 term top base M m",
            ]
        );
    }

    #[test]
    fn imports_and_the_implicit_prelude() {
        let prelude = (
            "Prelude.hs",
            "module Prelude (Int, id) where\nimport GHC.Types\nid x = x\n",
        );
        let base: &[(&str, &str)] = &[TYPES, prelude];
        let user: &[(&str, &str)] = &[("A.hs", "module A where\nf :: Int -> Int\nf = id\n")];
        let got = records(
            &[("base", &["NoImplicitPrelude"], base), ("user", &[], user)],
            "A.hs",
        );
        assert_eq!(
            got,
            [
                "2:1-2:2 term top user A f",
                "2:6-2:9 type top base GHC.Types Int",
                "2:13-2:16 type top base GHC.Types Int",
                "3:1-3:2 term top user A f",
                "3:5-3:7 term top base Prelude id",
            ]
        );
    }

    #[test]
    fn an_unsupported_construct_is_an_error() {
        let base: &[(&str, &str)] = &[("M.hs", "module M where\nx = 1\n")];
        let mut program = Program::new(&[]);
        program.add_package(&Package {
            name: "base".into(),
            extensions: vec!["NoImplicitPrelude".into()],
            modules: base
                .iter()
                .map(|(path, src)| SourceModule {
                    path: (*path).to_string(),
                    module: aihc_syntax::parse(src).unwrap(),
                })
                .collect(),
        });
        let err = crate::resolve_module(&program, "M.hs").unwrap_err();
        assert!(err.message.contains("not supported yet"), "{}", err.message);
    }
}
