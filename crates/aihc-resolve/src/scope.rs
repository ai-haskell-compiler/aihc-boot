//! The top-level names of the vendored packages: what each module
//! defines, imports and exports.
//!
//! The rules follow aihc-resolve, the reference resolver:
//!
//! - A module's own top-level names come first. Then come its imports, in
//!   source order: when two imports give the same name, the first import
//!   wins. Then comes the implicit `Prelude` import. Last come the list
//!   constructor `:` and the equality type `~` from the builtin modules.
//! - A module without an export list exports its own top-level names only.

use std::collections::{BTreeMap, HashMap};

use aihc_syntax::ast::{
    ConBody, Decl, Export, Import, ImportItem, Lhs, Module, Pat, Qualified, Subs,
};

use crate::record::Target;
use crate::ResolveError;

/// A top-level entity: the package and module that define it, and its
/// name.
#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Entity {
    pub package: String,
    pub module: String,
    pub name: String,
}

impl Entity {
    pub fn target(&self) -> Target {
        Target::Top {
            package: self.package.clone(),
            module: self.module.clone(),
            name: self.name.clone(),
        }
    }
}

/// Names in scope: terms and types, unqualified and by qualifier.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Scope {
    pub terms: BTreeMap<String, Entity>,
    pub types: BTreeMap<String, Entity>,
    pub qualified: BTreeMap<String, Scope>,
}

impl Scope {
    /// Add the names of `other` that this scope does not have yet. The
    /// names already in the scope win.
    pub fn union(&mut self, other: &Scope) {
        for (name, entity) in &other.terms {
            self.terms
                .entry(name.clone())
                .or_insert_with(|| entity.clone());
        }
        for (name, entity) in &other.types {
            self.types
                .entry(name.clone())
                .or_insert_with(|| entity.clone());
        }
        for (qualifier, scope) in &other.qualified {
            self.qualified
                .entry(qualifier.clone())
                .or_default()
                .union(scope);
        }
    }

    /// The unqualified names of this scope, without its qualifiers.
    fn unqualified(&self) -> Scope {
        Scope {
            terms: self.terms.clone(),
            types: self.types.clone(),
            qualified: BTreeMap::new(),
        }
    }

    /// The names under a qualifier, or the unqualified names for an empty
    /// qualifier.
    pub fn qualifier(&self, qualifier: &str) -> Option<&Scope> {
        if qualifier.is_empty() {
            Some(self)
        } else {
            self.qualified.get(qualifier)
        }
    }
}

/// Whether a name is in the term or the type namespace.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Space {
    Term,
    Type,
}

/// One module of a package, with the file it comes from.
#[derive(Clone, Debug)]
pub struct SourceModule {
    pub path: String,
    pub module: Module,
}

/// One package: its name, the extensions of its `.cabal` file, and its
/// modules.
#[derive(Clone, Debug)]
pub struct Package {
    pub name: String,
    /// The `default-extensions` of the `.cabal` file, such as
    /// `NoImplicitPrelude`.
    pub extensions: Vec<String>,
    pub modules: Vec<SourceModule>,
}

/// The builtin modules for a set of packages: the modules whose exports
/// the syntax uses without an import. `tools/resolve-oracle` gets the same
/// list from the manifest.
pub fn builtin_modules<'a>(packages: impl IntoIterator<Item = &'a str>) -> Vec<&'static str> {
    if packages.into_iter().any(|name| name == "base") {
        vec!["GHC.Types"]
    } else {
        Vec::new()
    }
}

/// The resolved top-level names of a module.
#[derive(Clone, Debug)]
struct ModuleData {
    /// Everything in scope at the top level of the module.
    scope: Result<Scope, ResolveError>,
    exports: Result<Scope, ResolveError>,
}

/// Every package that the resolver knows, and the top-level names of their
/// modules.
#[derive(Debug, Default)]
pub struct Program {
    /// The modules by name. Module names are unique in the vendored tree.
    modules: HashMap<String, ModuleData>,
    /// The module of each file, by path.
    paths: HashMap<String, (String, Module)>,
    /// The constructors and fields of each type, and the methods and
    /// associated types of each class.
    children: HashMap<Entity, Vec<(Space, String)>>,
    /// The modules whose `:` and `~` every module sees without an import.
    builtin_modules: Vec<String>,
}

impl Program {
    /// A program whose syntax takes `:` and `~` from `builtin_modules`.
    pub fn new(builtin_modules: &[&str]) -> Program {
        Program {
            builtin_modules: builtin_modules.iter().map(|m| (*m).to_string()).collect(),
            ..Program::default()
        }
    }

    /// Add a package. Its dependencies must be in the program already.
    pub fn add_package(&mut self, package: &Package) {
        let mut pending: BTreeMap<String, &SourceModule> = BTreeMap::new();
        for source in &package.modules {
            pending.insert(source.module.name.clone(), source);
            self.paths.insert(
                source.path.clone(),
                (package.name.clone(), source.module.clone()),
            );
            for (parent, children) in declared_children(&package.name, &source.module) {
                self.children.insert(parent, children);
            }
        }
        let names: Vec<String> = pending.keys().cloned().collect();
        for name in names {
            self.compute(&name, package, &pending, &mut Vec::new());
        }
    }

    /// The module that a file holds, with its package.
    pub fn module_at(&self, path: &str) -> Option<(&str, &Module)> {
        self.paths
            .get(path)
            .map(|(package, module)| (package.as_str(), module))
    }

    /// The top-level scope of a module.
    pub fn scope(&self, module: &str) -> Result<&Scope, ResolveError> {
        match self.modules.get(module) {
            Some(data) => data.scope.as_ref().map_err(Clone::clone),
            None => Err(unsupported(&format!("module {module}"), None)),
        }
    }

    /// The names that every module sees without an import: `:` and `~`
    /// from the builtin modules. A builtin module that is `module` itself,
    /// or that does not resolve, gives nothing.
    fn builtin_scope(&self, module: &str) -> Scope {
        let mut builtin = Scope::default();
        for name in &self.builtin_modules {
            if name == module {
                continue;
            }
            if let Some(Ok(exports)) = self.modules.get(name).map(|m| &m.exports) {
                if let Some(entity) = exports.terms.get(":") {
                    builtin.terms.insert(":".into(), entity.clone());
                }
                if let Some(entity) = exports.types.get("~") {
                    builtin.types.insert("~".into(), entity.clone());
                }
            }
        }
        builtin
    }

    /// The constructors, fields, methods and associated types of a type or
    /// class.
    pub fn children(&self, parent: &Entity) -> &[(Space, String)] {
        self.children.get(parent).map_or(&[], Vec::as_slice)
    }

    /// Compute the scope and the exports of a module of the package that is
    /// being added, and first those of the sibling modules it imports.
    fn compute(
        &mut self,
        name: &str,
        package: &Package,
        pending: &BTreeMap<String, &SourceModule>,
        active: &mut Vec<String>,
    ) {
        if self.modules.contains_key(name) || !pending.contains_key(name) {
            return;
        }
        if active.iter().any(|a| a == name) {
            let error = unsupported("mutually recursive modules", None);
            self.modules.insert(
                name.to_string(),
                ModuleData {
                    scope: Err(error.clone()),
                    exports: Err(error),
                },
            );
            return;
        }
        active.push(name.to_string());
        let module = &pending[name].module;
        let prelude = implicit_prelude(&package.extensions, module);
        for import in &module.imports {
            self.compute(&import.module, package, pending, active);
        }
        if prelude {
            self.compute("Prelude", package, pending, active);
        }
        // A builtin module is no import: when it is being computed, it just
        // gives nothing yet.
        for builtin in self.builtin_modules.clone() {
            if !active.contains(&builtin) {
                self.compute(&builtin, package, pending, active);
            }
        }
        active.pop();
        let scope = self.module_scope(&package.name, module, prelude);
        let exports = scope
            .as_ref()
            .map_err(Clone::clone)
            .and_then(|scope| self.exports(&package.name, module, scope));
        self.modules
            .insert(name.to_string(), ModuleData { scope, exports });
    }

    /// Everything in scope at the top level of a module.
    fn module_scope(
        &self,
        package: &str,
        module: &Module,
        prelude: bool,
    ) -> Result<Scope, ResolveError> {
        let own = own_scope(package, module)?;
        let mut scope = own.clone();
        // A module's own names are also in scope with its own name as the
        // qualifier.
        scope.qualified.insert(module.name.clone(), own);
        for import in &module.imports {
            let imported = self.import(import)?;
            let qualifier = import
                .alias
                .clone()
                .unwrap_or_else(|| import.module.clone());
            if import.qualified == Qualified::No {
                scope.union(&imported);
            }
            scope
                .qualified
                .entry(qualifier)
                .or_default()
                .union(&imported);
        }
        if prelude {
            let imported = self.exports_of("Prelude")?;
            scope.union(&imported);
            scope
                .qualified
                .entry("Prelude".to_string())
                .or_default()
                .union(&imported);
        }
        scope.union(&self.builtin_scope(&module.name));
        Ok(scope)
    }

    fn exports_of(&self, module: &str) -> Result<Scope, ResolveError> {
        match self.modules.get(module) {
            Some(data) => data.exports.clone().map_err(|e| ResolveError {
                message: format!("module {module} does not resolve: {}", e.message),
                pos: None,
            }),
            None => Err(ResolveError {
                message: format!(
                    "resolve: module {module} is not available: it is not vendored, or it does not parse"
                ),
                pos: None,
            }),
        }
    }

    /// The names that one import declaration brings into scope.
    fn import(&self, import: &Import) -> Result<Scope, ResolveError> {
        if import.package.is_some() {
            return Err(unsupported("a package import", Some(import.pos)));
        }
        let exports = self.exports_of(&import.module)?;
        let Some(items) = &import.items else {
            return Ok(exports);
        };
        let mut selected = Scope::default();
        for item in items {
            let (name, subs, space) = match item {
                ImportItem::Var(name) => (name, &Subs::None, Space::Term),
                ImportItem::Thing {
                    name,
                    subs,
                    explicit_type: false,
                } => (name, subs, Space::Type),
                _ => return Err(unsupported("this import item", Some(import.pos))),
            };
            let found = match space {
                Space::Term => exports.terms.get(name.as_str()),
                Space::Type => exports.types.get(name.as_str()),
            };
            let Some(entity) = found else {
                return Err(unsupported(
                    &format!(
                        "an import of {name}, which {} does not export,",
                        import.module
                    ),
                    Some(import.pos),
                ));
            };
            match space {
                Space::Term => {
                    selected.terms.insert(name.clone(), entity.clone());
                }
                Space::Type => {
                    selected.types.insert(name.clone(), entity.clone());
                    self.add_children(entity, subs, &exports, &mut selected)?;
                }
            }
        }
        if import.hiding {
            let mut rest = exports.unqualified();
            rest.terms
                .retain(|name, _| !selected.terms.contains_key(name));
            rest.types
                .retain(|name, _| !selected.types.contains_key(name));
            return Ok(rest);
        }
        Ok(selected)
    }

    /// Add the children of `parent` that `subs` names, as far as `source`
    /// has them.
    fn add_children(
        &self,
        parent: &Entity,
        subs: &Subs,
        source: &Scope,
        into: &mut Scope,
    ) -> Result<(), ResolveError> {
        let wanted: Vec<&(Space, String)> = match subs {
            Subs::None => return Ok(()),
            Subs::All => self.children(parent).iter().collect(),
            Subs::Some(names) => self
                .children(parent)
                .iter()
                .filter(|(_, child)| names.contains(child))
                .collect(),
            Subs::AllAnd(_) => return Err(unsupported("a bundled pattern synonym", None)),
        };
        for (space, child) in wanted {
            let (from, to) = match space {
                Space::Term => (&source.terms, &mut into.terms),
                Space::Type => (&source.types, &mut into.types),
            };
            if let Some(entity) = from.get(child) {
                to.insert(child.clone(), entity.clone());
            }
        }
        Ok(())
    }

    /// The names that a module exports.
    fn exports(
        &self,
        package: &str,
        module: &Module,
        scope: &Scope,
    ) -> Result<Scope, ResolveError> {
        let Some(exports) = &module.exports else {
            return own_scope(package, module);
        };
        let mut out = Scope::default();
        for export in exports {
            match export {
                Export::Var(name) => {
                    let source = scope
                        .qualifier(&name.qual)
                        .and_then(|s| s.terms.get(&name.name));
                    if let Some(entity) = source {
                        out.terms.insert(name.name.clone(), entity.clone());
                    }
                }
                Export::Thing {
                    name,
                    subs,
                    explicit_type: false,
                } => {
                    let Some(source) = scope.qualifier(&name.qual) else {
                        continue;
                    };
                    if let Some(entity) = source.types.get(&name.name) {
                        out.types.insert(name.name.clone(), entity.clone());
                        self.add_children(entity, subs, source, &mut out)?;
                    }
                }
                _ => return Err(unsupported("this export item", Some(module.pos))),
            }
        }
        Ok(out)
    }
}

/// A resolve error for a construct that the resolver does not handle yet.
pub fn unsupported(what: &str, pos: Option<aihc_syntax::Pos>) -> ResolveError {
    ResolveError {
        message: format!("resolve: {what} is not supported yet"),
        pos,
    }
}

/// Whether a module gets the implicit `import Prelude`. The `.cabal`
/// extensions come first, then the module's `LANGUAGE` pragmas. An explicit
/// import of `Prelude` replaces the implicit one.
pub fn implicit_prelude(extensions: &[String], module: &Module) -> bool {
    let mut on = true;
    let pragmas = module.pragmas.iter().flat_map(|pragma| {
        pragma
            .strip_prefix("LANGUAGE")
            .unwrap_or("")
            .split(',')
            .map(str::trim)
    });
    for ext in extensions.iter().map(String::as_str).chain(pragmas) {
        match ext {
            "ImplicitPrelude" => on = true,
            "NoImplicitPrelude" | "RebindableSyntax" => on = false,
            _ => {}
        }
    }
    on && !module.imports.iter().any(|i| i.module == "Prelude")
}

/// The names that a module defines at its top level.
fn own_scope(package: &str, module: &Module) -> Result<Scope, ResolveError> {
    let entity = |name: &str| Entity {
        package: package.to_string(),
        module: module.name.clone(),
        name: name.to_string(),
    };
    let mut scope = Scope::default();
    let term = |scope: &mut Scope, name: &str| {
        scope.terms.insert(name.to_string(), entity(name));
    };
    for decl in &module.decls {
        match decl {
            Decl::Bind { lhs, pos, .. } => match lhs {
                Lhs::Fun { name, .. } => term(&mut scope, name),
                Lhs::Infix { op, .. } => term(&mut scope, bare(op)),
                Lhs::Pat(pat) => {
                    let mut vars = Vec::new();
                    if !pattern_vars(pat, &mut vars) {
                        return Err(unsupported("this top-level pattern binding", Some(*pos)));
                    }
                    for var in vars {
                        term(&mut scope, &var);
                    }
                }
            },
            Decl::Data { name, cons, .. } => {
                scope.types.insert(name.to_string(), entity(name));
                for con in cons {
                    for child in con_children(&con.body) {
                        term(&mut scope, &child);
                    }
                }
            }
            Decl::TypeSyn { name, .. } | Decl::TypeFamily { name, .. } => {
                scope.types.insert(name.to_string(), entity(name));
            }
            Decl::Class { name, body, .. } => {
                scope.types.insert(name.to_string(), entity(name));
                for item in body {
                    match item {
                        Decl::TypeSig { names, .. } => {
                            for method in names {
                                term(&mut scope, method);
                            }
                        }
                        Decl::TypeFamily { name, .. } => {
                            scope.types.insert(name.to_string(), entity(name));
                        }
                        _ => {}
                    }
                }
            }
            Decl::PatSyn { pos, .. } | Decl::PatSynSig { pos, .. } => {
                return Err(unsupported("a pattern synonym", Some(*pos)));
            }
            _ => {}
        }
    }
    Ok(scope)
}

/// The children of every type and class that a module declares.
fn declared_children(package: &str, module: &Module) -> Vec<(Entity, Vec<(Space, String)>)> {
    let entity = |name: &str| Entity {
        package: package.to_string(),
        module: module.name.clone(),
        name: name.to_string(),
    };
    let mut out = Vec::new();
    for decl in &module.decls {
        match decl {
            Decl::Data { name, cons, .. } => {
                let children = cons
                    .iter()
                    .flat_map(|con| con_children(&con.body))
                    .map(|child| (Space::Term, child))
                    .collect();
                out.push((entity(name), children));
            }
            Decl::Class { name, body, .. } => {
                let mut children = Vec::new();
                for item in body {
                    match item {
                        Decl::TypeSig { names, .. } => {
                            children.extend(names.iter().map(|n| (Space::Term, n.to_string())));
                        }
                        Decl::TypeFamily { name, .. } => {
                            children.push((Space::Type, name.to_string()));
                        }
                        _ => {}
                    }
                }
                out.push((entity(name), children));
            }
            _ => {}
        }
    }
    out
}

/// The constructor and the field names of one constructor declaration.
fn con_children(body: &ConBody) -> Vec<String> {
    match body {
        ConBody::Prefix { name, .. } if name.name == "[]" => Vec::new(),
        ConBody::Prefix { name, .. } => vec![name.to_string()],
        ConBody::Infix { op, .. } => vec![bare(op).to_string()],
        ConBody::Record { name, fields } => std::iter::once(name.to_string())
            .chain(
                fields
                    .iter()
                    .flat_map(|f| f.names.iter().map(|n| n.to_string())),
            )
            .collect(),
    }
}

/// A name without the backquotes of `` `op` ``.
pub fn bare(name: &str) -> &str {
    name.strip_prefix('`')
        .and_then(|n| n.strip_suffix('`'))
        .unwrap_or(name)
}

/// The variables that a pattern binds, in source order. Returns `false`
/// for a pattern that the resolver does not handle.
pub fn pattern_vars(pat: &Pat, out: &mut Vec<String>) -> bool {
    match pat {
        Pat::Var(name) => {
            out.push(name.to_string());
            true
        }
        Pat::Wildcard => true,
        Pat::Con { args, .. } | Pat::Tuple(args) | Pat::List(args) => {
            args.iter().all(|p| pattern_vars(p, out))
        }
        Pat::Infix { first, rest } => {
            pattern_vars(first, out) && rest.iter().all(|(_, p)| pattern_vars(p, out))
        }
        Pat::Paren(p) | Pat::Lazy(p) | Pat::Bang(p) => pattern_vars(p, out),
        Pat::As(name, p) => {
            out.push(name.to_string());
            pattern_vars(p, out)
        }
        _ => false,
    }
}
