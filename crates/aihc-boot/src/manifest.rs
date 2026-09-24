//! The vendored packages: what `vendor/NAME/NAME.cabal` says about a
//! package, and the manifest that `tools/resolve-oracle` reads.
//!
//! The `.cabal` reader is line based and reads three fields of every
//! stanza: `build-depends`, `default-language` and `default-extensions`.
//! It takes the union over the stanzas, so a dependency of a test suite
//! counts as well. That is harmless: a dependency only matters here when
//! it is vendored.

use std::collections::{BTreeMap, BTreeSet};
use std::fmt::Write as _;
use std::path::{Path, PathBuf};

/// The fields of a package's `.cabal` file that the boot compiler reads.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct CabalPackage {
    /// The first `default-language` value.
    pub language: Option<String>,
    /// Every `default-extensions` value, in order, without duplicates.
    pub extensions: Vec<String>,
    /// Every package name in a `build-depends` field.
    pub depends: BTreeSet<String>,
}

impl CabalPackage {
    /// Read the first `.cabal` file in a package directory.
    pub fn read(pkg_dir: &Path) -> Option<CabalPackage> {
        let mut cabal_files: Vec<PathBuf> = std::fs::read_dir(pkg_dir)
            .ok()?
            .flatten()
            .map(|e| e.path())
            .filter(|p| p.extension().is_some_and(|e| e == "cabal"))
            .collect();
        cabal_files.sort();
        let text = std::fs::read_to_string(cabal_files.first()?).ok()?;
        Some(CabalPackage::parse(&text))
    }

    /// Read the fields from the text of a `.cabal` file.
    pub fn parse(text: &str) -> CabalPackage {
        let mut package = CabalPackage::default();
        for (name, value) in fields(text) {
            match name.as_str() {
                "build-depends" => {
                    for item in value.split(',') {
                        let dep: String = item
                            .trim()
                            .chars()
                            .take_while(|c| c.is_ascii_alphanumeric() || *c == '-')
                            .collect();
                        if !dep.is_empty() {
                            package.depends.insert(dep);
                        }
                    }
                }
                "default-language" => {
                    let value = value.trim();
                    if package.language.is_none() && !value.is_empty() {
                        package.language = Some(value.to_string());
                    }
                }
                "default-extensions" => {
                    for ext in value.split(|c: char| c == ',' || c.is_whitespace()) {
                        if !ext.is_empty() && !package.extensions.iter().any(|e| e == ext) {
                            package.extensions.push(ext.to_string());
                        }
                    }
                }
                _ => {}
            }
        }
        package
    }

    /// The `-X` flags for GHC: the language first, then the extensions.
    pub fn ghc_flags(&self) -> Vec<String> {
        self.language
            .iter()
            .chain(self.extensions.iter())
            .map(|x| format!("-X{x}"))
            .collect()
    }
}

/// Every `name: value` field of a `.cabal` file, in order, with the
/// field name in lower case. A field's value continues on the lines that
/// are indented more than the field. Comments (`--`) are removed.
fn fields(text: &str) -> Vec<(String, String)> {
    let mut out: Vec<(String, String)> = Vec::new();
    let mut field_indent: Option<usize> = None;
    for raw in text.lines() {
        let line = raw.split("--").next().unwrap_or("").trim_end();
        if line.trim().is_empty() {
            continue;
        }
        let indent = line.len() - line.trim_start().len();
        if let Some(fi) = field_indent {
            if indent > fi {
                let last = out.last_mut().unwrap();
                last.1.push(' ');
                last.1.push_str(line.trim());
                continue;
            }
            field_indent = None;
        }
        if let Some((name, value)) = line.trim().split_once(':') {
            let name = name.trim();
            // A stanza header such as `executable aihc` has no colon; a
            // field name has no spaces.
            if !name.is_empty() && !name.contains(char::is_whitespace) {
                out.push((name.to_ascii_lowercase(), value.trim().to_string()));
                field_indent = Some(indent);
            }
        }
    }
    out
}

/// Every Haskell source in a package, sorted, with the same rules as
/// `scripts/progress.py`: any `.hs`, `.lhs`, `.hsc` or `.hs-boot` file,
/// except `Setup.hs` at the package root.
pub fn haskell_files(pkg_dir: &Path) -> Vec<PathBuf> {
    fn walk(root: &Path, dir: &Path, out: &mut Vec<PathBuf>) {
        let Ok(entries) = std::fs::read_dir(dir) else {
            return;
        };
        for entry in entries.flatten() {
            let path = entry.path();
            if path.is_dir() {
                walk(root, &path, out);
                continue;
            }
            let is_haskell = path
                .extension()
                .and_then(|e| e.to_str())
                .is_some_and(|e| matches!(e, "hs" | "lhs" | "hsc" | "hs-boot"));
            let is_setup =
                path.parent() == Some(root) && path.file_stem().is_some_and(|s| s == "Setup");
            if is_haskell && !is_setup {
                out.push(path);
            }
        }
    }
    let mut files = Vec::new();
    walk(pkg_dir, pkg_dir, &mut files);
    files.sort();
    files
}

/// The directory of a vendored package.
pub fn package_dir(name: &str) -> PathBuf {
    PathBuf::from("vendor").join(name)
}

/// Every vendored package: each directory under `vendor/` with a `.cabal`
/// file, by directory name.
pub fn vendored_packages() -> BTreeMap<String, CabalPackage> {
    let mut packages = BTreeMap::new();
    let Ok(entries) = std::fs::read_dir("vendor") else {
        return packages;
    };
    for entry in entries.flatten() {
        let Some(name) = entry.file_name().to_str().map(str::to_string) else {
            continue;
        };
        if let Some(package) = CabalPackage::read(&entry.path()) {
            packages.insert(name, package);
        }
    }
    packages
}

/// The target package and the vendored packages it depends on, in
/// dependency order: a package comes after every package it depends on.
///
/// # Errors
///
/// The target is not vendored, or the dependencies contain a cycle.
pub fn package_order(
    target: &str,
    packages: &BTreeMap<String, CabalPackage>,
) -> Result<Vec<String>, String> {
    fn visit(
        name: &str,
        packages: &BTreeMap<String, CabalPackage>,
        done: &mut Vec<String>,
        active: &mut Vec<String>,
    ) -> Result<(), String> {
        if done.iter().any(|d| d == name) {
            return Ok(());
        }
        if active.iter().any(|a| a == name) {
            active.push(name.to_string());
            return Err(format!("dependency cycle: {}", active.join(" -> ")));
        }
        active.push(name.to_string());
        for dep in &packages[name].depends {
            if packages.contains_key(dep) && dep != name {
                visit(dep, packages, done, active)?;
            }
        }
        active.pop();
        done.push(name.to_string());
        Ok(())
    }
    if !packages.contains_key(target) {
        return Err(format!("vendor/{target} is not a vendored package"));
    }
    let mut done = Vec::new();
    visit(target, packages, &mut done, &mut Vec::new())?;
    Ok(done)
}

/// The manifest for `tools/resolve-oracle`: the target package and its
/// vendored dependencies, in dependency order, with their modules. Only
/// the target's records are reported.
///
/// # Errors
///
/// See [`package_order`].
pub fn manifest(target: &str) -> Result<String, String> {
    let packages = vendored_packages();
    let mut out = String::new();
    for name in package_order(target, &packages)? {
        let package = &packages[name.as_str()];
        writeln!(out, "package {name}").unwrap();
        if let Some(language) = &package.language {
            writeln!(out, "language {language}").unwrap();
        }
        for ext in &package.extensions {
            writeln!(out, "extension {ext}").unwrap();
        }
        for file in haskell_files(&package_dir(&name)) {
            writeln!(out, "module {}", file.display()).unwrap();
        }
    }
    writeln!(out, "report {target}").unwrap();
    Ok(out)
}

#[cfg(test)]
mod tests {
    use super::*;

    const CABAL: &str = "\
cabal-version: 3.8
name: demo

library
  build-depends:
    base >=4.16 && <5,
    containers, -- a comment
    text >=1.2
  default-language: GHC2021
  default-extensions: OverloadedStrings
    LambdaCase

executable demo
  main-is: Main.hs
  build-depends: base, demo
  default-language: Haskell2010
";

    #[test]
    fn reads_the_three_fields() {
        let package = CabalPackage::parse(CABAL);
        assert_eq!(package.language.as_deref(), Some("GHC2021"));
        assert_eq!(package.extensions, ["OverloadedStrings", "LambdaCase"]);
        let depends: Vec<&str> = package.depends.iter().map(String::as_str).collect();
        assert_eq!(depends, ["base", "containers", "demo", "text"]);
        assert_eq!(
            package.ghc_flags(),
            ["-XGHC2021", "-XOverloadedStrings", "-XLambdaCase"]
        );
    }

    fn package(depends: &[&str]) -> CabalPackage {
        CabalPackage {
            depends: depends.iter().map(|d| (*d).to_string()).collect(),
            ..CabalPackage::default()
        }
    }

    #[test]
    fn orders_dependencies_first() {
        let packages: BTreeMap<String, CabalPackage> = [
            ("a".to_string(), package(&["b", "c", "base"])),
            ("b".to_string(), package(&["c"])),
            ("c".to_string(), package(&[])),
            ("d".to_string(), package(&["a"])),
        ]
        .into_iter()
        .collect();
        assert_eq!(package_order("a", &packages).unwrap(), ["c", "b", "a"]);
        assert_eq!(package_order("c", &packages).unwrap(), ["c"]);
        assert!(package_order("x", &packages).is_err());
    }

    #[test]
    fn reports_a_cycle() {
        let packages: BTreeMap<String, CabalPackage> = [
            ("a".to_string(), package(&["b"])),
            ("b".to_string(), package(&["a"])),
        ]
        .into_iter()
        .collect();
        let err = package_order("a", &packages).unwrap_err();
        assert_eq!(err, "dependency cycle: a -> b -> a");
    }
}
