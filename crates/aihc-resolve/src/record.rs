//! The records that `aihc-boot dump --stage resolve` and
//! `tools/resolve-oracle` print, one per line, tab separated:
//!
//! ```text
//! module FILE ok
//! module FILE error MESSAGE
//! name FILE L1 C1 L2 C2 NS top PACKAGE MODULE NAME
//! name FILE L1 C1 L2 C2 NS local ID
//! name FILE L1 C1 L2 C2 NS syntax
//! name FILE L1 C1 L2 C2 NS error MESSAGE
//! ```
//!
//! `NS` is `term`, `type` or `module`. `L1 C1` is the start and `L2 C2`
//! the end of the identifier, 1-based, the end exclusive.

use std::collections::BTreeMap;
use std::fmt;

/// The source range of one identifier.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Span {
    pub start_line: u32,
    pub start_col: u32,
    pub end_line: u32,
    pub end_col: u32,
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}:{}-{}:{}",
            self.start_line, self.start_col, self.end_line, self.end_col
        )
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Namespace {
    Term,
    Type,
    Module,
}

impl fmt::Display for Namespace {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Namespace::Term => "term",
            Namespace::Type => "type",
            Namespace::Module => "module",
        })
    }
}

/// What an identifier occurrence names.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Target {
    /// A top-level entity, by the package and module that define it.
    Top {
        package: String,
        module: String,
        name: String,
    },
    /// A local binder. The id is only unique inside its file.
    Local(u32),
    /// Built-in syntax, such as a tuple constructor.
    Syntax,
    /// The occurrence does not resolve.
    Error(String),
}

impl fmt::Display for Target {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Target::Top {
                package,
                module,
                name,
            } => write!(f, "top\t{package}\t{module}\t{name}"),
            Target::Local(id) => write!(f, "local\t{id}"),
            Target::Syntax => f.write_str("syntax"),
            Target::Error(message) => write!(f, "error\t{message}"),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Occurrence {
    pub span: Span,
    pub namespace: Namespace,
    pub target: Target,
}

/// One module of a dump: whether it resolved, and its occurrences sorted
/// by span.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct ModuleRecords {
    /// `Some(message)` when the module did not resolve.
    pub error: Option<String>,
    pub occurrences: Vec<Occurrence>,
}

/// A whole dump, by file.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Dump {
    pub modules: BTreeMap<String, ModuleRecords>,
}

impl Dump {
    /// Read a dump from its text form.
    ///
    /// # Errors
    ///
    /// A line that is not a record, with its line number.
    pub fn parse(text: &str) -> Result<Dump, String> {
        let mut dump = Dump::default();
        for (i, line) in text.lines().enumerate() {
            if line.trim().is_empty() {
                continue;
            }
            parse_line(line, &mut dump).map_err(|e| format!("line {}: {e}: {line}", i + 1))?;
        }
        for module in dump.modules.values_mut() {
            module.occurrences.sort_by_key(|o| o.span);
        }
        Ok(dump)
    }

    /// Print a dump in its text form.
    pub fn write(&self, out: &mut String) {
        use std::fmt::Write as _;
        for (file, module) in &self.modules {
            match &module.error {
                None => writeln!(out, "module\t{file}\tok").unwrap(),
                Some(message) => writeln!(out, "module\t{file}\terror\t{message}").unwrap(),
            }
            for o in &module.occurrences {
                let s = o.span;
                writeln!(
                    out,
                    "name\t{file}\t{}\t{}\t{}\t{}\t{}\t{}",
                    s.start_line, s.start_col, s.end_line, s.end_col, o.namespace, o.target
                )
                .unwrap();
            }
        }
    }
}

fn parse_line(line: &str, dump: &mut Dump) -> Result<(), String> {
    let fields: Vec<&str> = line.split('\t').collect();
    match fields.as_slice() {
        ["module", file, "ok"] => {
            dump.modules.entry((*file).to_string()).or_default();
        }
        ["module", file, "error", message] => {
            dump.modules.entry((*file).to_string()).or_default().error =
                Some((*message).to_string());
        }
        ["name", file, l1, c1, l2, c2, ns, target @ ..] => {
            let span = Span {
                start_line: number(l1)?,
                start_col: number(c1)?,
                end_line: number(l2)?,
                end_col: number(c2)?,
            };
            let namespace = match *ns {
                "term" => Namespace::Term,
                "type" => Namespace::Type,
                "module" => Namespace::Module,
                other => return Err(format!("unknown namespace {other}")),
            };
            let target = match target {
                ["top", package, module, name] => Target::Top {
                    package: (*package).to_string(),
                    module: (*module).to_string(),
                    name: (*name).to_string(),
                },
                ["local", id] => Target::Local(number(id)?),
                ["syntax"] => Target::Syntax,
                ["error", message] => Target::Error((*message).to_string()),
                _ => return Err("unknown target".into()),
            };
            dump.modules
                .entry((*file).to_string())
                .or_default()
                .occurrences
                .push(Occurrence {
                    span,
                    namespace,
                    target,
                });
        }
        _ => return Err("unknown record".into()),
    }
    Ok(())
}

fn number(s: &str) -> Result<u32, String> {
    s.parse().map_err(|_| format!("not a number: {s}"))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_dump_round_trips() {
        let text = "module\ta.hs\tok\n\
                    name\ta.hs\t1\t2\t1\t5\tterm\ttop\tbase\tGHC.Base\tmap\n\
                    name\ta.hs\t2\t1\t2\t2\tterm\tlocal\t3\n\
                    name\ta.hs\t3\t1\t3\t2\ttype\tsyntax\n\
                    module\tb.hs\terror\tparse: 1:1: bad\n\
                    name\tb.hs\t1\t1\t1\t2\tmodule\terror\tnot found\n";
        let dump = Dump::parse(text).unwrap();
        assert_eq!(dump.modules["a.hs"].error, None);
        assert_eq!(dump.modules["a.hs"].occurrences.len(), 3);
        assert_eq!(
            dump.modules["b.hs"].error.as_deref(),
            Some("parse: 1:1: bad")
        );
        let mut printed = String::new();
        dump.write(&mut printed);
        assert_eq!(printed, text);
    }

    #[test]
    fn occurrences_are_sorted_by_span() {
        let text = "name\ta.hs\t2\t1\t2\t2\tterm\tsyntax\nname\ta.hs\t1\t1\t1\t2\tterm\tsyntax\n";
        let dump = Dump::parse(text).unwrap();
        let lines: Vec<u32> = dump.modules["a.hs"]
            .occurrences
            .iter()
            .map(|o| o.span.start_line)
            .collect();
        assert_eq!(lines, [1, 2]);
    }

    #[test]
    fn a_bad_line_is_an_error() {
        assert!(Dump::parse("name\ta.hs\tx\n")
            .unwrap_err()
            .starts_with("line 1:"));
    }
}
