//! The `aihc-boot` command.
//!
//! `docs/PLAN.md` ("Compiler interface") defines the contract between this
//! binary and `scripts/progress.py`. The commands:
//!
//! - `aihc-boot check --stage STAGE --package NAME`: check every module
//!   of `vendor/NAME/` up to `STAGE` and print one JSON object per module.
//! - `aihc-boot manifest --package NAME`: print the manifest that
//!   `tools/resolve-oracle` reads for the package. For debugging.
//! - `aihc-boot dump --stage resolve --package NAME`: print our
//!   resolution records of every module of the package. For debugging.
//! - `aihc-boot oracle --stage resolve --package NAME`: run the oracle on
//!   the package and print its records in the same form, so the two
//!   outputs `diff`. For debugging.
//! - `aihc-boot lex FILE`: print the tokens of one file, after layout.
//!   For debugging.
//! - `aihc-boot parse FILE`: print the syntax tree of one file. For
//!   debugging.
//! - `aihc-boot print FILE`: parse one file and print it back as source.
//!   This is what `check` sends to GHC.
//! - `aihc-boot run FILE.hs`: not implemented yet.

mod manifest;

use std::fmt::Write as _;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::ExitCode;

use aihc_resolve::record::{Dump, ModuleRecords};

const USAGE: &str = "\
usage:
  aihc-boot check --stage {parse|resolve|typecheck} --package NAME
  aihc-boot manifest --package NAME
  aihc-boot dump --stage resolve --package NAME
  aihc-boot oracle --stage resolve --package NAME
  aihc-boot lex FILE
  aihc-boot parse FILE
  aihc-boot print FILE
  aihc-boot run FILE.hs
";

fn main() -> ExitCode {
    let args: Vec<String> = std::env::args().skip(1).collect();
    let result = match args.first().map(String::as_str) {
        Some("check") => check(&args[1..]),
        Some("manifest") => print_manifest(&args[1..]),
        Some("dump") => dump(&args[1..]),
        Some("oracle") => oracle(&args[1..]),
        Some("lex") => lex(&args[1..]),
        Some("parse") => parse(&args[1..]),
        Some("print") => print(&args[1..]),
        Some("run") => Err("`run` is not implemented yet".to_string()),
        Some("--help" | "-h") => {
            print!("{USAGE}");
            Ok(())
        }
        _ => Err(USAGE.trim_end().to_string()),
    };
    match result {
        Ok(()) => ExitCode::SUCCESS,
        // A closed pipe (for example `aihc-boot lex FILE | head`) is not an
        // error worth a message.
        Err(msg) if msg.contains("Broken pipe") => ExitCode::SUCCESS,
        Err(msg) => {
            eprintln!("aihc-boot: {msg}");
            ExitCode::FAILURE
        }
    }
}

/// Take the value of `--name` out of an argument list.
fn take_option(args: &mut Vec<String>, name: &str) -> Result<String, String> {
    let i = args
        .iter()
        .position(|a| a == name)
        .ok_or_else(|| format!("missing {name}"))?;
    if i + 1 >= args.len() {
        return Err(format!("{name} needs a value"));
    }
    args.remove(i);
    Ok(args.remove(i))
}

/// `--stage` and `--package`, and the package directory.
fn stage_and_package(
    args: &[String],
    stages: &[&str],
) -> Result<(String, String, PathBuf), String> {
    let mut args = args.to_vec();
    let stage = take_option(&mut args, "--stage")?;
    let package = take_option(&mut args, "--package")?;
    if let Some(extra) = args.first() {
        return Err(format!("unexpected argument {extra}"));
    }
    if !stages.contains(&stage.as_str()) {
        return Err(format!("unknown stage {stage}"));
    }
    let pkg_dir = manifest::package_dir(&package);
    if !pkg_dir.is_dir() {
        return Err(format!("{} is not a directory", pkg_dir.display()));
    }
    Ok((stage, package, pkg_dir))
}

// --- check -----------------------------------------------------------------

fn check(args: &[String]) -> Result<(), String> {
    let (stage, package, pkg_dir) = stage_and_package(args, &["parse", "resolve", "typecheck"])?;
    let reference = Reference::for_package(&pkg_dir);
    let mut oracle = Oracle::new(&package);
    let mut stdout = std::io::stdout().lock();
    let mut out = String::new();
    for file in manifest::haskell_files(&pkg_dir) {
        let report = check_module(&file, &stage, &reference, &mut oracle);
        out.clear();
        write!(
            out,
            "{{\"file\": {}, \"ok\": {}",
            json_string(&file.display().to_string()),
            report.ok
        )
        .unwrap();
        if let Some(err) = &report.error {
            write!(out, ", \"error\": {}", json_string(err)).unwrap();
        }
        if let Some(pos) = report.pos {
            write!(out, ", \"line\": {}, \"col\": {}", pos.line, pos.col).unwrap();
        }
        out.push('}');
        writeln!(stdout, "{out}").map_err(|e| e.to_string())?;
    }
    Ok(())
}

struct Report {
    ok: bool,
    error: Option<String>,
    pos: Option<aihc_syntax::Pos>,
}

impl Report {
    fn fail(error: impl Into<String>, pos: Option<aihc_syntax::Pos>) -> Report {
        Report {
            ok: false,
            error: Some(error.into()),
            pos,
        }
    }
}

/// The reference parser, `ghc-parse` from `tools/ghc-parse`, and the
/// language flags of one package.
struct Reference {
    command: Option<PathBuf>,
    /// `-X` flags from the package's `.cabal` file.
    flags: Vec<String>,
}

impl Reference {
    fn for_package(pkg_dir: &Path) -> Reference {
        let command = std::env::var_os("AIHC_GHC_PARSE")
            .map(PathBuf::from)
            .or_else(|| find_in_path("ghc-parse"));
        Reference {
            command,
            flags: manifest::CabalPackage::read(pkg_dir)
                .map(|p| p.ghc_flags())
                .unwrap_or_default(),
        }
    }
}

/// The reference resolver, `resolve-oracle` from `tools/resolve-oracle`,
/// run once per package on first use.
struct Oracle {
    package: String,
    command: Option<PathBuf>,
    dump: Option<Result<Dump, String>>,
}

impl Oracle {
    fn new(package: &str) -> Oracle {
        Oracle {
            package: package.to_string(),
            command: std::env::var_os("AIHC_RESOLVE_ORACLE")
                .map(PathBuf::from)
                .or_else(|| find_in_path("resolve-oracle")),
            dump: None,
        }
    }

    /// The oracle's records for the package.
    fn dump(&mut self) -> &Result<Dump, String> {
        if self.dump.is_none() {
            self.dump = Some(self.run());
        }
        self.dump.as_ref().unwrap()
    }

    fn run(&self) -> Result<Dump, String> {
        let Some(command) = &self.command else {
            return Err(
                "resolve-oracle not found: set AIHC_RESOLVE_ORACLE or add it to PATH".into(),
            );
        };
        let manifest = manifest::manifest(&self.package)?;
        let mut child = std::process::Command::new(command)
            .stdin(std::process::Stdio::piped())
            .stdout(std::process::Stdio::piped())
            .stderr(std::process::Stdio::piped())
            .spawn()
            .map_err(|e| format!("cannot run {}: {e}", command.display()))?;
        child
            .stdin
            .take()
            .unwrap()
            .write_all(manifest.as_bytes())
            .map_err(|e| format!("cannot write to {}: {e}", command.display()))?;
        let output = child
            .wait_with_output()
            .map_err(|e| format!("cannot run {}: {e}", command.display()))?;
        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            let first = stderr
                .lines()
                .find(|l| !l.trim().is_empty())
                .unwrap_or("")
                .trim();
            return Err(format!("{} failed: {first}", command.display()));
        }
        Dump::parse(&String::from_utf8_lossy(&output.stdout))
            .map_err(|e| format!("cannot read the output of {}: {e}", command.display()))
    }
}

fn find_in_path(name: &str) -> Option<PathBuf> {
    let path = std::env::var_os("PATH")?;
    std::env::split_paths(&path)
        .map(|dir| dir.join(name))
        .find(|p| p.is_file())
}

fn read_module(file: &Path) -> Result<aihc_syntax::ast::Module, Report> {
    let src = std::fs::read_to_string(file)
        .map_err(|e| Report::fail(format!("cannot read file: {e}"), None))?;
    if file.extension().is_some_and(|e| e == "lhs") {
        return Err(Report::fail("literate Haskell is not supported", None));
    }
    aihc_syntax::parse(&src)
        .map_err(|e| Report::fail(format!("{}: {}", e.stage, e.message), Some(e.pos)))
}

fn check_module(file: &Path, stage: &str, reference: &Reference, oracle: &mut Oracle) -> Report {
    let module = match read_module(file) {
        Ok(module) => module,
        Err(report) => return report,
    };
    // The round trip: GHC must read the printed module as the original.
    let Some(command) = &reference.command else {
        return Report::fail(
            "ghc-parse not found: set AIHC_GHC_PARSE or add it to PATH",
            None,
        );
    };
    let printed = aihc_syntax::print_module(&module);
    let printed_path = PathBuf::from("target/roundtrip").join(file);
    if let Some(dir) = printed_path.parent() {
        if let Err(e) = std::fs::create_dir_all(dir) {
            return Report::fail(format!("cannot create {}: {e}", dir.display()), None);
        }
    }
    if let Err(e) = std::fs::write(&printed_path, &printed) {
        return Report::fail(
            format!("cannot write {}: {e}", printed_path.display()),
            None,
        );
    }
    let output = std::process::Command::new(command)
        .args(&reference.flags)
        .arg(file)
        .arg(&printed_path)
        .output();
    match output {
        Ok(out) if out.status.success() => {}
        Ok(out) => {
            let stderr = String::from_utf8_lossy(&out.stderr);
            let first = stderr
                .lines()
                .find(|l| !l.trim().is_empty())
                .unwrap_or("")
                .trim();
            return Report::fail(format!("roundtrip: {first}"), None);
        }
        Err(e) => return Report::fail(format!("cannot run {}: {e}", command.display()), None),
    }
    if stage == "parse" {
        return Report {
            ok: true,
            error: None,
            pos: None,
        };
    }
    // Resolve: our records must agree with the oracle's.
    let ours = match aihc_resolve::resolve_module(&module) {
        Ok(ours) => ours,
        Err(e) => return Report::fail(e.message, e.pos),
    };
    let theirs = match oracle.dump() {
        Ok(dump) => dump.modules.get(&file.display().to_string()),
        Err(e) => return Report::fail(format!("oracle: {e}"), None),
    };
    let Some(theirs) = theirs else {
        return Report::fail("oracle: no record for this module", None);
    };
    if let Some(message) = &theirs.error {
        return Report::fail(format!("oracle: {message}"), None);
    }
    if let Err(d) = aihc_resolve::compare(&ours, &theirs.occurrences) {
        let pos = aihc_syntax::Pos {
            line: d.span.start_line,
            col: d.span.start_col,
        };
        return Report::fail(format!("resolve: {}: {}", d.span, d.message), Some(pos));
    }
    if stage != "resolve" {
        return Report::fail(format!("stage {stage} is not implemented yet"), None);
    }
    Report {
        ok: true,
        error: None,
        pos: None,
    }
}

fn json_string(s: &str) -> String {
    let mut out = String::with_capacity(s.len() + 2);
    out.push('"');
    for c in s.chars() {
        match c {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            c if c.is_control() => write!(out, "\\u{:04x}", c as u32).unwrap(),
            c => out.push(c),
        }
    }
    out.push('"');
    out
}

// --- manifest, dump and oracle ---------------------------------------------

fn print_manifest(args: &[String]) -> Result<(), String> {
    let mut args = args.to_vec();
    let package = take_option(&mut args, "--package")?;
    if let Some(extra) = args.first() {
        return Err(format!("unexpected argument {extra}"));
    }
    let text = manifest::manifest(&package)?;
    let mut stdout = std::io::stdout().lock();
    write!(stdout, "{text}").map_err(|e| e.to_string())
}

fn dump(args: &[String]) -> Result<(), String> {
    let (_stage, _package, pkg_dir) = stage_and_package(args, &["resolve"])?;
    let mut dump = Dump::default();
    for file in manifest::haskell_files(&pkg_dir) {
        let records = match read_module(&file).map(|m| aihc_resolve::resolve_module(&m)) {
            Ok(Ok(occurrences)) => ModuleRecords {
                error: None,
                occurrences,
            },
            Ok(Err(e)) => ModuleRecords {
                error: Some(e.message),
                occurrences: Vec::new(),
            },
            Err(report) => ModuleRecords {
                error: report.error,
                occurrences: Vec::new(),
            },
        };
        dump.modules.insert(file.display().to_string(), records);
    }
    let mut text = String::new();
    dump.write(&mut text);
    let mut stdout = std::io::stdout().lock();
    write!(stdout, "{text}").map_err(|e| e.to_string())
}

fn oracle(args: &[String]) -> Result<(), String> {
    let (_stage, package, _pkg_dir) = stage_and_package(args, &["resolve"])?;
    let mut oracle = Oracle::new(&package);
    let dump = oracle.dump().as_ref().map_err(|e| format!("oracle: {e}"))?;
    let mut text = String::new();
    dump.write(&mut text);
    let mut stdout = std::io::stdout().lock();
    write!(stdout, "{text}").map_err(|e| e.to_string())
}

// --- parse -----------------------------------------------------------------

fn parse(args: &[String]) -> Result<(), String> {
    let [file] = args else {
        return Err("parse takes one file".into());
    };
    let src = std::fs::read_to_string(file).map_err(|e| format!("{file}: {e}"))?;
    let module = aihc_syntax::parse(&src).map_err(|e| format!("{file}:{e}"))?;
    let mut stdout = std::io::stdout().lock();
    writeln!(stdout, "{module:#?}").map_err(|e| e.to_string())?;
    Ok(())
}

// --- print -----------------------------------------------------------------

fn print(args: &[String]) -> Result<(), String> {
    let [file] = args else {
        return Err("print takes one file".into());
    };
    let src = std::fs::read_to_string(file).map_err(|e| format!("{file}: {e}"))?;
    let module = aihc_syntax::parse(&src).map_err(|e| format!("{file}:{e}"))?;
    let mut stdout = std::io::stdout().lock();
    write!(stdout, "{}", aihc_syntax::print_module(&module)).map_err(|e| e.to_string())?;
    Ok(())
}

// --- lex -------------------------------------------------------------------

fn lex(args: &[String]) -> Result<(), String> {
    let [file] = args else {
        return Err("lex takes one file".into());
    };
    let src = std::fs::read_to_string(file).map_err(|e| format!("{file}: {e}"))?;
    let tokens = aihc_syntax::tokenize(&src).map_err(|e| format!("{file}:{e}"))?;
    let mut stdout = std::io::stdout().lock();
    for tok in tokens {
        writeln!(stdout, "{}\t{}", tok.pos, tok.kind).map_err(|e| e.to_string())?;
    }
    Ok(())
}
