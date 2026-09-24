//! The `aihc-boot` command.
//!
//! `docs/PLAN.md` ("Compiler interface") defines the contract between this
//! binary and `scripts/progress.py`. The commands:
//!
//! - `aihc-boot check --stage STAGE --package NAME`: check every module
//!   of `vendor/NAME/` up to `STAGE` and print one JSON object per module.
//! - `aihc-boot lex FILE`: print the tokens of one file, after layout.
//!   For debugging.
//! - `aihc-boot parse FILE`: print the syntax tree of one file. For
//!   debugging.
//! - `aihc-boot print FILE`: parse one file and print it back as source.
//!   This is what `check` sends to the reference parser.
//! - `aihc-boot run FILE.hs`: not implemented yet.

use std::fmt::Write as _;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::ExitCode;

const USAGE: &str = "\
usage:
  aihc-boot check --stage {parse|resolve|typecheck} --package NAME
  aihc-boot lex FILE
  aihc-boot parse FILE
  aihc-boot print FILE
  aihc-boot run FILE.hs
";

fn main() -> ExitCode {
    let args: Vec<String> = std::env::args().skip(1).collect();
    let result = match args.first().map(String::as_str) {
        Some("check") => check(&args[1..]),
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

// --- check -----------------------------------------------------------------

fn check(args: &[String]) -> Result<(), String> {
    let mut args = args.to_vec();
    let stage = take_option(&mut args, "--stage")?;
    let package = take_option(&mut args, "--package")?;
    if let Some(extra) = args.first() {
        return Err(format!("unexpected argument {extra}"));
    }
    if !matches!(stage.as_str(), "parse" | "resolve" | "typecheck") {
        return Err(format!("unknown stage {stage}"));
    }
    let pkg_dir = PathBuf::from("vendor").join(&package);
    if !pkg_dir.is_dir() {
        return Err(format!("{} is not a directory", pkg_dir.display()));
    }
    let mut files = Vec::new();
    haskell_files(&pkg_dir, &pkg_dir, &mut files);
    files.sort();
    let reference = Reference::for_package(&pkg_dir);
    let mut stdout = std::io::stdout().lock();
    let mut out = String::new();
    for file in files {
        let report = check_module(&file, &stage, &reference);
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

/// The reference parser, `aihc-parse` from `tools/aihc-parse`, and the
/// language flags of one package.
struct Reference {
    command: Option<PathBuf>,
    /// `-X` flags from the package's `.cabal` file.
    flags: Vec<String>,
}

impl Reference {
    fn for_package(pkg_dir: &Path) -> Reference {
        let command = std::env::var_os("AIHC_PARSE")
            .map(PathBuf::from)
            .or_else(|| find_in_path("aihc-parse"));
        Reference {
            command,
            flags: cabal_language_flags(pkg_dir),
        }
    }
}

fn find_in_path(name: &str) -> Option<PathBuf> {
    let path = std::env::var_os("PATH")?;
    std::env::split_paths(&path)
        .map(|dir| dir.join(name))
        .find(|p| p.is_file())
}

/// `-X` flags for `default-language` and `default-extensions` in the
/// package's `.cabal` file. The scan is line based: it reads the first
/// value of each field and the continuation lines of
/// `default-extensions`.
fn cabal_language_flags(pkg_dir: &Path) -> Vec<String> {
    let mut flags = Vec::new();
    let Ok(entries) = std::fs::read_dir(pkg_dir) else {
        return flags;
    };
    for entry in entries.flatten() {
        let path = entry.path();
        if path.extension().is_none_or(|e| e != "cabal") {
            continue;
        }
        let Ok(text) = std::fs::read_to_string(&path) else {
            continue;
        };
        let mut in_extensions = false;
        for line in text.lines() {
            let trimmed = line.trim();
            let lower = trimmed.to_ascii_lowercase();
            if let Some(value) = lower.strip_prefix("default-language:") {
                in_extensions = false;
                let value = value.trim();
                if !value.is_empty()
                    && !flags
                        .iter()
                        .any(|f: &String| f == &format!("-X{}", trimmed[17..].trim()))
                {
                    flags.push(format!("-X{}", trimmed[17..].trim()));
                }
            } else if let Some(value) = lower.strip_prefix("default-extensions:") {
                in_extensions = true;
                push_extensions(&mut flags, &trimmed[19..], value.is_empty());
            } else if in_extensions
                && line.starts_with(char::is_whitespace)
                && !trimmed.is_empty()
                && !trimmed.contains(':')
            {
                push_extensions(&mut flags, trimmed, false);
            } else {
                in_extensions = false;
            }
        }
    }
    flags
}

fn push_extensions(flags: &mut Vec<String>, value: &str, _empty: bool) {
    for ext in value.split(|c: char| c == ',' || c.is_whitespace()) {
        let ext = ext.trim();
        if !ext.is_empty() {
            let flag = format!("-X{ext}");
            if !flags.contains(&flag) {
                flags.push(flag);
            }
        }
    }
}

fn check_module(file: &Path, stage: &str, reference: &Reference) -> Report {
    let src = match std::fs::read_to_string(file) {
        Ok(src) => src,
        Err(e) => return Report::fail(format!("cannot read file: {e}"), None),
    };
    if file.extension().is_some_and(|e| e == "lhs") {
        return Report::fail("literate Haskell is not supported", None);
    }
    let module = match aihc_syntax::parse(&src) {
        Ok(module) => module,
        Err(e) => return Report::fail(format!("{}: {}", e.stage, e.message), Some(e.pos)),
    };
    // The round trip: aihc-parser must read the printed module as the
    // original.
    let Some(command) = &reference.command else {
        return Report::fail(
            "aihc-parse not found: set AIHC_PARSE or add it to PATH",
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
    if stage != "parse" {
        return Report::fail(format!("stage {stage} is not implemented yet"), None);
    }
    Report {
        ok: true,
        error: None,
        pos: None,
    }
}

/// Every Haskell source in a package, with the same rules as
/// `scripts/progress.py`: any `.hs`, `.lhs`, `.hsc` or `.hs-boot` file,
/// except `Setup.hs` at the package root.
fn haskell_files(root: &Path, dir: &Path, out: &mut Vec<PathBuf>) {
    let Ok(entries) = std::fs::read_dir(dir) else {
        return;
    };
    for entry in entries.flatten() {
        let path = entry.path();
        if path.is_dir() {
            haskell_files(root, &path, out);
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
