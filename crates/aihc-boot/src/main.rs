//! The `aihc-boot` command.
//!
//! `docs/PLAN.md` ("Compiler interface") defines the contract between this
//! binary and `scripts/progress.py`. The commands:
//!
//! - `aihc-boot check --stage STAGE --package NAME`: check every module
//!   of `vendor/NAME/` up to `STAGE` and print one JSON object per module.
//! - `aihc-boot lex FILE`: print the tokens of one file, after layout.
//!   For debugging.
//! - `aihc-boot run FILE.hs`: not implemented yet.

use std::fmt::Write as _;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::ExitCode;

const USAGE: &str = "\
usage:
  aihc-boot check --stage {parse|resolve|typecheck} --package NAME
  aihc-boot lex FILE
  aihc-boot run FILE.hs
";

fn main() -> ExitCode {
    let args: Vec<String> = std::env::args().skip(1).collect();
    let result = match args.first().map(String::as_str) {
        Some("check") => check(&args[1..]),
        Some("lex") => lex(&args[1..]),
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
    let mut stdout = std::io::stdout().lock();
    let mut out = String::new();
    for file in files {
        let report = check_module(&file, &stage);
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

fn check_module(file: &Path, stage: &str) -> Report {
    let src = match std::fs::read_to_string(file) {
        Ok(src) => src,
        Err(e) => {
            return Report {
                ok: false,
                error: Some(format!("cannot read file: {e}")),
                pos: None,
            }
        }
    };
    if file.extension().is_some_and(|e| e == "lhs") {
        return Report {
            ok: false,
            error: Some("literate Haskell is not supported".into()),
            pos: None,
        };
    }
    if let Err(e) = aihc_syntax::tokenize(&src) {
        return Report {
            ok: false,
            error: Some(format!("{}: {}", e.stage, e.message)),
            pos: Some(e.pos),
        };
    }
    // The lexer and layout pass accept the module. No later stage exists
    // yet, so every module fails here.
    Report {
        ok: false,
        error: Some(format!("stage {stage} is not implemented yet")),
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
