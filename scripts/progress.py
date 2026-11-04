#!/usr/bin/env python3
"""Measure aihc-boot's progress towards its milestones.

Reads boot.toml, the vendored sources under vendor/, and (when it has been
built) runs the boot compiler over them. Prints a Markdown report, or with
--write splices it into README.md and records a row in progress/history.csv.

The report only depends on what is measured, never on the current date, so
running it without any change in progress leaves the repository untouched.
"""

from __future__ import annotations

import argparse
import csv
import datetime
import json
import os
import re
import shlex
import shutil
import subprocess
import sys
import tomllib
from dataclasses import dataclass, field
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
MANIFEST = ROOT / "boot.toml"
VENDOR = ROOT / "vendor"
LOCK = VENDOR / "lock.json"
README = ROOT / "README.md"
HISTORY = ROOT / "progress" / "history.csv"
EVAL_DIR = ROOT / "tests" / "eval"
STAGE1_DIR = ROOT / "tests" / "stage1"

START = "<!-- progress:start -->"
END = "<!-- progress:end -->"

STAGES = ["parse", "resolve", "typecheck"]
HS_SUFFIXES = {".hs", ".lhs", ".hsc", ".hs-boot"}
# Stanzas whose build-depends aihc-boot has to satisfy.
BUILD_STANZAS = {"library", "executable", "common", "foreign-library"}


# --- Vendored sources -------------------------------------------------------


def haskell_files(pkg_dir: Path) -> list[Path]:
    """Every Haskell source in a vendored package, relative to ROOT."""
    files = []
    for path in sorted(pkg_dir.rglob("*")):
        if path.suffix not in HS_SUFFIXES or not path.is_file():
            continue
        if path.parent == pkg_dir and path.stem == "Setup":
            continue
        files.append(path.relative_to(ROOT))
    return files


def count_lines(files: list[Path]) -> int:
    """Non-blank lines across the given files."""
    total = 0
    for f in files:
        with open(ROOT / f, encoding="utf-8", errors="replace") as h:
            total += sum(1 for line in h if line.strip())
    return total


# The compiler and the platform that `if` conditions in .cabal files see.
GHC_VERSION = (9, 12, 3)
FALSE_OS = {"windows", "mingw32", "ghcjs", "wasi"}
FALSE_ARCH = {"javascript", "wasm32"}


def _version(text: str) -> tuple[int, ...]:
    return tuple(int(x) for x in re.findall(r"\d+", text))


def _impl(arg: str) -> bool:
    """impl(ghc >= 8.0 && < 9.4): the compiler is GHC_VERSION."""
    parts = arg.split(None, 1)
    if parts[0].lower() != "ghc":
        return False
    if len(parts) == 1:
        return True
    ops = {
        ">=": lambda a, b: a >= b,
        "<=": lambda a, b: a <= b,
        ">": lambda a, b: a > b,
        "<": lambda a, b: a < b,
        "==": lambda a, b: a[: len(b)] == b,
    }
    for term in parts[1].split("&&"):
        m = re.match(r"\s*(>=|<=|==|>|<)\s*([\d.]+)\s*$", term)
        if not m:
            return True
        if not ops[m.group(1)](GHC_VERSION, _version(m.group(2))):
            return False
    return True


def _condition(text: str, flags: dict[str, bool]) -> bool:
    """Evaluate the condition of an `if` line in a .cabal file."""
    tokens = re.findall(r"\(|\)|!|&&|\|\||[A-Za-z_][\w-]*\s*\([^()]*\)|[A-Za-z_][\w-]*", text)
    pos = 0

    def atom() -> bool:
        nonlocal pos
        tok = tokens[pos]
        pos += 1
        if tok == "!":
            return not atom()
        if tok == "(":
            value = disjunction()
            pos += 1  # ")"
            return value
        m = re.match(r"([A-Za-z_][\w-]*)\s*\((.*)\)", tok)
        if not m:
            return tok.lower() == "true"
        name, arg = m.group(1).lower(), m.group(2).strip()
        if name == "impl":
            return _impl(arg)
        if name == "flag":
            return flags.get(arg.lower(), True)
        if name == "os":
            return arg.lower() not in FALSE_OS
        if name == "arch":
            return arg.lower() not in FALSE_ARCH
        return True

    def conjunction() -> bool:
        nonlocal pos
        value = atom()
        while pos < len(tokens) and tokens[pos] == "&&":
            pos += 1
            value = atom() and value
        return value

    def disjunction() -> bool:
        nonlocal pos
        value = conjunction()
        while pos < len(tokens) and tokens[pos] == "||":
            pos += 1
            value = conjunction() or value
        return value

    return disjunction()


def _cabal_flags(lines: list[str]) -> dict[str, bool]:
    """The default value of every flag of a .cabal file."""
    flags: dict[str, bool] = {}
    name = None
    for line in lines:
        if not line.strip():
            continue
        if not line[0].isspace():
            m = re.match(r"flag\s+(\S+)", line, re.IGNORECASE)
            name = m.group(1).lower() if m else None
            if name is not None:
                flags[name] = True
            continue
        m = re.match(r"\s*default\s*:\s*(\S+)", line, re.IGNORECASE)
        if name is not None and m:
            flags[name] = m.group(1).lower() == "true"
    return flags


def cabal_build_depends(pkg_dir: Path, stanzas: list[str] | None = None) -> set[str]:
    """Package names in build-depends of the stanzas aihc-boot must build.

    `stanzas` limits the result to the named stanzas (for example
    "library" or "executable aihc"). A common stanza counts when one of
    these stanzas imports it. A dependency in the branch of an `if` that
    GHC_VERSION on a Unix-like system does not take does not count, and
    neither does a stanza with `buildable: False`.
    """
    wanted = {" ".join(s.lower().split()) for s in stanzas} if stanzas else None
    deps: set[str] = set()
    for cabal in pkg_dir.glob("*.cabal"):
        text_lines = cabal.read_text(encoding="utf-8", errors="replace").splitlines()
        lines = [raw.split("--", 1)[0].rstrip() for raw in text_lines]
        flags = _cabal_flags(lines)
        # The build-depends and the imports of each stanza, by header. The
        # key "" holds the top-level fields before the first stanza.
        stanza_deps: dict[str, set[str]] = {"": set()}
        imports: dict[str, set[str]] = {"": set()}
        counted = {""}
        stanza = ""
        # The open `if` blocks: (indent, parent active, branch taken, any branch taken).
        blocks: list[tuple[int, bool, bool, bool]] = []
        field = None  # (indent, name) of the field that continues
        for line in lines:
            if not line.strip():
                continue
            indent = len(line) - len(line.lstrip())
            text = line.strip()
            if indent == 0:
                stanza = " ".join(text.lower().split())
                stanza_deps.setdefault(stanza, set())
                imports.setdefault(stanza, set())
                kind = stanza.split()[0]
                if kind in BUILD_STANZAS - {"common"} and (wanted is None or stanza in wanted):
                    counted.add(stanza)
                blocks, field = [], None
                continue
            if field is not None and indent > field[0]:
                value = text
            else:
                field = None
                lower = text.lower()
                if lower == "else" or re.match(r"elif\s", lower):
                    while blocks and blocks[-1][0] > indent:
                        blocks.pop()
                    if blocks and blocks[-1][0] == indent:
                        _, parent, _, taken = blocks.pop()
                        cond = not taken and (lower == "else" or _condition(text[4:], flags))
                        blocks.append((indent, parent, cond, taken or cond))
                    continue
                while blocks and blocks[-1][0] >= indent:
                    blocks.pop()
                active = all(parent and cond for _, parent, cond, _ in blocks)
                if re.match(r"if\s", lower):
                    cond = _condition(text[2:], flags)
                    blocks.append((indent, active, cond, cond))
                    continue
                m = re.match(r"([\w-]+)\s*:(.*)", text)
                if not m or not active:
                    continue
                field = (indent, m.group(1).lower())
                value = m.group(2)
            if field[1] == "buildable" and value.strip().lower() == "false":
                counted.discard(stanza)
            elif field[1] == "import":
                imports[stanza].update(v.strip().lower() for v in value.split(",") if v.strip())
            elif field[1] == "build-depends":
                for item in value.split(","):
                    m = re.match(r"\s*([A-Za-z0-9][A-Za-z0-9-]*)", item)
                    if m:
                        stanza_deps[stanza].add(m.group(1))
        # The counted stanzas, and the common stanzas that they import.
        todo = list(counted)
        seen: set[str] = set()
        while todo:
            s = todo.pop()
            if s in seen:
                continue
            seen.add(s)
            deps |= stanza_deps.get(s, set())
            todo.extend("common " + i for i in imports.get(s, set()))
    return deps


# --- Boot compiler ----------------------------------------------------------


def compiler_command(config: dict) -> list[str] | None:
    """The boot compiler command, or None if it has not been built."""
    env = os.environ.get("AIHC_BOOT")
    cmd = shlex.split(env) if env else list(config.get("command", []))
    if not cmd:
        return None
    exe = cmd[0]
    if "/" in exe:
        path = (ROOT / exe).resolve()
        if not (path.is_file() and os.access(path, os.X_OK)):
            return None
        cmd[0] = str(path)
    elif shutil.which(exe) is None:
        return None
    return cmd


def run_stage(cmd: list[str], pkg: str, stage: str, timeout: int) -> set[str]:
    """Files (relative to ROOT) that pass `stage` in `pkg`.

    The compiler prints one JSON object per module on stdout:
      {"file": "vendor/<pkg>/<path>", "ok": true}
    Anything not reported as ok counts as a failure, so a crash or timeout
    part-way through still keeps the modules reported before it.
    """
    try:
        proc = subprocess.run(
            cmd + ["check", "--stage", stage, "--package", pkg],
            cwd=ROOT,
            capture_output=True,
            text=True,
            timeout=timeout,
        )
        out = proc.stdout
    except subprocess.TimeoutExpired as e:
        out = e.stdout.decode() if isinstance(e.stdout, bytes) else (e.stdout or "")
    passed = set()
    for line in out.splitlines():
        try:
            obj = json.loads(line)
        except json.JSONDecodeError:
            continue
        if isinstance(obj, dict) and obj.get("ok") is True and "file" in obj:
            passed.add(Path(obj["file"]).as_posix())
    return passed


def run_eval(cmd: list[str], test: Path, timeout: int) -> bool:
    expected = test.with_suffix(".stdout")
    stdin = test.with_suffix(".stdin")
    try:
        proc = subprocess.run(
            cmd + ["run", str(test.relative_to(ROOT))],
            cwd=ROOT,
            input=stdin.read_bytes() if stdin.exists() else b"",
            capture_output=True,
            timeout=timeout,
        )
    except subprocess.TimeoutExpired:
        return False
    return proc.returncode == 0 and proc.stdout == expected.read_bytes()


def run_stage1(cmd: list[str], script: Path, timeout: int) -> bool:
    env = dict(os.environ, AIHC_BOOT=shlex.join(cmd))
    try:
        proc = subprocess.run(
            ["bash", str(script)], cwd=ROOT, env=env, capture_output=True, timeout=timeout
        )
    except subprocess.TimeoutExpired:
        return False
    return proc.returncode == 0


# --- Measurement ------------------------------------------------------------


@dataclass
class Package:
    name: str
    plan: str
    used_by: list[str]
    note: str
    vendored: bool = False
    modules: list[str] = field(default_factory=list)
    lines: int = 0
    upstream_lines: int | None = None
    deps: set[str] = field(default_factory=set)
    stages: dict[str, int] = field(default_factory=dict)
    eval_total: int = 0
    eval_passed: int = 0


@dataclass
class Milestone:
    key: str
    title: str
    done: int
    total: int

    @property
    def percent(self) -> float:
        return 100.0 * self.done / self.total if self.total else 0.0


def measure(run_compiler: bool = True) -> dict:
    manifest = tomllib.loads(MANIFEST.read_text())
    config = manifest.get("compiler", {})
    lock = json.loads(LOCK.read_text()) if LOCK.exists() else {}
    cmd = compiler_command(config) if run_compiler else None

    packages: list[Package] = []
    for name, spec in manifest["packages"].items():
        pkg = Package(name, spec["plan"], spec.get("used_by", []), spec.get("note", ""))
        pkg_dir = VENDOR / name
        if pkg.plan != "drop" and pkg_dir.is_dir():
            pkg.vendored = True
            files = haskell_files(pkg_dir)
            pkg.modules = [f.as_posix() for f in files]
            pkg.lines = count_lines(files)
            pkg.upstream_lines = lock.get(name, {}).get("upstream_lines")
            pkg.deps = cabal_build_depends(pkg_dir, spec.get("stanzas"))
        packages.append(pkg)

    by_name = {p.name: p for p in packages}
    vendored = [p for p in packages if p.vendored]

    # Frontend stages.
    for pkg in vendored:
        for stage in STAGES:
            if cmd is None or not pkg.modules:
                pkg.stages[stage] = 0
                continue
            passed = run_stage(cmd, pkg.name, stage, config.get("stage_timeout", 1800))
            pkg.stages[stage] = sum(1 for m in pkg.modules if m in passed)

    # Eval tests: tests/eval/<pkg>/<name>.hs with <name>.stdout.
    for pkg in packages:
        tests = sorted(t for t in (EVAL_DIR / pkg.name).glob("*.hs") if t.with_suffix(".stdout").exists())
        pkg.eval_total = len(tests)
        if cmd is not None:
            pkg.eval_passed = sum(run_eval(cmd, t, config.get("eval_timeout", 60)) for t in tests)

    # Stage 1 checks: tests/stage1/*.sh.
    stage1 = sorted(STAGE1_DIR.glob("*.sh"))
    stage1_passed = 0
    if cmd is not None:
        stage1_passed = sum(run_stage1(cmd, s, config.get("stage1_timeout", 3600)) for s in stage1)

    # Dependency hygiene.
    all_deps = set().union(*(p.deps for p in vendored)) if vendored else set()
    unaccounted = sorted(d for d in all_deps if d not in by_name)

    def drop_done(p: Package) -> bool:
        users = [by_name.get(u) for u in p.used_by]
        return (
            bool(users)
            and all(u is not None and u.vendored for u in users)
            and not any(p.name in u.deps for u in users)
        )

    fetch = [p for p in packages if p.plan in ("keep", "simplify")]
    prune = [p for p in packages if p.plan in ("replace", "drop")]
    prune_done = sum(drop_done(p) if p.plan == "drop" else p.vendored for p in prune)
    total_modules = sum(len(p.modules) for p in vendored)

    milestones = [
        Milestone("M1", "Vendor", sum(p.vendored for p in fetch), len(fetch)),
        Milestone("M2", "Prune", prune_done, len(prune) + len(unaccounted)),
        *(
            Milestone(key, title, sum(p.stages.get(stage, 0) for p in vendored), total_modules)
            for key, title, stage in [
                ("M3", "Parse", "parse"),
                ("M4", "Resolve", "resolve"),
                ("M5", "Typecheck", "typecheck"),
            ]
        ),
        Milestone(
            "M6",
            "Eval",
            sum(p.eval_passed for p in packages),
            sum(p.eval_total for p in packages),
        ),
        Milestone("M7", "Stage 1", stage1_passed, len(stage1)),
    ]

    return {
        "compiler": cmd is not None,
        "packages": packages,
        "milestones": milestones,
        "unaccounted": unaccounted,
        "drop_done": {p.name: drop_done(p) for p in packages if p.plan == "drop"},
    }


# --- Rendering --------------------------------------------------------------


def bar(percent: float, width: int = 10) -> str:
    filled = round(percent / 100 * width)
    return "█" * filled + "░" * (width - filled)


def frac(done: int, total: int) -> str:
    return f"{done}/{total}" if total else "—"


def render(result: dict, last_change: str | None) -> str:
    out = []
    compiler = "built" if result["compiler"] else "not built yet (M3–M7 read 0)"
    out.append(f"_Last change in progress: {last_change or 'n/a'}. Boot compiler: {compiler}._")
    out.append("")
    out.append("| Milestone | Done | Progress |")
    out.append("| --- | ---: | --- |")
    for m in result["milestones"]:
        out.append(f"| **{m.key}** {m.title} | {frac(m.done, m.total)} | `{bar(m.percent)}` {m.percent:.1f}% |")
    out.append("")
    out.append("<details><summary>Per-package breakdown</summary>")
    out.append("")
    out.append("| Package | Plan | Status | Modules | Lines (upstream → now) | Parse | Resolve | Typecheck | Eval |")
    out.append("| --- | --- | --- | ---: | ---: | ---: | ---: | ---: | ---: |")
    for p in result["packages"]:
        if p.plan == "drop":
            status = "dropped ✓" if result["drop_done"][p.name] else "still used"
            out.append(f"| {p.name} | drop | {status} | | | | | | |")
            continue
        status = "vendored" if p.vendored else "not vendored"
        n = len(p.modules)
        if p.upstream_lines:
            lines = f"{p.upstream_lines} → {p.lines}"
        else:
            lines = str(p.lines) if p.vendored else ""
        cells = [frac(p.stages.get(s, 0), n) if p.vendored else "" for s in STAGES]
        out.append(
            f"| {p.name} | {p.plan} | {status} | {n if p.vendored else ''} | {lines} | "
            + " | ".join(cells)
            + f" | {frac(p.eval_passed, p.eval_total)} |"
        )
    out.append("")
    out.append("</details>")
    if result["unaccounted"]:
        out.append("")
        out.append(
            "Unaccounted dependencies (in vendored `build-depends` but not in `boot.toml`): "
            + ", ".join(f"`{d}`" for d in result["unaccounted"])
        )
    return "\n".join(out)


# --- History ----------------------------------------------------------------

HISTORY_FIELDS = ["date"] + [f"M{i}" for i in range(1, 8)] + ["modules", "lines", "upstream_lines"]


def history_row(result: dict, date: str) -> dict:
    vendored = [p for p in result["packages"] if p.vendored]
    row = {"date": date}
    for m in result["milestones"]:
        row[m.key] = f"{m.done}/{m.total}"
    row["modules"] = str(sum(len(p.modules) for p in vendored))
    row["lines"] = str(sum(p.lines for p in vendored))
    row["upstream_lines"] = str(sum(p.upstream_lines or 0 for p in vendored))
    return row


def read_history() -> list[dict]:
    if not HISTORY.exists():
        return []
    with open(HISTORY, newline="") as h:
        return list(csv.DictReader(h))


def write_history(rows: list[dict]) -> None:
    HISTORY.parent.mkdir(parents=True, exist_ok=True)
    with open(HISTORY, "w", newline="") as h:
        w = csv.DictWriter(h, fieldnames=HISTORY_FIELDS, lineterminator="\n")
        w.writeheader()
        w.writerows(rows)


def same_metrics(a: dict, b: dict) -> bool:
    return all(a.get(k) == b.get(k) for k in HISTORY_FIELDS if k != "date")


# --- Main -------------------------------------------------------------------


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--write", action="store_true", help="update README.md and progress/history.csv")
    ap.add_argument("--no-compiler", action="store_true", help="skip running the boot compiler")
    ap.add_argument("--date", help="date to record in history (default: today, UTC)")
    args = ap.parse_args()

    result = measure(run_compiler=not args.no_compiler)
    history = read_history()
    date = args.date or datetime.datetime.now(datetime.timezone.utc).date().isoformat()
    row = history_row(result, date)

    if args.write:
        # Only record a row when something moved, so the README (which shows
        # the date of the last change) stays byte-identical otherwise.
        if not history or not same_metrics(history[-1], row):
            if history and history[-1]["date"] == date:
                history[-1] = row
            else:
                history.append(row)
            write_history(history)
        report = render(result, history[-1]["date"])
        text = README.read_text()
        if START not in text or END not in text:
            print(f"README.md is missing the {START} / {END} markers", file=sys.stderr)
            return 1
        before, rest = text.split(START, 1)
        _, after = rest.split(END, 1)
        README.write_text(f"{before}{START}\n{report}\n{END}{after}")
    else:
        last = history[-1]["date"] if history and same_metrics(history[-1], row) else date
        print(render(result, last))
    return 0


if __name__ == "__main__":
    sys.exit(main())
