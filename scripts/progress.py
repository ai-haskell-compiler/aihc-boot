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
HS_SUFFIXES = {".hs", ".lhs", ".hsc"}
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


def cabal_build_depends(pkg_dir: Path) -> set[str]:
    """Package names in build-depends of the stanzas aihc-boot must build."""
    deps: set[str] = set()
    for cabal in pkg_dir.glob("*.cabal"):
        in_stanza = True  # top-level fields before any stanza
        field_indent = None
        for raw in cabal.read_text(encoding="utf-8", errors="replace").splitlines():
            line = raw.split("--", 1)[0].rstrip()
            if not line.strip():
                continue
            indent = len(line) - len(line.lstrip())
            if indent == 0:
                in_stanza = line.split()[0].lower() in BUILD_STANZAS
                field_indent = None
                continue
            if field_indent is not None and indent > field_indent:
                value = line
            else:
                field_indent = None
                m = re.match(r"\s*build-depends\s*:(.*)", line, re.IGNORECASE)
                if not m:
                    continue
                field_indent = indent
                value = m.group(1)
            if not in_stanza:
                continue
            for item in value.split(","):
                m = re.match(r"\s*([A-Za-z0-9][A-Za-z0-9-]*)", item)
                if m:
                    deps.add(m.group(1))
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
            pkg.deps = cabal_build_depends(pkg_dir)
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

    foundations = [
        (ROOT / "flake.nix").exists(),
        (ROOT / "flake.lock").exists(),
        (ROOT / ".github" / "workflows" / "progress.yml").exists(),
        START in README.read_text() if README.exists() else False,
    ]

    milestones = [
        Milestone("M0", "Foundations", sum(foundations), len(foundations)),
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

HISTORY_FIELDS = ["date"] + [f"M{i}" for i in range(8)] + ["modules", "lines", "upstream_lines"]


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
