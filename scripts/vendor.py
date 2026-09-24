#!/usr/bin/env python3
"""Fetch upstream sources for packages in boot.toml into vendor/<name>/.

  scripts/vendor.py deepseq        # one package
  scripts/vendor.py --all          # every keep/simplify package not yet vendored
  scripts/vendor.py --force text   # re-fetch, overwriting local edits

Hackage packages use `version` from boot.toml, or the latest preferred
version if unset. Git packages use `url`, `rev` and optional `subdir`.
Test, benchmark and example directories are removed: aihc-boot only has to
compile libraries and executables. If the package has an `include` list in
boot.toml, only the paths that match its globs stay. The fetched version and the upstream size
(after that pruning) are recorded in vendor/lock.json, which progress.py uses
as the baseline for "lines (upstream → now)".
"""

from __future__ import annotations

import argparse
import datetime
import hashlib
import io
import json
import shutil
import subprocess
import sys
import tarfile
import tempfile
import tomllib
import urllib.request
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
from progress import LOCK, MANIFEST, VENDOR, count_lines, haskell_files  # noqa: E402

HACKAGE = "https://hackage.haskell.org"
PRUNE_DIRS = {"test", "tests", "testsuite", "test-suite", "bench", "benchmark", "benchmarks", "examples", ".github"}


def fetch(url: str, accept: str | None = None) -> bytes:
    req = urllib.request.Request(url, headers={"Accept": accept} if accept else {})
    with urllib.request.urlopen(req) as r:
        return r.read()


def latest_version(name: str) -> str:
    info = json.loads(fetch(f"{HACKAGE}/package/{name}/preferred", "application/json"))
    versions = info.get("normal-version") or []
    if not versions:
        raise SystemExit(f"{name}: no preferred versions on Hackage")
    return max(versions, key=lambda v: [int(x) for x in v.split(".")])


def vendor_hackage(name: str, spec: dict, dest: Path) -> dict:
    version = spec.get("version") or latest_version(name)
    blob = fetch(f"{HACKAGE}/package/{name}-{version}/{name}-{version}.tar.gz")
    with tarfile.open(fileobj=io.BytesIO(blob)) as tar, tempfile.TemporaryDirectory() as tmp:
        tar.extractall(tmp, filter="data")
        shutil.copytree(Path(tmp) / f"{name}-{version}", dest)
    return {"source": "hackage", "version": version, "sha256": hashlib.sha256(blob).hexdigest()}


def vendor_git(name: str, spec: dict, dest: Path) -> dict:
    url, rev = spec["url"], spec["rev"]
    with tempfile.TemporaryDirectory() as tmp:
        git = ["git", "-C", tmp]
        subprocess.run(["git", "init", "-q", tmp], check=True)
        subprocess.run(git + ["fetch", "-q", "--depth", "1", url, rev], check=True)
        subprocess.run(git + ["checkout", "-q", "FETCH_HEAD"], check=True)
        src = Path(tmp) / spec.get("subdir", ".")
        shutil.copytree(src, dest, ignore=shutil.ignore_patterns(".git"))
    return {"source": "git", "url": url, "rev": rev, "subdir": spec.get("subdir", ".")}


def prune(dest: Path, include: list[str] | None) -> None:
    for path in sorted(dest.rglob("*"), reverse=True):
        if path.is_dir() and path.name in PRUNE_DIRS:
            shutil.rmtree(path)
    if include is None:
        return
    keep = {p for pattern in include for p in dest.glob(pattern)}
    if not keep:
        raise SystemExit(f"{dest.name}: no path matches the include list")
    for path in sorted(dest.rglob("*"), reverse=True):
        if path.is_file() and not any(a in keep for a in (path, *path.parents)):
            path.unlink()
        elif path.is_dir() and not any(path.iterdir()):
            path.rmdir()


def vendor(name: str, spec: dict, force: bool) -> dict:
    dest = VENDOR / name
    if dest.exists():
        if not force:
            raise SystemExit(f"{dest.relative_to(VENDOR.parent)} exists; use --force to overwrite local edits")
        shutil.rmtree(dest)
    source = spec.get("source")
    if source == "hackage":
        entry = vendor_hackage(name, spec, dest)
    elif source == "git":
        entry = vendor_git(name, spec, dest)
    else:
        raise SystemExit(f"{name}: plan '{spec['plan']}' has no upstream source to fetch")
    prune(dest, spec.get("include"))
    files = haskell_files(dest)
    entry["upstream_modules"] = len(files)
    entry["upstream_lines"] = count_lines(files)
    entry["vendored_at"] = datetime.date.today().isoformat()
    print(f"vendored {name}: {len(files)} modules, {entry['upstream_lines']} lines")
    return entry


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("packages", nargs="*")
    ap.add_argument("--all", action="store_true", help="vendor every missing keep/simplify package")
    ap.add_argument("--force", action="store_true", help="overwrite an existing vendor/<name>")
    args = ap.parse_args()

    manifest = tomllib.loads(MANIFEST.read_text())["packages"]
    names = list(args.packages)
    if args.all:
        names += [
            n for n, s in manifest.items() if s["plan"] in ("keep", "simplify") and not (VENDOR / n).exists()
        ]
    if not names:
        ap.error("name a package or pass --all")
    for n in names:
        if n not in manifest:
            raise SystemExit(f"{n}: not in boot.toml")

    VENDOR.mkdir(exist_ok=True)
    lock = json.loads(LOCK.read_text()) if LOCK.exists() else {}
    for n in names:
        lock[n] = vendor(n, manifest[n], args.force)
        LOCK.write_text(json.dumps(dict(sorted(lock.items())), indent=2) + "\n")
    return 0


if __name__ == "__main__":
    sys.exit(main())
