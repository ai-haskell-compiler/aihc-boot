# AGENTS.md

These rules apply to all contributors: humans and AI agents.

## Project

aihc-boot is a minimal Haskell compiler. It has one goal: to compile
[aihc](https://github.com/ai-haskell-compiler/aihc) and its vendored
dependencies. Read [docs/PLAN.md](docs/PLAN.md) before you start work.

## Language: ASD-STE100

Write all communication in ASD-STE100 Simplified Technical English. This
rule applies to commit messages, pull request titles and descriptions,
review comments, issues, code comments and documentation.

- Use short sentences. Procedural sentences: 20 words maximum. Descriptive
  sentences: 25 words maximum.
- Write one instruction in each sentence.
- Use the active voice. Write "The script updates the README", not "The
  README is updated by the script".
- Use the imperative for instructions. Write "Run the script", not "You
  should run the script".
- Use one word for one meaning. Do not use synonyms for variety.
- Use simple verb tenses: simple present, simple past, simple future.
- Do not use contractions or idioms.
- Technical names (for example, package names, commands, file paths) are
  permitted as they are.

## Commits and pull requests: Conventional Commits

Use [Conventional Commits](https://www.conventionalcommits.org/) for every
commit message. Pull request titles also use this format, because a
squash merge uses the title as the commit message.

```
<type>(<optional scope>): <description>
```

- Types: `feat`, `fix`, `docs`, `refactor`, `perf`, `test`, `build`, `ci`,
  `chore`, `revert`.
- Scopes: a package name (for example `vendor/deepseq`) or an area (for
  example `tracker`, `parser`, `nix`).
- Write the description in the imperative and in lower case. Do not put a
  period at the end.
- Show a breaking change with `!` after the type or scope, and add a
  `BREAKING CHANGE:` footer.

Examples:

```
feat(parser): parse operator sections
build(nix): pin nixpkgs to a new revision
refactor(vendor/text): remove lazy Text
```

The daily progress workflow commits as `chore(progress): update progress`.

## Dependencies and builds: Nix

Nix captures all dependencies. Builds and runs must be reproducible.

- Add every tool and library to `flake.nix`. Do not tell contributors to
  install tools globally.
- Run all commands in the dev shell: `nix develop` for development,
  `nix develop .#tracker` for the progress tracker.
- Commit `flake.lock`. To update inputs, run `nix flake update` and commit
  the result in a separate `build(nix): ...` commit.
- Scripts must not download tools at run time. Downloads of source
  packages by `scripts/vendor.py` are permitted, because `vendor/lock.json`
  records the version and hash.
- Before you push, run `nix flake check`. It must pass.

## Workflow

- Do not edit the text between `<!-- progress:start -->` and
  `<!-- progress:end -->` in `README.md` manually. `scripts/progress.py`
  writes it.
- Add all packages to `boot.toml` before you vendor them with
  `scripts/vendor.py`.
- The vendored tree must continue to build with GHC. Make sure that
  `cabal build` succeeds in the dev shell before you push a simplification.
