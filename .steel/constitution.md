# Project Constitution

## Project Identity

`gemini-cli-ide.el` is a single-package Emacs Lisp library that integrates the
Gemini CLI with Emacs through the Model Context Protocol (MCP). It runs an
in-Emacs MCP server (WebSocket and HTTP transports) that the Gemini CLI
connects to, so Gemini can see editor state (selection, buffers, diagnostics,
project) and operate on files. The package is a derivative of
`claude-code-ide.el` and inherits its architectural shape.

License: GPL-3.0-or-later. Author: Enze Chi. Version line of record:
`gemini-cli-ide.el` header `Version:` field.

## Governing Principles

1. **Emacs-native first.** Behavior must feel like Emacs, not like a port of
   another editor. Use `project.el`, `xref`, `imenu`, `treesit`, `flymake`,
   `flycheck`, `transient`, and standard buffer/window conventions. Do not
   reinvent these.
2. **MCP is the contract.** The package's public surface to external tools is
   the MCP protocol. Tool names, parameter schemas, and return shapes are an
   API; breaking them requires a deliberate version bump and changelog entry.
3. **Project-scoped sessions.** All state (server port, session, working
   directory) is keyed by project root from `project.el`. Multiple concurrent
   sessions across projects must work without interference.
4. **Defensive over external processes.** The Gemini CLI is an external
   subprocess that can crash, hang, or version-skew. Code that touches it must
   tolerate that — handle missing binary, non-zero exit, and protocol drift
   without wedging Emacs.
5. **No network calls from the Emacs package itself.** Gemini's network calls
   happen inside the `gemini` CLI subprocess. The Emacs side talks only to
   localhost over the MCP transport.
6. **Reversible, opt-in integrations.** Tool-set installation
   (`gemini-cli-ide-emacs-tools-setup`) and shell prompt-tracking integrations
   are opt-in and must be safe to omit. Defaults must not modify the user's
   shell, init file, or global state.
7. **Don't leak credentials, paths, or buffer contents.** Logging, debug
   output, and error messages must not inadvertently expose user data or
   filesystem layout beyond what the user already sees in Emacs.

## Technology Stack

- **Language:** Emacs Lisp (`lexical-binding: t` required in every file).
- **Emacs floor:** 28.1. Do not use APIs that are only available in 29+
  without a `fboundp`/`featurep` guard and a documented fallback.
- **Hard runtime dependencies** (declared in `Package-Requires`):
  - `websocket` 1.12+
  - `transient` 0.9.0+
  - `web-server` 0.1.2+
- **Soft / optional integrations:** `vterm`, `eat`, `with-editor`, `flymake`,
  `flycheck`, `treesit`. Code that touches these MUST gate on `featurep` /
  `fboundp` and use `declare-function` / `defvar` to keep byte-compilation
  warning-free when the optional package is absent.
- **External tool:** `gemini` CLI (https://github.com/google/gemini-cli). The
  package detects, launches, and speaks MCP to it; it does not bundle it.
- **Testing:** ERT, run via `emacs -batch -L . -l ert -l
  gemini-cli-ide-tests.el -f ert-run-tests-batch-and-exit`.
- **Build / lint:** byte-compile and (optionally) native-compile, both wired
  through `scripts/compile-and-test.sh`.

## Coding Standards

### File and naming conventions
- One concern per file. File name = feature prefix.
- Every public symbol is namespaced `gemini-cli-ide-` (or
  `gemini-cli-ide-<sub>-` for submodules). Private helpers use
  `gemini-cli-ide--` (double dash). Never introduce un-prefixed top-level
  symbols.
- Each `.el` file ends with `(provide 'feature-name)` matching its filename.
- Each `.el` file starts with the standard library header comment block, a
  `;;; Commentary:` section, and `;;; Code:`, and ends with `;;; <file> ends
  here`.

### Style
- `lexical-binding: t` is mandatory.
- Prefer `let*` over nested `let` for chained bindings; this matches the
  recently merged refactor (commit `ffbf5cd`).
- Use `cl-lib` (`cl-defun`, `cl-loop`, `cl-destructuring-bind`) — do not
  require the deprecated `cl` package.
- Use `pcase` for shape-matching over chained `cond`/`equal` when it improves
  clarity.
- `seq-*` and `map-*` over hand-rolled list/hash recursion.
- Indentation, trailing whitespace, and final newline are enforced by
  `scripts/format-and-clean.sh`. Run it before committing.
- Docstrings are required on every `defun`, `defmacro`, `defcustom`,
  `defvar`, and `defgroup`. Wrap to ~70 columns. First line is a complete
  sentence.

### Public API surface
- Anything intended for end-user invocation is `(interactive)` and documented
  in the README's *Usage* section.
- `defcustom` for user-tunable values, with `:type`, `:group
  'gemini-cli-ide'`, and a docstring that says what the value affects.
- Breaking changes to interactive command names, MCP tool names, or
  `defcustom` names require a CHANGELOG entry and a major/minor version bump.

### Error handling
- Surface user-actionable errors with `user-error`. Reserve `error` for
  programmer mistakes / invariant violations.
- Validate inputs at the MCP boundary; trust internal callers.
- Never silently swallow errors from the Gemini subprocess or the WebSocket
  layer — surface them, with enough context that the user can act.

## Development Guidelines

### Branching and flow
- Default integration branch: `develop`. Stable: `master`. PRs land into
  `develop`; `develop` periodically merges to `master`.
- New work branches off `develop` with prefix `feature/<short-slug>` (matches
  `.steel/config.json`). Use `fix/`, `chore/`, `docs/`, `refactor/` prefixes
  when more accurate.
- Steel-Kit workflow branches use `spec/` or whatever
  `.steel/config.json:git.branchPrefix` is set to at the time. `.steel/`
  artifacts are committed; ephemeral skill files
  (`.claude/commands/steel-*`, `.agents/skills/steel-*`) are gitignored.

### Commits
- Follow Conventional Commits: `feat(scope):`, `fix(scope):`,
  `refactor(scope):`, `chore(scope):`, `docs(scope):`, `perf(scope):`,
  `test(scope):`. Scope is the affected module, e.g. `mcp`, `terminal`,
  `prompt`, `cli`.
- Subject line ≤ 72 chars, imperative mood. Body explains *why*, not *what*.
- Steel-Kit Forge/Gauge commits keep their generated subjects unmodified
  (e.g. `forge(specification): iteration N output [iteration N]`).

### Pre-commit / pre-push expectations
Before pushing or asking for review, the following must pass locally:
1. `scripts/format-and-clean.sh <changed-files>` — formatting and whitespace.
2. `scripts/compile-and-test.sh` — byte-compile + ERT must both pass with
   zero warnings (treat warnings as failures).
3. `--with-native-compile` is recommended where the local Emacs supports it.

These are also enforced by Claude Code stop hooks in `.claude/settings.json`.

### Tests
- Every new MCP handler, every new interactive command with non-trivial
  logic, and every bug fix gets an ERT test in `gemini-cli-ide-tests.el`.
- Tests are isolated: no real Gemini CLI process, no real network, no
  external filesystem outside `make-temp-file` / `with-temp-buffer`.
- Tests for optional integrations (`vterm`, `eat`, `with-editor`) must skip
  cleanly when the dependency is absent — never fail because an optional
  package is missing.

### Code review
- All non-trivial changes go through PR review. Direct pushes to `develop`
  or `master` are reserved for release chores and trivial doc edits.
- A Steel-Kit Forge-Gauge cycle that ends in `VERDICT: APPROVE` does not
  replace human review; it precedes it.

## Constraints

### Compatibility
- **Emacs:** 28.1 is the floor. CI / local checks must pass on 28.1, 29.x,
  and the current stable. Any 29+-only API requires a guarded fallback.
- **OS:** macOS and Linux are first-class. Windows is best-effort — do not
  break it gratuitously, but platform-specific bugs do not block a release.
- **Shells:** vterm prompt-tracking snippets in the README must remain
  copy-pasteable for both bash and zsh.

### Performance
- MCP request handlers must not block Emacs redisplay perceptibly on
  realistic inputs. Long work goes through `make-thread`, async processes,
  or chunked timers — not synchronous loops on large buffers.
- Selection / buffer-state notifications fire frequently; they must be O(1)
  in user input size, not O(buffer).
- Byte-compilation must produce zero warnings. Native compilation must
  produce zero errors.

### Security
- The MCP server binds only to localhost. Any change that exposes it to
  non-loopback interfaces requires explicit user opt-in and a documented
  threat model.
- Do not log buffer contents, file paths outside the project, or
  authentication tokens. Debug logging is gated on
  `gemini-cli-ide-debug` / similar and off by default.
- Tool handlers that accept file paths must reject paths that escape the
  active project root unless the user has explicitly enabled wider access.

### Dependency hygiene
- Hard dependencies are the three listed in `Package-Requires`. Adding a
  fourth is a constitution-level decision and requires a documented reason.
- Optional dependencies are gated, never required at load time. The package
  must byte-compile and load with only the three hard dependencies installed.

### Versioning and release
- SemVer-ish: bump minor for new commands / new MCP tools / breaking changes
  to `defcustom` names; bump patch for bug fixes and internal refactors.
- Version is the source of truth in the `gemini-cli-ide.el` header. Tags
  follow `vX.Y.Z`.
