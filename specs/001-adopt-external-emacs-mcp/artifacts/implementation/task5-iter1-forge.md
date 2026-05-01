# Task 5: Update build/format scripts to know about emacs-mcp — Forge Iteration 1

## Files Changed
- `scripts/compile-and-test.sh` — modified (find_emacs_package
  search list, dependency probe block, test runner load-path).
- `scripts/format-and-clean.sh` — modified (Emacs invocation `-L`
  flags, require list).
- `gemini-cli-ide-tools.el` — modified (added two more requires for
  emacs-mcp submodules; see "Key Implementation Decisions" below).

## Key Implementation Decisions
- **`find_emacs_package`** gained `~/Projects/"$package"` as a
  search location. This is needed because `emacs-mcp` is currently
  hosted at `~/Projects/emacs-mcp/` (per Task 1) and not yet on
  MELPA / `straight`. The new entry is generic — any sibling
  Projects directory will be discovered.
- **emacs-mcp probe replaces websocket probe.** The dropped
  `WEBSOCKET_DIR=$(find_emacs_package "emacs-websocket")` block is
  replaced with `EMACS_MCP_DIR=$(find_emacs_package "emacs-mcp")`.
- **Test runner load-path fix.** Discovered that the
  `ert-run-tests-batch-and-exit` invocation hard-coded `-L .` and
  did NOT use the `$LOAD_PATH` accumulated for the byte-compile
  step. With `emacs-mcp` outside `.`, the test load failed with
  "Cannot open load file: emacs-mcp". Fixed by changing the line
  to `emacs -batch $LOAD_PATH -l ert ...` so the test runner
  inherits the same load path. This is technically a wider scope
  than Task 5 strictly named, but it is necessary for `compile-
  and-test.sh` to actually pass against the new dep set, and was
  presumed by the spec / plan when they say "must remain green
  after Task 5". Without this fix the script could not validate
  any subsequent task.
- **format-and-clean.sh** — `-L .` → `-L . -L ~/Projects/emacs-mcp`
  so the Emacs invocation can `(require 'emacs-mcp nil t)`.
- **gemini-cli-ide-tools.el** — additional requires:
  `emacs-mcp-tools` (defines `emacs-mcp-deftool`) and
  `emacs-mcp-session` (defines `emacs-mcp-session-project-dir`).
  Without these, `byte-compile` or load-time expansion of
  `emacs-mcp-deftool` failed with "Symbol's function definition is
  void" — `emacs-mcp.el` only requires `cl-lib` at top level and
  loads its submodules lazily inside `emacs-mcp-start`. Documented
  with inline comments. This is a logically-Task-3 fix discovered
  while doing Task 5; rolled into this commit for the same reason
  as the test-runner fix.

## Deviations from Plan
- Plan listed only two file edits (`compile-and-test.sh` and
  `format-and-clean.sh`). Two additional touches were necessary:
  the test-runner `$LOAD_PATH` propagation and the two extra
  requires in `gemini-cli-ide-tools.el`. Both are direct
  consequences of running the build under the new dep set; without
  them, `./scripts/compile-and-test.sh` cannot pass.

## Tests Added
- None (script changes are verified by running
  `./scripts/compile-and-test.sh`).

## Build verification
- `./scripts/compile-and-test.sh` exits 0 with 84 tests,
  76 expected pass, 0 unexpected, 8 pre-existing skips. Confirms
  AC-3 and partial AC-4.
