# Task 6: Delete bundled MCP / diagnostics / emacs-tools files — Forge Iteration 1

## Files Changed
- DELETED: `gemini-cli-ide-mcp.el`,
  `gemini-cli-ide-mcp-handlers.el`, `gemini-cli-ide-mcp-server.el`,
  `gemini-cli-ide-mcp-http-server.el`,
  `gemini-cli-ide-diagnostics.el`,
  `gemini-cli-ide-emacs-tools.el`.
- `gemini-cli-ide.el` — modified (drop legacy `(require ...)` lines
  for the deleted modules; only `emacs-mcp`, `gemini-cli-ide-debug`,
  `gemini-cli-ide-transient`, `gemini-cli-ide-tools` remain).
- `gemini-cli-ide-tests.el` — modified (drop top-level
  `(require 'gemini-cli-ide-mcp-handlers)` near former line 185 and
  the top-level `(require 'gemini-cli-ide-mcp)` /
  `(require 'gemini-cli-ide-mcp-server)` block near former line
  1242. Inner-test-body `(require ...)` lines are NOT touched —
  Task 11 deletes the test bodies wholesale).

## Key Implementation Decisions
- **Two top-level test-file requires deleted in this task** (not
  Task 11 as the plan originally suggested). Without this, the
  byte-compile of `gemini-cli-ide-tests.el` fails with "Cannot open
  load file" — which would mean the post-Task-6 tree would not even
  byte-compile, contradicting the plan's "byte-compile MUST pass"
  requirement for this phase. The inner-test-body requires (e.g.,
  inside `ert-deftest` forms) only fire at run time and do NOT
  trip byte-compile, so those stay until Task 11.
- **Transient module's `declare-function` lines kept** — they refer
  to symbols that will be void at runtime, but `declare-function`
  doesn't fail at compile time. The active calls in the bodies of
  `gemini-cli-ide-show-mcp-sessions` etc. only run when the user
  invokes those interactive commands — Task 10 rewires them.

## Deviations from Plan
- Plan said test-file cleanup belonged to Task 11. We had to do
  the *minimum* test-file edits (two top-level require deletions)
  in this task to keep byte-compile green; Task 11 owns the rest.

## Tests Added
- None (this task is pure deletion).

## Build verification
- `./scripts/compile-and-test.sh` after this commit:
  - Byte-compilation: PASSED.
  - ERT: FAILED — many tests reference deleted symbols. This is
    the documented interim state for Task 6.

## Acceptance criteria progress
- AC-1 (deletion list): satisfied by this commit.
- AC-2 (no `websocket` / `web-server` references): the deleted
  files removed most of them. A full grep at this point shows
  matches only inside `gemini-cli-ide-tests.el` (those tests are
  doomed in Task 11) and possibly inside doc strings / commentary
  that Task 13 / 14 rewrites.
