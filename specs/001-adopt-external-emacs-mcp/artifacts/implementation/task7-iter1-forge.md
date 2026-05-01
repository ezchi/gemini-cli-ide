# Task 7: Add server-ownership refcount + --require-emacs-mcp guard — Forge Iteration 1

## Files Changed
- `gemini-cli-ide.el` — modified (new `;;; emacs-mcp ownership
  tracking` section after the existing `;;; Variables` block).

## Key Implementation Decisions
- **`gemini-cli-ide--owns-mcp-server`** — `defvar-local`, exactly as
  the plan specified.
- **`gemini-cli-ide--mcp-server-owner-count`** — package-global,
  always non-negative, guarded with `(max 0 ...)` on decrement.
- **`gemini-cli-ide--deprecation-shown`** — defvar guard for the
  FR-13 deprecation shim. Defined here in Task 7 because it is a
  module-level variable; the shim function itself is added in Task
  9.
- **Three `declare-function` forwards** for `emacs-mcp-start`,
  `emacs-mcp-stop`, `emacs-mcp-connection-info`. The actual
  `(require 'emacs-mcp)` happens earlier in the file.
- **`--require-emacs-mcp`** — exact AC-6 wording: the Emacs-floor
  branch contains the literal "Emacs 29.1"; the missing-dep branch
  contains the literal "emacs-mcp". Uses `cond` with two branches
  so the order is: version check first (cheaper than
  `featurep`), missing-dep check second.
- **`--ensure-mcp-server`** — idempotent: if a server is already
  running we attach without owning. The buffer-local flag is set
  via `setq-local` so the value is per-Gemini-buffer.
- **`--release-mcp-server`** — only acts when the buffer-local
  flag is non-nil; clears the flag immediately so a duplicate
  release is a no-op (idempotency requirement from plan §2.6).
  Counter is decremented through `(max 0 ...)` so it can never
  drift negative.

## Deviations from Plan
- The plan listed the deprecation `defvar` (`gemini-cli-ide--deprecation-shown`)
  in Task 9. It is defined here in Task 7 because state goes with
  state, and Task 9 will reference it. Defining it ahead of time is
  cleaner than splitting state and behavior across tasks. The
  function that uses it is still added in Task 9.

## Tests Added
- None in this task; Task 12 owns the new ERT tests (refcount
  acquire/release, --require-emacs-mcp missing/old-emacs).

## Build verification
- `./scripts/compile-and-test.sh` byte-compile: PASSED.
  ERT: still red (interim state per plan; Task 11 fixes).
