# Task 10: Rewire gemini-cli-ide-transient.el — Forge Iteration 1

## Files Changed
- `gemini-cli-ide-transient.el` — modified.

## Key Implementation Decisions
- **Drop seven `declare-function` lines** for symbols that lived in
  the deleted `gemini-cli-ide-mcp` module
  (`--active-sessions`, `-session-project-dir`, `-session-port`,
  `-session-client`, `-session-buffer`, `-session-last-buffer`,
  `--get-current-session`).
- **Add `(require 'emacs-mcp)`** so `emacs-mcp-connection-info` is
  resolvable.
- **Add three forward declarations** into `gemini-cli-ide`:
  `--get-process`, `--get-buffer-name`, `emacs-mcp-connection-info`
  (the last from emacs-mcp).
- **`gemini-cli-ide--has-active-session-p` rewritten** to check for
  the buffer + process pair via the surviving private state
  (`gemini-cli-ide--processes`). Same user-visible answer as before
  but uses the new ground truth.
- **`gemini-cli-ide--session-status` rewritten** similarly. The
  previous version reported "connected / disconnected" status of
  the MCP client — that distinction is no longer meaningful since
  the MCP server lives in `emacs-mcp` and connection state is
  per-session not per-project. New behavior simply says "Active
  session in [project]" or "No active session".
- **`gemini-cli-ide-show-mcp-sessions` rewritten** to dump
  `(emacs-mcp-connection-info)` (URL, host, port, lockfile path)
  and explicitly note that per-Gemini-buffer session enumeration
  is not yet exposed by `emacs-mcp` — tracked upstream.
- **`gemini-cli-ide-show-active-ports` rewritten** to a one-line
  `gemini-cli-ide-log` of the running server's port + URL, or "No
  emacs-mcp server is running."

## Deviations from Plan
- Plan said the helper additions should follow the existing
  `find_emacs_package` pattern; this file doesn't use that helper —
  this is just `(require 'emacs-mcp)` and forward declarations.
- The "connected/disconnected" indicator on `--session-status` is
  silently dropped — there is no equivalent semantic in the new
  architecture (the connection is per-session, not per-project).
  This is a minor UX regression in the transient menu's status
  line, not an FR-1 file deletion or AC-1 file presence issue.

## Tests Added
- None.

## Build verification
- `./scripts/compile-and-test.sh` byte-compile: PASSED.
- ERT: still red until Task 11.
