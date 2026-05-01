# Task 9: Rewire session lifecycle, add guards, ship deprecation shim — Forge Iteration 1

## Files Changed
- `gemini-cli-ide.el` — modified extensively (this is the largest
  behavioral change in the implementation).

## Key Implementation Decisions

### 9.1 Drop `port` argument throughout
- `gemini-cli-ide--build-gemini-command` lost the `session-id`
  parameter and the entire MCP-config block (lines that used to call
  `gemini-cli-ide-mcp-server-ensure-server`,
  `gemini-cli-ide-mcp-server-get-config`, and shell-out to `gemini
  mcp add`). Endpoint discovery now lives in
  `.gemini/settings.json` (per FR-10), so the launcher does not
  need to manipulate the Gemini CLI's MCP registry imperatively.
- `gemini-cli-ide--create-terminal-session` lost the `port` and
  `session-id` parameters. The `GEMINI_CODE_SSE_PORT` env var was
  removed (Gemini CLI now reads the URL from the project-local
  settings file).

### 9.2 Wire helpers into `--start-session`
- First line of `--start-session` is now
  `(gemini-cli-ide--require-emacs-mcp)`.
- Replaced `(setq port (gemini-cli-ide-mcp-start working-dir))`
  with `(let ((we-started-it (gemini-cli-ide--ensure-mcp-server)))
  ... (gemini-cli-ide--write-gemini-settings working-dir) ...)`.
- The `we-started-it` boolean is propagated to the terminal buffer
  via `(setq-local gemini-cli-ide--owns-mcp-server t)` after the
  buffer is created, so the buffer-local flag lives on the right
  buffer (the Gemini terminal, not the source buffer the user was
  in when invoking the command).
- Refactored `--ensure-mcp-server` from "set buffer-local in caller"
  to "return whether we started it; caller decides where to put
  the flag." Avoids accidentally tagging the user's source buffer.
- The error-recovery branch of the `condition-case` now releases
  the server (decrement counter, stop if zero) only when
  `we-started-it` is non-nil.

### 9.3 Wire `--release-mcp-server` into shutdown paths
- `gemini-cli-ide--cleanup-on-exit` now calls
  `(gemini-cli-ide--release-mcp-server)` inside `with-current-buffer`
  on the Gemini terminal buffer **before** the buffer is killed.
  Order matters: the helper inspects the buffer-local
  `--owns-mcp-server` flag, so reading it must happen while the
  buffer is alive.
- The `kill-buffer-hook` set on the Gemini terminal buffer in
  `--start-session` continues to chain to `--cleanup-on-exit`, so
  direct `(kill-buffer terminal-buffer)` triggers the same flow.

### 9.4 Add `--require-emacs-mcp` guard at the top of every
retained interactive command (NFR-1 / AC-6 BLOCKING fix from
gauge iter-1):
- `gemini-cli-ide` — explicit call (fires before `--start-session`,
  which itself calls the guard for defense in depth).
- `gemini-cli-ide-resume` — explicit call.
- `gemini-cli-ide-continue` — explicit call.
- `gemini-cli-ide-check-status` — explicit call.
- `gemini-cli-ide-stop` — explicit call.
- `gemini-cli-ide-switch-to-buffer` — explicit call.
- `gemini-cli-ide-list-sessions` — explicit call.
- `gemini-cli-ide-insert-at-mentioned` — explicit call.

### 9.5 `gemini-cli-ide-check-status` — new behavior
- Per FR-13 plan §2.1, the command now reports BOTH the Gemini CLI
  status AND the `emacs-mcp` connection-info. Output format:
  ```
  Gemini CLI: <version>      (or "not installed")
  emacs-mcp: <url>           (or "not running")
  ```

### 9.6 `gemini-cli-ide-insert-at-mentioned` — rewritten
- The previous body called four deleted symbols
  (`gemini-cli-ide-mcp--get-buffer-project`,
  `gemini-cli-ide-mcp--get-session-for-project`,
  `gemini-cli-ide-mcp-session-client`,
  `gemini-cli-ide-mcp-send-at-mentioned`) and depended on the
  bundled MCP server's push-notification mechanism — which is
  dropped in this release per FR-14 / C-4.
- New behavior: get the active region, find the project's Gemini
  terminal buffer, and **type the selection directly into the
  terminal** via `gemini-cli-ide--terminal-send-string`. No Return
  is sent — the user can edit and then press Enter themselves.
- This is a deliberate change in *mechanism* (push notification →
  terminal injection) but preserves the user-visible outcome of
  the command (selected text appears in the Gemini prompt). The
  spec NFR-1 explicitly allows changes in behavior driven by
  FR-14; documenting in the CHANGELOG (Task 15).

### 9.7 FR-13 deprecation shim
- `gemini-cli-ide-emacs-tools-setup` is now a no-op `defun`,
  autoloaded, that emits a `display-warning` once per session
  (guarded by the `gemini-cli-ide--deprecation-shown` defvar from
  Task 7).
- It explicitly does NOT call `(emacs-mcp-mode 1)` and does NOT
  register tools. Tool registration happens automatically via
  `(require 'gemini-cli-ide-tools)`.
- Warning text is the exact wording from C-5: *"...is deprecated.
  Use (emacs-mcp-mode 1) and require 'gemini-cli-ide instead. Will
  be removed in v0.4.0."*

### 9.8 Repurpose `gemini-cli-ide-mcp-allowed-tools` defcustom
- Symbol name preserved (it's part of the user-facing API).
- Docstring rewritten to describe the new semantics: this variable
  drives the `mcpServers.emacs.tools` filter in
  `.gemini/settings.json`.
- `:type` widget unchanged in shape; tags clarified.

## Deviations from Plan
- `--cleanup-on-exit` was simplified more than the plan strictly
  required. The previous version called several deleted-symbol
  helpers (`gemini-cli-ide-mcp-stop-session`,
  `gemini-cli-ide-mcp-server-session-ended`); those calls had to
  be removed for byte-compile to pass anyway. The replacement
  release-and-kill flow is equivalent for our cleanup needs.
- `--ensure-mcp-server` semantics evolved from the Task 7 design:
  it now RETURNS the "we started it" boolean instead of poking a
  buffer-local in the caller. This is the right design — the
  buffer-local flag belongs on the terminal buffer, not the
  user's source buffer. The Task 7 helper was harmless either
  way (buffer-local was set on whatever buffer called the helper),
  but the explicit return value makes the wiring in `--start-session`
  obviously correct.
- `gemini-cli-ide-insert-at-mentioned` is rewritten to a different
  *mechanism* (terminal injection vs. MCP push notification) under
  FR-14's "push notifications dropped" allowance. CHANGELOG (Task 15)
  must call this out.

## Tests Added
- None in this task; Task 12 owns the new ERT.

## Build verification
- `./scripts/compile-and-test.sh` after this commit:
  - Byte-compilation: PASSED (zero warnings).
  - Native-compilation: skipped (not requested).
  - ERT: still red (Task 11 fixes — many tests still reference
    deleted symbols).
