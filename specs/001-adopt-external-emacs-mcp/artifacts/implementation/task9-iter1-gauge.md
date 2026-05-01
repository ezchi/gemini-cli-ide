# Gauge Review — Task 9 Iteration 1

(Provider: gemini)

## Summary
The session lifecycle wiring, deprecation shim, and defcustom repurpose have been implemented. The `--require-emacs-mcp` guard has been successfully added to all required commands. However, there are remaining references to deleted MCP session symbols in the main file, which violates the deletion requirements and will break at runtime.

## Guard coverage check
- `gemini-cli-ide`: present, at top, confirmed.
- `gemini-cli-ide-resume`: present, at top, confirmed.
- `gemini-cli-ide-continue`: present, at top, confirmed.
- `gemini-cli-ide-check-status`: present, at top, confirmed.
- `gemini-cli-ide-stop`: present, at top, confirmed.
- `gemini-cli-ide-switch-to-buffer`: present, at top, confirmed.
- `gemini-cli-ide-list-sessions`: present, at top, confirmed.
- `gemini-cli-ide-insert-at-mentioned`: present, at top, confirmed.

## Refcount audit
- **Start success**: `--ensure-mcp-server` correctly increments the global counter and returns `t`. This is captured as `we-started-it` and successfully propagated to the terminal buffer via `gemini-cli-ide--owns-mcp-server`.
- **Start failure**: The `condition-case` error handler decrements the global counter and stops the server if `we-started-it` was `t`.
- **Normal stop / Kill buffer**: `--cleanup-on-exit` correctly calls `--release-mcp-server` inside `with-current-buffer` before the buffer is killed, safely inspecting the local flag and decrementing the count.

## Issues

### BLOCKING
- **Deleted symbols still present**: In `gemini-cli-ide--toggle-existing-window`, there are calls to `gemini-cli-ide-mcp--get-session-for-project` and `gemini-cli-ide-mcp-session-original-tab` (lines ~542-543). These symbols belonged to the deleted session management system and must be scrubbed from the file.

### WARNING
- **Refcount edge case in error path**: In `gemini-cli-ide--start-session`, if an error occurs *after* the terminal buffer is successfully created and `(setq-local gemini-cli-ide--owns-mcp-server t)` has executed (e.g., during `sleep-for` or window display), the error handler blindly decrements the global counter. Because the terminal buffer still exists and holds the `t` ownership flag, its `kill-buffer-hook` will later call `--release-mcp-server` and decrement the counter a *second* time. Consider clearing `we-started-it` after successfully transferring ownership to the buffer.

### NOTE
- `gemini-cli-ide-insert-at-mentioned` was rewritten to inject text into the terminal directly. As noted in the forge artifact, ensure this mechanism change is captured in the Task 15 CHANGELOG.
- ERT remaining red due to deleted symbol references in tests is expected at this stage and will be addressed in Task 11.

VERDICT: REVISE
