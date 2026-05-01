# Gauge Code Review — Task 9 Iteration 1

## Task
Wire the emacs-mcp helpers into the session lifecycle, add the
`--require-emacs-mcp` guard at the top of every retained interactive
command (eight per NFR-1), ship the FR-13 deprecation shim, and
repurpose the `gemini-cli-ide-mcp-allowed-tools` defcustom. This is
the largest behavioral change in the implementation.

## Inputs to read

1. `gemini-cli-ide.el` — the entire file. Particularly:
   - The eight retained interactive commands (`gemini-cli-ide`,
     `gemini-cli-ide-resume`, `gemini-cli-ide-continue`,
     `gemini-cli-ide-check-status`, `gemini-cli-ide-stop`,
     `gemini-cli-ide-switch-to-buffer`,
     `gemini-cli-ide-list-sessions`,
     `gemini-cli-ide-insert-at-mentioned`).
   - `gemini-cli-ide-emacs-tools-setup` (the deprecation shim).
   - `gemini-cli-ide-mcp-allowed-tools` defcustom.
   - `gemini-cli-ide--build-gemini-command`.
   - `gemini-cli-ide--create-terminal-session`.
   - `gemini-cli-ide--start-session`.
   - `gemini-cli-ide--cleanup-on-exit`.
   - `gemini-cli-ide--ensure-mcp-server` (Task 7 helper, refactored
     in this task to return a boolean).

2. Forge artifact:
   `specs/001-adopt-external-emacs-mcp/artifacts/implementation/task9-iter1-forge.md`

3. Spec: FR-7, FR-8, FR-9, FR-10, FR-13, FR-14, NFR-1, NFR-7,
   AC-5, AC-6, AC-10.

4. Constitution coding standards.

## Verify

### 1. AC-6 BLOCKING fix from gauge iter-1
**Every** one of the eight commands MUST start with
`(gemini-cli-ide--require-emacs-mcp)` as its first non-trivial form
(after the `(interactive)` declaration). Iterate through the eight
listed in the inputs section and confirm. This was BLOCKING in
the previous gauge round; if any are missing it must be re-flagged.

### 2. Ownership refcount correctness
- `--ensure-mcp-server` returns t iff this call started the server.
- `--start-session` captures that return value and propagates it to
  the **terminal** buffer's `--owns-mcp-server` flag (NOT the
  user's source buffer).
- The error-recovery branch in `--start-session` decrements the
  counter and stops the server iff `we-started-it` was non-nil.
- `--cleanup-on-exit` calls `--release-mcp-server` inside
  `with-current-buffer terminal-buffer` BEFORE killing the buffer.
  Order matters because the helper reads the buffer-local flag.

### 3. Deleted-symbol scrubbing
- Confirm no remaining references to `gemini-cli-ide-mcp-...` (any
  hyphenated symbol from the deleted modules) inside
  `gemini-cli-ide.el`. The transient module is allowed to retain
  its `declare-function` lines until Task 10.

### 4. `gemini-cli-ide-insert-at-mentioned` rewrite
- Verify the new body uses `(use-region-p)` / `region-beginning` /
  `region-end` / `gemini-cli-ide--terminal-send-string`.
- Verify it does NOT call any deleted MCP push-notification helper.
- Acceptable behavioral change under FR-14? — it must be explicitly
  noted in CHANGELOG by Task 15.

### 5. `gemini-cli-ide-check-status` new behavior
- Reports BOTH Gemini CLI version (or "not installed") AND
  `emacs-mcp` connection-info (or "not running").

### 6. Deprecation shim (FR-13)
- `gemini-cli-ide-emacs-tools-setup` is autoloaded.
- Body emits `display-warning` once per Emacs session (guarded by
  the `gemini-cli-ide--deprecation-shown` defvar from Task 7).
- Warning text contains the literal strings:
  - "deprecated"
  - "(emacs-mcp-mode 1)"
  - "v0.4.0"
- Function does NOT call `emacs-mcp-mode`.
- Function does NOT register tools.

### 7. Defcustom repurpose
- `gemini-cli-ide-mcp-allowed-tools` defcustom symbol name
  preserved.
- Docstring describes mapping to `mcpServers.emacs.tools` JSON.
- `:type` widget still includes `auto`, nil, string, list-of-string.

### 8. Build cleanliness
- Forge artifact reports byte-compile PASSED.
- ERT is allowed to still be red (Task 11 fixes); the report should
  acknowledge that.

### 9. Constitutional alignment
- Principle 7 (no leakage): the `insert-at-mentioned` rewrite types
  user-selected text into the terminal — that text is going to
  Gemini regardless. The new code does NOT log it. Verify.
- Coding standards: no `cl` (deprecated), uses `cl-lib` /
  `setq-local` / `with-current-buffer` correctly.
- Performance: no synchronous loops; no O(buffer) selection
  handling.

## Severity rubric
- BLOCKING: missing guard on any of the eight commands; missing
  refcount on error path; deleted-symbol still present in main file;
  byte-compile fails; deprecation shim missing or wrong wording.
- WARNING: refcount edge case not exercised but fixable; minor
  docstring inconsistency.
- NOTE: stylistic observations.

## Output

```markdown
# Gauge Review — Task 9 Iteration 1

## Summary
(2–4 sentences.)

## Guard coverage check
(Per the eight commands: present? at top? confirmed.)

## Refcount audit
(Walk through start-success, start-failure, normal-stop, kill-buffer.)

## Issues

### BLOCKING
(...)

### WARNING
(...)

### NOTE
(...)

VERDICT: APPROVE
```
or REVISE.

## Hard rules
- Be strict on the guard coverage and the refcount audit — these
  are the high-stakes parts of the implementation.
- ERT being red is expected at this point. Do NOT flag it as
  blocking.
- End with exactly `VERDICT: APPROVE` or `VERDICT: REVISE`.
- Do NOT modify any files.
