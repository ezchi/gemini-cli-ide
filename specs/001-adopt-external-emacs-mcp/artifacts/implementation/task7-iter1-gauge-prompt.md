# Gauge Code Review — Task 7 Iteration 1

## Task
Add the server-ownership refcount data model and the
`--require-emacs-mcp` silent-failure guard helper. Helpers only —
wiring into interactive commands is Task 9.

## Inputs to read
1. `gemini-cli-ide.el` — read the new section starting at the
   `;;; emacs-mcp ownership tracking` heading.
2. Forge artifact:
   `/Users/ezchi/Projects/gemini-cli-ide/specs/001-adopt-external-emacs-mcp/artifacts/implementation/task7-iter1-forge.md`
3. Tasks: Task 7.
4. Spec: FR-9, NFR-7, AC-6.
5. Constitution: §"Coding Standards" (namespacing, lexical-binding,
   docstrings).

## Verify

1. **Required symbols exist with correct shapes:**
   - `(defvar-local gemini-cli-ide--owns-mcp-server nil ...)`
   - `(defvar gemini-cli-ide--mcp-server-owner-count 0 ...)`
   - `(defun gemini-cli-ide--require-emacs-mcp () ...)`
   - `(defun gemini-cli-ide--ensure-mcp-server () ...)`
   - `(defun gemini-cli-ide--release-mcp-server () ...)`

2. **`--require-emacs-mcp` semantics:**
   - Signals `user-error` (NOT plain `error`).
   - Old-Emacs branch contains the LITERAL string "Emacs 29.1" so
     Task 12's `gemini-cli-ide-test-require-emacs-mcp-old-emacs`
     can assert on it.
   - Missing-dep branch contains the LITERAL string "emacs-mcp" so
     Task 12's `gemini-cli-ide-test-require-emacs-mcp-missing` can
     assert on it.

3. **`--ensure-mcp-server` correctness:**
   - When `(emacs-mcp-connection-info)` is non-nil: do not start a
     new server, leave buffer-local flag nil.
   - When nil: start, set buffer-local flag, increment counter.

4. **`--release-mcp-server` correctness:**
   - Only acts when buffer-local flag is non-nil.
   - Clears the flag before decrementing (so duplicate release is
     a no-op).
   - Decrement is guarded with `(max 0 ...)`.
   - Stops the server when counter is zero AND the buffer owned
     it.

5. **Constitution compliance:**
   - Every `defvar` / `defun` has a docstring.
   - All new symbols use `gemini-cli-ide--` (private) prefix.
   - `lexical-binding: t` cookie was already at the top of
     `gemini-cli-ide.el` (unchanged).
   - No `cl` (deprecated) dependency; uses `cl-incf` from cl-lib.

6. **No scope creep:**
   - This task adds helpers; it does NOT wire them into any
     interactive command. Verify that no `(gemini-cli-ide--require-emacs-mcp)`
     calls exist inside other functions yet — that's Task 9's job.

## Output

```markdown
# Gauge Review — Task 7 Iteration 1

## Summary
(1–3 sentences.)

## Issues

### BLOCKING
(...)

### WARNING
(...)

### NOTE
(...)

VERDICT: APPROVE
```
or
```markdown
... VERDICT: REVISE
```

## Hard rules
- Be strict.
- ERT being red is expected (Task 6/11 owns that).
- End with exactly `VERDICT: APPROVE` or `VERDICT: REVISE`.
- Do NOT modify any files.
