# Gauge Code Review — Task 6 Iteration 1

## Task
Hard-delete the bundled MCP / diagnostics / emacs-tools files and
drop the corresponding top-level requires in `gemini-cli-ide.el` and
`gemini-cli-ide-tests.el`. Byte-compile must remain green; ERT may
be red until Task 11.

## Inputs to read
1. Forge artifact:
   `/Users/ezchi/Projects/gemini-cli-ide/specs/001-adopt-external-emacs-mcp/artifacts/implementation/task6-iter1-forge.md`
2. Tasks:
   `specs/001-adopt-external-emacs-mcp/tasks.md` Task 6.
3. Spec: FR-1, FR-3, AC-1.

## Verify

1. **Deletion completeness** — `git ls-files` (effectively, scan
   the working tree) MUST NOT contain any of:
   - `gemini-cli-ide-mcp.el`
   - `gemini-cli-ide-mcp-handlers.el`
   - `gemini-cli-ide-mcp-server.el`
   - `gemini-cli-ide-mcp-http-server.el`
   - `gemini-cli-ide-diagnostics.el`
   - `gemini-cli-ide-emacs-tools.el`

2. **Surviving file integrity:**
   - `gemini-cli-ide-tools.el` — present.
   - `gemini-cli-ide.el` — present, with require block now reading
     exactly:
     ```
     (require 'cl-lib)
     (require 'project)
     (require 'emacs-mcp)
     (require 'gemini-cli-ide-debug)
     (require 'gemini-cli-ide-transient)
     (require 'gemini-cli-ide-tools)
     ```

3. **Test file:** Top-level requires for the deleted modules are
   gone. (Inner-test-body requires inside `ert-deftest` may still
   exist — Task 11 removes those tests.)

4. **Byte-compile state:** Verify by reading the forge artifact's
   "Build verification" section that byte-compile is reported
   green and ERT is reported red.

5. **Scope discipline:** The deviation note honestly admits two
   test-file edits had to be done in Task 6 to keep byte-compile
   green; verify those are minimal (only top-level requires) and
   not a sneaky Task-11 workload absorbed into this commit.

## Output
Standard format. End with VERDICT.

```markdown
# Gauge Review — Task 6 Iteration 1

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

OR

```markdown
... VERDICT: REVISE
```

## Hard rules
- Be strict.
- ERT being red is EXPECTED for this task. Do NOT flag it as
  blocking.
- End with exactly `VERDICT: APPROVE` or `VERDICT: REVISE`.
- Do NOT modify any files.
