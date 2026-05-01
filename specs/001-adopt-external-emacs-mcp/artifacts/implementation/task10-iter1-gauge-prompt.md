# Gauge Code Review — Task 10 Iteration 1

## Task
Rewire `gemini-cli-ide-transient.el` to use `emacs-mcp` instead of
the deleted `gemini-cli-ide-mcp` symbols.

## Inputs
1. `/Users/ezchi/Projects/gemini-cli-ide/gemini-cli-ide-transient.el`
2. `specs/001-adopt-external-emacs-mcp/artifacts/implementation/task10-iter1-forge.md`
3. Tasks: Task 10. Spec: FR-3.

## Verify
1. No reference to any `gemini-cli-ide-mcp...` symbol remains.
2. `(require 'emacs-mcp)` is present near top.
3. `gemini-cli-ide-show-mcp-sessions` body uses
   `(emacs-mcp-connection-info)`.
4. `gemini-cli-ide-show-active-ports` body uses
   `(emacs-mcp-connection-info)`.
5. `gemini-cli-ide--has-active-session-p` and
   `--session-status` use `--get-process` /
   `--get-buffer-name` ground truth, not deleted helpers.
6. Byte-compile passes.

## Output
Standard review format. End with VERDICT.

## Hard rules
- Be strict.
- End with `VERDICT: APPROVE` or `VERDICT: REVISE`.
- Do NOT modify any files.
