# Gauge Code Review — Tasks 4 & 5 (Iteration 1, batched)

The Forge batched Tasks 4 and 5 into a single commit. You must produce
**two separate verdicts**, one per task. Output two complete review
documents (with their own VERDICT lines) — see the output format at
the end.

## Inputs to read

1. The current state of the changed files:
   - `/Users/ezchi/Projects/gemini-cli-ide/gemini-cli-ide.el` (lines 1–80 only — the header + require block)
   - `/Users/ezchi/Projects/gemini-cli-ide/gemini-cli-ide-tools.el`
   - `/Users/ezchi/Projects/gemini-cli-ide/scripts/compile-and-test.sh`
   - `/Users/ezchi/Projects/gemini-cli-ide/scripts/format-and-clean.sh`

2. Forge artifacts:
   - `/Users/ezchi/Projects/gemini-cli-ide/specs/001-adopt-external-emacs-mcp/artifacts/implementation/task4-iter1-forge.md`
   - `/Users/ezchi/Projects/gemini-cli-ide/specs/001-adopt-external-emacs-mcp/artifacts/implementation/task5-iter1-forge.md`

3. Spec / tasks:
   - `specs/001-adopt-external-emacs-mcp/spec.md` FR-5, FR-6, FR-17,
     FR-22, AC-3, AC-4.
   - `specs/001-adopt-external-emacs-mcp/tasks.md` Tasks 4 & 5.

## Verify Task 4

1. **AC-3 verbatim:** `Package-Requires` line in `gemini-cli-ide.el`
   must be exactly:
   `((emacs "29.1") (emacs-mcp "0.1.0") (transient "0.9.0"))`.
2. `Version:` is `0.3.0`.
3. `Keywords:` is `ai, gemini, cli, assistant, mcp` (no `websocket`).
4. The require block contains the new requires (`emacs-mcp` and
   `gemini-cli-ide-tools`) AND still contains the legacy
   `gemini-cli-ide-mcp` / `gemini-cli-ide-mcp-server` /
   `gemini-cli-ide-emacs-tools` requires (Task 6 deletes those —
   keeping them now is intentional per the plan).

## Verify Task 5

1. `find_emacs_package` accepts `~/Projects/<pkg>` lookup.
2. `EMACS_MCP_DIR` probe is present; `WEBSOCKET_DIR` probe is gone.
3. The test-runner line uses `$LOAD_PATH`, not a hard-coded `-L .`.
4. `format-and-clean.sh` Emacs invocation includes
   `-L ~/Projects/emacs-mcp` and the require list uses
   `(require 'emacs-mcp nil t)`.
5. Forge artifact is honest about the deviations:
   - Test-runner `$LOAD_PATH` fix.
   - Extra `(require 'emacs-mcp-tools)` /
     `(require 'emacs-mcp-session)` inside `gemini-cli-ide-tools.el`.
   - Both deviations are technically justified (without them
     `compile-and-test.sh` cannot pass on the new dep set).
6. Build verification claim is true: at HEAD,
   `./scripts/compile-and-test.sh` exits 0.

## Output format — produce TWO review blocks, each ending with its own VERDICT

```markdown
# Gauge Review — Task 4 Iteration 1

## Summary
(1–2 sentences.)

## Issues
(BLOCKING / WARNING / NOTE.)

VERDICT: APPROVE
```

then

```markdown
# Gauge Review — Task 5 Iteration 1

## Summary
(1–2 sentences.)

## Issues
(BLOCKING / WARNING / NOTE.)

VERDICT: APPROVE
```

(Substitute `VERDICT: REVISE` if appropriate. Each task has its own
verdict — don't conflate them.)

## Hard rules
- Be strict.
- End each task's block with exactly `VERDICT: APPROVE` or
  `VERDICT: REVISE`.
- Do NOT modify any files.
