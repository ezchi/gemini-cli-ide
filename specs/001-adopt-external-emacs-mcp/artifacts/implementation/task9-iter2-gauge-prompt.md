# Gauge Code Review — Task 9 Iteration 2

You reviewed iteration 1 and flagged one BLOCKING (deleted-symbol
references in `--toggle-existing-window`) and one WARNING (refcount
double-decrement edge case). The Forge has now revised. Determine
whether iter 2 ships.

## Inputs
1. `gemini-cli-ide.el` — particularly:
   - `gemini-cli-ide--toggle-existing-window` (around line 860).
   - `gemini-cli-ide--start-session` (around line 1045).
2. Forge iter-2 artifact:
   `specs/001-adopt-external-emacs-mcp/artifacts/implementation/task9-iter2-forge.md`
3. Iter-1 gauge:
   `specs/001-adopt-external-emacs-mcp/artifacts/implementation/task9-iter1-gauge.md`

## Verify

### Iter-1 BLOCKING fix
- `gemini-cli-ide--toggle-existing-window` no longer contains any
  reference to `gemini-cli-ide-mcp--get-session-for-project` or
  `gemini-cli-ide-mcp-session-original-tab`.
- The "tab tracking" feature is replaced by a comment that
  documents the drop and points at FR-14.
- `working-dir` parameter may now be unused; that's acceptable
  as long as byte-compile still passes.

### Iter-1 WARNING fix
- After the terminal buffer's `setq-local gemini-cli-ide--owns-mcp-server t`,
  there is a `(setq we-started-it nil)` so the surrounding
  `condition-case` error branch is a no-op for any failure that
  happens after ownership has been transferred.
- The buffer's `kill-buffer-hook` is the sole owner of the
  decrement past that point; counter cannot drift.

### Sanity
- Byte-compile is reported PASSED in the forge artifact.
- No new symbols introduced; no unrelated changes.

## Output

```markdown
# Gauge Review — Task 9 Iteration 2

## Summary
(1–2 sentences.)

## Iter-1 issue follow-up
- BLOCKING (deleted-symbol scrub): RESOLVED / PARTIAL / NOT RESOLVED.
- WARNING (refcount double-decrement): RESOLVED / PARTIAL / NOT RESOLVED.

## New Issues
### BLOCKING / WARNING / NOTE

VERDICT: APPROVE
```
or REVISE.

## Hard rules
- Be strict.
- End with exactly `VERDICT: APPROVE` or `VERDICT: REVISE`.
- Do NOT modify any files.
