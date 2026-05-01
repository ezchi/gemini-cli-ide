# Gauge Review Prompt — Task 1 Iteration 1

Task 1 is a pre-flight diagnostic task with NO code changes. Your
job is to verify the forge artifact records a real, green starting
state and a real `emacs-mcp` SHA.

## Inputs to read

- `specs/001-adopt-external-emacs-mcp/artifacts/implementation/task1-iter1-forge.md`
- `specs/001-adopt-external-emacs-mcp/tasks.md` (to confirm Task 1's verification criteria)

## Verify

1. The forge artifact reports `compile-and-test.sh` exit code 0 and
   "All tests passed".
2. The forge artifact records a 40-character hex SHA for
   `emacs-mcp` with a non-empty commit subject.
3. No code files were modified — this is a read-only task and the
   `git log -1 --stat` for the most recent forge commit should
   show only the artifact file.

## Output

Same review format used in earlier stages. End with VERDICT.

```markdown
# Gauge Review — Task 1 Iteration 1

## Summary
(1–2 sentences.)

## Issues
(BLOCKING / WARNING / NOTE — likely none for a diagnostic task.)

VERDICT: APPROVE
```
or
```
VERDICT: REVISE
```

## Hard rules
- End with exactly `VERDICT: APPROVE` or `VERDICT: REVISE`.
- Do NOT modify any files.
