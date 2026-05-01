# Gauge Review Prompt — Task Breakdown Iteration 2

You are the **Gauge** in a dual-agent (Forge / Gauge) task-breakdown
loop. You reviewed iteration 1 and flagged one BLOCKING and two
WARNING issues. The Forge has now revised the task list. Determine
whether iteration 2 is ready to ship.

## Inputs you must read

1. **Project Constitution** (highest authority):
   - `/Users/ezchi/Projects/gemini-cli-ide/.steel/constitution.md`

2. **The current task list:**
   - `/Users/ezchi/Projects/gemini-cli-ide/specs/001-adopt-external-emacs-mcp/tasks.md`

3. **Your previous review:**
   - `/Users/ezchi/Projects/gemini-cli-ide/specs/001-adopt-external-emacs-mcp/artifacts/task_breakdown/iter1-gauge.md`

4. **The previous task list (for diffing):**
   - `/Users/ezchi/Projects/gemini-cli-ide/specs/001-adopt-external-emacs-mcp/artifacts/task_breakdown/iter1-forge.md`

5. **Context (read as needed):**
   - `specs/001-adopt-external-emacs-mcp/plan.md`
   - `specs/001-adopt-external-emacs-mcp/spec.md`
   - `specs/001-adopt-external-emacs-mcp/clarifications.md`

## Review focus

For each iter-1 issue, mark RESOLVED, PARTIAL, or NOT RESOLVED:

### Iter-1 BLOCKING
1. **AC-6 / NFR-7 silent-failure guard** — every retained
   interactive command must call `--require-emacs-mcp`. Verify
   iter-2's Task 9 step 4 lists all eight commands per NFR-1 and
   says "explicit call at top" for each.

### Iter-1 WARNING
1. **Task 8 granularity** — Task 8 was to be split into a
   write-settings helper task and a lifecycle/shim task. Verify
   iter-2's Task 8 (write-settings) and Task 9 (lifecycle + shim +
   defcustom + guards) are now distinct.
2. **Task 6 dependency note** — iter-1 said "tests broken until
   Task 9", which was wrong. Verify iter-2 says "Task 11" (or
   accurately points at the test-cleanup task).

### Iter-1 NOTE
1. **Phase 5 vs Phase 4 labeling for transient task** — verify
   iter-2 places the transient rewiring task under Phase 5
   alongside the test tasks (matching plan.md §6).

## Additional review

- Are any *new* issues introduced by iter-2's reshuffling (e.g.,
  forward references, broken numbering, lost verification of an
  AC)?
- Is the renumbering consistent? (16 tasks → 17 tasks; references
  in earlier tasks to "Task N" should match the new numbering.)
- Are all ACs still covered by some task's verification step?

## Output format

```markdown
# Gauge Review — Task Breakdown Iteration 2

## Summary
(2–4 sentences.)

## Iter-1 issue follow-up
- BLOCKING #1 (AC-6 guards): RESOLVED / PARTIAL / NOT RESOLVED — justification.
- WARNING #1 (Task 8 granularity): RESOLVED / PARTIAL / NOT RESOLVED — justification.
- WARNING #2 (Task 6 dep note): RESOLVED / PARTIAL / NOT RESOLVED — justification.
- NOTE #1 (phase mapping): RESOLVED / PARTIAL / NOT RESOLVED — justification.

## New Issues

### BLOCKING
(...)

### WARNING
(...)

### NOTE
(...)

## Numbering Consistency
(Walk through references to "Task N" inside other tasks; flag mismatches.)

## AC Coverage
(Per-AC: which task covers it, post-renumbering?)

VERDICT: APPROVE
```

OR

```markdown
... (same structure) ...

VERDICT: REVISE
```

## Hard rules

- Be strict.
- Do not invent new tasks the plan does not call for.
- The Project Constitution is the highest authority.
- End your output with **exactly** one of:
  `VERDICT: APPROVE`
  `VERDICT: REVISE`
  on its own line, with no other text after it.
- Do NOT modify any files.
