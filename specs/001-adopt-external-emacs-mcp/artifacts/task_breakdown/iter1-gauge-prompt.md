# Gauge Review Prompt — Task Breakdown Iteration 1

You are the **Gauge** in a dual-agent (Forge / Gauge) task-breakdown
loop. Review the ordered task list for spec
`001-adopt-external-emacs-mcp`.

## Inputs you must read

1. **Project Constitution** (highest authority):
   - `/Users/ezchi/Projects/gemini-cli-ide/.steel/constitution.md`

2. **The task list under review:**
   - `/Users/ezchi/Projects/gemini-cli-ide/specs/001-adopt-external-emacs-mcp/tasks.md`

3. **The plan that drives the tasks:**
   - `/Users/ezchi/Projects/gemini-cli-ide/specs/001-adopt-external-emacs-mcp/plan.md`

4. **The spec and clarifications the tasks must satisfy:**
   - `/Users/ezchi/Projects/gemini-cli-ide/specs/001-adopt-external-emacs-mcp/spec.md`
   - `/Users/ezchi/Projects/gemini-cli-ide/specs/001-adopt-external-emacs-mcp/clarifications.md`

## Review criteria

### 1. Task completeness
For every phase in `plan.md` §6 and every numbered helper in
`plan.md` §2.1 / §10, verify the tasks list has at least one task
that owns it. Build a mental checklist:
- Phase 0: 1 task minimum (pre-flight).
- Phase 1: 1 task (constitution).
- Phase 2: ≥3 tasks (new file, package metadata, scripts).
- Phase 3: 1 task (deletion).
- Phase 4: ≥2 tasks (refcount/guard, settings/lifecycle/shim) +
  transient.
- Phase 5: ≥2 tasks (delete tests, add tests).
- Phase 6: ≥3 tasks (Commentary, README, CHANGELOG).
- Phase 7: ≥1 task (verification + AC sweep).

Flag any phase with too few tasks or any deliverable in §10 that no
task owns.

### 2. Ordering and dependencies
- Are dependencies between tasks declared correctly?
- Could any task be safely parallelized? (Note in WARNING; not a
  blocker.)
- Are there any forward references where a task uses something
  declared in a *later* task?
- Does Task 6's "tests broken until Task N" interim state get
  resolved before Task 15's verification?

### 3. Granularity
- Is any task too large to land in one or two commits?
- Is any task too small to be a meaningful work unit?
- For the largest task (likely Task 8 — `--write-gemini-settings`
  + lifecycle wiring + deprecation shim + defcustom repurpose),
  could / should it be split?

### 4. Verification criteria
- Does each task have a concrete, mechanical verification step?
- Are the verifications mapped to the spec's acceptance criteria
  (FR / NFR / AC numbers)?
- Are any ACs not covered by any task's verification?

### 5. Constitutional alignment
- Does any task introduce code or process that violates a
  constitutional rule (e.g., logging buffer contents, synchronous
  long loops, namespacing violations)?
- Is the byte-compile-zero-warnings constraint maintained at every
  intermediate state? (Phase 3 has a documented exception for ERT
  redness; that's acceptable per the plan.)

## Output format

```markdown
# Gauge Review — Task Breakdown Iteration 1

## Summary
(2–4 sentences.)

## Task Completeness
(Phase × Task table; flag any phase or §10 deliverable with no
owner.)

## Ordering and Dependencies
(Walk through the dependency chain; flag any forward refs or
unresolved interim states.)

## Granularity
(Is any task too large or too small?)

## Verification Criteria
(Per-AC: which task verifies it?)

## Constitutional Alignment
(Per-section.)

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
