# Gauge Code Review — Task 2 Iteration 1

## Task

Task 2 — Amend `.steel/constitution.md` for `emacs-mcp` adoption per
FR-23 and FR-24 of the spec.

## Inputs to read

1. **The amended constitution:**
   - `/Users/ezchi/Projects/gemini-cli-ide/.steel/constitution.md`

2. **The forge artifact for this task:**
   - `/Users/ezchi/Projects/gemini-cli-ide/specs/001-adopt-external-emacs-mcp/artifacts/implementation/task2-iter1-forge.md`

3. **The task description:**
   - `/Users/ezchi/Projects/gemini-cli-ide/specs/001-adopt-external-emacs-mcp/tasks.md` (Task 2)

4. **The full diff of the change:**
   - Run `git -C /Users/ezchi/Projects/gemini-cli-ide diff steel/001-adopt-external-emacs-mcp/task_breakdown-complete -- .steel/constitution.md`
     (i.e. compare against the tag set just before implementation
     started).

5. **Spec requirements that drove this:**
   - `specs/001-adopt-external-emacs-mcp/spec.md` FR-23, FR-24, AC-11.

## Verify

1. **FR-23 coverage:**
   - Constraints > Compatibility: Emacs floor `28.1` → `29.1` with
     stated rationale (emacs-mcp dep).
   - Technology Stack > Hard runtime dependencies: `websocket` and
     `web-server` removed; `emacs-mcp 0.1.0+` added; `transient`
     retained.

2. **FR-24 coverage:**
   - Principle 3 rephrased to the FR-24 final text (per-session
     routing via `initialize.projectDir` and
     `emacs-mcp/setProjectDir`; multi-session coexistence on one
     server).

3. **Internal consistency:**
   - Project Identity paragraph updated (no claim that this package
     itself runs an MCP server).
   - Coding Standards / Error handling: no orphan reference to
     "WebSocket layer".
   - Performance: no orphan reference to "selection / buffer-state
     notifications" (those are dropped per FR-14).
   - Security: bind-localhost rule still present, attributed
     correctly to `emacs-mcp`.

4. **Scope discipline:**
   - The forge artifact lists "additional consistency fixes" beyond
     the plan's three explicit items. Confirm each such fix is a
     direct logical consequence of FR-23 / FR-24 (not new policy).

5. **No silent dropping:**
   - Every other principle / standard / constraint that was present
     before still exists in some form. Use the diff to confirm
     deletions are limited to:
     - the `websocket 1.12+` and `web-server 0.1.2+` bullet entries,
     - the `selection / buffer-state notifications` performance
       bullet,
     - and inline references that no longer apply.

6. **AC-11 readiness:**
   - The diff (post-Task 2) of `.steel/constitution.md` against the
     tag `steel/001-adopt-external-emacs-mcp/planning-complete` is
     non-empty and consists of only the changes above. (You can
     verify against the `task_breakdown-complete` tag too — they
     point to the same constitution.)

## Output format

```markdown
# Gauge Review — Task 2 Iteration 1

## Summary
(1–3 sentences.)

## Coverage check
- FR-23 (Emacs floor): RESOLVED / PARTIAL / NOT RESOLVED
- FR-23 (Hard deps): RESOLVED / PARTIAL / NOT RESOLVED
- FR-24 (Principle 3 rephrasing): RESOLVED / PARTIAL / NOT RESOLVED

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
- The constitution IS the highest authority — but if a previous
  draft of the constitution itself contradicts the spec, the spec
  wins (this task is the very fix for that).
- End with exactly `VERDICT: APPROVE` or `VERDICT: REVISE`.
- Do NOT modify any files.
