# Gauge Review Prompt — Planning Iteration 1

You are the **Gauge** in a dual-agent (Forge / Gauge) planning loop.
Review the implementation plan for spec
`001-adopt-external-emacs-mcp`.

## Inputs you must read

1. **Project Constitution** (highest authority):
   - `/Users/ezchi/Projects/gemini-cli-ide/.steel/constitution.md`

2. **The plan under review:**
   - `/Users/ezchi/Projects/gemini-cli-ide/specs/001-adopt-external-emacs-mcp/plan.md`

3. **Spec and clarifications it must satisfy:**
   - `/Users/ezchi/Projects/gemini-cli-ide/specs/001-adopt-external-emacs-mcp/spec.md`
   - `/Users/ezchi/Projects/gemini-cli-ide/specs/001-adopt-external-emacs-mcp/clarifications.md`

4. **Optional context** for feasibility judgment:
   - The codebase rooted at `/Users/ezchi/Projects/gemini-cli-ide/`
   - The external `emacs-mcp` at `/Users/ezchi/Projects/emacs-mcp/`
     **NOTE:** if your sandbox blocks reading the latter, the plan
     and clarifications cite specific file:line refs into it; rely
     on those.

## Review criteria

### 1. Spec coverage
For every FR / NFR / AC in the spec, verify the plan addresses it.
Build a mental checklist FR-1 .. FR-24, NFR-1 .. NFR-8, AC-1 ..
AC-11. Flag any that the plan silently skips or only hand-waves.

### 2. Architecture soundness
- Is the proposed component breakdown coherent?
- Are the new private helpers (`--ensure-mcp-server`,
  `--release-mcp-server`, `--write-gemini-settings`,
  `--require-emacs-mcp`) sufficient to deliver the FRs they claim
  to deliver? Are any obvious helpers missing?
- Does the refcount / ownership model in §2.6 actually preserve the
  invariant in FR-9 ("server started by package gets stopped by
  package; user-started server is never stopped")? Walk through
  edge cases: kill-buffer without stop, restart-Emacs while server
  is running, two Gemini buffers in same project.
- Is the `.gemini/settings.json` write contract in §3.1 robust?
  Specifically: atomic write, malformed-file handling, deep merge.

### 3. Simplicity
- Is anything over-engineered for the stated requirements?
- Does any phase introduce code that the constitution forbids
  (e.g., synchronous loops on large buffers, network calls from
  Emacs side, logging buffer contents)?
- Is anything in the plan beyond spec scope (§6 Out of Scope)? E.g.
  did the Forge sneak in a refactor that the spec did not ask for?

### 4. Risk assessment
- Are the §8 risks the *real* risks, or boilerplate?
- Are mitigations specific and actionable?
- Did the plan miss any obvious risk (e.g., `kill-emacs` while
  refcount > 0; `emacs-mcp-stop` call that itself errors)?

### 5. Testing strategy
- Does the new ERT list in §7.2 cover every new helper?
- Does §7.4 (AC coverage map) actually cover every AC, or only the
  easy ones?
- Are there ACs (e.g., AC-5, AC-10) that are listed as "manual
  verification" — is that the right call given they require a real
  Gemini CLI binary, or could they be automated with a mock?

### 6. Constitutional alignment
For each constitutional principle and constraint, does the plan
respect it? Specifically:
- Principle 3 — preserved via per-session model (verify §2 and the
  refcount logic don't accidentally undermine this).
- Principle 5 — no network calls from Emacs side (verify the
  `.gemini/settings.json` write doesn't sneak in any).
- Principle 7 — no leakage of paths/credentials in logs (verify the
  new helpers don't log full paths).
- Coding Standards — namespacing (`gemini-cli-ide-` /
  `gemini-cli-ide--`); `lexical-binding: t`; `let*` preference;
  `cl-lib` not `cl`.
- Constraints > Compatibility — Phase 1 amendment captured.
- Constraints > Performance — no synchronous loops; tools
  byte-compile with zero warnings.

## Output format

```markdown
# Gauge Review — Planning Iteration 1

## Summary
(2–4 sentences.)

## Spec Coverage
(Table or list: FR-N / AC-N → covered? where? hand-wavy?)

## Architecture Soundness
(Walk through the refcount model edge cases at minimum.)

## Simplicity
(Anything over-engineered or out-of-scope?)

## Risk Assessment
(Are §8 risks real and mitigated? Anything missed?)

## Testing Strategy
(Does §7 cover every new helper and every AC?)

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

- Be strict. Do not approve to be agreeable.
- Do not invent new requirements that the spec did not ask for.
- The Project Constitution is the highest authority.
- End your output with **exactly** one of:
  `VERDICT: APPROVE`
  `VERDICT: REVISE`
  on its own line, with no other text after it.
- Do NOT modify any files.
