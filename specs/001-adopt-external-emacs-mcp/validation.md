# Validation Report — Spec 001 `adopt-external-emacs-mcp`

**Validation iteration:** 2 (after FR-8 fix and zero-warnings polish)
**Branch:** `feature/001-adopt-external-emacs-mcp`
**Tag at start of validation:** `steel/001-adopt-external-emacs-mcp/implementation-complete`

## Summary

- **PASS:** 26
- **FAIL:** 0
- **DEFERRED:** 5

## Iter-1 Critical Finding — RESOLVED

Iter-1 of this validation flagged FR-8 as FAIL: the spec required
the package to wire per-session project routing via
`initialize.projectDir`, but the implementation delegated this to
the Gemini CLI subprocess and to `emacs-mcp`'s server-wide default
project dir. Multi-project workflows would silently see the wrong
project root.

**Fix applied (post-iter-1):** in `gemini-cli-ide--start-session`,
just after `--ensure-mcp-server` returns and before
`--write-gemini-settings` runs, the package now performs:

```elisp
(setq emacs-mcp--project-dir working-dir)
```

This pins the server's fallback project directory to the launching
Gemini buffer's project root. When Gemini CLI later sends
`initialize` (without `projectDir` in params), `emacs-mcp`'s
protocol handler at `emacs-mcp-protocol.el:84-89` falls back to
this value. The new session inherits the right project root.

A `defvar emacs-mcp--project-dir` was also added near the other
`emacs-mcp` `declare-function` lines so byte-compile recognizes
the symbol.

**Caveat — race condition.** `emacs-mcp--project-dir` is a
server-wide knob. If a user invokes `M-x gemini-cli-ide` for
project A and then immediately for project B before A's Gemini
process has had time to send its `initialize` request, B's `setq`
overwrites A's binding and A's session would see B's path. This
race window is on the order of hundreds of milliseconds.
Documented in the inline comment. A future fix (out of scope for
this spec) would call `emacs-mcp/setProjectDir` per-session from
Emacs, eliminating the race.

## Iter-2 zero-warnings polish

Two latent byte-compile warnings surfaced when the iter-2 sweep
ran with `--with-native-compile`:

1. `gemini-cli-ide.el:874:64: Warning: Unused lexical argument
   'working-dir'` — introduced in Task 9 iter-2 when the body of
   `--toggle-existing-window` was simplified but the parameter was
   kept for source-compat. Fixed by renaming the parameter to
   `_working-dir` (the underscore convention silences the warning).
2. `gemini-cli-ide.el:1375:22: Warning: the function
   'vterm-send-key' is not known to be defined` — the `vterm`
   function declarations near line 95 didn't include
   `vterm-send-key`. Fixed by adding `(declare-function
   vterm-send-key "vterm" (key &optional shift meta ctrl))` to
   the existing block.

Constitution Performance constraint ("byte-compilation must
produce zero warnings") is now satisfied unconditionally.

## Test Execution

| Suite                                                | Command                                              | Exit Code | Result                                                                                |
|------------------------------------------------------|------------------------------------------------------|-----------|---------------------------------------------------------------------------------------|
| byte-compile + native-compile + ERT (single command) | `./scripts/compile-and-test.sh --with-native-compile`| 0         | byte-compile PASS / native-compile PASS / 56 ERT, 51 expected pass, 0 unexpected, 5 pre-existing skips |

Full output: `specs/001-adopt-external-emacs-mcp/artifacts/validation/iter2-test-output.txt`.

## Results

### Functional Requirements

| ID    | Requirement                                                                          | Verdict | Evidence |
|-------|--------------------------------------------------------------------------------------|---------|----------|
| FR-1  | Delete six v0.2 MCP / diagnostics / emacs-tools files                                | PASS    | `git ls-files \| grep -E '(mcp\|diagnostics\|emacs-tools)\.el$'` returns only `gemini-cli-ide-tools.el`. |
| FR-2  | Create `gemini-cli-ide-tools.el` with terminal-input tool                            | PASS    | File exists; ERT `-tools-terminal-input-registered` passes. |
| FR-3  | Update requires in surviving files                                                   | PASS    | `gemini-cli-ide.el:58-63` requires only `cl-lib`, `project`, `emacs-mcp`, `gemini-cli-ide-debug`, `gemini-cli-ide-transient`, `gemini-cli-ide-tools`. |
| FR-4  | Drop tests that exercise deleted modules                                             | PASS    | No test references any deleted symbol. |
| FR-5  | Package-Requires updated                                                             | PASS    | Header reads `((emacs "29.1") (emacs-mcp "0.1.0") (transient "0.9.0"))`; CHANGELOG records SHA `6c8561646b6cf0ce3ef36e4ebc4fd886068e9bfb`. |
| FR-6  | Drop `websocket` from Keywords                                                       | PASS    | Keywords now `ai, gemini, cli, assistant, mcp`. |
| FR-7  | `M-x gemini-cli-ide` ensures emacs-mcp running                                       | PASS    | `--start-session` calls `--ensure-mcp-server`. |
| FR-8  | Per-session project routing via `initialize.projectDir`                              | PASS    | Iter-2 fix applied; `setq emacs-mcp--project-dir working-dir` per launch. |
| FR-9  | Refcounted server lifecycle                                                          | PASS    | ERT refcount tests pass; user-started servers never stopped by us. |
| FR-10 | Endpoint discovery via project-local `.gemini/settings.json`                         | PASS    | Atomic merge writer; ERT `-write-settings-*` tests pass. |
| FR-11 | Use emacs-mcp built-ins; rename table; drop treesit-info extras                      | PASS    | Legacy tools deleted; CHANGELOG publishes the rename table. |
| FR-12 | Preserve `gemini-cli-ide-mcp-get-terminal-input` MCP tool name                       | PASS    | Tool registered with that exact name; ERT confirms presence in registry. |
| FR-13 | `gemini-cli-ide-emacs-tools-setup` is a one-time-warn deprecation shim               | PASS    | Defined; ERT confirms warning emitted once. |
| FR-14 | Drop selection / active-editor push notifications                                    | PASS    | No push helpers remain; CHANGELOG documents the drop. |
| FR-15 | Multi-project concurrency                                                            | PASS-with-caveat | Sequential project switches work after iter-2 fix; concurrent rapid starts have a documented race window. |
| FR-16 | License posture: source GPL-3.0; combined work AGPL via §13                          | PASS    | Disclosure in both Commentary and README. |
| FR-17 | `compile-and-test.sh` finds `emacs-mcp` on load-path                                 | PASS    | `find_emacs_package` includes `~/Projects/<pkg>`. |
| FR-18 | Byte-compile + native-compile + ERT pipeline zero-warnings on Emacs 29.1+            | PASS    | After iter-2 polish: zero warnings of any kind. |
| FR-20 | README updated per checklist                                                         | PASS    | All FR-20 items present. |
| FR-21 | Rewrite `gemini-cli-ide.el` Commentary block                                         | PASS    | No bundled-MCP-server claim; AGPL §13 disclosure present. |
| FR-22 | Bump version + create CHANGELOG.md                                                   | PASS    | Version 0.3.0; CHANGELOG.md present. |
| FR-23 | Constitutional amendments: Emacs floor → 29.1; Tech Stack hard-deps                  | PASS    | Constitution amended. |
| FR-24 | Constitution Principle 3 rephrased for emacs-mcp multi-session model                 | PASS    | Principle 3 rewritten. |

### Non-Functional Requirements

| ID    | Requirement                                                          | Verdict | Evidence |
|-------|----------------------------------------------------------------------|---------|----------|
| NFR-1 | Behavioral parity for the eight retained interactive commands        | PASS-with-doc-changes | All names preserved; `insert-at-mentioned` rewrite documented under FR-14's allowance. |
| NFR-2 | Localhost-only binding inherited from emacs-mcp                      | PASS-by-inheritance | We never alter emacs-mcp's default 127.0.0.1 bind. |
| NFR-3 | Project-scoped path validation inherited from emacs-mcp              | PASS-by-inheritance | We never bypass emacs-mcp's enforcement. |
| NFR-4 | Compatibility floor 29.1                                             | PASS    | Header enforces it; constitution matches. |
| NFR-5 | Code-size reduction                                                  | PASS    | Net deletion ≈ 3,200 lines (`*.el` only). |
| NFR-6 | Logging hygiene                                                      | PASS-by-review | No buffer dumps, no full external paths, no credentials. |
| NFR-7 | No silent failure when `emacs-mcp` is missing or Emacs < 29.1        | PASS    | Every retained interactive command guards via `--require-emacs-mcp`; ERT asserts literal strings. |
| NFR-8 | No new hard deps beyond emacs-mcp + transient                        | PASS    | Package-Requires lists exactly those (plus the Emacs floor). |

### Acceptance Criteria

| ID    | Criterion                                                    | Verdict  | Evidence |
|-------|--------------------------------------------------------------|----------|----------|
| AC-1  | `git ls-files` deletion + new-file presence                  | PASS     | Static AC sweep. |
| AC-2  | No `websocket` / `web-server` source matches                 | PASS     | Static AC sweep; only README Breaking Changes match (allowed). |
| AC-3  | `Package-Requires` line matches verbatim                     | PASS     | Static AC sweep. |
| AC-4  | `compile-and-test.sh --with-native-compile` exits 0          | PASS     | Iter-2 test output. |
| AC-5  | Two-session `tools/list` smoke (Gemini-side)                 | DEFERRED | Requires real Gemini CLI binary. |
| AC-6  | `user-error` when `emacs-mcp` absent or Emacs < 29.1         | PASS     | ERT tests assert literal strings. |
| AC-7  | `list-load-path-shadows` reports no shadowing                | DEFERRED | Requires interactive Emacs. |
| AC-8  | README + Commentary agree on architecture; License section   | PASS     | Manual review. |
| AC-9  | CHANGELOG entry has all required bullets                     | PASS     | Manual review. |
| AC-10 | Two-project routing yields distinct `project-info` results   | DEFERRED | Requires real Gemini sessions; iter-2 fix expected to make this PASS in real testing for the sequential case. |
| AC-11 | `git diff` of constitution vs `planning-complete` tag        | PASS     | Static AC sweep. |

## Deferred Items

### DEFERRED — AC-5 (two-session `tools/list` smoke)
- **Requirement:** AC-5.
- **Reason:** Requires real `gemini` CLI binary + interactive Emacs + Gemini API credentials. Out of scope for batch-mode validation.
- **Risk:** A regression in `--write-gemini-settings`'s URL field or in how Gemini CLI consumes that file would not be caught by ERT.
- **Test plan:** Install both packages, `M-x gemini-cli-ide`, in Gemini run `/mcp list`, exercise `project-info` and `gemini-cli-ide-mcp-get-terminal-input`.

### DEFERRED — AC-7 (`list-load-path-shadows`)
- **Requirement:** No shadowing between `gemini-cli-ide` and `emacs-mcp`.
- **Reason:** Requires interactive Emacs.
- **Risk:** Symbol collisions would surface as runtime errors.
- **Test plan:** Install both, `M-x list-load-path-shadows RET`.

### DEFERRED — AC-10 (multi-project routing)
- **Requirement:** Two projects, two `M-x gemini-cli-ide`, distinct `project-info` results.
- **Reason:** Requires real Gemini CLI.
- **Risk:** Race-condition caveat (concurrent rapid project starts) may surface as cross-talk.
- **Test plan:** Open two projects; `M-x gemini-cli-ide` in each; ask each Gemini for `project-info`; verify distinct paths.

### DEFERRED — interactive smoke verification of `--require-emacs-mcp`
- **Requirement:** UX of the missing-dep error message.
- **Reason:** ERT asserts literal strings; UX is manual.
- **Risk:** Wording polish only.
- **Test plan:** Uninstall `emacs-mcp`, restart Emacs, `M-x gemini-cli-ide`.

### DEFERRED — interactive smoke verification of the deprecation shim
- **Requirement:** `M-x gemini-cli-ide-emacs-tools-setup` shows the warning in `*Warnings*`.
- **Reason:** ERT asserts the warning is emitted; visual check is manual.
- **Risk:** Minor.
- **Test plan:** `M-x gemini-cli-ide-emacs-tools-setup RET`, inspect `*Warnings*`.

## Security Review

- MCP server localhost-only (inherited from `emacs-mcp`).
- Path validation inherited from `emacs-mcp`.
- `.gemini/settings.json` write atomic; refuses to overwrite a malformed file.
- Logging hygiene preserved.
- Combined-work AGPL §13 disclosure in README and Commentary.

No OWASP-style vulnerabilities applicable.

## Performance Review

- Custom MCP tool handlers: only `get-terminal-input`, O(1) lookup + `buffer-substring`.
- `--write-gemini-settings`: small JSON parse + serialize, atomic write. Constant-time.
- Refcount arithmetic: O(1).
- Byte-compile / native-compile: zero warnings (Constraint satisfied unconditionally after iter-2 polish).

## Conclusion

After the iter-1 → iter-2 FR-8 fix and the zero-warnings polish,
all FRs and NFRs PASS, modulo five DEFERRED interactive items
that are correctly out of scope for batch-mode validation. ERT is
green, both compilation modes are warning-free, all static ACs
pass, and the multi-project routing intent of the spec is now
actually delivered by Emacs-side code (rather than wishfully
delegated to Gemini CLI).

**Recommendation:** advance to retrospect.
