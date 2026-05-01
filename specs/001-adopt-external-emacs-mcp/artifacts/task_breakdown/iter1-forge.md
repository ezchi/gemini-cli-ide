# Tasks — Spec 001 `adopt-external-emacs-mcp`

Ordered, actionable task list derived from `plan.md`. Each task is a
self-contained unit of work that should produce one git commit
(unless explicitly noted as combining with a follow-up task).

The phase numbering matches `plan.md` §6 (Implementation Strategy).

---

## Phase 0 — Pre-flight

### Task 1 — Confirm green starting state and capture upstream SHA

**Description.** Verify that the unmodified tree byte-compiles and
tests pass before any deletion. Capture the exact `emacs-mcp` git
SHA we will document as the tested reference in `CHANGELOG.md`.

**Steps.**
1. From the repo root, run `./scripts/compile-and-test.sh` and
   confirm exit 0.
2. Run `git -C ~/Projects/emacs-mcp rev-parse HEAD` and record the
   value; this becomes the tested-SHA reference for FR-5 / OQ-7
   resolution.
3. Run `git -C ~/Projects/emacs-mcp log -1 --pretty='%H %s'` and
   record the commit subject for the CHANGELOG entry.

**Files.** None modified. (Read-only.)

**Dependencies.** None.

**Verification.**
- `compile-and-test.sh` exit code is 0.
- The recorded SHA is a 40-char hex string committed to scratch
  notes (used in Task 18).

---

## Phase 1 — Constitutional amendments

### Task 2 — Amend `.steel/constitution.md` for emacs-mcp adoption

**Description.** Land the FR-23 / FR-24 amendments BEFORE any code
changes, so the rest of the work is trivially compliant.

**Steps.**
1. Edit `.steel/constitution.md`:
   - **Constraints > Compatibility:** change Emacs floor from
     `28.1` to `29.1`. Add a one-sentence rationale: "Required by
     the `emacs-mcp` dependency adopted in spec 001."
   - **Technology Stack > Hard runtime dependencies:** drop
     `websocket 1.12+` and `web-server 0.1.2+`; add
     `emacs-mcp 0.1.0+`.
   - **Governing Principles > Principle 3 (Project-scoped
     sessions):** rephrase to "Each Gemini buffer corresponds to a
     distinct `emacs-mcp` session pinned to its project root via
     the `initialize.projectDir` parameter and adjustable via
     `emacs-mcp/setProjectDir`; multiple Gemini buffers across
     projects must coexist on a single underlying server process
     without cross-contamination."
2. Commit with message
   `steel(constitution): amend for spec 001 — emacs-mcp adoption`.

**Files.** `.steel/constitution.md`.

**Dependencies.** Task 1.

**Verification (AC-11).**
- `git diff steel/001-adopt-external-emacs-mcp/planning-complete -- .steel/constitution.md`
  shows exactly the three changes above and no others.
- Reading the file end-to-end shows no contradictions between the
  amended sections and the rest of the constitution.

---

## Phase 2 — Add new file and update package metadata

### Task 3 — Create `gemini-cli-ide-tools.el` with the terminal-input tool

**Description.** Create the new module that registers
Gemini-specific MCP tools. In this release, the only tool is
`gemini-cli-ide-mcp-get-terminal-input`. Tool registration runs at
file load time via `emacs-mcp-deftool`. The terminal-extraction
helper is moved verbatim from the soon-to-be-deleted
`gemini-cli-ide-emacs-tools.el`.

**Steps.**
1. Create `gemini-cli-ide-tools.el` with the standard library
   header (lexical-binding, GPL-3.0-or-later notice, Author,
   Keywords).
2. Add `(require 'emacs-mcp)` and `(require 'gemini-cli-ide-debug)`.
3. Move `gemini-cli-ide--get-terminal-input` and any private
   helpers it depends on from `gemini-cli-ide-emacs-tools.el` into
   the new file. (DO NOT delete the original yet — Task 6 owns the
   deletion.)
4. Register the tool with `emacs-mcp-deftool` using the exact
   MCP-facing name `gemini-cli-ide-mcp-get-terminal-input` and the
   exact description text from
   `gemini-cli-ide-emacs-tools.el:369`.
5. Inside the handler, look up the active session's project dir via
   `(emacs-mcp-session-project-dir (emacs-mcp--session-get
   emacs-mcp--current-session-id))`, derive the Gemini buffer name,
   and call the moved extraction helper.
6. End the file with `(provide 'gemini-cli-ide-tools)` and the
   trailing `;;; gemini-cli-ide-tools.el ends here`.
7. Run `./scripts/format-and-clean.sh gemini-cli-ide-tools.el` and
   `./scripts/compile-and-test.sh` — both must remain green.

**Files.** Create: `gemini-cli-ide-tools.el`.

**Dependencies.** Task 2.

**Verification (FR-2, FR-12).**
- `byte-compile-file "gemini-cli-ide-tools.el"` produces zero
  warnings.
- After `(require 'gemini-cli-ide-tools)`, the form
  `(emacs-mcp--tools)` (or whatever stable accessor exists) shows
  an entry whose `:name` is `gemini-cli-ide-mcp-get-terminal-input`.

---

### Task 4 — Update package metadata in `gemini-cli-ide.el`

**Description.** Update headers and the require block, but DO NOT
yet delete the old `(require 'gemini-cli-ide-mcp...)` lines —
that's Task 6's responsibility, after the new module is wired up.

**Steps.**
1. Edit `gemini-cli-ide.el` lines 6–8:
   - `Version:` → `0.3.0`.
   - `Package-Requires:` → `((emacs "29.1") (emacs-mcp "0.1.0") (transient "0.9.0"))`.
   - `Keywords:` → drop `websocket`. Final list: `ai, gemini, cli, assistant, mcp`.
2. Add `(require 'emacs-mcp)` and `(require 'gemini-cli-ide-tools)`
   to the require block (lines 61–67), positioned alongside the
   other requires. Leave the existing
   `(require 'gemini-cli-ide-mcp ...)` entries in place for now.
3. Run `./scripts/compile-and-test.sh` — must remain green
   (intentional duplication is fine).

**Files.** `gemini-cli-ide.el`.

**Dependencies.** Task 3.

**Verification (FR-5, FR-6, FR-22, AC-3).**
- The `Package-Requires` line in `gemini-cli-ide.el` matches AC-3
  exactly: `(emacs "29.1") (emacs-mcp "0.1.0") (transient "0.9.0")`.
- Byte-compile against the new dep set succeeds with zero warnings.

---

### Task 5 — Update build/format scripts to know about `emacs-mcp`

**Description.** Make the local CI scripts able to find `emacs-mcp`
on `load-path` and stop requiring deleted modules for Lisp
indentation.

**Steps.**
1. Edit `scripts/compile-and-test.sh`: add an `emacs-mcp` lookup
   to the `find_emacs_package` block (mirrors the existing
   `websocket` / `transient` / `vterm` entries). Append the
   resolved path to `LOAD_PATH`.
2. Edit `scripts/format-and-clean.sh:31-46`: replace
   `(require 'gemini-cli-ide-mcp-server nil t)` with
   `(require 'emacs-mcp nil t)`. The `nil t` form means
   "don't error if missing" so this is safe in environments without
   `emacs-mcp`.
3. Run `./scripts/compile-and-test.sh` — must remain green.

**Files.** `scripts/compile-and-test.sh`,
`scripts/format-and-clean.sh`.

**Dependencies.** Task 4.

**Verification (FR-17).**
- Running `./scripts/compile-and-test.sh` from a fresh shell finds
  `emacs-mcp` and adds it to `LOAD_PATH` (echo `LOAD_PATH` to
  stderr if needed for verification).
- Format-and-clean script runs without errors against any `.el`
  file in the tree.

---

## Phase 3 — Delete the bundled MCP layer

### Task 6 — Delete bundled MCP / diagnostics / emacs-tools files

**Description.** Hard-delete the six files identified in FR-1 and
C-9. After this commit the tree intentionally has broken tests;
Task 9 cleans them up.

**Steps.**
1. `git rm` the following files:
   - `gemini-cli-ide-mcp.el`
   - `gemini-cli-ide-mcp-handlers.el`
   - `gemini-cli-ide-mcp-server.el`
   - `gemini-cli-ide-mcp-http-server.el`
   - `gemini-cli-ide-diagnostics.el`
   - `gemini-cli-ide-emacs-tools.el`
2. Edit `gemini-cli-ide.el`:
   - Delete `(require 'gemini-cli-ide-mcp)` (was line 64).
   - Delete `(require 'gemini-cli-ide-mcp-server)` (was line 66).
   - Delete `(require 'gemini-cli-ide-emacs-tools)` (was line 67).
   - Confirm `(require 'emacs-mcp)` and
     `(require 'gemini-cli-ide-tools)` from Task 4 remain in place.
3. Edit `gemini-cli-ide-transient.el` (line ~59 onward): delete any
   `(require 'gemini-cli-ide-mcp...)` line; replace with
   `(require 'emacs-mcp)` if needed.
4. Run `./scripts/compile-and-test.sh`. Byte-compile MUST pass.
   ERT WILL fail at this point — that is expected; Task 9 fixes
   it. Commit anyway with message:
   `refactor(mcp): delete bundled MCP layer (tests broken until Task 9)`.

**Files.**
- DELETE: `gemini-cli-ide-mcp.el`,
  `gemini-cli-ide-mcp-handlers.el`,
  `gemini-cli-ide-mcp-server.el`,
  `gemini-cli-ide-mcp-http-server.el`,
  `gemini-cli-ide-diagnostics.el`,
  `gemini-cli-ide-emacs-tools.el`.
- EDIT: `gemini-cli-ide.el`,
  `gemini-cli-ide-transient.el`.

**Dependencies.** Task 5.

**Verification (FR-1, FR-3, AC-1, AC-2).**
- `git ls-files | grep -E '(mcp|diagnostics|emacs-tools)\.el$'`
  outputs only `gemini-cli-ide-tools.el`.
- `grep -nE "websocket|web-server" *.el` returns no matches outside
  comments / historical strings.
- Byte-compile is green; ERT is red (acceptable until Task 9).

---

## Phase 4 — Rewire `gemini-cli-ide.el`

### Task 7 — Add server-ownership refcount + `--require-emacs-mcp` guard

**Description.** Introduce the data model from plan §2.6 and the
NFR-7 silent-failure guard.

**Steps.**
1. In `gemini-cli-ide.el`, after the existing `defvar` block:
   - Add `(defvar-local gemini-cli-ide--owns-mcp-server nil ...)`.
   - Add `(defvar gemini-cli-ide--mcp-server-owner-count 0 ...)`.
2. Add private helpers:
   - `gemini-cli-ide--require-emacs-mcp` — checks
     `(featurep 'emacs-mcp)` AND `(version<= "29.1" emacs-version)`
     and signals a `user-error` whose message names whichever
     condition failed and how to install / upgrade. The Emacs-floor
     branch must explicitly include the string "Emacs 29.1" for
     AC-6 / Task-22 verification.
   - `gemini-cli-ide--ensure-mcp-server` — if
     `(emacs-mcp-connection-info)` is nil, call `(emacs-mcp-start)`,
     set buffer-local `--owns-mcp-server` to `t`, increment
     `--mcp-server-owner-count`. Otherwise leave the buffer-local
     at `nil`.
   - `gemini-cli-ide--release-mcp-server` — when buffer-local
     `--owns-mcp-server` is `t`: `(cl-decf --mcp-server-owner-count)`,
     guard with `(setq ... (max 0 ...))`. When the counter reaches
     zero, call `(emacs-mcp-stop)`. Idempotent — calling twice on
     the same buffer is a no-op the second time (clear the flag
     after first release).
3. Run `./scripts/compile-and-test.sh` — byte-compile green; ERT
   still red (Task 9 fixes).

**Files.** `gemini-cli-ide.el`.

**Dependencies.** Task 6.

**Verification (FR-9, NFR-7).**
- The three helpers compile with zero warnings.
- A scratch-buffer manual test of the refcount:
  acquire→acquire→release→release on two different buffers brings
  the counter back to zero exactly.

---

### Task 8 — Add `--write-gemini-settings` and rewire session start/stop

**Description.** Implement the `.gemini/settings.json` writer per
plan §3.1, and integrate the new helpers into `--start-session` /
`gemini-cli-ide-stop`.

**Steps.**
1. Add `gemini-cli-ide--write-gemini-settings` — accepts a project
   root path, computes the target file at
   `<project>/.gemini/settings.json`, performs the merge contract
   from plan §3.1:
   - Read the current file if it exists; `json-parse-string` to a
     hash table or alist.
   - On parse failure: signal `user-error` naming the file.
   - Update / insert `mcpServers.emacs.url` =
     `(emacs-mcp-connection-info)` (or the URL form derived from
     it).
   - Encode with `json-encode` (2-space indent if available).
   - Atomic write: `make-temp-file` in the same directory, then
     `rename-file` over the target.
2. Edit `gemini-cli-ide--build-gemini-command` (line 705): drop the
   `port` parameter and any port-related substitutions.
3. Edit `gemini-cli-ide--create-terminal-session` (line 813): drop
   the `port` parameter. Update its callers.
4. Edit `gemini-cli-ide--start-session` (line 895): at the top, call
   `(gemini-cli-ide--require-emacs-mcp)`. After determining the
   project root and before spawning the subprocess, call
   `(gemini-cli-ide--ensure-mcp-server)` then
   `(gemini-cli-ide--write-gemini-settings project-root)`. Spawn
   with `default-directory` set to the project root.
5. Edit `gemini-cli-ide-stop` (line 1039): after tearing down the
   buffer, call `(gemini-cli-ide--release-mcp-server)`.
6. Edit `gemini-cli-ide--cleanup-on-exit` (line 631): also call
   `--release-mcp-server` (covers the `kill-buffer` path).
7. Edit `gemini-cli-ide-check-status` (line 1026): in addition to
   the CLI status, print
   `(format "emacs-mcp: %s" (or (emacs-mcp-connection-info) "not running"))`.
8. Add `gemini-cli-ide-emacs-tools-setup` deprecation shim (FR-13)
   as an autoloaded `defun` that calls `display-warning` exactly
   once per session (use a `defvar` flag) with the documented
   text. The shim does NOT call `emacs-mcp-mode` and does NOT
   register tools.
9. Repurpose `gemini-cli-ide-mcp-allowed-tools` (line 142): keep
   the defcustom name; update its docstring to describe how it
   maps to `mcpServers.emacs.tools` filter in the JSON output.
   Apply the filter inside `--write-gemini-settings`.
10. Run `./scripts/compile-and-test.sh` — byte-compile green; ERT
    still red (Task 9 fixes).

**Files.** `gemini-cli-ide.el`.

**Dependencies.** Task 7.

**Verification (FR-7, FR-8, FR-9, FR-10, FR-13, NFR-1, NFR-7).**
- Byte-compile clean.
- `gemini-cli-ide-emacs-tools-setup` exists, is autoloaded, and
  emits `display-warning` once when called.
- The new helpers exist with the documented signatures.
- All eight retained interactive commands keep their existing
  names and docstrings.

---

### Task 9 — Rewire `gemini-cli-ide-transient.el`

**Description.** Update the transient module's two MCP-coupled
commands.

**Steps.**
1. `gemini-cli-ide-show-mcp-sessions` (line ~161): replace the body
   with one of:
   - If a stable public accessor exists upstream
     (e.g., a `defun` exported in `emacs-mcp.el`), use it to
     enumerate live sessions and project dirs.
   - Otherwise, replace the body with an `(message ...)` saying
     "Session enumeration is not available in the current
     emacs-mcp release; tracked upstream." DO NOT touch
     `emacs-mcp` internals from this file.
2. `gemini-cli-ide-show-active-ports` (line ~181): replace the body
   with `(message "%s" (or (emacs-mcp-connection-info) "No emacs-mcp server active"))`.
3. Add `(require 'emacs-mcp)` near the top if not already present.
4. Drop any `(require 'gemini-cli-ide-mcp...)` left over.
5. Run `./scripts/compile-and-test.sh` — byte-compile must remain
   green.

**Files.** `gemini-cli-ide-transient.el`.

**Dependencies.** Task 8.

**Verification.**
- Byte-compile clean with zero warnings.
- The two commands no longer reference any deleted symbol.

---

## Phase 5 — Tests

### Task 10 — Delete obsolete tests and the WebSocket mock module

**Description.** Mechanical removal of every test that touches a
deleted symbol, plus the embedded WebSocket mock.

**Steps.**
1. Generate the deletion list:
   ```
   grep -nE "gemini-cli-ide-mcp-|websocket-|json-rpc|gemini-cli-ide-diagnostics|gemini-cli-ide-emacs-tools" \
        gemini-cli-ide-tests.el
   ```
2. Remove every `(ert-deftest ...)` form whose body or name matches
   any of those prefixes. Estimated ~50 tests.
3. Remove the WebSocket mock block (current lines 64–103) and the
   additional websocket mocks block (current lines 159–183).
4. Remove `(require 'gemini-cli-ide-mcp-handlers)` (current line
   185).
5. Manually scan the file for orphaned `defun` helpers (per the
   Gauge's NOTE on planning iter-1) — delete any that no surviving
   test calls.
6. Run `./scripts/compile-and-test.sh` — byte-compile + ERT MUST
   both be green now.

**Files.** `gemini-cli-ide-tests.el`.

**Dependencies.** Task 9.

**Verification (FR-4, FR-18, AC-4).**
- ERT runs cleanly (no failures, no errors, no skipped tests
  beyond the pre-existing optional-dep skips).
- Byte-compile reports zero warnings.

---

### Task 11 — Add new ERT coverage

**Description.** Add the nine new tests listed in plan §7.2.

**Steps.**
1. Add the following `ert-deftest`s to `gemini-cli-ide-tests.el`:
   - `gemini-cli-ide-test-write-settings-creates-file`
   - `gemini-cli-ide-test-write-settings-merges-existing`
   - `gemini-cli-ide-test-write-settings-rejects-malformed`
   - `gemini-cli-ide-test-require-emacs-mcp-missing` — uses
     `cl-letf` to fake `(featurep 'emacs-mcp)` returning nil.
   - `gemini-cli-ide-test-require-emacs-mcp-old-emacs` — fakes
     `emacs-version` to "28.1"; asserts `user-error` whose message
     contains the literal "Emacs 29.1".
   - `gemini-cli-ide-test-server-refcount-acquire-release`
   - `gemini-cli-ide-test-server-refcount-no-touch-when-not-owner`
   - `gemini-cli-ide-test-tools-terminal-input-registered` —
     `(skip-unless (featurep 'emacs-mcp))`; asserts the tool name
     is in `(emacs-mcp--tools)`.
   - `gemini-cli-ide-test-emacs-tools-setup-deprecation-warning` —
     captures `display-warning` calls and asserts the documented
     message.
2. For tests that need `emacs-mcp` available, use
   `(skip-unless (featurep 'emacs-mcp))` so the suite stays green
   in environments without the dep.
3. Run `./scripts/compile-and-test.sh --with-native-compile` —
   must be green with zero byte-compile, native-compile, and ERT
   failures.

**Files.** `gemini-cli-ide-tests.el`.

**Dependencies.** Task 10.

**Verification (FR-12, FR-13, FR-18, FR-9 → ACs 4, 6).**
- Each new test exists with the listed name.
- All tests pass on Emacs 29.1+ with `emacs-mcp` present.
- All tests pass with `emacs-mcp` absent (the integration-style
  ones skip cleanly).

---

## Phase 6 — Documentation

### Task 12 — Rewrite `gemini-cli-ide.el` Commentary block

**Description.** Remove all WebSocket / built-in MCP server
language; describe the new architecture.

**Steps.**
1. Edit lines 26–57 (`;;; Commentary:` through to before
   `;;; Code:`):
   - Drop the line "starts a WebSocket server".
   - Add a paragraph saying the package depends on `emacs-mcp` for
     the MCP server functionality and registers Gemini-specific
     tools (currently one) into it.
   - Update the Features list: drop "MCP WebSocket server"; add
     "Streamable HTTP MCP transport via `emacs-mcp`".
   - Drop the "Emacs MCP Tools" footer that references
     `gemini-cli-ide-emacs-tools-setup`.
2. Run `./scripts/compile-and-test.sh` — green.

**Files.** `gemini-cli-ide.el`.

**Dependencies.** Task 11.

**Verification (FR-21, AC-8).**
- Reading the Commentary block makes no claim that the package
  itself runs an MCP server.

---

### Task 13 — Rewrite `README.md`

**Description.** Apply FR-20 in full.

**Steps.**
1. *Requirements* section — set Emacs floor to 29.1; replace
   `websocket` + `web-server` bullets with a single bullet for
   `emacs-mcp` linking to `https://github.com/ezchi/emacs-mcp`.
2. *Features* section — drop "MCP WebSocket server"; insert
   "Streamable HTTP MCP transport (provided by `emacs-mcp`)".
3. *Installation* section — replace the existing snippet with the
   literal recipe specified in plan §10 deliverables / FR-20:
   ```elisp
   (use-package emacs-mcp
     :straight (emacs-mcp :type git :host github :repo "ezchi/emacs-mcp"))
   (use-package gemini-cli-ide
     :straight (gemini-cli-ide :type git :host github :repo "ezchi/gemini-cli-ide.el"))
   ```
   Also add a short manual-install paragraph (`git clone` +
   `add-to-list 'load-path`) for non-`straight` users. Note that
   MELPA submission for `emacs-mcp` is tracked upstream.
4. Add a new *License* section (or expand the existing "License: GPL-3.0-or-later" line) explaining the
   AGPL §13 combined-work disclosure — neutral, factual wording per
   plan §8 risk mitigation.
5. Add a new *Breaking changes* section listing every regression
   from C-8 / FR-11 / FR-13 / FR-14 / FR-15 fallback no-op:
   - Dropped Emacs 28.1 support.
   - Dropped `websocket` / `web-server` deps.
   - MCP tool rename mapping (the table from C-8).
   - Dropped `treesit-info` extension params (`whole_file`,
     `include_ancestors`, `include_children`).
   - Dropped real-time selection / active-editor push
     notifications.
   - `gemini-cli-ide-emacs-tools-setup` deprecated; removal in
     v0.4.0.
6. Document the `.gemini/settings.json` write behavior briefly so
   users know where the URL goes.

**Files.** `README.md`.

**Dependencies.** Task 12.

**Verification (FR-20, AC-8).**
- Manual read-through — no WebSocket or web-server references in
  Features / Requirements / Installation.
- License section mentions AGPL §13.
- Breaking changes section lists every item above.

---

### Task 14 — Create `CHANGELOG.md` with the v0.3.0 entry

**Description.** Apply FR-22 / FR-9 / AC-9.

**Steps.**
1. Create `CHANGELOG.md` at the repo root.
2. Add a `## v0.3.0 — YYYY-MM-DD` heading. Body must list, at minimum:
   - **Removed:** `websocket` and `web-server` runtime deps.
   - **Removed:** the bundled MCP server (six files: list them).
   - **Added:** hard dependency on `emacs-mcp` `0.1.0`.
   - **Added:** `gemini-cli-ide-tools.el` registering
     `gemini-cli-ide-mcp-get-terminal-input`.
   - **Changed:** Emacs floor raised to 29.1.
   - **Changed:** combined work distributed under
     AGPL-3.0-or-later (per AGPL §13); source headers remain
     GPL-3.0-or-later. Pointer to the new README License section.
   - **Renamed (MCP tool names):** the table from C-8.
   - **Regressions:** dropped `treesit-info` extension params;
     dropped selection / active-editor push notifications. Each
     with a one-line explanation and a pointer to a follow-up task.
   - **Deprecated:** `gemini-cli-ide-emacs-tools-setup` — shim now
     emits a `display-warning`; will be removed in v0.4.0.
   - **Tested against:** `emacs-mcp` git SHA from Task 1 and a
     short link to the upstream commit subject.

**Files.** Create: `CHANGELOG.md`.

**Dependencies.** Task 13.

**Verification (AC-9).**
- File exists at the repo root.
- Every required bullet from above is present.

---

## Phase 7 — Verification

### Task 15 — Final compile + test sweep

**Description.** Run the full local CI script in
maximum-strictness mode.

**Steps.**
1. `rm -f *.elc` to clear any stale bytecode.
2. `./scripts/compile-and-test.sh --with-native-compile`. Must
   exit 0 with no byte-compile, no native-compile, and no ERT
   failures or warnings.
3. If any warning appears, return to the responsible task and fix
   the underlying issue (Constitution: "byte-compilation must
   produce zero warnings").

**Files.** None directly modified (any fixes go back to whichever
task introduced the regression).

**Dependencies.** Task 14.

**Verification (AC-4).**
- Script exit code 0.
- Output banner reads:
  `✓ Byte-compilation: PASSED`,
  `✓ Native-compilation: PASSED`,
  `✓ Tests: PASSED`,
  `✓ All checks passed!`.

---

### Task 16 — Static AC sweep

**Description.** Run the mechanical-only acceptance checks here so
the Validation stage can focus on interactive verification only.

**Steps.**
1. **AC-1:** `git ls-files` MUST NOT contain
   `gemini-cli-ide-mcp.el`,
   `gemini-cli-ide-mcp-handlers.el`,
   `gemini-cli-ide-mcp-server.el`,
   `gemini-cli-ide-mcp-http-server.el`,
   `gemini-cli-ide-diagnostics.el`,
   `gemini-cli-ide-emacs-tools.el`. MUST contain
   `gemini-cli-ide-tools.el`.
2. **AC-2:** `grep -nE "websocket|web-server" *.el README.md` —
   zero matches outside CHANGELOG history (CHANGELOG may mention
   the dropped deps in its "Removed:" line; that line is the
   exception).
3. **AC-3:** `grep -n "Package-Requires" gemini-cli-ide.el` shows
   exactly the three entries.
4. **AC-11:** `git diff steel/001-adopt-external-emacs-mcp/planning-complete -- .steel/constitution.md`
   shows the three Phase-1 amendments and nothing else.
5. Record any failures and return to the responsible task.

**Files.** None modified.

**Dependencies.** Task 15.

**Verification (AC-1, AC-2, AC-3, AC-11).**
- Each `grep` / `git ls-files` invocation returns the expected set.
- A small text artifact summarizing the four checks is written to
  `specs/001-adopt-external-emacs-mcp/artifacts/implementation/`
  (the implementation skill will likely produce this; if not, it
  is a manual note for the Validation stage).
