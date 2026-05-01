# Plan — Spec 001 `adopt-external-emacs-mcp`

**Spec ID:** `001-adopt-external-emacs-mcp`
**Branch:** `feature/001-adopt-external-emacs-mcp`
**Stage:** planning (in_progress)
**Source documents:**
`specs/001-adopt-external-emacs-mcp/spec.md`,
`specs/001-adopt-external-emacs-mcp/clarifications.md`,
`.steel/constitution.md`

---

## 1. Architecture Overview

The package transitions from "Emacs as MCP server **and** Gemini CLI
launcher" to "Gemini CLI launcher **that wires up** an external
`emacs-mcp` server and registers Gemini-specific tools into it."

### Current (pre-change) topology

```
┌────────────────────────────── gemini-cli-ide repo ──────────────────────────────┐
│                                                                                  │
│  gemini-cli-ide.el          (launcher: vterm/eat, transient, prompt buffer)     │
│  gemini-cli-ide-transient.el                                                    │
│  gemini-cli-ide-debug.el                                                        │
│                                                                                  │
│  gemini-cli-ide-mcp.el           ─┐                                             │
│  gemini-cli-ide-mcp-handlers.el   │                                             │
│  gemini-cli-ide-mcp-server.el     ├── BUNDLED MCP SERVER (~3,200 LoC)           │
│  gemini-cli-ide-mcp-http-server.el│   (WebSocket + HTTP, JSON-RPC, sessions,    │
│  gemini-cli-ide-diagnostics.el    │    tool handlers, lockfiles)                │
│  gemini-cli-ide-emacs-tools.el   ─┘                                             │
└──────────────────────────────────────────────────────────────────────────────────┘
        │ WebSocket
        ▼
   Gemini CLI subprocess
```

### Target (post-change) topology

```
┌────────────── gemini-cli-ide repo ──────────────┐         ┌──── emacs-mcp ────┐
│                                                 │         │                    │
│  gemini-cli-ide.el          (launcher only)     │         │  emacs-mcp.el      │
│  gemini-cli-ide-transient.el                    │         │  emacs-mcp-*.el    │
│  gemini-cli-ide-debug.el                        │ require │  (Streamable HTTP, │
│  gemini-cli-ide-tools.el  (NEW — registers      │◀────────│   JSON-RPC,        │
│      `gemini-cli-ide-mcp-get-terminal-input`    │         │   sessions,        │
│      via emacs-mcp-deftool)                     │         │   built-in tools,  │
│                                                 │         │   lockfiles)       │
└─────────────────────────────────────────────────┘         └────────┬───────────┘
        │ writes .gemini/settings.json                               │
        │ launches subprocess with cwd=project-root                  │ HTTP /mcp
        ▼                                                            ▼
   Gemini CLI subprocess  ────────── tools/list, tools/call ──────►  same emacs-mcp
                                                                     (multi-session,
                                                                      one per Gemini
                                                                      buffer)
```

### Key architectural decisions (from clarifications)

1. **One `emacs-mcp` process, many sessions.** Each Gemini buffer
   opens its own MCP session via `initialize` with its project root
   in `projectDir` (per `emacs-mcp-protocol.el:74-114`). Multiple
   project sessions coexist. (C-3.)
2. **Endpoint discovery via project-local `.gemini/settings.json`
   only.** Package never touches the global config. (C-2.)
3. **Server ownership via refcounting.** A package-global counter
   tracks how many Gemini buffers depend on a server *that this
   package started*. When the counter hits zero on stop, the
   package calls `emacs-mcp-stop`. A user-started server is never
   stopped by the package. (C-10.)
4. **Push notifications dropped this release.** No public push API
   in upstream `emacs-mcp`. (C-4.)
5. **No source relicense; combined-work AGPL disclosed in README.**
   (C-1.)

### Constraint envelope (constitution + spec)

- Emacs floor 29.1 (constitution amendment per FR-23).
- Hard deps: `(emacs "29.1") (emacs-mcp "0.1.0") (transient "0.9.0")`
  — nothing else.
- Localhost-only binding inherited from `emacs-mcp` (NFR-2).
- Project-scoped path validation inherited from `emacs-mcp` (NFR-3).
- All custom tools registered via `emacs-mcp-deftool` /
  `emacs-mcp-register-tool` — no direct fiddling with `emacs-mcp`
  internals.
- No new logging that exposes buffer contents, paths outside the
  project, or credentials (NFR-6 / Constitution Principle 7).

---

## 2. Components

### 2.1 `gemini-cli-ide.el` (existing, heavily edited)

**Responsibility:** Launch the Gemini CLI subprocess for a project,
manage its terminal buffer, expose interactive commands.

**Surviving code (largely unchanged):**
- All `defcustom`s in lines 111–293 except those listed below.
- `gemini-cli-ide--vterm-smart-renderer`,
  `gemini-cli-ide--configure-vterm-buffer`, terminal backend
  helpers, buffer-name helpers, process registry.
- `gemini-cli-ide--build-gemini-command` (signature changes — see
  below).
- `gemini-cli-ide--create-terminal-session` (signature changes).
- `gemini-cli-ide--start-session`.
- All eight interactive commands (`gemini-cli-ide`,
  `gemini-cli-ide-continue`, `gemini-cli-ide-resume`,
  `gemini-cli-ide-stop`, `gemini-cli-ide-switch-to-buffer`,
  `gemini-cli-ide-list-sessions`, `gemini-cli-ide-check-status`,
  `gemini-cli-ide-insert-at-mentioned`).

**Code that must change:**

| Symbol / line                                            | Change                                                                                                                                                                |
|----------------------------------------------------------|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Header `Package-Requires:` (line 7)                      | Replace with `((emacs "29.1") (emacs-mcp "0.1.0") (transient "0.9.0"))`. Remove `websocket`, `web-server`.                                                            |
| Header `Keywords:` (line 8)                              | Remove `websocket`.                                                                                                                                                   |
| `Version:` (line 6)                                      | Bump from `0.2.0` → `0.3.0`.                                                                                                                                          |
| `;;; Commentary:` block                                  | Remove WebSocket/MCP-server claims; document that the package depends on `emacs-mcp` for the MCP server.                                                              |
| `(require 'gemini-cli-ide-mcp)` (line 64)                | Delete.                                                                                                                                                               |
| `(require 'gemini-cli-ide-mcp-server)` (line 66)         | Delete.                                                                                                                                                               |
| `(require 'gemini-cli-ide-emacs-tools)` (line 67)        | Replace with `(require 'gemini-cli-ide-tools)`.                                                                                                                       |
| Add: `(require 'emacs-mcp)`                              | New require for the external dep.                                                                                                                                     |
| `gemini-cli-ide-mcp-allowed-tools` (line 142, defcustom) | Repurpose: still controls which tools to advertise to Gemini, but now translates into the `mcpServers.emacs.tools` filter in the project-local `.gemini/settings.json` write (or `nil` to advertise all). |
| `gemini-cli-ide--session-ids` hash (line 300)            | Remove. Replace with `gemini-cli-ide--mcp-server-owners` (a buffer-local flag plus a package-global counter — see §2.6).                                              |
| `gemini-cli-ide--build-gemini-command` (line 705)        | Drop the `port` parameter. The Gemini subprocess discovers the URL via `.gemini/settings.json` instead.                                                                |
| `gemini-cli-ide--create-terminal-session` (line 813)     | Drop the `port` parameter. Before spawning, call the new `gemini-cli-ide--ensure-mcp-server` and `gemini-cli-ide--write-gemini-settings`.                              |
| `gemini-cli-ide-stop` (line 1039)                        | After tearing down the buffer, call `gemini-cli-ide--release-mcp-server` (decrement refcount; stop server if zero AND we own it).                                     |
| Add: `gemini-cli-ide--ensure-mcp-server` (new private)   | If `emacs-mcp` server isn't running, start it; record ownership. If already running, just bump the refcount.                                                          |
| Add: `gemini-cli-ide--release-mcp-server` (new private)  | Decrement refcount; if zero AND owned-by-us, call `emacs-mcp-stop`.                                                                                                   |
| Add: `gemini-cli-ide--write-gemini-settings` (new private) | Read `emacs-mcp-connection-info` (or fallback to lockfile parsing); merge `mcpServers.emacs.url` into `<project>/.gemini/settings.json`; create file if absent.   |
| Add: `gemini-cli-ide--require-emacs-mcp` (new private)   | Called at the top of every `;;;###autoload` interactive command. `user-error` if `emacs-mcp` is not on load-path or Emacs < 29.1. (NFR-7.)                            |
| Add: `gemini-cli-ide-emacs-tools-setup` (deprecation shim)| `(defun ... (interactive) (display-warning ...))`. Carries `;;;###autoload` cookie. Removal scheduled for v0.4.0. (FR-13.)                                            |

### 2.2 `gemini-cli-ide-tools.el` (NEW)

**Responsibility:** Register Gemini-specific MCP tools into the
external `emacs-mcp` registry. In this release, the only such tool
is the terminal-input reader.

**File outline:**

```elisp
;;; gemini-cli-ide-tools.el --- Gemini-specific MCP tools  -*- lexical-binding: t; -*-

;; Copyright (C) 2025
;; Author: Enze Chi
;; Keywords: ai, gemini, mcp

;;; Commentary:
;;
;; Registers Gemini-specific MCP tools into the external `emacs-mcp'
;; server.  In this release the only tool is the terminal-input
;; reader, which lets Gemini see what the user is currently typing
;; in the Gemini terminal buffer before they press Enter.
;;
;; Loaded automatically by `gemini-cli-ide'.

;;; Code:

(require 'emacs-mcp)
(require 'gemini-cli-ide-debug)

(defun gemini-cli-ide--get-terminal-input (buffer)
  "Return the unsent input the user is typing in BUFFER.
Moved verbatim from the deleted `gemini-cli-ide-emacs-tools.el'."
  ...)

(emacs-mcp-deftool gemini-cli-ide-mcp-get-terminal-input
  "Read what the user is currently typing in the Gemini terminal before
they press Enter. Use this to provide real-time assistance or clarify
context."
  ()  ;; no parameters
  (lambda (_args)
    ...))

(provide 'gemini-cli-ide-tools)
;;; gemini-cli-ide-tools.el ends here
```

**Implementation notes:**
- The `emacs-mcp-deftool` macro registers the tool at file load time
  (verified in `emacs-mcp/emacs-mcp-tools.el:73-95`). No setup
  function is needed.
- The body looks up the active Gemini session context (project dir
  via `emacs-mcp--current-session-id` →
  `emacs-mcp-session-project-dir`), maps to the corresponding
  Gemini buffer, then calls the surviving terminal-extraction
  helper. The helper logic is moved verbatim from
  `gemini-cli-ide-emacs-tools.el:340-350` (the
  `gemini-cli-ide--get-terminal-input` body).

### 2.3 `gemini-cli-ide-transient.el` (existing, light edit)

**Surviving code (unchanged):** all transient prefix definitions,
session-status helpers, version-info helper, debug toggle.

**Code that must change:**

| Symbol                                          | Change                                                                                                                                                  |
|-------------------------------------------------|---------------------------------------------------------------------------------------------------------------------------------------------------------|
| `gemini-cli-ide-show-mcp-sessions` (line 161)   | Re-implement using `emacs-mcp`'s public APIs (e.g. iterate `emacs-mcp--sessions` defensively via a stable accessor — confirm whether one exists, otherwise drop the command). |
| `gemini-cli-ide-show-active-ports` (line 181)   | Replace body with `(emacs-mcp-connection-info)`. If no server is running, message "No emacs-mcp server active."                                          |
| Any `(require 'gemini-cli-ide-mcp...)`          | Replace with `(require 'emacs-mcp)`.                                                                                                                    |

### 2.4 `gemini-cli-ide-debug.el` (existing, no functional change)

Pure logging utility. Surviving code unchanged. Verify it does NOT
require any of the deleted modules. (Earlier inspection showed it
requires only `cl-lib` and is required by the deleted modules — the
direction is safe.)

### 2.5 `gemini-cli-ide-tests.el` (existing, large deletion + small additions)

**Survives:** all tests for buffer-name handling
(line 228 ff), working-directory detection (246), buffer-name
construction (254), process management (272), cleanup (291),
CLI detection (320, 333, 346), terminal-backend selection (383,
400). Approximately 30–35 of the 84 existing `ert-deftest` survive.

**Deleted:** all tests touching `gemini-cli-ide-mcp--*` symbols, the
WebSocket mock module (lines 64–103, 159–183), JSON-RPC parsing
tests, HTTP-server tests, tool-handler unit tests for deleted
handlers, diagnostics-converter tests. Estimate: 50+ tests removed.

**Added:** see §7 (Testing Strategy).

### 2.6 Server ownership state model

Two pieces of state, both private:

```elisp
;; Buffer-local on each Gemini session buffer.
;; Non-nil iff this buffer's MCP server was started by this package
;; (as opposed to a pre-existing user-started one).
(defvar-local gemini-cli-ide--owns-mcp-server nil)

;; Package-global. Counts the number of live Gemini buffers whose
;; `gemini-cli-ide--owns-mcp-server' is non-nil.
(defvar gemini-cli-ide--mcp-server-owner-count 0)
```

Lifecycle:

- `gemini-cli-ide--ensure-mcp-server`:
  - If `(emacs-mcp-connection-info)` returns nil: call
    `(emacs-mcp-start)`, set buffer-local `--owns-mcp-server` to
    `t`, increment counter.
  - Else (already running, started elsewhere): set buffer-local to
    `nil`. Do not touch the counter.
- `gemini-cli-ide--release-mcp-server` (called from
  `gemini-cli-ide-stop` and from `--cleanup-on-exit`):
  - If buffer-local `--owns-mcp-server` was `t`: decrement counter.
    If counter hits zero: call `(emacs-mcp-stop)`.
- Counter never goes negative; guard with `cl-decf` + `(max 0 ...)`.

This satisfies FR-9 unambiguously.

---

## 3. Data Model

The package itself is largely stateless beyond the existing process
hash table and buffer-local flags. The only new persistent state
this work introduces is the project-local `.gemini/settings.json`
write (FR-10).

### 3.1 `.gemini/settings.json` write contract

- File path: `(expand-file-name ".gemini/settings.json" (project-root (project-current)))`.
- If the file does not exist:
  - Create the parent directory if needed.
  - Write a fresh JSON object containing only the
    `{ "mcpServers": { "emacs": { "url": "<discovered-url>" } } }`
    structure.
- If the file exists:
  - Read it with `json-parse-string`.
  - If it is valid JSON: deep-merge the
    `mcpServers.emacs.url` field, preserving all other top-level
    keys and other entries under `mcpServers`.
  - If it is malformed JSON: signal `user-error` with a message
    that names the file and tells the user to fix or delete it.
    DO NOT overwrite a malformed file (this is the user's data).
- Write back with `json-encode` + 2-space pretty-print so the file
  remains hand-editable.
- Always perform an atomic write (`write-region` to a temp file,
  then `rename-file`).

### 3.2 Endpoint discovery contract

Use `(emacs-mcp-connection-info)` as the primary mechanism (verified
to exist at `emacs-mcp/emacs-mcp.el:268`). If it returns nil (no
server running), call `(emacs-mcp-start)` first, then re-query.

Lockfile parsing under `~/.emacs-mcp/<PORT>.lock` is the documented
fallback per the `emacs-mcp` README; but as long as the package is
the one starting the server, `connection-info` is authoritative and
no lockfile parsing is needed in `gemini-cli-ide`.

---

## 4. API Design

### 4.1 Public Emacs Lisp API (interactive commands)

| Command                                 | Behavior change                                                                                                                                  |
|-----------------------------------------|--------------------------------------------------------------------------------------------------------------------------------------------------|
| `gemini-cli-ide`                        | Same as before externally. Internally: now calls `--ensure-mcp-server`, `--write-gemini-settings`, then spawns Gemini.                            |
| `gemini-cli-ide-continue`               | Same.                                                                                                                                            |
| `gemini-cli-ide-resume`                 | Same.                                                                                                                                            |
| `gemini-cli-ide-stop`                   | Same external behavior. Internally: also calls `--release-mcp-server`.                                                                           |
| `gemini-cli-ide-switch-to-buffer`       | Unchanged.                                                                                                                                       |
| `gemini-cli-ide-list-sessions`          | Unchanged.                                                                                                                                       |
| `gemini-cli-ide-check-status`           | Unchanged plus: also reports `emacs-mcp` availability and current connection-info.                                                               |
| `gemini-cli-ide-insert-at-mentioned`    | Unchanged.                                                                                                                                       |
| `gemini-cli-ide-emacs-tools-setup`      | Becomes a deprecation shim. (FR-13.)                                                                                                             |

### 4.2 MCP-side API (visible to Gemini CLI / clients)

- All `emacs-mcp` built-ins enabled by default (per its
  `emacs-mcp-enable-tool-*` defcustoms).
- One Gemini-specific tool: `gemini-cli-ide-mcp-get-terminal-input`,
  zero parameters, returns string of unsent input or a polite "no
  input" message.
- The legacy MCP names listed in FR-11 are gone — clients calling
  them get `tools/list` responses without those names; their first
  `tools/call` against an old name returns a JSON-RPC method-not-found
  via `emacs-mcp`'s normal dispatch.

### 4.3 Internal-only Emacs Lisp API (private helpers)

All `gemini-cli-ide--*` (double dash). Not part of the public
contract; can change freely between releases.

---

## 5. Dependencies

### 5.1 Hard runtime dependencies (post-change)

```elisp
;; Package-Requires: ((emacs "29.1") (emacs-mcp "0.1.0") (transient "0.9.0"))
```

- `emacs-mcp` source: `https://github.com/ezchi/emacs-mcp.git`.
- Tested against git SHA `6c85616` (current HEAD on
  `~/Projects/emacs-mcp/`). Recorded in `CHANGELOG.md`.

### 5.2 Optional runtime dependencies (unchanged)

`vterm`, `eat`, `with-editor`, `flymake`, `flycheck`, `treesit`. All
gated with `featurep` / `fboundp`.

### 5.3 Build-time / test dependencies

- `scripts/compile-and-test.sh`'s `find_emacs_package` helper gets a
  new lookup for `emacs-mcp` (mirrors the existing entries for
  `websocket`, `transient`, `emacs-libvterm`).
- `scripts/format-and-clean.sh:31-46` — current invocation requires
  `(require 'gemini-cli-ide-mcp-server nil t)` for indentation hints.
  Replace with `(require 'emacs-mcp nil t)` (and
  `(require 'gemini-cli-ide-tools nil t)` if needed).

### 5.4 Things removed

- `websocket` (was 1.12+).
- `web-server` (was 0.1.2+).

---

## 6. Implementation Strategy

Phased so each phase leaves the tree byte-compileable and the test
suite green (with relevant tests temporarily removed where
appropriate). Each phase is intended to be one or two commits.

### Phase 0 — Pre-flight

**Goal:** confirm the implementation environment is sane before
deleting anything.

- Run `./scripts/compile-and-test.sh` on the unchanged tree to
  verify a green starting point.
- Confirm `~/Projects/emacs-mcp/` is discoverable from
  `find_emacs_package` (or temporarily symlink it to a path the
  helper finds).
- Snapshot the current tested SHA of `emacs-mcp` for the CHANGELOG
  entry: `git -C ~/Projects/emacs-mcp rev-parse HEAD`.

### Phase 1 — Constitutional amendments

**Goal:** edit `.steel/constitution.md` first so that Phase 2+ are
trivially compliant. This phase has no Emacs Lisp changes.

Edits, all documented in commit message:

1. **Constraints > Compatibility:** Emacs floor `28.1` → `29.1`.
   Justification line: "Driven by adoption of `emacs-mcp` (spec
   001), whose own `Package-Requires` mandates 29.1."
2. **Technology Stack > Hard runtime dependencies:** drop
   `websocket 1.12+`, drop `web-server 0.1.2+`, add
   `emacs-mcp 0.1.0+`.
3. **Governing Principle 3 (Project-scoped sessions):** rephrase
   per FR-24. New text: "Each Gemini buffer corresponds to a
   distinct `emacs-mcp` session pinned to its project root via the
   `initialize.projectDir` parameter and adjustable via
   `emacs-mcp/setProjectDir`; multiple Gemini buffers across
   projects must coexist on a single underlying server process
   without cross-contamination."

Commit: `steel(constitution): amend for spec 001 — emacs-mcp adoption`.

### Phase 2 — Add new files / dependencies

**Goal:** add `gemini-cli-ide-tools.el` and update
`Package-Requires`/`Keywords`/`Version` headers, BEFORE deleting
the bundled MCP layer. This temporarily creates duplicate tool
registrations (the old ones in `gemini-cli-ide-emacs-tools-setup`
and the new one in `gemini-cli-ide-tools.el`), but those duplicates
go away at Phase 3.

Steps:

1. Create `gemini-cli-ide-tools.el` (only the terminal-input tool,
   registered via `emacs-mcp-deftool`).
2. Update `gemini-cli-ide.el` header `Package-Requires`, `Version`,
   `Keywords`, `;;; Commentary:`. Add `(require 'emacs-mcp)` and
   `(require 'gemini-cli-ide-tools)` to the require block. Do NOT
   yet delete the old requires.
3. Update `scripts/compile-and-test.sh` and
   `scripts/format-and-clean.sh` to know about `emacs-mcp`.
4. Verify `./scripts/compile-and-test.sh` is still green.

### Phase 3 — Delete bundled MCP layer

**Goal:** the spec's headline outcome.

Steps:

1. Delete files (and their `.elc` siblings if generated):
   `gemini-cli-ide-mcp.el`,
   `gemini-cli-ide-mcp-handlers.el`,
   `gemini-cli-ide-mcp-server.el`,
   `gemini-cli-ide-mcp-http-server.el`,
   `gemini-cli-ide-diagnostics.el`,
   `gemini-cli-ide-emacs-tools.el`.
2. Remove the corresponding `(require ...)` lines in
   `gemini-cli-ide.el` and `gemini-cli-ide-transient.el`.
3. Run `./scripts/compile-and-test.sh` — expect failures from tests
   that referenced deleted symbols. Do NOT fix code yet; this
   informs the test-deletion list in Phase 5.

### Phase 4 — Rewire `gemini-cli-ide.el`

**Goal:** restore green compile / interactive behavior.

Steps:

1. Add private helpers `--ensure-mcp-server`,
   `--release-mcp-server`, `--write-gemini-settings`,
   `--require-emacs-mcp`.
2. Edit `--build-gemini-command` and `--create-terminal-session` to
   drop the `port` parameter (callers are inside this same file and
   `--start-session`).
3. Edit `--start-session` to call `--require-emacs-mcp` first, then
   `--ensure-mcp-server`, then `--write-gemini-settings`, then the
   existing session-creation path.
4. Edit `gemini-cli-ide-stop` to call `--release-mcp-server` after
   tearing down the buffer.
5. Repurpose `gemini-cli-ide-mcp-allowed-tools` defcustom: it now
   shapes the JSON written into `.gemini/settings.json`'s
   `mcpServers.emacs.tools` filter (or stays nil to advertise all).
6. Add the deprecation shim
   `gemini-cli-ide-emacs-tools-setup`.

### Phase 5 — Rewire transient module + tests

**Steps:**

1. `gemini-cli-ide-transient.el`:
   - Replace `gemini-cli-ide-show-mcp-sessions` body with a call
     into `emacs-mcp`'s public session-info accessor (or, if no
     stable accessor exists, replace with an "unimplemented in this
     release; track upstream" message).
   - Replace `gemini-cli-ide-show-active-ports` body with
     `(message "%s" (emacs-mcp-connection-info))`.
   - Drop any `(require 'gemini-cli-ide-mcp...)`.
2. `gemini-cli-ide-tests.el`:
   - Remove the WebSocket mock module (lines 64–103, 159–183).
   - Remove all `ert-deftest` that reference deleted symbols
     (`gemini-cli-ide-mcp-*`, `websocket-*`, etc.). Use
     `grep -nE "gemini-cli-ide-mcp-|websocket-" gemini-cli-ide-tests.el`
     to drive the deletion list.
   - Remove `(require 'gemini-cli-ide-mcp-handlers)` (line 185).
   - Add new tests per §7 (Testing Strategy).
3. Run `./scripts/compile-and-test.sh` — must be green.

### Phase 6 — Documentation + CHANGELOG

**Steps:**

1. Rewrite `gemini-cli-ide.el` `;;; Commentary:` block — remove
   WebSocket / built-in MCP server claims; describe the
   `emacs-mcp` dependency and the role of this package as a launcher
   + tool registrar.
2. Update `README.md` per FR-20:
   - Requirements section.
   - Features section (drop WebSocket, mention Streamable HTTP via
     `emacs-mcp`).
   - Installation section (literal `straight.el` recipe).
   - License section (combined-work AGPL §13 disclosure).
   - New Breaking Changes section listing every regression in the
     changelog.
3. Create `CHANGELOG.md` with v0.3.0 entry: dropped deps, raised
   floor, license disclosure, MCP tool rename mapping, dropped
   `treesit-info` extension params, dropped push notifications,
   `gemini-cli-ide-emacs-tools-setup` deprecation timeline, tested
   git SHA of `emacs-mcp`.

### Phase 7 — Verification

**Steps:**

1. `./scripts/compile-and-test.sh --with-native-compile` — must be
   green and report zero byte-compile / native-compile warnings.
   Maps to AC-4.
2. `M-x list-load-path-shadows` after install — no shadowing
   between `gemini-cli-ide` and `emacs-mcp`. Maps to AC-7.
3. Manual interactive verification (no automation; document each in
   the validation stage):
   - In Emacs 29.1+ with `emacs-mcp` installed, `M-x gemini-cli-ide`
     in two different projects; verify each Gemini session sees its
     own `project-info`. Maps to AC-5 and AC-10.
   - With `emacs-mcp` NOT on `load-path`, every interactive command
     signals `user-error` naming `emacs-mcp`. Maps to AC-6.
4. `git ls-files | grep -E '(mcp|diagnostics|emacs-tools)\.el$'`
   should output exactly `gemini-cli-ide-tools.el`. Maps to AC-1.
5. `grep -nE "websocket|web-server" *.el README.md` returns zero.
   Maps to AC-2.

---

## 7. Testing Strategy

### 7.1 Test classification

- **Pure unit tests** (no Emacs side effects beyond temp buffers):
  cover `--write-gemini-settings` JSON-merge logic, refcount
  arithmetic, `--require-emacs-mcp` error messages.
- **Integration-style ERT** (require `emacs-mcp` on load-path):
  cover tool registration via `emacs-mcp-deftool`, the
  `emacs-mcp-connection-info` round-trip, the `initialize.projectDir`
  flow.
- **Skip-if-missing-dep tests**: any test that needs `emacs-mcp`
  must `(skip-unless (featurep 'emacs-mcp))` so the suite passes in
  environments without the dep — same pattern as the existing
  optional-dep tests.
- **Manual verification only**: the multi-project AC-5 / AC-10 and
  the `user-error` AC-6. Documented in
  `specs/<id>/artifacts/validation/` once we hit the validation
  stage.

### 7.2 New tests to add (target list)

| Test name                                                       | Covers                                                                                          |
|-----------------------------------------------------------------|-------------------------------------------------------------------------------------------------|
| `gemini-cli-ide-test-write-settings-creates-file`               | `--write-gemini-settings` creates `.gemini/settings.json` from scratch with correct JSON shape. |
| `gemini-cli-ide-test-write-settings-merges-existing`            | Existing JSON is preserved; only `mcpServers.emacs.url` is touched.                             |
| `gemini-cli-ide-test-write-settings-rejects-malformed`          | A pre-existing malformed file triggers `user-error` and is NOT overwritten.                     |
| `gemini-cli-ide-test-require-emacs-mcp-missing`                 | When `emacs-mcp` not on load-path, `--require-emacs-mcp` signals `user-error` whose message names the dep. |
| `gemini-cli-ide-test-require-emacs-mcp-old-emacs`               | Mock `emacs-version` to "28.1"; expect `user-error` mentioning version.                          |
| `gemini-cli-ide-test-server-refcount-acquire-release`           | Fresh refcount = 0; acquire bumps to 1; release brings to 0; over-release stays at 0.            |
| `gemini-cli-ide-test-server-refcount-no-touch-when-not-owner`   | When server was already running before acquire, owns-flag stays nil; release does NOT call stop. |
| `gemini-cli-ide-test-tools-terminal-input-registered`           | After `(require 'gemini-cli-ide-tools)`, the tool name appears in `(emacs-mcp--tools)`.          |
| `gemini-cli-ide-test-emacs-tools-setup-deprecation-warning`     | Calling the shim emits `display-warning` with the documented message.                            |

### 7.3 Tests to delete (driven by grep)

Approximately 50+ `ert-deftest` will be deleted. The deletion list
is generated mechanically in Phase 5:

```sh
grep -nE "gemini-cli-ide-mcp-|websocket-|json-rpc|gemini-cli-ide-diagnostics|gemini-cli-ide-emacs-tools" \
     gemini-cli-ide-tests.el
```

Every test whose body or `ert-deftest` name contains any of those
prefixes is removed.

### 7.4 Acceptance Criteria coverage map

| AC    | Verified by                                                                                                  |
|-------|--------------------------------------------------------------------------------------------------------------|
| AC-1  | `git ls-files` snapshot at end of Phase 7                                                                    |
| AC-2  | `grep` snapshot at end of Phase 7                                                                            |
| AC-3  | Manual inspection of `gemini-cli-ide.el` header at end of Phase 2                                            |
| AC-4  | `scripts/compile-and-test.sh --with-native-compile` at end of Phase 7                                        |
| AC-5  | Manual interactive test in validation stage                                                                  |
| AC-6  | New ERT `gemini-cli-ide-test-require-emacs-mcp-missing` + manual verification                                 |
| AC-7  | Manual `list-load-path-shadows` in validation stage                                                          |
| AC-8  | Manual review of `README.md` and the rewritten `;;; Commentary:`                                             |
| AC-9  | Manual review of `CHANGELOG.md`                                                                              |
| AC-10 | Manual two-project interactive test in validation stage                                                       |
| AC-11 | `git diff` of `.steel/constitution.md` at end of Phase 1                                                     |

---

## 8. Risks and Mitigations

| Risk                                                                                                                         | Impact | Mitigation                                                                                                                                                                                       |
|------------------------------------------------------------------------------------------------------------------------------|--------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `emacs-mcp` is at v0.1.0 with no MELPA presence; its API may shift before MELPA submission                                   | High   | Pin a tested git SHA in the CHANGELOG. Add the new ERT tests so a future `emacs-mcp` change that breaks our integration trips loud.                                                              |
| `emacs-mcp-connection-info` may not yet exist or may have a different signature than assumed                                 | Med    | Verified at `emacs-mcp/emacs-mcp.el:268`. Phase 0 re-confirms before code is written.                                                                                                            |
| Project-local `.gemini/settings.json` write conflicts with a user-managed file                                              | Med    | Always merge non-destructively; never overwrite a malformed file; document the write in the README. Atomic temp-file + rename.                                                                  |
| Refcount drift if users `kill-buffer` a Gemini session without `gemini-cli-ide-stop`                                         | Med    | Wire `--release-mcp-server` into `kill-buffer-hook` for Gemini buffers and `--cleanup-on-exit`. Idempotent release.                                                                              |
| Tool-name rename (FR-11) breaks user automation that called legacy `gemini-cli-ide-mcp-*` names                              | Med    | Documented as breaking change in CHANGELOG / README. Ship in v0.3.0 (minor bump).                                                                                                                |
| `treesit-info` extension params disappearance regresses a user workflow                                                       | Low    | Documented. Follow-up upstream task tracked outside this spec.                                                                                                                                   |
| Selection / active-editor push-notification feature gone (FR-14) is noticed by Gemini-side automation                         | Low    | Documented in CHANGELOG as a regression with a follow-up upstream-PR commitment.                                                                                                                 |
| AGPL §13 disclosure in README is incorrect or inflammatory                                                                   | Low    | Use neutral, factual language: "When you distribute this package together with `emacs-mcp`, the resulting combined work is governed by AGPL-3.0-or-later including its §13 obligations." Lawyer-review optional. |
| Phase 3 leaves the tree red between Phase 3 and Phase 5                                                                      | Low    | Each phase is a separate commit; Phase 3 commits with a "WIP — tests will be cleaned up in Phase 5" note. The full PR is still atomic.                                                           |
| Native-compilation surfaces warnings that byte-compile didn't                                                                 | Low    | Run with `--with-native-compile` in Phase 7 and fix any warnings; treat warnings as errors per Constitution's Performance constraint.                                                            |
| `.gemini/settings.json` already in `.gitignore` → fine for our repo, but other projects may not have it ignored               | Low    | Document in README: users should add `.gemini/settings.json` to their projects' `.gitignore` if they don't want the URL committed.                                                               |

---

## 9. Out of Scope (re-stated from spec §6)

This plan does NOT include any of:

- Adding new tools that have no equivalent in either codebase.
- Sending PRs to upstream `emacs-mcp` (the push-notification hook
  follow-up is a separate spec).
- Modifying Gemini CLI's own settings format.
- Refactors of prompt buffer / vterm / eat / transient unrelated to
  the MCP refactor.
- Performance work on `emacs-mcp`.
- MELPA submission for either package.
- GitHub Actions / CI configuration for this repo.

---

## 10. File-level deliverable summary

| File                                  | Action                                                                |
|---------------------------------------|-----------------------------------------------------------------------|
| `gemini-cli-ide-mcp.el`               | DELETE                                                                |
| `gemini-cli-ide-mcp-handlers.el`      | DELETE                                                                |
| `gemini-cli-ide-mcp-server.el`        | DELETE                                                                |
| `gemini-cli-ide-mcp-http-server.el`   | DELETE                                                                |
| `gemini-cli-ide-diagnostics.el`       | DELETE                                                                |
| `gemini-cli-ide-emacs-tools.el`       | DELETE                                                                |
| `gemini-cli-ide-tools.el`             | CREATE (NEW)                                                          |
| `gemini-cli-ide.el`                   | EDIT (header, Commentary, requires, helpers, lifecycle)               |
| `gemini-cli-ide-debug.el`             | UNCHANGED                                                             |
| `gemini-cli-ide-transient.el`         | EDIT (light — two MCP-coupled commands)                               |
| `gemini-cli-ide-tests.el`             | EDIT (large — delete MCP tests, add 9 new)                            |
| `scripts/compile-and-test.sh`         | EDIT (add `emacs-mcp` to `find_emacs_package`)                        |
| `scripts/format-and-clean.sh`         | EDIT (replace `gemini-cli-ide-mcp-server` require with `emacs-mcp`)  |
| `README.md`                           | EDIT (substantial — Requirements, Install, Features, License, Breaking) |
| `CHANGELOG.md`                        | CREATE (NEW)                                                          |
| `.steel/constitution.md`              | EDIT (Phase 1 — three sections)                                       |
