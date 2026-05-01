# Spec 001 — Adopt External `emacs-mcp` Package

**Spec ID:** `001-adopt-external-emacs-mcp`
**Branch:** `feature/001-adopt-external-emacs-mcp`
**Status:** specification (in_progress)
**Source request:** "remove emacs-mcp to use external one @~/Projects/emacs-mcp/"

---

## 1. Overview

`gemini-cli-ide.el` today ships its own MCP (Model Context Protocol) server
implementation across five files (~3,200 lines):

- `gemini-cli-ide-mcp.el` (933 lines) — JSON-RPC dispatcher, session model,
  WebSocket connection management, push notifications.
- `gemini-cli-ide-mcp-handlers.el` (816 lines) — file/diagnostic/buffer tool
  handlers.
- `gemini-cli-ide-mcp-server.el` (406 lines) — WebSocket server lifecycle.
- `gemini-cli-ide-mcp-http-server.el` (352 lines) — HTTP transport.
- `gemini-cli-ide-emacs-tools.el` (441 lines) — `xref`/`imenu`/`treesit` tool
  wrappers and a Gemini-specific terminal-input reader.
- `gemini-cli-ide-diagnostics.el` (240 lines) — flycheck/flymake →
  VSCode-format JSON converter, used only by the bundled MCP handlers.

A separate, more focused project — `emacs-mcp` at `~/Projects/emacs-mcp/` —
provides the same MCP server functionality as a standalone library
(Streamable HTTP, MCP protocol `2025-03-26`, lockfile-based discovery,
`emacs-mcp-deftool` extension API, `set-project-dir` protocol method for
per-session project routing). Most of `gemini-cli-ide`'s MCP code is
duplicated, divergent, or strictly less general than what `emacs-mcp` now
offers.

This feature deletes the bundled MCP implementation from `gemini-cli-ide`
and rebases the package on `emacs-mcp` as an external dependency. After this
change, `gemini-cli-ide` is reduced to:

1. A thin Gemini-CLI launcher (vterm/eat subprocess management, transient
   menus, prompt buffer, `with-editor` integration).
2. A small set of Gemini-specific MCP tools (e.g. terminal-input reader)
   registered into `emacs-mcp` via its public extension API.

The intent is **smaller surface, less duplication, and a cleaner separation
between "Emacs as MCP server" (which lives in `emacs-mcp`) and "drive the
Gemini CLI from Emacs" (which is what this package exists to do).**

This spec is a breaking change. It triggers two **constitutional
amendments** that are explicit deliverables of this work:

- The Emacs version floor must be raised from 28.1 to 29.1 (matches
  `emacs-mcp`'s `Package-Requires`).
- The "Project-scoped sessions" principle must be re-stated in terms of
  `emacs-mcp`'s actual session model (per-session project routing via the
  `set-project-dir` protocol method) rather than the bundled
  implementation's WebSocket-per-project model.

Both are tracked under FR-23/FR-24 below.

---

## 2. User Stories

- **US-1** — As the maintainer of `gemini-cli-ide`, I want to delete the
  bundled MCP server so that I have ~3,200 fewer lines to maintain and bug
  fixes flow through one upstream library instead of two divergent copies.

- **US-2** — As an end user of `gemini-cli-ide`, I want my existing
  `M-x gemini-cli-ide` workflow to keep working after upgrading, so that
  the refactor is invisible to me other than installing one new dependency
  and being on Emacs 29.1+.

- **US-3** — As an end user, I want the Gemini CLI subprocess that the
  package launches to discover and connect to the local `emacs-mcp` server
  automatically, so that I do not have to hand-edit
  `~/.gemini/settings.json` per project.

- **US-4** — As a contributor, I want Gemini-specific MCP tools (e.g. the
  terminal-input reader, anything that touches the Gemini vterm/eat
  buffer) to register through `emacs-mcp`'s public extension API
  (`emacs-mcp-deftool` / `emacs-mcp-register-tool`), so that adding
  Gemini-only behavior does not require forking `emacs-mcp`.

- **US-5** — As a security-conscious user, I want the new server to keep
  the same guarantees the bundled implementation provided —
  localhost-only binding, project-scoped path validation, no credential
  or buffer-content leakage in logs — so that the refactor is not a
  regression in security posture.

- **US-6** — As an Emacs configuration hoarder, I want a clear error
  message (not a silent no-op) if I upgrade `gemini-cli-ide` without also
  installing `emacs-mcp` or while running an unsupported Emacs version,
  so I know exactly what to do.

- **US-7** — As a user with multiple projects open in one Emacs session,
  I want each project to talk to its own Gemini CLI without the active
  project of one session corrupting another. ("Project-scoped sessions"
  principle, restated for the new architecture.)

---

## 3. Functional Requirements

### Code removal

- **FR-1** — All of the following files MUST be deleted from the
  repository:
  - `gemini-cli-ide-mcp.el`
  - `gemini-cli-ide-mcp-handlers.el`
  - `gemini-cli-ide-mcp-server.el`
  - `gemini-cli-ide-mcp-http-server.el`
  - `gemini-cli-ide-diagnostics.el` (a flycheck/flymake → VSCode-JSON
    converter used only by the bundled MCP handlers; superseded by
    `emacs-mcp`'s `get-diagnostics` built-in tool).

- **FR-2** — `gemini-cli-ide-emacs-tools.el` MUST be either deleted or
  reduced to only the Gemini-specific tools that have no equivalent in
  `emacs-mcp`. At minimum the **terminal-input reader** —
  `gemini-cli-ide-mcp-get-terminal-input` per `gemini-cli-ide-emacs-tools.el`
  — MUST be retained (re-registered, see FR-12). All `xref-*`, `imenu-*`,
  `treesit-*`, `project-info`, `get-diagnostics`, `list-buffers`,
  `open-file`, and `get-buffer-content` wrappers MUST be deleted (these
  are duplicates of `emacs-mcp` built-ins per its README *Built-in Tools*
  table).

- **FR-3** — All `(require 'gemini-cli-ide-mcp...)`,
  `(require 'gemini-cli-ide-emacs-tools)`, and
  `(require 'gemini-cli-ide-diagnostics)` forms in surviving files MUST
  be updated to `(require 'emacs-mcp)` (and any sub-modules actually
  used).

- **FR-4** — `gemini-cli-ide-tests.el` MUST drop every test that
  exercises the deleted modules: JSON-RPC parsing, WebSocket session
  handling, HTTP transport, tool-handler unit tests for removed
  handlers, diagnostics-converter tests. Tests for surviving Gemini-glue
  code MUST be retained.

### Dependency declaration

- **FR-5** — The `Package-Requires` line in `gemini-cli-ide.el` MUST be
  updated to:
  - **Add:** `emacs-mcp` with an explicit version pin
    [NEEDS CLARIFICATION OQ-7: pin to `0.1.0` exactly, `>= 0.1.0`, or a
    specific git revision until MELPA].
  - **Remove:** `websocket`, `web-server`.
  - **Keep:** `transient`.
  - **Raise:** the Emacs floor from `28.1` to `29.1` (matches
    `emacs-mcp`'s minimum). This change is gated by FR-23.

- **FR-6** — `Keywords:` MUST drop `websocket`. Other keywords (`ai`,
  `gemini`, `cli`, `assistant`, `mcp`) MUST be retained.

### Server lifecycle

- **FR-7** — When the user invokes `M-x gemini-cli-ide` for a project,
  the package MUST ensure an `emacs-mcp` server is running. If no server
  is running, the package MUST start one (e.g., via `emacs-mcp-start` or
  by enabling `emacs-mcp-mode`).

- **FR-8** — When a Gemini buffer is created or activated for a given
  project, the package MUST ensure the `emacs-mcp` server's active
  project directory matches that buffer's project root. The PRIMARY
  mechanism MUST be the `set-project-dir` JSON-RPC protocol method
  exposed by `emacs-mcp` (see `emacs-mcp-protocol.el` →
  `emacs-mcp--handle-set-project-dir`), which switches the session's
  project root without restarting the server. If `set-project-dir` is
  not feasible per-session, the FALLBACK is restarting the server with
  the new `emacs-mcp-project-directory` value, accepting that this
  drops in-flight sessions for other projects.
  [NEEDS CLARIFICATION OQ-3: confirm `set-project-dir` is callable
  per-session without server restart; if not, the fallback must be
  documented as a regression.]

- **FR-9** — When `M-x gemini-cli-ide-stop` is invoked for a project,
  the package MUST tear down its own Gemini subprocess and buffer. The
  package MUST track which `emacs-mcp` server instances it started; an
  `emacs-mcp` server that this package started SHALL be stopped only
  when no Gemini sessions remain that depend on it. An `emacs-mcp`
  server that the user started independently (e.g., via
  `emacs-mcp-mode`) MUST NOT be stopped by this package.

### Endpoint discovery by Gemini CLI

- **FR-10** — The Gemini CLI subprocess that the package launches MUST
  be pointed at the running `emacs-mcp` endpoint
  (`http://127.0.0.1:<PORT>/mcp`). The PRIMARY mechanism is:
  1. Read `emacs-mcp-connection-info` (or the lockfile at
     `~/.emacs-mcp/<PORT>.lock`) to determine the active port.
  2. Write a project-local `.gemini/settings.json` with an
     `mcpServers.emacs.url` entry pointing to the discovered URL,
     creating the file if absent and merging non-destructively if
     present.
  3. Spawn the Gemini subprocess with `default-directory` set to the
     project root so it picks up the project-local settings file.

  The `.gemini/settings.json` file is already in this repo's
  `.gitignore`, so the project-local write does not pollute version
  control.
  [NEEDS CLARIFICATION OQ-2: whether the package additionally writes a
  global `~/.gemini/settings.json` entry as a fallback for non-project
  invocations.]

### Tool surface

- **FR-11** — Tools that already exist as built-ins in `emacs-mcp` —
  `project-info`, `list-buffers`, `open-file`, `get-buffer-content`,
  `get-diagnostics`, `imenu-symbols`, `xref-find-references`,
  `xref-find-apropos`, `treesit-info`, `execute-elisp` — MUST NOT be
  re-implemented in `gemini-cli-ide`. The `emacs-mcp` versions are
  authoritative.

- **FR-12** — Gemini-specific tools that have no upstream equivalent —
  at minimum `gemini-cli-ide-mcp-get-terminal-input` (described as
  *"Read what the user is currently typing in the Gemini terminal
  before they press Enter"* in
  `gemini-cli-ide-emacs-tools.el:369`) — MUST be re-registered via
  `emacs-mcp-deftool` or `emacs-mcp-register-tool` so they appear in
  the external server's `tools/list`. The MCP-facing tool name MUST be
  preserved for backward compatibility unless OQ-9 resolves otherwise.

- **FR-13** — `gemini-cli-ide-emacs-tools-setup` MUST be retired.
  [NEEDS CLARIFICATION OQ-6: removed outright (breaking change for
  users who call it from their init), OR kept as a deprecated alias
  that emits a `display-warning` and registers the surviving
  Gemini-specific tools.]

### Push notifications / editor state

- **FR-14** — Today the bundled server pushes selection-change and
  active-editor notifications (`gemini-cli-ide-mcp.el` lines ~233–562
  via `gemini-cli-ide-mcp--send-notification`). Implementation MUST:
  1. Inspect `emacs-mcp`'s public API for a notification-push hook
     before this work begins.
  2. If a stable mechanism exists (e.g., a hook that lets a tool
     handler emit `notifications/*` to the active session), preserve
     the feature by registering against it.
  3. If no such mechanism exists, this feature is **explicitly dropped
     in this release**, the dropped behavior MUST be listed in
     `CHANGELOG.md` under "Breaking changes," and a follow-up task
     MUST be filed (outside this spec) to add the mechanism upstream
     to `emacs-mcp` and re-enable the feature in a later release.
  [NEEDS CLARIFICATION OQ-4: outcome of step 1 — confirm presence or
  absence of the upstream hook before implementation begins.]

### Multi-project session model

- **FR-15** — The "multiple concurrent sessions per project" claim in
  the current README MUST be honored under the new architecture.
  Implementation MUST use FR-8's `set-project-dir` mechanism to route
  each Gemini buffer's MCP traffic to the correct project root. The
  package MAY track at most one `emacs-mcp` server process per Emacs
  session (the upstream model), but per-Gemini-buffer routing MUST
  remain the user-visible guarantee.

  If — and only if — clarification of OQ-3 establishes that
  `set-project-dir` cannot achieve per-session routing without server
  restart, the package SHALL fall back to "one Gemini session at a
  time per Emacs," and that regression MUST be:
  - Documented in `README.md` under "Breaking changes."
  - Reflected by a constitutional amendment to Principle 3
    (see FR-24).

### License

- **FR-16** — `emacs-mcp` is **AGPL-3.0-or-later**; `gemini-cli-ide`
  is currently **GPL-3.0-or-later**. AGPL §13 explicitly permits
  combination with GPL-3.0 code, but the combined work that is
  distributed must be offered under AGPL-3.0-or-later terms. The
  PRIMARY position adopted by this spec is:
  - `gemini-cli-ide` source files KEEP their GPL-3.0-or-later
    headers.
  - `README.md` and the package commentary block MUST add a clear
    notice that the distributed combined work (this package linked
    against `emacs-mcp`) is effectively AGPL-3.0-or-later, including
    the AGPL §13 network-use disclosure obligation.
  - No relicensing of source files is required by this spec.

  [NEEDS CLARIFICATION OQ-1: maintainer accepts this position, or
  prefers (a) relicensing all of `gemini-cli-ide` source headers to
  AGPL-3.0-or-later for clarity.]

### Build / CI

- **FR-17** — `scripts/compile-and-test.sh` MUST locate `emacs-mcp`
  and add it to the `-L` load path. Add an entry for `emacs-mcp` to
  the existing `find_emacs_package` lookup (matches the pattern used
  for `websocket`, `transient`, `vterm`).

- **FR-18** — The byte-compile + native-compile + ERT pipeline MUST
  pass with **zero warnings** against the new dependency set on Emacs
  29.1+.

  *(Former FR-19 about CI workflows is dropped: this repo does not
  ship a `.github/workflows/` directory and the only CI surface is
  the local script. If a CI pipeline is added in a separate
  initiative, it will inherit FR-17/FR-18.)*

### Documentation

- **FR-20** — `README.md` MUST be updated to:
  - List `emacs-mcp` as a hard dependency in *Requirements*; raise
    the documented Emacs floor to 29.1.
  - Drop *WebSocket* references in the *Features* section; replace
    with "Streamable HTTP MCP transport (provided by `emacs-mcp`)".
  - Update *Installation* `use-package` snippets to install
    `emacs-mcp` alongside `gemini-cli-ide`, including its source URL
    until it lands on MELPA.
  - Document the chosen endpoint-discovery mechanism (FR-10).
  - Add a *License* paragraph reflecting FR-16 (combined work is
    AGPL-3.0-or-later).
  - Add a *Breaking changes* section listing: dropped 28.1 support,
    dropped `websocket`/`web-server` deps, any feature regression
    accepted under FR-14 / FR-15.

- **FR-21** — `gemini-cli-ide.el`'s own `;;; Commentary:` block MUST
  be rewritten to remove any claim that the package itself runs a
  WebSocket/MCP server.

### Versioning

- **FR-22** — The `Version:` header in `gemini-cli-ide.el` MUST be
  bumped to a new minor version (currently `0.2.0`; this change is
  breaking enough to warrant `0.3.0` at minimum). A `CHANGELOG.md`
  MUST be created if one does not exist, with an entry summarizing
  this refactor.

### Constitutional amendments (meta-deliverables)

- **FR-23** — `.steel/constitution.md` MUST be amended in the same PR
  that lands this spec's implementation:
  - **Constraints > Compatibility:** Emacs floor change from `28.1`
    to `29.1`. Rationale (the new hard dep `emacs-mcp` requires it)
    MUST appear in the commit message and in the constitution's
    Compatibility paragraph.
  - **Technology Stack:** drop `websocket 1.12+` and `web-server
    0.1.2+` from hard runtime dependencies; add `emacs-mcp` (with
    the version pinned per FR-5).

- **FR-24** — `.steel/constitution.md` Principle 3
  ("Project-scoped sessions") MUST be re-stated to reflect the new
  architecture — "Each Gemini buffer is routed to its project root
  via `emacs-mcp`'s `set-project-dir` session mechanism; multiple
  Gemini buffers across projects must coexist without
  cross-contamination." If FR-15's fallback is triggered (no
  per-session routing), this principle MUST be amended further to
  explicitly drop the multi-project concurrency guarantee.

---

## 4. Non-Functional Requirements

- **NFR-1 — Behavioral parity (where in scope).** The eight retained
  interactive commands (`gemini-cli-ide`, `gemini-cli-ide-continue`,
  `gemini-cli-ide-resume`, `gemini-cli-ide-stop`,
  `gemini-cli-ide-switch-to-buffer`, `gemini-cli-ide-list-sessions`,
  `gemini-cli-ide-check-status`, `gemini-cli-ide-insert-at-mentioned`)
  MUST keep their existing names and externally visible behavior,
  except for changes explicitly accepted under FR-14 / FR-15.

- **NFR-2 — Localhost-only binding.** The MCP server bound to the
  project MUST listen on `127.0.0.1` only. (Inherited from
  `emacs-mcp`'s default; the package MUST NOT alter that default.)

- **NFR-3 — Project-scoped path validation.** All file-path arguments
  passed to MCP tools MUST be rejected if they escape the project
  root. (Inherited from `emacs-mcp`; the package MUST NOT bypass it.)

- **NFR-4 — Compatibility floor.** Emacs 29.1 is the new minimum,
  contingent on FR-23. Anything that was using a 28.1-only fallback
  can be simplified.

- **NFR-5 — Code-size reduction.** Net deletion target is the five
  MCP/diagnostics files (~3,200 lines of `*.el`) plus the
  corresponding tests. The PR for this feature SHOULD show a strongly
  negative diff.

- **NFR-6 — Logging hygiene.** The new code MUST NOT log buffer
  contents, full file paths outside the project root, or any
  token/credential. (Constitution Principle 7.)

- **NFR-7 — No silent failure.** If `emacs-mcp` is not on
  `load-path` when `gemini-cli-ide` loads, OR if Emacs is below 29.1,
  the user MUST see a `user-error` from any interactive command
  attempt that explicitly says what is missing and how to fix it.

- **NFR-8 — No new hard dependencies beyond `emacs-mcp` and
  `transient`.** Soft/optional integrations (`vterm`, `eat`,
  `with-editor`) remain optional and gated.

---

## 5. Acceptance Criteria

- **AC-1** — `git ls-files` after the change does NOT contain
  `gemini-cli-ide-mcp.el`, `gemini-cli-ide-mcp-handlers.el`,
  `gemini-cli-ide-mcp-server.el`, `gemini-cli-ide-mcp-http-server.el`,
  or `gemini-cli-ide-diagnostics.el`.

- **AC-2** — `grep -nE "websocket|web-server" *.el README.md` returns
  zero matches in source code (CHANGELOG / historical text is
  allowed).

- **AC-3** — `Package-Requires` in `gemini-cli-ide.el` reads, in some
  order: `(emacs "29.1") (emacs-mcp "<pinned-version>") (transient
  "0.9.0")` and contains no other entries.

- **AC-4** — `./scripts/compile-and-test.sh --with-native-compile` on
  Emacs 29.1 with `emacs-mcp` available exits 0 and reports zero
  byte-compile warnings.

- **AC-5** — In an interactive Emacs 29.1 session with `emacs-mcp`
  installed, after `M-x gemini-cli-ide`, the spawned `gemini` process
  successfully reaches `tools/list`, and the response includes:
  - All `emacs-mcp` built-ins enabled by default, AND
  - At least the Gemini-specific tool(s) re-registered under FR-12.

- **AC-6** — In an interactive Emacs 29.1 session **without**
  `emacs-mcp` installed, calling any of the eight retained
  interactive commands signals a `user-error` whose message names
  `emacs-mcp` and includes installation guidance. The same applies
  on Emacs < 29.1.

- **AC-7** — `M-x list-load-path-shadows` after installing both
  packages reports no shadowing between this package and `emacs-mcp`.

- **AC-8** — `README.md` and the `;;; Commentary:` block in
  `gemini-cli-ide.el` agree with the new architecture (no
  WebSocket/MCP-server claims for this package itself; `emacs-mcp`
  named as the dependency that provides the server). The README
  *License* section explicitly reflects the AGPL combined-work
  position from FR-16.

- **AC-9** — `CHANGELOG.md` contains an entry for the new version
  that explicitly lists: dropped deps (`websocket`, `web-server`),
  raised Emacs floor (28.1 → 29.1), license disclosure (combined
  work AGPL), and any feature regressions accepted under FR-14 /
  FR-15.

- **AC-10** — In an interactive Emacs 29.1 session with two project
  buffers active, switching between them and invoking
  `M-x gemini-cli-ide` in each: the second buffer's MCP traffic is
  routed to the second buffer's project root (verifiable by calling
  `project-info` from each Gemini session and observing distinct
  project paths returned). If FR-15's fallback is triggered, this AC
  is dropped and replaced by an AC asserting that the second
  invocation displays the documented "single Gemini session at a
  time" message.

- **AC-11** — `.steel/constitution.md` in the same PR has been
  amended per FR-23 and FR-24. `git diff` of the constitution shows
  the Emacs floor change and (if applicable) the Principle 3 update.

---

## 6. Out of Scope

- Adding new MCP tools that have no equivalent in either the current
  package or `emacs-mcp`.
- Modifying upstream `emacs-mcp` (sending PRs to it). If this work
  uncovers gaps in `emacs-mcp`'s extension API (notably the FR-14
  notification-push hook), those gaps are tracked separately as new
  specs and a follow-up CHANGELOG note.
- Changing the Gemini CLI binary's own behavior or its native
  settings format.
- Migrating to a non-Streamable-HTTP transport.
- Refactors to the prompt buffer, vterm/eat integration, or
  transient menu that are unrelated to MCP.
- Performance optimizations of the new dependency.
- Renaming or restructuring the eight retained interactive commands.
- Creating a unified MELPA recipe for either package.
- Adding GitHub Actions / other CI configuration (none exists today;
  the local `scripts/compile-and-test.sh` is the only check).

---

## 7. Open Questions

All marked **[NEEDS CLARIFICATION]** in the requirements above;
collected here for the clarification stage:

1. **OQ-1 (License — FR-16).** Maintainer accepts the spec's primary
   position (combined work AGPL, no source relicense needed), or
   prefers explicit relicensing of all source files to
   AGPL-3.0-or-later?

2. **OQ-2 (Endpoint discovery — FR-10).** Does the package also write
   a global `~/.gemini/settings.json` entry, or only the project-local
   `.gemini/settings.json`?

3. **OQ-3 (set-project-dir feasibility — FR-8, FR-15).** Confirm
   whether `emacs-mcp`'s `set-project-dir` JSON-RPC method allows
   per-session project routing without restarting the underlying
   server process. If not, the fallback (one Gemini session at a
   time) is in effect and FR-24 must amend Principle 3 accordingly.

4. **OQ-4 (Push notifications — FR-14).** Does `emacs-mcp` expose a
   stable mechanism to push `notifications/*` (selection change,
   active editor) to connected clients? If not, confirm dropping the
   feature in this release with a CHANGELOG entry and a follow-up
   upstream task.

5. **OQ-6 (Tool-set bridge — FR-13).**
   `gemini-cli-ide-emacs-tools-setup`: remove outright, or keep as a
   deprecated alias for one release?

6. **OQ-7 (Version pin — FR-5).** Pin `emacs-mcp` to `0.1.0` exactly,
   `>= 0.1.0`, or a specific git SHA until it is on MELPA?

7. **OQ-8 (Distribution — FR-5).** `emacs-mcp` is not yet on MELPA.
   Do we ship `gemini-cli-ide` with a `straight.el` recipe that
   fetches `emacs-mcp` from a specific git host, document a manual
   install, or wait until `emacs-mcp` is on MELPA?

8. **OQ-9 (Tool-name compatibility — FR-12).** Do any current users
   have automation that calls Gemini-specific tools by their current
   names? If so, FR-12 must preserve those exact names; if not, a
   rename is permissible during re-registration.

9. **OQ-10 (Surviving file boundary — FR-2).** After deletion, do
   the surviving Gemini-specific tools live in a new file
   (`gemini-cli-ide-tools.el`) registered into `emacs-mcp`, or
   inline in `gemini-cli-ide.el`?

*(OQ-5 from iter-1 is folded into FR-15's primary path + fallback;
OQ-11 from iter-1 is dropped — no CI exists; OQ-12 from iter-1 is
resolved by adding `gemini-cli-ide-diagnostics.el` to FR-1.)*
