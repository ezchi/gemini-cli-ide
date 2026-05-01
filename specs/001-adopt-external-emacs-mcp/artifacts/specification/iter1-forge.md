# Spec 001 — Adopt External `emacs-mcp` Package

**Spec ID:** `001-adopt-external-emacs-mcp`
**Branch:** `feature/001-adopt-external-emacs-mcp`
**Status:** specification (in_progress)
**Source request:** "remove emacs-mcp to use external one @~/Projects/emacs-mcp/"

---

## 1. Overview

`gemini-cli-ide.el` today ships its own MCP (Model Context Protocol) server
implementation across five files (~2,950 lines):

- `gemini-cli-ide-mcp.el` (933 lines) — JSON-RPC dispatcher, session model,
  WebSocket connection management, push notifications.
- `gemini-cli-ide-mcp-handlers.el` (816 lines) — file/diagnostic/buffer tool
  handlers.
- `gemini-cli-ide-mcp-server.el` (406 lines) — WebSocket server lifecycle.
- `gemini-cli-ide-mcp-http-server.el` (352 lines) — HTTP transport.
- `gemini-cli-ide-emacs-tools.el` (441 lines) — `xref`/`imenu`/`treesit` tool
  wrappers and a Gemini-specific terminal-input reader.

A separate, more focused project — `emacs-mcp` at `~/Projects/emacs-mcp/` —
provides the same MCP server functionality as a standalone library
(Streamable HTTP, MCP protocol `2025-03-26`, lockfile-based discovery,
`emacs-mcp-deftool` extension API). Most of `gemini-cli-ide`'s MCP code is
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

---

## 2. User Stories

- **US-1** — As the maintainer of `gemini-cli-ide`, I want to delete the
  bundled MCP server so that I have ~3,000 fewer lines to maintain and bug
  fixes flow through one upstream library instead of two divergent copies.

- **US-2** — As an end user of `gemini-cli-ide`, I want my existing
  `M-x gemini-cli-ide` workflow to keep working after upgrading, so that the
  refactor is invisible to me other than installing one new dependency.

- **US-3** — As an end user, I want the Gemini CLI subprocess that the
  package launches to discover and connect to the local Emacs MCP server
  automatically, so that I do not have to hand-edit
  `~/.gemini/settings.json` per project.

- **US-4** — As a contributor, I want Gemini-specific MCP tools (e.g. the
  terminal-input reader, anything that touches the Gemini vterm/eat buffer)
  to register through `emacs-mcp`'s public extension API
  (`emacs-mcp-deftool` / `emacs-mcp-register-tool`), so that adding
  Gemini-only behavior does not require forking `emacs-mcp`.

- **US-5** — As a security-conscious user, I want the new server to keep
  the same guarantees the bundled implementation provided —
  localhost-only binding, project-scoped path validation, no credential or
  buffer-content leakage in logs — so that the refactor is not a regression
  in security posture.

- **US-6** — As an Emacs configuration hoarder, I want a clear error
  message (not a silent no-op) if I upgrade `gemini-cli-ide` without also
  installing `emacs-mcp`, so I know exactly what to do.

---

## 3. Functional Requirements

### Code removal

- **FR-1** — All of the following files MUST be deleted from the repository:
  - `gemini-cli-ide-mcp.el`
  - `gemini-cli-ide-mcp-handlers.el`
  - `gemini-cli-ide-mcp-server.el`
  - `gemini-cli-ide-mcp-http-server.el`

- **FR-2** — `gemini-cli-ide-emacs-tools.el` MUST be either deleted or
  reduced to only the Gemini-specific tools that have no equivalent in
  `emacs-mcp` (currently: the terminal-input reader for what the user is
  typing in the Gemini vterm/eat buffer, plus any other Gemini-only
  helpers identified during clarification).

- **FR-3** — All `(require 'gemini-cli-ide-mcp...)` and
  `(require 'gemini-cli-ide-emacs-tools)` forms in surviving files MUST be
  updated to `(require 'emacs-mcp)` (and any sub-modules actually used).

- **FR-4** — `gemini-cli-ide-tests.el` MUST drop every test that exercises
  the deleted modules: JSON-RPC parsing, WebSocket session handling, HTTP
  transport, tool-handler unit tests for removed handlers. Tests for
  surviving Gemini-glue code MUST be retained.

### Dependency declaration

- **FR-5** — The `Package-Requires` line in `gemini-cli-ide.el` MUST be
  updated to:
  - **Add:** `emacs-mcp` with an explicit version pin
    [NEEDS CLARIFICATION: which version — `0.1.0` exists today; do we pin
    to that, to a `>=` semantic, or to a git revision until MELPA?].
  - **Remove:** `websocket`, `web-server`.
  - **Keep:** `transient`.
  - **Raise:** the Emacs floor to `29.1` (matches `emacs-mcp`'s minimum).

- **FR-6** — `Keywords:` MUST drop `websocket`. Other keywords (`ai`,
  `gemini`, `cli`, `assistant`, `mcp`) MUST be retained.

### Server lifecycle

- **FR-7** — When the user invokes `M-x gemini-cli-ide` for a project,
  the package MUST ensure an `emacs-mcp` server is running and bound to
  the project's root directory. If no server is running, the package MUST
  start one (e.g., via `emacs-mcp-start` or by enabling
  `emacs-mcp-mode`).

- **FR-8** — When the package starts an `emacs-mcp` server, it MUST do so
  with `emacs-mcp-project-directory` set to the current
  `project-current` root.

- **FR-9** — When the user invokes `M-x gemini-cli-ide-stop` for a
  project, the package MUST tear down its own Gemini subprocess and
  buffer, but MUST NOT stop the `emacs-mcp` server unless the package
  itself started that server AND no other Gemini sessions are still using
  it. [NEEDS CLARIFICATION: precise ownership/refcount rule.]

### Endpoint discovery by Gemini CLI

- **FR-10** — The Gemini CLI subprocess that the package launches MUST be
  pointed at the running `emacs-mcp` endpoint
  (`http://127.0.0.1:<PORT>/mcp`). Mechanism:
  [NEEDS CLARIFICATION — choose one and document]:
  - (a) Read `~/.emacs-mcp/<PORT>.lock` and pass the URL to Gemini via an
    environment variable or CLI flag.
  - (b) Write/update a project-local `.gemini/settings.json` with an
    `mcpServers.emacs.url` entry before spawning the subprocess.
  - (c) Rely on the user configuring `~/.gemini/settings.json` once,
    globally, and document the snippet.
  - (d) Some combination (e.g., (a) auto with (c) fallback).

### Tool surface

- **FR-11** — Tools that already exist as built-ins in `emacs-mcp` —
  `project-info`, `list-buffers`, `open-file`, `get-buffer-content`,
  `get-diagnostics`, `imenu-symbols`, `xref-find-references`,
  `xref-find-apropos`, `treesit-info`, `execute-elisp` — MUST NOT be
  re-implemented in `gemini-cli-ide`. The `emacs-mcp` versions are
  authoritative.

- **FR-12** — Gemini-specific tools that today live in
  `gemini-cli-ide-emacs-tools.el` and have no upstream equivalent (at
  minimum: the terminal-input reader currently described as
  *"Read what the user is currently typing in the Gemini terminal before
  they press Enter"*) MUST be re-registered via
  `emacs-mcp-deftool` or `emacs-mcp-register-tool` so they appear in the
  external server's `tools/list`.

- **FR-13** — `gemini-cli-ide-emacs-tools-setup` MUST be retired.
  [NEEDS CLARIFICATION: removed outright (breaking change for users who
  call it from their init), OR kept as a deprecated alias that emits a
  `display-warning` and registers the surviving Gemini-specific tools.]

### Push notifications / editor state

- **FR-14** — Today the bundled server pushes selection-change and
  active-editor notifications (`gemini-cli-ide-mcp.el` lines ~233–562).
  This behavior MUST be either:
  - (a) preserved by hooking into `emacs-mcp` notification/hook points
    (if `emacs-mcp` exposes a stable mechanism for pushing
    `notifications/*` to connected clients), OR
  - (b) explicitly dropped, with a CHANGELOG entry, if `emacs-mcp` does
    not expose such a mechanism. [NEEDS CLARIFICATION: which path,
    contingent on emacs-mcp's actual API.]

### Multi-session model

- **FR-15** — Today's README claims *"multiple concurrent sessions per
  project."* `emacs-mcp` exposes a single server per Emacs instance
  bound to a single `emacs-mcp-project-directory`. The package MUST
  pick one of:
  - (a) Keep the multi-project promise by switching the active
    `emacs-mcp-project-directory` per Gemini buffer (and document
    serialization caveats), OR
  - (b) Officially drop multi-project concurrency in this release,
    documenting the regression in CHANGELOG and README.
  [NEEDS CLARIFICATION.]

### License

- **FR-16** — `emacs-mcp` is **AGPL-3.0-or-later**; `gemini-cli-ide` is
  currently **GPL-3.0-or-later**. AGPL-licensed code combined with this
  package at distribution time forces the combined work to be distributed
  under AGPL-3.0-or-later. The package MUST resolve this. Options:
  - (a) Relicense `gemini-cli-ide` to AGPL-3.0-or-later.
  - (b) Negotiate a relicense of `emacs-mcp` to a GPL-3.0-compatible
    license.
  - (c) Keep `gemini-cli-ide` source under GPL-3.0-or-later but accept
    that any *redistributed combined work* is AGPL (this is the standard
    "compatible upward" reading; verify with the actual project owner of
    `emacs-mcp`).
  [NEEDS CLARIFICATION — pick one and document the rationale in the
  package header and `README.md`.]

### Build / CI

- **FR-17** — `scripts/compile-and-test.sh` MUST locate `emacs-mcp` and
  add it to `-L` load path. The existing helper `find_emacs_package`
  pattern is sufficient; add an entry for `emacs-mcp`.

- **FR-18** — The byte-compile + native-compile + ERT pipeline MUST pass
  with **zero warnings** against the new dependency set on Emacs 29.1+.

- **FR-19** — `.github/workflows/*` (if any CI exists) MUST install
  `emacs-mcp` from its source location before running the test script.
  [NEEDS CLARIFICATION: confirm CI config currently exists; if not, this
  FR is dropped from this spec.]

### Documentation

- **FR-20** — `README.md` MUST be updated to:
  - List `emacs-mcp` as a hard dependency in *Requirements*.
  - Drop *WebSocket* references in the *Features* section; replace with
    "Streamable HTTP MCP transport (via `emacs-mcp`)".
  - Update *Installation* `use-package` snippets to install `emacs-mcp`
    alongside `gemini-cli-ide`.
  - Document the chosen endpoint-discovery mechanism (FR-10).
  - Note the license change (if any) per FR-16.
  - Note any dropped features (FR-14, FR-15) in a "Breaking changes"
    section.

- **FR-21** — `gemini-cli-ide.el`'s own `;;; Commentary:` block MUST be
  rewritten to remove any claim that the package itself runs a
  WebSocket/MCP server.

### Versioning

- **FR-22** — The `Version:` header in `gemini-cli-ide.el` MUST be bumped
  to a new minor version (currently `0.2.0`; this change is breaking
  enough to warrant `0.3.0` at minimum). A `CHANGELOG.md` MUST be created
  if one does not exist, with an entry summarizing this refactor.

---

## 4. Non-Functional Requirements

- **NFR-1 — Behavioral parity (where in scope).** The eight retained
  interactive commands (`gemini-cli-ide`, `gemini-cli-ide-continue`,
  `gemini-cli-ide-resume`, `gemini-cli-ide-stop`,
  `gemini-cli-ide-switch-to-buffer`, `gemini-cli-ide-list-sessions`,
  `gemini-cli-ide-check-status`, `gemini-cli-ide-insert-at-mentioned`)
  MUST keep their existing names and externally visible behavior, except
  for changes explicitly accepted under FR-14 / FR-15.

- **NFR-2 — Localhost-only binding.** The MCP server bound to the project
  MUST listen on `127.0.0.1` only. (Inherited from `emacs-mcp`'s default;
  the package MUST NOT alter that default.)

- **NFR-3 — Project-scoped path validation.** All file-path arguments
  passed to MCP tools MUST be rejected if they escape the project root.
  (Inherited from `emacs-mcp`; the package MUST NOT bypass it.)

- **NFR-4 — Compatibility floor.** Emacs 29.1 is the new minimum.
  Anything that was using a 28.1-only fallback can be simplified.

- **NFR-5 — Code-size reduction.** Net deletion target is the four MCP
  files (~2,500+ lines of `*.el`) plus the corresponding tests. The PR
  for this feature SHOULD show a strongly negative diff.

- **NFR-6 — Logging hygiene.** The new code MUST NOT log buffer contents,
  full file paths outside the project root, or any token/credential.
  (Constitution principle 7.)

- **NFR-7 — No silent failure.** If `emacs-mcp` is not on `load-path`
  when `gemini-cli-ide` loads, the user MUST see a `user-error` from any
  interactive command attempt that explicitly says
  `emacs-mcp` is required and how to install it.

- **NFR-8 — No new hard dependencies beyond `emacs-mcp` and
  `transient`.** Soft/optional integrations (`vterm`, `eat`,
  `with-editor`) remain optional and gated.

---

## 5. Acceptance Criteria

- **AC-1** — `git ls-files` after the change does NOT contain
  `gemini-cli-ide-mcp.el`, `gemini-cli-ide-mcp-handlers.el`,
  `gemini-cli-ide-mcp-server.el`, or `gemini-cli-ide-mcp-http-server.el`.

- **AC-2** — `grep -nE "websocket|web-server" gemini-cli-ide*.el README.md`
  returns zero matches in source code (CHANGELOG/historical text is
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
  `emacs-mcp` installed, calling any of the eight retained interactive
  commands signals a `user-error` whose message names `emacs-mcp` and
  installation guidance.

- **AC-7** — `M-x list-load-path-shadows` after installing both packages
  reports no shadowing between this package and `emacs-mcp`.

- **AC-8** — `README.md` and the `;;; Commentary:` block in
  `gemini-cli-ide.el` agree with the new architecture (no
  WebSocket/MCP-server claims for this package itself; `emacs-mcp` named
  as the dependency that provides the server).

- **AC-9** — `CHANGELOG.md` contains an entry for the new version that
  explicitly lists: dropped deps, raised Emacs floor, license change (if
  any), and any feature regressions accepted under FR-14 / FR-15.

---

## 6. Out of Scope

- Adding new MCP tools that have no equivalent in either the current
  package or `emacs-mcp`.
- Modifying upstream `emacs-mcp` (sending PRs to it). If this work
  uncovers gaps in `emacs-mcp`'s extension API, those gaps are tracked
  separately as new specs.
- Changing the Gemini CLI binary's own behavior or its native settings
  format.
- Migrating to a non-Streamable-HTTP transport.
- Refactors to the prompt buffer, vterm/eat integration, or transient
  menu that are unrelated to MCP.
- Performance optimizations of the new dependency.
- Renaming or restructuring the eight retained interactive commands.
- Creating a unified MELPA recipe for either package.

---

## 7. Open Questions

All marked **[NEEDS CLARIFICATION]** in the requirements above; collected
here for the clarification stage:

1. **OQ-1 (License — FR-16).** Relicense `gemini-cli-ide` to AGPL-3.0,
   negotiate a relicense of `emacs-mcp`, or accept "combined work
   redistributed under AGPL"? This decision blocks PR merge.

2. **OQ-2 (Endpoint discovery — FR-10).** Lockfile read + env var, write
   `.gemini/settings.json`, document a one-time global config, or a
   combination?

3. **OQ-3 (Server ownership / lifecycle — FR-9).** When
   `gemini-cli-ide-stop` is invoked, does it stop the `emacs-mcp` server
   it started? Refcount sessions? Leave the server running?

4. **OQ-4 (Push notifications — FR-14).** Does `emacs-mcp` expose a
   stable mechanism to push `notifications/*` (selection change, active
   editor) to connected clients? If not, do we drop the feature with a
   CHANGELOG entry, or contribute the mechanism upstream first?

5. **OQ-5 (Multi-project concurrency — FR-15).** Keep the
   "multiple concurrent sessions per project" promise, or drop it?

6. **OQ-6 (Tool-set bridge — FR-13).** `gemini-cli-ide-emacs-tools-setup`:
   remove outright, or keep as a deprecated alias for one release?

7. **OQ-7 (Version pin — FR-5).** Pin `emacs-mcp` to `0.1.0`, a `>=`
   range, or a specific git SHA until it is on MELPA?

8. **OQ-8 (Distribution — FR-5).** `emacs-mcp` is not yet on MELPA. Do
   we ship `gemini-cli-ide` with a `straight.el` recipe that fetches
   `emacs-mcp` from a specific git host, document a manual install, or
   wait until `emacs-mcp` is on MELPA?

9. **OQ-9 (Tool name compatibility).** Do any current users have
   automation that calls Gemini-specific tools by names defined in this
   repo today? If so, does FR-12 preserve those exact names, or is a
   rename acceptable?

10. **OQ-10 (Surviving file boundary — FR-2).** After deletion, do the
    surviving Gemini-specific tools live in a renamed file
    (`gemini-cli-ide-tools.el`) registered into `emacs-mcp`, or inline
    in `gemini-cli-ide.el`?

11. **OQ-11 (CI — FR-19).** Does this repo have CI today that needs
    updating, or is `scripts/compile-and-test.sh` the only check?

12. **OQ-12 (`gemini-cli-ide-diagnostics.el`).** This file (240 lines)
    is not in the deletion list above. Verify it is independent of MCP
    and survives unchanged, or add it to FR-1.
