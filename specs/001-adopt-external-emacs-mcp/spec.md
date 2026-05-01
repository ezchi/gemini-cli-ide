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
  - `gemini-cli-ide-emacs-tools.el` (full deletion; surviving
    Gemini-specific tool registration moves to a new file — see
    FR-2).

- **FR-2** — A new file `gemini-cli-ide-tools.el` MUST be created. It
  contains only the Gemini-specific tools that have no equivalent in
  `emacs-mcp`. In this release that is exactly one tool: the
  terminal-input reader (`gemini-cli-ide-mcp-get-terminal-input` per
  the deleted `gemini-cli-ide-emacs-tools.el:369`). The new file MUST
  register the tool via `emacs-mcp-deftool` /
  `emacs-mcp-register-tool` at load time, so that
  `(require 'gemini-cli-ide)` makes the tool available to any
  subsequent `emacs-mcp` server start. All `xref-*`, `imenu-*`,
  `treesit-*`, `project-info`, `get-diagnostics`, `list-buffers`,
  `open-file`, and `get-buffer-content` wrappers from the deleted
  file are NOT carried forward — `emacs-mcp`'s built-ins replace
  them (see FR-11 and the rename table in CHANGELOG).

- **FR-3** — All `(require 'gemini-cli-ide-mcp...)`,
  `(require 'gemini-cli-ide-emacs-tools)`, and
  `(require 'gemini-cli-ide-diagnostics)` forms in surviving files
  MUST be updated. `gemini-cli-ide.el` MUST `(require 'emacs-mcp)`
  and `(require 'gemini-cli-ide-tools)`. No other module from the
  deleted set may be required.

- **FR-4** — `gemini-cli-ide-tests.el` MUST drop every test that
  exercises the deleted modules: JSON-RPC parsing, WebSocket session
  handling, HTTP transport, tool-handler unit tests for removed
  handlers, diagnostics-converter tests. Tests for surviving Gemini-glue
  code MUST be retained.

### Dependency declaration

- **FR-5** — The `Package-Requires` line in `gemini-cli-ide.el` MUST be
  updated to:
  - **Add:** `(emacs-mcp "0.1.0")`. Emacs `Package-Requires` semantics
    treat this as ">= 0.1.0", which is appropriate while
    `emacs-mcp` itself is at version 0.1.0 (per its
    `emacs-mcp.el:7` `Version:` header). The `CHANGELOG.md` entry
    for this release MUST additionally record the exact git SHA of
    `emacs-mcp` that this version was tested against (source URL
    `https://github.com/ezchi/emacs-mcp.git`) so users can pin a
    reproducible install.
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

- **FR-8** — When a Gemini buffer is created for a given project, the
  package MUST open a fresh `emacs-mcp` session pinned to that
  project's root by passing `projectDir` in the `initialize` request
  (see `emacs-mcp/emacs-mcp-protocol.el:74-114`). When an existing
  Gemini buffer's project root changes (rare), the package MUST send
  `emacs-mcp/setProjectDir` on that buffer's session
  (`emacs-mcp-protocol.el:205-243`). Both mechanisms operate
  per-session and require no server restart; multiple sessions for
  multiple projects coexist on a single underlying server process.

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
  (`http://127.0.0.1:<PORT>/mcp`). The mechanism is:
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
  control. The package MUST NOT write to or modify the global
  `~/.gemini/settings.json` — that file is user-managed.

### Tool surface

- **FR-11** — Tools that already exist as built-ins in `emacs-mcp` —
  `project-info`, `list-buffers`, `open-file`, `get-buffer-content`,
  `get-diagnostics`, `imenu-symbols`, `xref-find-references`,
  `xref-find-apropos`, `treesit-info`, `execute-elisp` — MUST NOT be
  re-implemented in `gemini-cli-ide`. The `emacs-mcp` versions are
  authoritative. The MCP tool names that today exist in
  `gemini-cli-ide-emacs-tools.el` (`gemini-cli-ide-mcp-xref-find-*`,
  `gemini-cli-ide-mcp-project-info`,
  `gemini-cli-ide-mcp-imenu-list-symbols`,
  `gemini-cli-ide-mcp-treesit-info`) WILL DISAPPEAR — Gemini and any
  other MCP client MUST use the equivalent `emacs-mcp` built-in
  names. The rename mapping MUST be published in the CHANGELOG (see
  FR-20). The Gemini-only extension parameters on `treesit-info`
  (`whole_file`, `include_ancestors`, `include_children`) ARE NOT
  carried over — they are dropped this release; this MUST be listed
  as a regression in the CHANGELOG.

- **FR-12** — The Gemini-specific tool
  `gemini-cli-ide-mcp-get-terminal-input` (described as *"Read what
  the user is currently typing in the Gemini terminal before they
  press Enter"* in `gemini-cli-ide-emacs-tools.el:369` of the
  to-be-deleted file) MUST be re-registered via `emacs-mcp-deftool`
  or `emacs-mcp-register-tool` from the new
  `gemini-cli-ide-tools.el` (see FR-2) so it appears in the external
  server's `tools/list`. The MCP-facing tool name MUST be preserved
  verbatim (`gemini-cli-ide-mcp-get-terminal-input`).

- **FR-13** — `gemini-cli-ide-emacs-tools-setup` MUST be kept as a
  deprecation shim for the v0.3.x cycle and removed in v0.4.0. The
  shim MUST:
  - Be defined in `gemini-cli-ide.el` (or `gemini-cli-ide-tools.el`)
    as a no-op `defun` carrying the same `;;;###autoload` cookie as
    today.
  - On invocation, emit a one-time `display-warning` of severity
    `:warning` with text: "gemini-cli-ide-emacs-tools-setup is
    deprecated. Use `(emacs-mcp-mode 1)` and require 'gemini-cli-ide
    instead. Will be removed in v0.4.0."
  - Do NOT call `(emacs-mcp-mode 1)` itself.
  - Do NOT register any tools (Gemini-specific tools auto-register
    when `gemini-cli-ide-tools` is loaded — see FR-2).

### Push notifications / editor state

- **FR-14** — The bundled server's selection-change and active-editor
  push notifications (`gemini-cli-ide-mcp.el` lines ~233–562 via
  `gemini-cli-ide-mcp--send-notification`) ARE DROPPED in this
  release. Verified absence of a public push API in `emacs-mcp`: the
  protocol dispatch table (`emacs-mcp-protocol.el:26-36`) lists no
  outbound-notification helper; `tools.listChanged` capability is
  explicitly advertised as `:false` (`emacs-mcp-protocol.el:101`);
  the only SSE usage is for completed deferred tool responses. The
  dropped behavior MUST be listed in `CHANGELOG.md` under "Breaking
  changes." A follow-up upstream spec/PR MUST be filed against
  `emacs-mcp` to add a public push-notification mechanism; that work
  is OUT OF SCOPE for this spec.

### Multi-project session model

- **FR-15** — The "multiple concurrent sessions per project" claim in
  the current README is preserved under the new architecture. Each
  Gemini buffer corresponds to its own `emacs-mcp` session, opened
  via `initialize` with the buffer's project root in `projectDir`
  (FR-8). Multiple sessions for multiple projects coexist on a
  single underlying `emacs-mcp` server process. No fallback to
  "single Gemini session at a time" is in scope for this release.

### License

- **FR-16** — `emacs-mcp` is **AGPL-3.0-or-later**; `gemini-cli-ide`
  is currently **GPL-3.0-or-later**. AGPL §13 explicitly permits
  combination with GPL-3.0 code, but the combined work that is
  distributed must be offered under AGPL-3.0-or-later terms. The
  position for this release is:
  - `gemini-cli-ide` source files KEEP their GPL-3.0-or-later
    headers.
  - `README.md` and the package commentary block MUST add a clear
    notice that the distributed combined work (this package linked
    against `emacs-mcp`) is effectively AGPL-3.0-or-later, including
    the AGPL §13 network-use disclosure obligation.
  - No relicensing of source files is required by this spec.

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
    `emacs-mcp` alongside `gemini-cli-ide`. The recommended snippet
    is:
    ```elisp
    (use-package emacs-mcp
      :straight (emacs-mcp :type git :host github :repo "ezchi/emacs-mcp"))
    (use-package gemini-cli-ide
      :straight (gemini-cli-ide :type git :host github :repo "ezchi/gemini-cli-ide.el"))
    ```
    The README MUST also document a manual `git clone` +
    `add-to-list 'load-path` install for non-`straight` users, and
    note that MELPA submission for `emacs-mcp` is tracked upstream
    as future work.
  - Document the chosen endpoint-discovery mechanism (FR-10).
  - Add a *License* paragraph reflecting FR-16 (combined work is
    AGPL-3.0-or-later).
  - Add a *Breaking changes* section listing: dropped 28.1 support,
    dropped `websocket`/`web-server` deps, MCP-tool rename mapping
    (per FR-11), dropped `treesit-info` extension params, dropped
    selection/active-editor push notifications (FR-14), and the
    deprecation timeline for `gemini-cli-ide-emacs-tools-setup`
    (FR-13).

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
  architecture — "Each Gemini buffer corresponds to a distinct
  `emacs-mcp` session pinned to its project root via the
  `initialize.projectDir` parameter and adjustable via
  `emacs-mcp/setProjectDir`; multiple Gemini buffers across projects
  must coexist on a single underlying server process without
  cross-contamination." The multi-project concurrency guarantee is
  preserved.

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
  `gemini-cli-ide-diagnostics.el`, or
  `gemini-cli-ide-emacs-tools.el`. `git ls-files` after the change
  DOES contain `gemini-cli-ide-tools.el` (per FR-2).

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
  project paths returned). Both sessions remain alive concurrently
  on a single underlying `emacs-mcp` server process.

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

All open questions from the specification stage have been resolved
during the clarification stage. See `clarifications.md` for the full
record. Resolution summary:

- **OQ-1** (License posture) — RESOLVED (no spec change). Source
  stays GPL-3.0-or-later; combined-work disclosure in README per
  AGPL §13.
- **OQ-2** (Endpoint discovery scope) — RESOLVED. Project-local
  `.gemini/settings.json` only; global file untouched.
- **OQ-3** (`setProjectDir` per-session feasibility) — RESOLVED.
  Per-session routing confirmed in `emacs-mcp-protocol.el:205-243`;
  fallback path removed from FR-15 / FR-24.
- **OQ-4** (Push notifications) — RESOLVED. No public push API in
  `emacs-mcp`; feature dropped this release; upstream follow-up
  tracked separately.
- **OQ-6** (`gemini-cli-ide-emacs-tools-setup` retirement) —
  RESOLVED. Deprecation shim with `display-warning` for v0.3.x;
  removed in v0.4.0.
- **OQ-7** (Version pin) — RESOLVED. `(emacs-mcp "0.1.0")`
  (semantically ≥ 0.1.0); tested SHA recorded in CHANGELOG.
- **OQ-8** (Distribution path) — RESOLVED. `straight.el` recipe
  documented; manual install documented; MELPA tracked upstream.
- **OQ-9** (Tool-name compatibility) — RESOLVED.
  `gemini-cli-ide-mcp-get-terminal-input` preserved; legacy
  `gemini-cli-ide-mcp-*` names for built-in duplicates disappear;
  rename mapping in CHANGELOG.
- **OQ-10** (Surviving file boundary) — RESOLVED. New file
  `gemini-cli-ide-tools.el`.

(Iter-1 OQ-5 folded into the resolution of OQ-3; iter-1 OQ-11
dropped — no CI exists; iter-1 OQ-12 resolved by adding
`gemini-cli-ide-diagnostics.el` to FR-1.)

---

## 8. Changelog

- **[Clarification iter1] FR-2** — Replaced "either deleted or
  reduced" wording with definitive "deleted; surviving tool moves to
  new file `gemini-cli-ide-tools.el`." Resolves OQ-10.
- **[Clarification iter1] FR-3** — Concretized the require update:
  surviving file requires `emacs-mcp` and `gemini-cli-ide-tools`. No
  other deleted module may be required.
- **[Clarification iter1] FR-5** — Pinned `(emacs-mcp "0.1.0")`;
  added requirement to record tested SHA in CHANGELOG. Resolves
  OQ-7. Source URL noted (resolves part of OQ-8).
- **[Clarification iter1] FR-8** — Removed `[NEEDS CLARIFICATION
  OQ-3]`. Affirmative statement of `initialize.projectDir` /
  `emacs-mcp/setProjectDir` per-session routing, with file-line
  citations into `emacs-mcp-protocol.el`. Resolves OQ-3.
- **[Clarification iter1] FR-10** — Removed `[NEEDS CLARIFICATION
  OQ-2]`. Locked to project-local-only writes; global file
  explicitly off-limits. Resolves OQ-2.
- **[Clarification iter1] FR-11** — Added explicit rename mapping
  for built-in duplicates (legacy `gemini-cli-ide-mcp-*` names
  disappear) and dropped-extension call-out for `treesit-info`
  Gemini-only params. Resolves OQ-9.
- **[Clarification iter1] FR-12** — Affirmed preservation of
  `gemini-cli-ide-mcp-get-terminal-input`; tied registration to the
  new `gemini-cli-ide-tools.el` per FR-2. Resolves part of OQ-9.
- **[Clarification iter1] FR-13** — Replaced `[NEEDS CLARIFICATION
  OQ-6]` with definitive deprecation-shim spec
  (`display-warning`, no auto `emacs-mcp-mode 1`, removal in
  v0.4.0). Resolves OQ-6.
- **[Clarification iter1] FR-14** — Removed `[NEEDS CLARIFICATION
  OQ-4]`. Affirmative drop of push notifications, with file-line
  citations into `emacs-mcp-protocol.el` proving the absence of a
  public push API. Resolves OQ-4.
- **[Clarification iter1] FR-15** — Removed the conditional
  fallback path. Multi-project concurrency is preserved via
  multi-session architecture.
- **[Clarification iter1] FR-16** — Removed `[NEEDS CLARIFICATION
  OQ-1]`. Resolves OQ-1.
- **[Clarification iter1] FR-20** — Concretized README install
  snippet (literal `straight.el` recipe). Added rename mapping +
  dropped extensions to required *Breaking changes* content.
  Resolves part of OQ-8.
- **[Clarification iter1] FR-24** — Removed conditional language;
  Principle 3 amendment is now unconditional and aligned with the
  preserved multi-project guarantee.
- **[Clarification iter1] AC-1** — Added
  `gemini-cli-ide-emacs-tools.el` to the must-not-be-present list;
  added `gemini-cli-ide-tools.el` to the must-be-present
  expectation.
- **[Clarification iter1] AC-10** — Removed the conditional
  fallback branch; AC asserts concurrent multi-session operation
  unconditionally.
