# Clarifications — Spec 001 `adopt-external-emacs-mcp`

(Forge iteration 1)

This document resolves the open questions and ambiguities flagged in
`spec.md` §7 ("Open Questions"). For each clarification, the
resolution is marked **[SPEC UPDATE]** if it changes a requirement in
`spec.md`, or **[NO SPEC CHANGE]** if it merely makes an already-stated
position explicit.

The Project Constitution (`.steel/constitution.md`) is the highest
authority. Where a clarification has a constitutional implication
(e.g., FR-15's potential amendment to Principle 3), that implication
is noted explicitly.

---

## C-1 — License posture (resolves OQ-1, FR-16) **[NO SPEC CHANGE]**

**Decision:** Adopt the spec's primary position. `gemini-cli-ide`
source files keep their `GPL-3.0-or-later` headers. README and the
package commentary block disclose that the *distributed combined work*
(this package linked against `emacs-mcp`) is effectively
`AGPL-3.0-or-later` per AGPL §13, including the §13 network-use
disclosure obligation.

**Rationale:**
- AGPL-3.0 §13 explicitly permits combination with GPL-3.0 code.
- Bulk-relicensing every source-file header is mechanical busywork
  that yields no legal benefit if the combined-work distribution is
  already documented as AGPL.
- Keeping the source under GPL-3.0-or-later preserves the option for
  this code to be reused outside the AGPL combined work in the
  future.
- This matches the way many GPL-3.0 packages treat AGPL plugin
  dependencies in the Emacs ecosystem.

**Spec change:** None. FR-16's "PRIMARY position" stands; the
`[NEEDS CLARIFICATION OQ-1]` marker on FR-16 is removed.

---

## C-2 — Endpoint discovery scope (resolves OQ-2, FR-10) **[SPEC UPDATE]**

**Decision:** The package writes the discovered emacs-mcp endpoint
URL **only** into the project-local `.gemini/settings.json`. It does
**not** write to or modify the global `~/.gemini/settings.json`.

**Rationale:**
- Project-local writes are precisely scoped: only this project's
  Gemini invocations talk to this project's emacs-mcp session.
- The global file is a user-managed config; the package has no
  authority to overwrite it. Different projects may target different
  emacs-mcp instances or none at all.
- `.gemini/settings.json` is already in this repo's `.gitignore`, so
  the project-local write does not pollute version control.
- If a user wants a global default, they can set it manually once
  per machine — the package documents this in the README.

**Spec change:** FR-10 — drop the `[NEEDS CLARIFICATION OQ-2]` line
about a global fallback. The mechanism is project-local-only.

---

## C-3 — `setProjectDir` per-session feasibility (resolves OQ-3, FR-8 / FR-15 / FR-24) **[SPEC UPDATE]**

**Decision:** Per-session project routing IS feasible without server
restart. Verified by reading `emacs-mcp/emacs-mcp-protocol.el`:

- The dispatch table maps `emacs-mcp/setProjectDir` →
  `emacs-mcp--handle-set-project-dir`
  (`emacs-mcp-protocol.el:34-35`).
- That handler mutates the session's `project-dir` field via
  `setf (emacs-mcp-session-project-dir session) new-dir` and fires
  `emacs-mcp-project-dir-changed-hook`
  (`emacs-mcp-protocol.el:230-236`). No server restart, no other
  sessions affected.
- Even simpler: `emacs-mcp--handle-initialize` accepts a `projectDir`
  param, so a freshly opened session can be created with its target
  project root from the very first request
  (`emacs-mcp-protocol.el:74-114`).

**Architecture commitment:** ONE `emacs-mcp` server process, MULTIPLE
sessions (one per Gemini buffer), each pinned to its own project
root. This preserves Constitution Principle 3 verbatim.

**Spec changes:**
- **FR-8** — drop `[NEEDS CLARIFICATION OQ-3]`. State the primary
  mechanism affirmatively: each Gemini session is created via
  `initialize` with the correct `projectDir`; subsequent project
  switches (rare) use `emacs-mcp/setProjectDir`. Drop the
  "fallback to server restart" language entirely.
- **FR-15** — drop the conditional fallback path and its constitutional
  amendment trigger. The "multiple concurrent sessions" promise is
  preserved.
- **FR-24** — narrow to "re-state Principle 3 in terms of emacs-mcp's
  multi-session model." Drop the conditional "if FR-15's fallback is
  triggered" branch.
- **AC-10** — drop the conditional "if fallback" branch; keep only
  the multi-project verification.

---

## C-4 — Push notifications must be dropped (resolves OQ-4, FR-14) **[SPEC UPDATE]**

**Decision:** `emacs-mcp` does NOT expose a stable public API to push
server-initiated `notifications/*` (selection-change, active-editor)
to connected clients. Verified:

- The protocol dispatch table in
  `emacs-mcp/emacs-mcp-protocol.el:26-36` lists only
  client-initiated methods. No outbound-notification helper.
- SSE infrastructure (`sse-streams` field on each session,
  `emacs-mcp--http-send-sse-event`) exists, but it is wired
  exclusively to deliver completed *deferred tool responses* —
  there is no public function for arbitrary push notifications.
- The server explicitly advertises `tools.listChanged: :false`
  capability (`emacs-mcp-protocol.el:101`), confirming that
  list-change notifications are not supported.

**Decision:** This release **drops** the selection-change /
active-editor push-notification feature. A follow-up upstream
spec/PR will be filed against `emacs-mcp` to add a public push API,
and a later `gemini-cli-ide` release will re-enable the feature once
that lands.

**Spec changes:**
- **FR-14** — replace the three-step "inspect / preserve / drop"
  flow with a definitive statement: "Push-notification feature is
  dropped in this release. Feature regression listed in CHANGELOG.
  Follow-up upstream task tracked outside this spec."
- **CHANGELOG / FR-20** — add an explicit "Removed: real-time
  selection and active-editor notifications to the connected Gemini
  CLI; will return after upstream `emacs-mcp` exposes a push API."
  bullet to the Breaking changes section.

---

## C-5 — `gemini-cli-ide-emacs-tools-setup` deprecation shim (resolves OQ-6, FR-13) **[SPEC UPDATE]**

**Decision:** Keep `gemini-cli-ide-emacs-tools-setup` as a
deprecated shim for one minor-version cycle (i.e., for v0.3.x), then
remove in v0.4.0. The shim:

1. Emits a one-time `display-warning` of severity `:warning` with the
   message: "gemini-cli-ide-emacs-tools-setup is deprecated. Use
   `(emacs-mcp-mode 1)` and require 'gemini-cli-ide instead. Will be
   removed in v0.4.0."
2. Does NOT call `(emacs-mcp-mode 1)` itself — the user controls
   when the server starts.
3. Does NOT register any tools — Gemini-specific tool registration
   moves to a load-time effect in the new
   `gemini-cli-ide-tools.el` (see C-9).

**Rationale:**
- Removing the function outright would break existing user inits
  with a hard "void function" error at load time. A deprecation
  warning is friendlier and gives users one release to migrate.
- The shim has no behavioral effect beyond the warning, so it does
  not split code paths.

**Spec change:** **FR-13** — replace
`[NEEDS CLARIFICATION OQ-6]` with the affirmative shim description
above. Add `display-warning` requirement to `NFR-7` family.

---

## C-6 — Version pin and dependency declaration (resolves OQ-7, FR-5) **[SPEC UPDATE]**

**Decision:** Pin as `(emacs-mcp "0.1.0")` in `Package-Requires`.

**Rationale:**
- Verified by reading `emacs-mcp/emacs-mcp.el:7`: the package's own
  `Version:` header is `0.1.0`.
- `git tag -l` on `~/Projects/emacs-mcp` returns no version tags
  yet, so we cannot pin to a tag.
- Emacs `Package-Requires` semantics: `(foo "0.1.0")` means
  "0.1.0 or later." This gives forward compatibility without
  pinning to an unreleased SHA.
- The README will additionally document the **exact git SHA tested
  against** so users have a reproducible install reference (the
  current SHA on `~/Projects/emacs-mcp` is `6c85616`, see C-7).

**Spec change:** **FR-5** — replace `[NEEDS CLARIFICATION OQ-7]`
with `(emacs-mcp "0.1.0")`.

---

## C-7 — Distribution / install path (resolves OQ-8, FR-5 / FR-20) **[SPEC UPDATE]**

**Decision:** `emacs-mcp` is hosted at
`https://github.com/ezchi/emacs-mcp.git` and is not on MELPA. The
README MUST document:

1. A `straight.el` recipe:
   ```elisp
   (use-package emacs-mcp
     :straight (emacs-mcp :type git :host github :repo "ezchi/emacs-mcp"))
   ```
2. A manual install fallback (clone + `load-path`) for non-`straight`
   users.
3. A note that MELPA submission is tracked upstream as future work.

The tested git SHA at the time of release SHALL be recorded in the
`CHANGELOG.md` entry for this version, so a reproducible install is
possible.

**Spec changes:**
- **FR-5** — note the source URL.
- **FR-20** — concretize the install snippet (replace the abstract
  "including its source URL until it lands on MELPA" with the
  literal recipe above).

---

## C-8 — Tool-name preservation and dropped extension params (resolves OQ-9, FR-11 / FR-12) **[SPEC UPDATE]**

**Decision A — Preserve Gemini-specific tool name.** The MCP tool
name `gemini-cli-ide-mcp-get-terminal-input` is preserved verbatim
when re-registering through `emacs-mcp-deftool`. Gemini CLI users
calling this tool via existing automation will see no change.

**Decision B — Other tools that today live in
`gemini-cli-ide-emacs-tools.el` are deleted, and their MCP names
disappear.** This is a deliberate breaking change at the MCP-tool-name
level (not the Emacs Lisp API level). The rename mapping for
documentation:

| Removed name (gemini-cli-ide)              | Replacement name (emacs-mcp built-in) | Notes |
|--------------------------------------------|---------------------------------------|-------|
| `gemini-cli-ide-mcp-xref-find-references`  | `xref-find-references`                | semantic equivalent |
| `gemini-cli-ide-mcp-xref-find-apropos`     | `xref-find-apropos`                   | semantic equivalent |
| `gemini-cli-ide-mcp-project-info`          | `project-info`                        | semantic equivalent |
| `gemini-cli-ide-mcp-imenu-list-symbols`    | `imenu-symbols`                       | semantic equivalent |
| `gemini-cli-ide-mcp-treesit-info`          | `treesit-info`                        | **REGRESSION:** the gemini version had extra params `whole_file`, `include_ancestors`, `include_children`. emacs-mcp's built-in does not. These are dropped in this release. |

**Decision C — `treesit-info` extension params dropped.** Treesit
extension params (`whole_file`, `include_ancestors`,
`include_children`) are explicitly dropped this release. A
follow-up upstream task to extend emacs-mcp's `treesit-info` MAY be
filed; it is not blocking this release.

**Spec changes:**
- **FR-11** — add an explicit note that the rename and dropped
  extension params are intentional and part of this release.
- **FR-12** — affirm preservation of
  `gemini-cli-ide-mcp-get-terminal-input`.
- **FR-20 / CHANGELOG entry** — add the rename table above (or a
  pointer to it) under "Breaking changes."

---

## C-9 — Surviving-file naming (resolves OQ-10, FR-2) **[SPEC UPDATE]**

**Decision:** The surviving Gemini-specific tool registration code
moves to a new file: **`gemini-cli-ide-tools.el`** (note: the
`-emacs-` infix is dropped, since the file is now Gemini-specific
tools registered into `emacs-mcp`, not "Emacs tools generally").

**Contents of the new file:**
- Required boilerplate: `;;; Commentary:`, `;;; Code:`, lexical
  binding, `(provide 'gemini-cli-ide-tools)`.
- `(require 'emacs-mcp)`.
- Definition + registration of `gemini-cli-ide-mcp-get-terminal-input`
  (the only surviving Gemini-specific tool in this release).
- The registration MUST happen at file load time, so that
  `(require 'gemini-cli-ide)` automatically makes the tool available
  to any subsequent emacs-mcp server start.

**Rationale:**
- Inline registration in `gemini-cli-ide.el` (1418 lines) would
  bloat the main file.
- The new filename signals intent — these are gemini-cli-ide's
  custom tools, not generic Emacs tools.
- Constitution Coding Standards "One concern per file. File name =
  feature prefix" is honored.

**Spec changes:**
- **FR-2** — replace "MUST be either deleted or reduced to only the
  Gemini-specific tools" with "MUST be deleted; surviving Gemini-
  specific tool registration moves to a new file
  `gemini-cli-ide-tools.el`."
- **FR-3** — add `(require 'gemini-cli-ide-tools)` to the surviving
  `gemini-cli-ide.el`.
- **AC-1** — add `gemini-cli-ide-emacs-tools.el` to the list of
  files that MUST NOT be present in `git ls-files`.

---

## C-10 — Implicit assumption made explicit: emacs-mcp lifecycle ownership signal **[NO SPEC CHANGE]**

**Clarification:** FR-9 says "the package MUST track which `emacs-mcp`
server instances it started." Make explicit that the tracking
mechanism is a single buffer-local (per-Gemini-buffer) flag plus a
single Emacs-session-global counter of how many Gemini buffers depend
on the server this package started. When that counter drops to zero
on `gemini-cli-ide-stop`, the package calls `emacs-mcp-stop`. If the
counter is incremented because a user-started server already
existed, the package never calls `emacs-mcp-stop`.

**Rationale:** Implementation detail, but worth pinning down so the
implementer doesn't invent something fragile (e.g., scanning
`emacs-mcp` internals).

**Spec change:** None — FR-9 already covers this functionally.
Implementation note will appear in the planning stage.

---

## C-11 — Implicit assumption made explicit: `gemini-cli-ide-debug` and `gemini-cli-ide-transient` survive **[NO SPEC CHANGE]**

**Clarification:** Confirm the deletion list in FR-1 / C-9 / FR-2
covers all MCP-coupled files and **only** those:
- DELETED: `gemini-cli-ide-mcp.el`,
  `gemini-cli-ide-mcp-handlers.el`, `gemini-cli-ide-mcp-server.el`,
  `gemini-cli-ide-mcp-http-server.el`,
  `gemini-cli-ide-diagnostics.el`, `gemini-cli-ide-emacs-tools.el`.
- SURVIVES (unchanged or near-unchanged):
  `gemini-cli-ide.el` (main launcher; needs require-list update and
  Commentary rewrite), `gemini-cli-ide-debug.el` (debug helpers,
  pure utility), `gemini-cli-ide-transient.el` (transient menu, no
  MCP coupling).
- NEW: `gemini-cli-ide-tools.el` (per C-9).

**Spec change:** None — informational.

---

## C-12 — Implicit assumption made explicit: scripts / tests that already deal with `websocket` shadows **[NO SPEC CHANGE]**

**Clarification:** Confirm that
`scripts/format-and-clean.sh:31-46` references
`gemini-cli-ide-mcp-server` in the Emacs batch invocation that
formats `.el` files. After deletion, that reference must be removed
(replaced with `(require 'emacs-mcp nil t)` if needed for
indentation hints, otherwise removed entirely).

**Spec change:** None — informational. Will be a task in the
implementation stage.

---

## Summary table

| OQ ID | Decision summary                                                                 | Spec section affected | [SPEC UPDATE]? |
|-------|----------------------------------------------------------------------------------|-----------------------|----------------|
| OQ-1  | Source GPL-3.0; combined work AGPL per §13                                       | FR-16                 | NO             |
| OQ-2  | Project-local `.gemini/settings.json` only                                       | FR-10                 | YES            |
| OQ-3  | Per-session via `setProjectDir`/`initialize.projectDir` — fallback NOT triggered | FR-8, FR-15, FR-24, AC-10 | YES        |
| OQ-4  | Drop push notifications this release; upstream follow-up tracked separately     | FR-14, FR-20 (CHANGELOG) | YES         |
| OQ-6  | Deprecation shim until v0.4.0 with `display-warning`                             | FR-13                 | YES            |
| OQ-7  | `(emacs-mcp "0.1.0")` (≥ 0.1.0) + tested-SHA in CHANGELOG                        | FR-5                  | YES            |
| OQ-8  | `straight.el` recipe + manual install; MELPA tracked upstream                   | FR-5, FR-20           | YES            |
| OQ-9  | Preserve `gemini-cli-ide-mcp-get-terminal-input`; rename-table for built-ins; treesit extras dropped | FR-11, FR-12, CHANGELOG | YES |
| OQ-10 | New file `gemini-cli-ide-tools.el`                                              | FR-2, FR-3, AC-1      | YES            |
