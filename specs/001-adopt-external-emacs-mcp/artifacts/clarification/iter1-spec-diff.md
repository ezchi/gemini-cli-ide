diff --git a/specs/001-adopt-external-emacs-mcp/spec.md b/specs/001-adopt-external-emacs-mcp/spec.md
index b8e4f73..6fa95ab 100644
--- a/specs/001-adopt-external-emacs-mcp/spec.md
+++ b/specs/001-adopt-external-emacs-mcp/spec.md
@@ -111,22 +111,30 @@ Both are tracked under FR-23/FR-24 below.
   - `gemini-cli-ide-diagnostics.el` (a flycheck/flymake → VSCode-JSON
     converter used only by the bundled MCP handlers; superseded by
     `emacs-mcp`'s `get-diagnostics` built-in tool).
-
-- **FR-2** — `gemini-cli-ide-emacs-tools.el` MUST be either deleted or
-  reduced to only the Gemini-specific tools that have no equivalent in
-  `emacs-mcp`. At minimum the **terminal-input reader** —
-  `gemini-cli-ide-mcp-get-terminal-input` per `gemini-cli-ide-emacs-tools.el`
-  — MUST be retained (re-registered, see FR-12). All `xref-*`, `imenu-*`,
+  - `gemini-cli-ide-emacs-tools.el` (full deletion; surviving
+    Gemini-specific tool registration moves to a new file — see
+    FR-2).
+
+- **FR-2** — A new file `gemini-cli-ide-tools.el` MUST be created. It
+  contains only the Gemini-specific tools that have no equivalent in
+  `emacs-mcp`. In this release that is exactly one tool: the
+  terminal-input reader (`gemini-cli-ide-mcp-get-terminal-input` per
+  the deleted `gemini-cli-ide-emacs-tools.el:369`). The new file MUST
+  register the tool via `emacs-mcp-deftool` /
+  `emacs-mcp-register-tool` at load time, so that
+  `(require 'gemini-cli-ide)` makes the tool available to any
+  subsequent `emacs-mcp` server start. All `xref-*`, `imenu-*`,
   `treesit-*`, `project-info`, `get-diagnostics`, `list-buffers`,
-  `open-file`, and `get-buffer-content` wrappers MUST be deleted (these
-  are duplicates of `emacs-mcp` built-ins per its README *Built-in Tools*
-  table).
+  `open-file`, and `get-buffer-content` wrappers from the deleted
+  file are NOT carried forward — `emacs-mcp`'s built-ins replace
+  them (see FR-11 and the rename table in CHANGELOG).
 
 - **FR-3** — All `(require 'gemini-cli-ide-mcp...)`,
   `(require 'gemini-cli-ide-emacs-tools)`, and
-  `(require 'gemini-cli-ide-diagnostics)` forms in surviving files MUST
-  be updated to `(require 'emacs-mcp)` (and any sub-modules actually
-  used).
+  `(require 'gemini-cli-ide-diagnostics)` forms in surviving files
+  MUST be updated. `gemini-cli-ide.el` MUST `(require 'emacs-mcp)`
+  and `(require 'gemini-cli-ide-tools)`. No other module from the
+  deleted set may be required.
 
 - **FR-4** — `gemini-cli-ide-tests.el` MUST drop every test that
   exercises the deleted modules: JSON-RPC parsing, WebSocket session
@@ -138,9 +146,14 @@ Both are tracked under FR-23/FR-24 below.
 
 - **FR-5** — The `Package-Requires` line in `gemini-cli-ide.el` MUST be
   updated to:
-  - **Add:** `emacs-mcp` with an explicit version pin
-    [NEEDS CLARIFICATION OQ-7: pin to `0.1.0` exactly, `>= 0.1.0`, or a
-    specific git revision until MELPA].
+  - **Add:** `(emacs-mcp "0.1.0")`. Emacs `Package-Requires` semantics
+    treat this as ">= 0.1.0", which is appropriate while
+    `emacs-mcp` itself is at version 0.1.0 (per its
+    `emacs-mcp.el:7` `Version:` header). The `CHANGELOG.md` entry
+    for this release MUST additionally record the exact git SHA of
+    `emacs-mcp` that this version was tested against (source URL
+    `https://github.com/ezchi/emacs-mcp.git`) so users can pin a
+    reproducible install.
   - **Remove:** `websocket`, `web-server`.
   - **Keep:** `transient`.
   - **Raise:** the Emacs floor from `28.1` to `29.1` (matches
@@ -156,19 +169,15 @@ Both are tracked under FR-23/FR-24 below.
   is running, the package MUST start one (e.g., via `emacs-mcp-start` or
   by enabling `emacs-mcp-mode`).
 
-- **FR-8** — When a Gemini buffer is created or activated for a given
-  project, the package MUST ensure the `emacs-mcp` server's active
-  project directory matches that buffer's project root. The PRIMARY
-  mechanism MUST be the `set-project-dir` JSON-RPC protocol method
-  exposed by `emacs-mcp` (see `emacs-mcp-protocol.el` →
-  `emacs-mcp--handle-set-project-dir`), which switches the session's
-  project root without restarting the server. If `set-project-dir` is
-  not feasible per-session, the FALLBACK is restarting the server with
-  the new `emacs-mcp-project-directory` value, accepting that this
-  drops in-flight sessions for other projects.
-  [NEEDS CLARIFICATION OQ-3: confirm `set-project-dir` is callable
-  per-session without server restart; if not, the fallback must be
-  documented as a regression.]
+- **FR-8** — When a Gemini buffer is created for a given project, the
+  package MUST open a fresh `emacs-mcp` session pinned to that
+  project's root by passing `projectDir` in the `initialize` request
+  (see `emacs-mcp/emacs-mcp-protocol.el:74-114`). When an existing
+  Gemini buffer's project root changes (rare), the package MUST send
+  `emacs-mcp/setProjectDir` on that buffer's session
+  (`emacs-mcp-protocol.el:205-243`). Both mechanisms operate
+  per-session and require no server restart; multiple sessions for
+  multiple projects coexist on a single underlying server process.
 
 - **FR-9** — When `M-x gemini-cli-ide-stop` is invoked for a project,
   the package MUST tear down its own Gemini subprocess and buffer. The
@@ -182,7 +191,7 @@ Both are tracked under FR-23/FR-24 below.
 
 - **FR-10** — The Gemini CLI subprocess that the package launches MUST
   be pointed at the running `emacs-mcp` endpoint
-  (`http://127.0.0.1:<PORT>/mcp`). The PRIMARY mechanism is:
+  (`http://127.0.0.1:<PORT>/mcp`). The mechanism is:
   1. Read `emacs-mcp-connection-info` (or the lockfile at
      `~/.emacs-mcp/<PORT>.lock`) to determine the active port.
   2. Write a project-local `.gemini/settings.json` with an
@@ -194,10 +203,8 @@ Both are tracked under FR-23/FR-24 below.
 
   The `.gemini/settings.json` file is already in this repo's
   `.gitignore`, so the project-local write does not pollute version
-  control.
-  [NEEDS CLARIFICATION OQ-2: whether the package additionally writes a
-  global `~/.gemini/settings.json` entry as a fallback for non-project
-  invocations.]
+  control. The package MUST NOT write to or modify the global
+  `~/.gemini/settings.json` — that file is user-managed.
 
 ### Tool surface
 
@@ -206,58 +213,66 @@ Both are tracked under FR-23/FR-24 below.
   `get-diagnostics`, `imenu-symbols`, `xref-find-references`,
   `xref-find-apropos`, `treesit-info`, `execute-elisp` — MUST NOT be
   re-implemented in `gemini-cli-ide`. The `emacs-mcp` versions are
-  authoritative.
-
-- **FR-12** — Gemini-specific tools that have no upstream equivalent —
-  at minimum `gemini-cli-ide-mcp-get-terminal-input` (described as
-  *"Read what the user is currently typing in the Gemini terminal
-  before they press Enter"* in
-  `gemini-cli-ide-emacs-tools.el:369`) — MUST be re-registered via
-  `emacs-mcp-deftool` or `emacs-mcp-register-tool` so they appear in
-  the external server's `tools/list`. The MCP-facing tool name MUST be
-  preserved for backward compatibility unless OQ-9 resolves otherwise.
-
-- **FR-13** — `gemini-cli-ide-emacs-tools-setup` MUST be retired.
-  [NEEDS CLARIFICATION OQ-6: removed outright (breaking change for
-  users who call it from their init), OR kept as a deprecated alias
-  that emits a `display-warning` and registers the surviving
-  Gemini-specific tools.]
+  authoritative. The MCP tool names that today exist in
+  `gemini-cli-ide-emacs-tools.el` (`gemini-cli-ide-mcp-xref-find-*`,
+  `gemini-cli-ide-mcp-project-info`,
+  `gemini-cli-ide-mcp-imenu-list-symbols`,
+  `gemini-cli-ide-mcp-treesit-info`) WILL DISAPPEAR — Gemini and any
+  other MCP client MUST use the equivalent `emacs-mcp` built-in
+  names. The rename mapping MUST be published in the CHANGELOG (see
+  FR-20). The Gemini-only extension parameters on `treesit-info`
+  (`whole_file`, `include_ancestors`, `include_children`) ARE NOT
+  carried over — they are dropped this release; this MUST be listed
+  as a regression in the CHANGELOG.
+
+- **FR-12** — The Gemini-specific tool
+  `gemini-cli-ide-mcp-get-terminal-input` (described as *"Read what
+  the user is currently typing in the Gemini terminal before they
+  press Enter"* in `gemini-cli-ide-emacs-tools.el:369` of the
+  to-be-deleted file) MUST be re-registered via `emacs-mcp-deftool`
+  or `emacs-mcp-register-tool` from the new
+  `gemini-cli-ide-tools.el` (see FR-2) so it appears in the external
+  server's `tools/list`. The MCP-facing tool name MUST be preserved
+  verbatim (`gemini-cli-ide-mcp-get-terminal-input`).
+
+- **FR-13** — `gemini-cli-ide-emacs-tools-setup` MUST be kept as a
+  deprecation shim for the v0.3.x cycle and removed in v0.4.0. The
+  shim MUST:
+  - Be defined in `gemini-cli-ide.el` (or `gemini-cli-ide-tools.el`)
+    as a no-op `defun` carrying the same `;;;###autoload` cookie as
+    today.
+  - On invocation, emit a one-time `display-warning` of severity
+    `:warning` with text: "gemini-cli-ide-emacs-tools-setup is
+    deprecated. Use `(emacs-mcp-mode 1)` and require 'gemini-cli-ide
+    instead. Will be removed in v0.4.0."
+  - Do NOT call `(emacs-mcp-mode 1)` itself.
+  - Do NOT register any tools (Gemini-specific tools auto-register
+    when `gemini-cli-ide-tools` is loaded — see FR-2).
 
 ### Push notifications / editor state
 
-- **FR-14** — Today the bundled server pushes selection-change and
-  active-editor notifications (`gemini-cli-ide-mcp.el` lines ~233–562
-  via `gemini-cli-ide-mcp--send-notification`). Implementation MUST:
-  1. Inspect `emacs-mcp`'s public API for a notification-push hook
-     before this work begins.
-  2. If a stable mechanism exists (e.g., a hook that lets a tool
-     handler emit `notifications/*` to the active session), preserve
-     the feature by registering against it.
-  3. If no such mechanism exists, this feature is **explicitly dropped
-     in this release**, the dropped behavior MUST be listed in
-     `CHANGELOG.md` under "Breaking changes," and a follow-up task
-     MUST be filed (outside this spec) to add the mechanism upstream
-     to `emacs-mcp` and re-enable the feature in a later release.
-  [NEEDS CLARIFICATION OQ-4: outcome of step 1 — confirm presence or
-  absence of the upstream hook before implementation begins.]
+- **FR-14** — The bundled server's selection-change and active-editor
+  push notifications (`gemini-cli-ide-mcp.el` lines ~233–562 via
+  `gemini-cli-ide-mcp--send-notification`) ARE DROPPED in this
+  release. Verified absence of a public push API in `emacs-mcp`: the
+  protocol dispatch table (`emacs-mcp-protocol.el:26-36`) lists no
+  outbound-notification helper; `tools.listChanged` capability is
+  explicitly advertised as `:false` (`emacs-mcp-protocol.el:101`);
+  the only SSE usage is for completed deferred tool responses. The
+  dropped behavior MUST be listed in `CHANGELOG.md` under "Breaking
+  changes." A follow-up upstream spec/PR MUST be filed against
+  `emacs-mcp` to add a public push-notification mechanism; that work
+  is OUT OF SCOPE for this spec.
 
 ### Multi-project session model
 
 - **FR-15** — The "multiple concurrent sessions per project" claim in
-  the current README MUST be honored under the new architecture.
-  Implementation MUST use FR-8's `set-project-dir` mechanism to route
-  each Gemini buffer's MCP traffic to the correct project root. The
-  package MAY track at most one `emacs-mcp` server process per Emacs
-  session (the upstream model), but per-Gemini-buffer routing MUST
-  remain the user-visible guarantee.
-
-  If — and only if — clarification of OQ-3 establishes that
-  `set-project-dir` cannot achieve per-session routing without server
-  restart, the package SHALL fall back to "one Gemini session at a
-  time per Emacs," and that regression MUST be:
-  - Documented in `README.md` under "Breaking changes."
-  - Reflected by a constitutional amendment to Principle 3
-    (see FR-24).
+  the current README is preserved under the new architecture. Each
+  Gemini buffer corresponds to its own `emacs-mcp` session, opened
+  via `initialize` with the buffer's project root in `projectDir`
+  (FR-8). Multiple sessions for multiple projects coexist on a
+  single underlying `emacs-mcp` server process. No fallback to
+  "single Gemini session at a time" is in scope for this release.
 
 ### License
 
@@ -265,7 +280,7 @@ Both are tracked under FR-23/FR-24 below.
   is currently **GPL-3.0-or-later**. AGPL §13 explicitly permits
   combination with GPL-3.0 code, but the combined work that is
   distributed must be offered under AGPL-3.0-or-later terms. The
-  PRIMARY position adopted by this spec is:
+  position for this release is:
   - `gemini-cli-ide` source files KEEP their GPL-3.0-or-later
     headers.
   - `README.md` and the package commentary block MUST add a clear
@@ -274,10 +289,6 @@ Both are tracked under FR-23/FR-24 below.
     the AGPL §13 network-use disclosure obligation.
   - No relicensing of source files is required by this spec.
 
-  [NEEDS CLARIFICATION OQ-1: maintainer accepts this position, or
-  prefers (a) relicensing all of `gemini-cli-ide` source headers to
-  AGPL-3.0-or-later for clarity.]
-
 ### Build / CI
 
 - **FR-17** — `scripts/compile-and-test.sh` MUST locate `emacs-mcp`
@@ -302,14 +313,27 @@ Both are tracked under FR-23/FR-24 below.
   - Drop *WebSocket* references in the *Features* section; replace
     with "Streamable HTTP MCP transport (provided by `emacs-mcp`)".
   - Update *Installation* `use-package` snippets to install
-    `emacs-mcp` alongside `gemini-cli-ide`, including its source URL
-    until it lands on MELPA.
+    `emacs-mcp` alongside `gemini-cli-ide`. The recommended snippet
+    is:
+    ```elisp
+    (use-package emacs-mcp
+      :straight (emacs-mcp :type git :host github :repo "ezchi/emacs-mcp"))
+    (use-package gemini-cli-ide
+      :straight (gemini-cli-ide :type git :host github :repo "ezchi/gemini-cli-ide.el"))
+    ```
+    The README MUST also document a manual `git clone` +
+    `add-to-list 'load-path` install for non-`straight` users, and
+    note that MELPA submission for `emacs-mcp` is tracked upstream
+    as future work.
   - Document the chosen endpoint-discovery mechanism (FR-10).
   - Add a *License* paragraph reflecting FR-16 (combined work is
     AGPL-3.0-or-later).
   - Add a *Breaking changes* section listing: dropped 28.1 support,
-    dropped `websocket`/`web-server` deps, any feature regression
-    accepted under FR-14 / FR-15.
+    dropped `websocket`/`web-server` deps, MCP-tool rename mapping
+    (per FR-11), dropped `treesit-info` extension params, dropped
+    selection/active-editor push notifications (FR-14), and the
+    deprecation timeline for `gemini-cli-ide-emacs-tools-setup`
+    (FR-13).
 
 - **FR-21** — `gemini-cli-ide.el`'s own `;;; Commentary:` block MUST
   be rewritten to remove any claim that the package itself runs a
@@ -337,12 +361,13 @@ Both are tracked under FR-23/FR-24 below.
 
 - **FR-24** — `.steel/constitution.md` Principle 3
   ("Project-scoped sessions") MUST be re-stated to reflect the new
-  architecture — "Each Gemini buffer is routed to its project root
-  via `emacs-mcp`'s `set-project-dir` session mechanism; multiple
-  Gemini buffers across projects must coexist without
-  cross-contamination." If FR-15's fallback is triggered (no
-  per-session routing), this principle MUST be amended further to
-  explicitly drop the multi-project concurrency guarantee.
+  architecture — "Each Gemini buffer corresponds to a distinct
+  `emacs-mcp` session pinned to its project root via the
+  `initialize.projectDir` parameter and adjustable via
+  `emacs-mcp/setProjectDir`; multiple Gemini buffers across projects
+  must coexist on a single underlying server process without
+  cross-contamination." The multi-project concurrency guarantee is
+  preserved.
 
 ---
 
@@ -393,7 +418,9 @@ Both are tracked under FR-23/FR-24 below.
 - **AC-1** — `git ls-files` after the change does NOT contain
   `gemini-cli-ide-mcp.el`, `gemini-cli-ide-mcp-handlers.el`,
   `gemini-cli-ide-mcp-server.el`, `gemini-cli-ide-mcp-http-server.el`,
-  or `gemini-cli-ide-diagnostics.el`.
+  `gemini-cli-ide-diagnostics.el`, or
+  `gemini-cli-ide-emacs-tools.el`. `git ls-files` after the change
+  DOES contain `gemini-cli-ide-tools.el` (per FR-2).
 
 - **AC-2** — `grep -nE "websocket|web-server" *.el README.md` returns
   zero matches in source code (CHANGELOG / historical text is
@@ -440,10 +467,8 @@ Both are tracked under FR-23/FR-24 below.
   `M-x gemini-cli-ide` in each: the second buffer's MCP traffic is
   routed to the second buffer's project root (verifiable by calling
   `project-info` from each Gemini session and observing distinct
-  project paths returned). If FR-15's fallback is triggered, this AC
-  is dropped and replaced by an AC asserting that the second
-  invocation displays the documented "single Gemini session at a
-  time" message.
+  project paths returned). Both sessions remain alive concurrently
+  on a single underlying `emacs-mcp` server process.
 
 - **AC-11** — `.steel/constitution.md` in the same PR has been
   amended per FR-23 and FR-24. `git diff` of the constitution shows
@@ -474,52 +499,90 @@ Both are tracked under FR-23/FR-24 below.
 
 ## 7. Open Questions
 
-All marked **[NEEDS CLARIFICATION]** in the requirements above;
-collected here for the clarification stage:
-
-1. **OQ-1 (License — FR-16).** Maintainer accepts the spec's primary
-   position (combined work AGPL, no source relicense needed), or
-   prefers explicit relicensing of all source files to
-   AGPL-3.0-or-later?
-
-2. **OQ-2 (Endpoint discovery — FR-10).** Does the package also write
-   a global `~/.gemini/settings.json` entry, or only the project-local
-   `.gemini/settings.json`?
-
-3. **OQ-3 (set-project-dir feasibility — FR-8, FR-15).** Confirm
-   whether `emacs-mcp`'s `set-project-dir` JSON-RPC method allows
-   per-session project routing without restarting the underlying
-   server process. If not, the fallback (one Gemini session at a
-   time) is in effect and FR-24 must amend Principle 3 accordingly.
-
-4. **OQ-4 (Push notifications — FR-14).** Does `emacs-mcp` expose a
-   stable mechanism to push `notifications/*` (selection change,
-   active editor) to connected clients? If not, confirm dropping the
-   feature in this release with a CHANGELOG entry and a follow-up
-   upstream task.
-
-5. **OQ-6 (Tool-set bridge — FR-13).**
-   `gemini-cli-ide-emacs-tools-setup`: remove outright, or keep as a
-   deprecated alias for one release?
-
-6. **OQ-7 (Version pin — FR-5).** Pin `emacs-mcp` to `0.1.0` exactly,
-   `>= 0.1.0`, or a specific git SHA until it is on MELPA?
-
-7. **OQ-8 (Distribution — FR-5).** `emacs-mcp` is not yet on MELPA.
-   Do we ship `gemini-cli-ide` with a `straight.el` recipe that
-   fetches `emacs-mcp` from a specific git host, document a manual
-   install, or wait until `emacs-mcp` is on MELPA?
-
-8. **OQ-9 (Tool-name compatibility — FR-12).** Do any current users
-   have automation that calls Gemini-specific tools by their current
-   names? If so, FR-12 must preserve those exact names; if not, a
-   rename is permissible during re-registration.
-
-9. **OQ-10 (Surviving file boundary — FR-2).** After deletion, do
-   the surviving Gemini-specific tools live in a new file
-   (`gemini-cli-ide-tools.el`) registered into `emacs-mcp`, or
-   inline in `gemini-cli-ide.el`?
-
-*(OQ-5 from iter-1 is folded into FR-15's primary path + fallback;
-OQ-11 from iter-1 is dropped — no CI exists; OQ-12 from iter-1 is
-resolved by adding `gemini-cli-ide-diagnostics.el` to FR-1.)*
+All open questions from the specification stage have been resolved
+during the clarification stage. See `clarifications.md` for the full
+record. Resolution summary:
+
+- **OQ-1** (License posture) — RESOLVED (no spec change). Source
+  stays GPL-3.0-or-later; combined-work disclosure in README per
+  AGPL §13.
+- **OQ-2** (Endpoint discovery scope) — RESOLVED. Project-local
+  `.gemini/settings.json` only; global file untouched.
+- **OQ-3** (`setProjectDir` per-session feasibility) — RESOLVED.
+  Per-session routing confirmed in `emacs-mcp-protocol.el:205-243`;
+  fallback path removed from FR-15 / FR-24.
+- **OQ-4** (Push notifications) — RESOLVED. No public push API in
+  `emacs-mcp`; feature dropped this release; upstream follow-up
+  tracked separately.
+- **OQ-6** (`gemini-cli-ide-emacs-tools-setup` retirement) —
+  RESOLVED. Deprecation shim with `display-warning` for v0.3.x;
+  removed in v0.4.0.
+- **OQ-7** (Version pin) — RESOLVED. `(emacs-mcp "0.1.0")`
+  (semantically ≥ 0.1.0); tested SHA recorded in CHANGELOG.
+- **OQ-8** (Distribution path) — RESOLVED. `straight.el` recipe
+  documented; manual install documented; MELPA tracked upstream.
+- **OQ-9** (Tool-name compatibility) — RESOLVED.
+  `gemini-cli-ide-mcp-get-terminal-input` preserved; legacy
+  `gemini-cli-ide-mcp-*` names for built-in duplicates disappear;
+  rename mapping in CHANGELOG.
+- **OQ-10** (Surviving file boundary) — RESOLVED. New file
+  `gemini-cli-ide-tools.el`.
+
+(Iter-1 OQ-5 folded into the resolution of OQ-3; iter-1 OQ-11
+dropped — no CI exists; iter-1 OQ-12 resolved by adding
+`gemini-cli-ide-diagnostics.el` to FR-1.)
+
+---
+
+## 8. Changelog
+
+- **[Clarification iter1] FR-2** — Replaced "either deleted or
+  reduced" wording with definitive "deleted; surviving tool moves to
+  new file `gemini-cli-ide-tools.el`." Resolves OQ-10.
+- **[Clarification iter1] FR-3** — Concretized the require update:
+  surviving file requires `emacs-mcp` and `gemini-cli-ide-tools`. No
+  other deleted module may be required.
+- **[Clarification iter1] FR-5** — Pinned `(emacs-mcp "0.1.0")`;
+  added requirement to record tested SHA in CHANGELOG. Resolves
+  OQ-7. Source URL noted (resolves part of OQ-8).
+- **[Clarification iter1] FR-8** — Removed `[NEEDS CLARIFICATION
+  OQ-3]`. Affirmative statement of `initialize.projectDir` /
+  `emacs-mcp/setProjectDir` per-session routing, with file-line
+  citations into `emacs-mcp-protocol.el`. Resolves OQ-3.
+- **[Clarification iter1] FR-10** — Removed `[NEEDS CLARIFICATION
+  OQ-2]`. Locked to project-local-only writes; global file
+  explicitly off-limits. Resolves OQ-2.
+- **[Clarification iter1] FR-11** — Added explicit rename mapping
+  for built-in duplicates (legacy `gemini-cli-ide-mcp-*` names
+  disappear) and dropped-extension call-out for `treesit-info`
+  Gemini-only params. Resolves OQ-9.
+- **[Clarification iter1] FR-12** — Affirmed preservation of
+  `gemini-cli-ide-mcp-get-terminal-input`; tied registration to the
+  new `gemini-cli-ide-tools.el` per FR-2. Resolves part of OQ-9.
+- **[Clarification iter1] FR-13** — Replaced `[NEEDS CLARIFICATION
+  OQ-6]` with definitive deprecation-shim spec
+  (`display-warning`, no auto `emacs-mcp-mode 1`, removal in
+  v0.4.0). Resolves OQ-6.
+- **[Clarification iter1] FR-14** — Removed `[NEEDS CLARIFICATION
+  OQ-4]`. Affirmative drop of push notifications, with file-line
+  citations into `emacs-mcp-protocol.el` proving the absence of a
+  public push API. Resolves OQ-4.
+- **[Clarification iter1] FR-15** — Removed the conditional
+  fallback path. Multi-project concurrency is preserved via
+  multi-session architecture.
+- **[Clarification iter1] FR-16** — Removed `[NEEDS CLARIFICATION
+  OQ-1]`. Resolves OQ-1.
+- **[Clarification iter1] FR-20** — Concretized README install
+  snippet (literal `straight.el` recipe). Added rename mapping +
+  dropped extensions to required *Breaking changes* content.
+  Resolves part of OQ-8.
+- **[Clarification iter1] FR-24** — Removed conditional language;
+  Principle 3 amendment is now unconditional and aligned with the
+  preserved multi-project guarantee.
+- **[Clarification iter1] AC-1** — Added
+  `gemini-cli-ide-emacs-tools.el` to the must-not-be-present list;
+  added `gemini-cli-ide-tools.el` to the must-be-present
+  expectation.
+- **[Clarification iter1] AC-10** — Removed the conditional
+  fallback branch; AC asserts concurrent multi-session operation
+  unconditionally.
