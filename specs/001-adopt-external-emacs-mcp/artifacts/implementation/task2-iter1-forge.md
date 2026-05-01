# Task 2: Amend `.steel/constitution.md` for emacs-mcp adoption — Forge Iteration 1

## Files Changed
- `.steel/constitution.md` — modified (constitutional amendments per FR-23 / FR-24)

## Key Implementation Decisions
- **Project Identity** rewritten to clarify that the MCP server now lives
  in `emacs-mcp`; the package is a launcher + tool-registrar.
- **Principle 3 (Project-scoped sessions)** rephrased per FR-24 final
  text — multi-session model on a single shared `emacs-mcp` server.
- **Principle 6 (Reversible, opt-in integrations)** updated: removed the
  `gemini-cli-ide-emacs-tools-setup` example (function is being deprecated)
  and added the explicit `.gemini/settings.json` project-local-only
  constraint from C-2.
- **Technology Stack** — Emacs floor `28.1` → `29.1`; hard deps trimmed to
  `emacs-mcp 0.1.0+` and `transient 0.9.0+` (`websocket` and `web-server`
  removed).
- **Coding Standards / Error handling** — replaced the "WebSocket layer"
  reference with `emacs-mcp` since this package no longer hosts a
  WebSocket server.
- **Constraints / Compatibility** — Emacs floor synchronously updated to
  29.1; explicitly notes 28.x is no longer supported.
- **Constraints / Performance** — bullet about "MCP request handlers"
  rewritten to describe handlers we register *into* `emacs-mcp`; removed
  the "selection / buffer-state notifications" bullet entirely (push
  notifications are dropped per FR-14).
- **Constraints / Security** — MCP-server binding bullet now correctly
  attributes the binding to `emacs-mcp` and forbids us from altering its
  default.
- **Constraints / Dependency hygiene** — wording adjusted: "the three"
  was historical (websocket, transient, web-server). Now describes "those
  listed in `Package-Requires`" without a fixed count, naming the current
  set.

## Deviations from Plan
- Plan §6 Phase 1 listed exactly three changes (Emacs floor, deps,
  Principle 3). Implementation made additional, *internally consistent*
  edits to remove orphan references to "WebSocket layer", the deleted
  `selection / buffer-state notifications` performance bullet, and the
  `gemini-cli-ide-emacs-tools-setup` example in Principle 6. These are
  not new policy — they're the same amendment expressed at every
  affected location to keep the constitution self-consistent.

## Tests Added
- None (constitution is documentation, not code).
