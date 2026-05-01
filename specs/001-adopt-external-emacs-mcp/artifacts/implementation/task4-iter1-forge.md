# Task 4: Update package metadata in gemini-cli-ide.el — Forge Iteration 1

## Files Changed
- `gemini-cli-ide.el` — modified (lines 6–8 header bumps; lines 61–67
  require block).

## Key Implementation Decisions
- **`Version:`** `0.2.0` → `0.3.0` per FR-22.
- **`Package-Requires:`** rewritten to
  `((emacs "29.1") (emacs-mcp "0.1.0") (transient "0.9.0"))`. Drops
  `websocket 1.12+` and `web-server 0.1.2+`. Matches AC-3 verbatim.
- **`Keywords:`** drops `websocket`. Final list:
  `ai, gemini, cli, assistant, mcp`.
- **Require block** — added two new requires above the legacy MCP
  requires:
  - `(require 'emacs-mcp)` — the new hard dep.
  - `(require 'gemini-cli-ide-tools)` — the new module from Task 3.
  - Legacy `(require 'gemini-cli-ide-mcp ...)` /
    `gemini-cli-ide-mcp-server` / `gemini-cli-ide-emacs-tools`
    entries are intentionally KEPT for now per the plan; Task 6
    deletes them when it deletes the corresponding files.

## Deviations from Plan
- None. Plan §6 Phase 2 step 2 specified this exact sequence.

## Tests Added
- None (tests for the new metadata are mechanical verification via
  AC-3 / Task 17).

## Build verification
After this task plus Task 5: `./scripts/compile-and-test.sh` exits
0, 84 tests, 76 expected pass, 0 unexpected, 8 pre-existing skips.
