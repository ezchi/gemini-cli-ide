# Task 11: Delete obsolete tests and the WebSocket mock module — Forge Iteration 1

## Files Changed
- `gemini-cli-ide-tests.el` — modified. 
  - Deleted ~50 tests referencing legacy MCP symbols (`gemini-cli-ide-mcp-`, `websocket-`, `json-rpc`, `diagnostics`, `emacs-tools`).
  - Removed WebSocket mock blocks and associated commentary.
  - Dropped `(require 'gemini-cli-ide-mcp-handlers)`.
  - Fixed infinite recursion bug in `vterm`/`eat` terminal backend tests.

## Key Implementation Decisions
- **Systematic Deletion**: Used a Python script to surgically remove `ert-deftest` blocks containing forbidden patterns, ensuring a clean and syntactically correct Emacs Lisp file.
- **Mock Cleanup**: Completely purged the file of its embedded WebSocket server mock, as `emacs-mcp` is now a hard requirement and provided externally.
- **Bug Fix**: Discovered and fixed a regression in `gemini-cli-ide-test-run-without-vterm` and `gemini-cli-ide-test-run-without-eat` where mocking `require` without capturing the original function caused infinite recursion.

## Deviations from Plan
- **Bug Fix included**: The plan didn't explicitly mention fixing existing test bugs, but this was necessary to achieve the "green tests" requirement of the task.

## Tests Added
- None (this was a cleanup task).

## Build verification
- `./scripts/compile-and-test.sh`: PASSED (44 tests passed, 5 skipped).
