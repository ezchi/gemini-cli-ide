# Gauge Review — Task Breakdown Iteration 1

(Provider: gemini)

## Summary
The task breakdown for `001-adopt-external-emacs-mcp` is comprehensive and follows the implementation strategy outlined in the plan. It correctly identifies the core deletion and rewiring tasks. However, it fails to fully implement the silent-failure guard required by AC-6 and Plan §2.1, and there are minor issues with task granularity and dependency descriptions.

## Task Completeness

| Phase | Tasks | Plan §6 Requirement | Deliverables §10 Owner |
| :--- | :--- | :--- | :--- |
| Phase 0 | Task 1 | Phase 0 (Pre-flight) | Manual / Notes |
| Phase 1 | Task 2 | Phase 1 (Constitution) | `.steel/constitution.md` |
| Phase 2 | Tasks 3, 4, 5 | Phase 2 (Add files/deps) | `gemini-cli-ide-tools.el`, `gemini-cli-ide.el` (header), scripts |
| Phase 3 | Task 6 | Phase 3 (Delete bundled MCP) | Six `.el` files deleted |
| Phase 4 | Tasks 7, 8, 9 | Phase 4 (Rewire core) | `gemini-cli-ide.el` (plumbing), `gemini-cli-ide-transient.el` |
| Phase 5 | Tasks 10, 11 | Phase 5 (Tests) | `gemini-cli-ide-tests.el` |
| Phase 6 | Tasks 12, 13, 14 | Phase 6 (Docs) | `gemini-cli-ide.el` (Commentary), `README.md`, `CHANGELOG.md` |
| Phase 7 | Tasks 15, 16 | Phase 7 (Verification) | Full sweep |

All phases from the plan have corresponding tasks. Every deliverable in §10 is owned.

## Ordering and Dependencies
The dependency chain is strictly linear and logical.
- Task 6 correctly identifies that tests will be broken after deletion.
- Task 10 is the one that actually removes the failing tests and restores a green state for ERT.

## Granularity
- **Task 8** is exceptionally large. It combines the implementation of a complex JSON-merge utility (`--write-gemini-settings`), rewiring of the entire session lifecycle across multiple functions, adding a deprecation shim, and repurposing a user option. This should ideally be split into two tasks: one for the `write-settings` helper and one for the lifecycle/shim/option wiring.

## Verification Criteria

| AC | Task(s) Verifying |
| :--- | :--- |
| AC-1 (Deletions) | Task 6, Task 16 |
| AC-2 (No websocket) | Task 6, Task 16 |
| AC-3 (Metadata) | Task 4, Task 16 |
| AC-4 (Zero warnings) | Task 15 |
| AC-5 (Concurrent projects) | Manual (Validation Stage) |
| AC-6 (Silent-failure guard) | Task 7, Task 11 (test) — **INCOMPLETE** |
| AC-7 (Shadowing) | Manual (Validation Stage) |
| AC-8 (Docs/Commentary) | Task 12, Task 13 |
| AC-9 (CHANGELOG) | Task 14 |
| AC-10 (Multi-session routing) | Manual (Validation Stage) |
| AC-11 (Constitution) | Task 2, Task 16 |

AC-5, AC-7, and AC-10 are correctly deferred to manual validation as they involve interactive/environmental checks.

## Constitutional Alignment
The breakdown respects the namespacing and file-naming conventions. Task 2 correctly sequences the constitutional amendments before the code changes. Task 15 enforces the "byte-compile-zero-warnings" constraint.

## Issues

### BLOCKING
- **Task 8: Missing Guards in Interactive Commands.** AC-6 and Plan §2.1 require that `gemini-cli-ide--require-emacs-mcp` be called at the top of **every** `;;;###autoload` interactive command to prevent silent failures or "void-function" crashes when `emacs-mcp` is missing. Task 8 only specifies calling it in `gemini-cli-ide--start-session` (a private helper) and `gemini-cli-ide-stop`. Other interactive commands like `gemini-cli-ide-check-status` (which calls `emacs-mcp-connection-info`) are left unguarded. Task 8 must be updated to include these guards in all eight interactive commands listed in NFR-1.

### WARNING
- **Task 8 Granularity.** As noted in Summary, this task touches ~9 different functions/areas. Splitting it would make for cleaner, more focused commits.
- **Task 6 Dependency Note.** The description says "(tests broken until Task 9)". Task 9 only rewires the transient module. It is Task 10 that actually removes the failing tests and restores a green ERT state. This note should be updated to reference Task 10 to avoid confusion during execution.

### NOTE
- **Task 9 Phase Mapping.** The Plan §6 titles Phase 5 as "Rewire transient module + tests". Task 9 (transient rewiring) is grouped into Phase 4 in the task list. This is a harmless labeling mismatch as long as the functional sequence is preserved.

VERDICT: REVISE
