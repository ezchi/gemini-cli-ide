# Retrospect Report — Spec 001 `adopt-external-emacs-mcp`

## Workflow Summary
- **Stages completed**: Specification, Clarification, Planning, Task Breakdown, Implementation (17 tasks), Validation.
- **Iterations per stage**:
    - Specification, Clarification, Planning: 1 iteration each.
    - Task Breakdown: 2 iterations.
    - Implementation: 17 tasks total; 14 tasks finished in 1 iteration, 3 tasks (9, 14, 16) required 2 iterations.
    - Validation: 1 iteration.
- **Total Forge-Gauge cycles**: 28.
- **LLMs**: Forge (Claude Opus 4.7), Gauge (Gemini 1.5 Pro).
- **Skills invoked**: `steel-implement`, `steel-validate`.

## Memories to Save
- **Type**: `feedback`
- **Name**: `mock-capture-orig-builtins`
- **Content**: When mocking Emacs built-ins or core package functions (e.g., `require`, `featurep`, `set-window-configuration`), always capture the original function using `symbol-function` and call it within the mock to avoid infinite recursion or breakage of unrelated code paths.
- **Evidence**: `artifacts/implementation/task11-iter1-forge.md:Key Implementation Decisions` and `artifacts/implementation/task16-iter1-forge.md:Files Changed`.
- **Rationale**: This caused a recursive crash in Task 11 and a native-compilation argument-count failure in Task 16. It is a critical pattern for robust testing of core Emacs features.

- **Type**: `project`
- **Name**: `emacs-29-json-standards`
- **Content**: For Emacs 29.1+, prefer `json-parse-buffer` and `json-serialize` over the older `json-read` family. Always specify `:object-type 'alist` for predictable structure.
- **Evidence**: `artifacts/implementation/task12-iter1-forge.md:Key Implementation Decisions`.
- **Rationale**: The project now mandates Emacs 29.1. Using modern JSON APIs ensures better performance and avoids deprecated dependencies.

- **Type**: `project`
- **Name**: `autoload-dependency-guards`
- **Content**: Every interactive command marked with `;;;###autoload` must include a call to the dependency guard (e.g., `gemini-cli-ide--require-emacs-mcp`) at the very top.
- **Evidence**: `artifacts/task_breakdown/iter1-gauge.md:BLOCKING`.
- **Rationale**: Ensures the package fails gracefully with a helpful message instead of "void-function" crashes when external dependencies are missing.

## Skill Updates
- **Skill**: `steel-tasks` (Task Breakdown command)
- **Issue found**: Task 8 was flagged for being "exceptionally large" (`artifacts/task_breakdown/iter1-gauge.md:Granularity`), combining JSON logic, lifecycle rewiring, and shim implementation.
- **Proposed change**: Add a heuristic to the task breakdown logic: "No single task should modify more than 3 distinct logical areas (e.g., a utility, a lifecycle hook, and a user option) or more than 4 functions."
- **Expected impact**: Prevents complex, multi-concern commits and makes the Forge-Gauge review cycle more focused.

- **Skill**: `steel-implement` (Implementation loop)
- **Issue found**: Native compilation surfaced argument-count mismatches in mocks that byte-compilation missed (`artifacts/implementation/task16-iter1-forge.md:Key Implementation Decisions`).
- **Proposed change**: Add a "Validation Checklist" item to the Forge phase: "If mocking built-in functions, use `&rest _args` to ensure compatibility with all call sites, especially under native compilation."
- **Expected impact**: Eliminates the need for a separate "cleanup" iteration for native-compilation regressions.

## Process Improvements
- **Bottlenecks**: Task Breakdown took 2 iterations due to the Gauge enforcing the "guards in every interactive command" requirement. This was a valid architectural catch. Implementation of Task 9 and 16 took 2 iterations due to residual legacy symbols and native-compilation mock bugs, respectively.
- **Forge-Gauge dynamics**:
    - `artifacts/task_breakdown/iter1-gauge.md`: (a) Caught real defect. The Gauge identified a missing requirement (AC-6) that the Forge had overlooked.
    - `artifacts/implementation/task9-iter1-gauge.md`: (a) Caught real defect. The Gauge found remaining references to deleted files that would have caused runtime crashes.
- **Constitution gaps**: None. The constitution correctly governed the migration, and the Phase 1 amendment (Task 2) was essential for setting the Emacs 29.1 baseline.
