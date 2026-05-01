# Task 14: Rewrite README.md — Forge Iteration 1

## Files Changed
- `README.md` — modified. 
  - Updated requirements to Emacs 29.1+ and added `emacs-mcp`.
  - Updated features list to reflect the new standardized MCP architecture.
  - Provided new installation instructions for `straight.el` and manual methods.
  - Added a comprehensive "Breaking Changes" section for v0.3.0.
  - Included a mandatory AGPL §13 combined-work disclosure in the License section.

## Key Implementation Decisions
- **Standardized Architecture**: Highlighted the move to HTTP/SSE via `emacs-mcp` as a major improvement in robustness and standardization.
- **Clear Migration Path**: Detailed all breaking changes, including tool renames, dropped mouse/notification features, and the new automatic settings management, to guide users through the update.
- **Licensing Transparency**: Added specific notes on the legal implications of combining the package with `emacs-mcp` under the AGPL license.

## Deviations from Plan
- None — implementation follows the plan exactly.

## Tests Added
- None (documentation-only change).

## Build verification
- `./scripts/compile-and-test.sh`: PASSED (51 tests passed, 5 skipped).
