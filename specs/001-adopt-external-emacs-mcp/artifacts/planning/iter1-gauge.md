# Gauge Review — Planning Iteration 1

(Provider: gemini)

## Summary
The implementation plan for `001-adopt-external-emacs-mcp` is a comprehensive and well-structured roadmap for rebasing the package on the external `emacs-mcp` library. It successfully addresses the complex transition from a bundled, project-per-server model to a shared-server, multi-session architecture while maintaining project isolation and constitutional integrity. The phased approach is logical and prioritizes stability, though some minor refinements in the refcount logic and testing strategy are recommended.

## Spec Coverage
The plan demonstrates excellent coverage of the requirements defined in `spec.md` and `clarifications.md`.

| ID | Requirement | Covered? | Note |
|:---|:---|:---:|:---|
| FR-1 | Code removal (5 files + diagnostics) | Yes | Targeted for Phase 3. |
| FR-2 | New `gemini-cli-ide-tools.el` | Yes | Defined in §2.2 and Phase 2. |
| FR-5 | Dependency / Version floor updates | Yes | Updated in `gemini-cli-ide.el` header and constitution. |
| FR-8 | Multi-session project routing | Yes | Uses `initialize.projectDir` and `setProjectDir` via `emacs-mcp`. |
| FR-9 | Server lifecycle / Refcount | Yes | Detailed in §2.6 and Phase 4. |
| FR-10 | Discovery via `.gemini/settings.json` | Yes | Detailed in §3.1. |
| FR-13 | Deprecation shim | Yes | Included in §2.1 and Phase 4. |
| FR-14 | Drop push notifications | Yes | Acknowledged in §1; listed in breaking changes. |
| AC-10 | Multi-project concurrency | Yes | Explicitly mapped in §7.4. |
| FR-23/24| Constitutional amendments | Yes | Detailed in Phase 1. |

**Observation:** The plan addresses the "Silent Failure" requirement (NFR-7) through the new `--require-emacs-mcp` helper, which is a robust way to gate all interactive entry points.

## Architecture Soundness
The proposed architecture is sound and strictly follows the clarified session model.

- **Refcount Model:** The state model in §2.6 (`gemini-cli-ide--owns-mcp-server` buffer-local + `gemini-cli-ide--mcp-server-owner-count` global) correctly implements the requirement in FR-9. By checking `(emacs-mcp-connection-info)` before starting, it ensures we only "own" (and thus only stop) servers we actually launched.
- **Atomic Writes:** The contract for `.gemini/settings.json` in §3.1 correctly specifies atomic writes and deep merging, preventing corruption of user-managed configuration.
- **Edge Cases:**
    - **Multiple project sessions:** The plan correctly identifies that `emacs-mcp` handles multiple sessions on one server.
    - **Cleanup:** By wiring `--release-mcp-server` into `kill-buffer-hook` and `gemini-cli-ide--cleanup-on-exit`, the plan ensures the refcount stays synchronized even if the user kills the buffer directly.

## Simplicity
The plan is highly focused on the spec scope. It avoids unnecessary refactoring of the terminal backends or UI components, focusing strictly on the MCP rebase. The deletion of ~3,200 lines of code is a significant simplification of the project's maintenance surface.

## Risk Assessment
The §8 risks are realistic and technical.
- **Upstream drift:** The plan mitigates this by pinning the SHA in the CHANGELOG and adding integration-style tests.
- **Malformed JSON:** The plan correctly chooses to signal a `user-error` rather than blindly overwriting or ignoring a broken settings file.
- **Refcount Drift:** The mitigation (hooking into buffer-kill) is standard for Emacs and effective.

## Testing Strategy
The testing strategy in §7 is strong, but could be slightly more rigorous for the version floor:
- **AC-6 coverage:** The new ERT `gemini-cli-ide-test-require-emacs-mcp-old-emacs` should ensure that the user-error explicitly mentions the requirement for Emacs 29.1+ to prevent confusion.
- **Mocking:** The plan smartly uses mocks for `emacs-version` and `featurep` to test error paths without needing a multi-version environment.
- **Parity:** The plan retains ~35 relevant tests, ensuring no regression in CLI detection or terminal handling.

## Constitutional Alignment
- **Principle 3 (Project-scoped sessions):** Preserved through the multi-session routing logic.
- **Principle 5 (No network calls):** Upheld; `emacs-mcp` uses localhost HTTP, and the package itself makes no network requests.
- **Principle 7 (No path leakage):** The plan avoids adding logging of full paths or buffer contents.
- **Constraint (Compatibility):** The floor is raised to 29.1 as required by the dependency.
- **Constraint (Performance):** The plan uses async process management and avoids synchronous loops.

## Issues

### NOTE
- **Refcount Reset:** Ensure `gemini-cli-ide--mcp-server-owner-count` is reset to 0 if `emacs-mcp-stop` is called externally or fails, though `gemini-cli-ide--release-mcp-server`'s `(max 0 ...)` guard provides a reasonable safety net.
- **Test Discovery:** In Phase 5, the mechanical deletion of tests might leave some orphaned helper functions in `gemini-cli-ide-tests.el`. A quick manual pass to prune unused `defun`s in the test file is recommended.

VERDICT: APPROVE
