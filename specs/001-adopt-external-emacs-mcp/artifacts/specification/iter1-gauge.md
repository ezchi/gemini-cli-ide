# Gauge Review — Iteration 1

(Provider: gemini)

## Summary
The specification is comprehensive and correctly identifies the major architectural shifts and risks involved in adopting the external `emacs-mcp` library. It covers all necessary dimensions (code removal, dependency updates, server lifecycle, etc.) and explicitly flags the most critical issues as "Open Questions" for the clarification stage. However, it proposes a change to the Emacs version floor which contradicts the current Constitution and identifies a potential regression in multi-project support that must be reconciled with existing principles.

## Issues

### BLOCKING

1. **Constitutional Conflict: Emacs Floor (FR-5).** The spec proposes raising the Emacs floor to 29.1 to match `emacs-mcp`. The Constitution (**Constraints > Compatibility**) mandates 28.1 as the floor. This is a blocking conflict. If `emacs-mcp` strictly requires 29.1, the Constitution must be amended, or a fallback strategy for 28.1 must be defined.
2. **Principle Violation: Multi-project Concurrency (FR-15, OQ-5).** Constitution **Principle 3** requires that "Multiple concurrent sessions across projects must work without interference." If `emacs-mcp` uses a single-server-per-Emacs model that only supports one project root at a time, this is a significant architectural regression. The spec must define how to maintain the project-scoped session guarantee (e.g., via the switching logic mentioned in FR-15a) before it can be approved.

### WARNING

1. **License Compatibility (FR-16, OQ-1).** The combination of GPL-3.0 and AGPL-3.0-or-later is a high-risk area. While section 13 of the AGPL-3.0 allows for combination with GPL-3.0, the "combined work" redistributed to users would likely fall under AGPL-3.0. A firm decision on relicensing vs. accepting the combined-work license is needed.
2. **Endpoint Discovery (FR-10, OQ-2).** The chosen mechanism for Gemini CLI to discover the `emacs-mcp` endpoint is still open. This is critical for fulfilling US-3 ("Automatic discovery") and ensuring a seamless user experience.

### NOTE

1. **Redundancy of `gemini-cli-ide-diagnostics.el` (OQ-12).** Since `emacs-mcp` provides a built-in `get-diagnostics` tool (FR-11), `gemini-cli-ide-diagnostics.el` is likely redundant. It should be added to the deletion list (FR-1) unless it provides unique value (like specific Flycheck integration features) not present in `emacs-mcp`.
2. **CI Configuration (FR-19, OQ-11).** There is no evidence of a `.github` folder or other CI configuration in the current workspace. This requirement and its corresponding open question should be dropped or clarified to focus only on local `scripts/compile-and-test.sh` updates.

## Constitutional Alignment

- **Project Identity:** Aligned.
- **Governing Principles:**
    - **Principle 3 (Project-scoped sessions):** **POTENTIAL VIOLATION** (FR-15/OQ-5). Requires resolution to ensure concurrent project support.
    - **Principle 7 (No leak):** Aligned (NFR-6 specifically addresses this).
- **Technology Stack:** **VIOLATION** (FR-5 proposes 29.1; Constitution requires 28.1).
- **Constraints:**
    - **Compatibility:** **VIOLATION** regarding the Emacs version floor.
    - **Dependency hygiene:** Aligned (net reduction in hard dependencies).

## Feasibility Check

- **API Verification:** I was unable to verify the `emacs-mcp` API (Streamable HTTP, lockfile discovery, `emacs-mcp-deftool`) because `/Users/ezchi/Projects/emacs-mcp/` is outside the allowed workspace. However, the spec's assertions are consistent with the latest MCP standards.
- **`find_emacs_package` (FR-17):** Verified that `scripts/compile-and-test.sh` contains the `find_emacs_package` helper, making this requirement feasible.
- **Terminal Input (FR-12):** Verified that `gemini-cli-ide-emacs-tools.el` contains `gemini-cli-ide-mcp-get-terminal-input`, which is the primary candidate for re-registration.

VERDICT: REVISE
