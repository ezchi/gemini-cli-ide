# Gauge Review — Task 6 Iteration 1

(Provider: gemini)

## Summary
Task 6 has been implemented correctly. The six bundled MCP/diagnostics/emacs-tools files have been hard-deleted, and the surviving `gemini-cli-ide.el` require block matches the specification exactly. The minimal edits to `gemini-cli-ide-tests.el` successfully maintain byte-compilation green while leaving the test bodies for Task 11 cleanup.

## Issues

### NOTE
- `gemini-cli-ide-transient.el` retains legacy `declare-function` and `gemini-cli-ide-mcp` symbol references (e.g., in `gemini-cli-ide--has-active-session-p`). These will void at runtime, but as noted by the Forge agent, this is intentional until Task 10 rewires them.
- `gemini-cli-ide-tools.el` includes explicit requires for `emacs-mcp-tools` and `emacs-mcp-session` which were not explicitly in the Task 3 instruction but are logically necessary for the implementation of the terminal-input tool.

VERDICT: APPROVE
