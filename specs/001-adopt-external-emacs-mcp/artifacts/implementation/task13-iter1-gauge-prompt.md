# Gauge Code Review — Task 13 Iteration 1

## Task
Rewrite `gemini-cli-ide.el` Commentary block.

## Verification Criteria
1. WebSocket and built-in MCP server claims are removed.
2. `emacs-mcp` is described as the providing dependency.
3. Features list is updated to reflect the new architecture.
4. Outdated `emacs-tools-setup` instructions are gone.

## Git Diff
```diff
$(git diff HEAD~1)
```

## Full File Content
### gemini-cli-ide.el
$(cat gemini-cli-ide.el | head -n 100)

## Verification
- Run `./scripts/compile-and-test.sh`.

## Output
Standard review format. End with VERDICT.
