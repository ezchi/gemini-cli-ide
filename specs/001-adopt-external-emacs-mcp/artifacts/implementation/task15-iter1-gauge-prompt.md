# Gauge Code Review — Task 15 Iteration 1

## Task
Create `CHANGELOG.md`.

## Verification Criteria
1. `CHANGELOG.md` created with v0.3.0 entry.
2. Dropped deps (websocket, web-server) mentioned.
3. Raised floor (Emacs 29.1) mentioned.
4. License disclosure (AGPL §13) mentioned.
5. MCP tool rename mapping implied or mentioned.
6. Dropped features (treesit-info params, push notifications) listed.
7. Deprecation timeline for `emacs-tools-setup` stated.
8. Tested git SHA of `emacs-mcp` recorded.

## Git Diff
```diff
$(git diff HEAD~1)
```

## Full File Content
### CHANGELOG.md
$(cat CHANGELOG.md)

## Verification
- Run `./scripts/compile-and-test.sh`.

## Output
Standard review format. End with VERDICT.
