# Gauge Code Review — Task 12 Iteration 1

## Task
Add new ERT coverage for `emacs-mcp` integration.

## Verification Criteria
1. Nine new tests added as per plan §7.2.
2. Tests cover: settings writer (3 cases), dependency guards (2 cases), refcounting (2 cases), tool registration (1 case), and deprecation shim (1 case).
3. Byte-compile and ERT tests are green.

## Git Diff
```diff
$(git diff HEAD~1)
```

## Full File Content
### gemini-cli-ide-tests.el
$(cat gemini-cli-ide-tests.el)

## Verification
- Run `./scripts/compile-and-test.sh`.

## Output
Standard review format. End with VERDICT.
