# Gauge Code Review — Task 8 Iteration 1

## Task
Implement `gemini-cli-ide--write-gemini-settings` and the helper
`gemini-cli-ide--allowed-tools-filter` per FR-10 / plan §3.1. Helper
only — no callers wired yet.

## Inputs to read
1. `gemini-cli-ide.el` — read the two new helpers added immediately
   after `--release-mcp-server`.
2. Forge artifact:
   `specs/001-adopt-external-emacs-mcp/artifacts/implementation/task8-iter1-forge.md`
3. Spec FR-10. Plan §3.1 (JSON write contract).

## Verify

1. **Atomic write contract.**
   - Temp file is created in the same directory as the target
     (so `rename-file` is a same-filesystem atomic move).
   - Temp prefix starts with `.` (so it's hidden / not picked up
     by other tools mid-write).

2. **Malformed-file safety.**
   - When the existing file fails to parse, we signal `user-error`
     and DO NOT overwrite the file. Verify the `condition-case`
     wraps `json-parse-buffer` and re-raises as `user-error` with
     a clear message naming the file.

3. **Merge correctness.**
   - Top-level keys other than `mcpServers` are preserved.
   - Entries under `mcpServers` other than `emacs` are preserved.
   - On `mcpServers.emacs`, only `url` and (optionally) `tools`
     are touched; other fields are preserved.

4. **Tools filter logic** (in `--allowed-tools-filter`):
   - `'auto`  → nil (no `tools` key written).
   - `nil`    → empty vector (`tools: []` written).
   - string   → one-element vector containing that string.
   - list     → vector of those strings.
   - When the helper returns nil, the writer uses `remhash` so a
     pre-existing `tools` key is removed (handle the case where a
     user switches the defcustom from a list back to `'auto`).

5. **Emacs 29 native APIs.**
   - Uses `json-parse-buffer` / `json-serialize` (29.1+), not the
     legacy `json.el`. This is allowed because the constitution
     floor is now 29.1 (per Task 2).
   - `:null-object :null` and `:false-object :false` are passed
     for round-trip stability.

6. **Server-running guard.**
   - The function signals a `user-error` if
     `(emacs-mcp-connection-info)` returns nil — defense in depth
     against a Task 9 wiring mistake.

7. **No regression in byte-compile.**
   - The forge artifact reports byte-compile PASSED.
   - Visually scan for unresolved symbols: `json-parse-buffer`,
     `json-serialize`, `make-temp-file`, `rename-file`,
     `make-directory`, `puthash`, `gethash`, `remhash` — all
     standard, no `(require ...)` needed.

8. **No scope creep.**
   - This task adds helpers; it does NOT call them from
     `--start-session` or any interactive command. Confirm by
     scanning for `(gemini-cli-ide--write-gemini-settings ` —
     should appear only in the helper's own definition.

## Output

```markdown
# Gauge Review — Task 8 Iteration 1

## Summary

## Issues
### BLOCKING / WARNING / NOTE

VERDICT: APPROVE
```
or REVISE.

## Hard rules
- Be strict on the malformed-file path and the atomic-write
  semantics — these protect user data.
- End with exactly `VERDICT: APPROVE` or `VERDICT: REVISE`.
- Do NOT modify any files.
