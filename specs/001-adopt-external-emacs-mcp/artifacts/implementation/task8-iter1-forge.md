# Task 8: Implement --write-gemini-settings JSON merge writer — Forge Iteration 1

## Files Changed
- `gemini-cli-ide.el` — modified (two new private helpers added
  immediately after `--release-mcp-server`).

## Key Implementation Decisions
- **`--allowed-tools-filter`** — small helper that translates the
  `gemini-cli-ide-mcp-allowed-tools` defcustom into the Gemini
  `mcpServers.emacs.tools` JSON shape.
  - `'auto`   → return `nil` (no `tools` key written; Gemini sees
    everything).
  - `nil`     → return `[]` (advertise nothing — testing only).
  - string    → return `[that-string]`.
  - list      → return that list as a `vector` (so `json-serialize`
    encodes it as a JSON array, not a JSON object).
  Encapsulated separately from `--write-gemini-settings` so Task 12
  can unit-test the filter logic in isolation.
- **`--write-gemini-settings`** uses Emacs 29's native
  `json-parse-buffer` / `json-serialize` (we no longer need the
  legacy `json.el` / `json-encode` because the constitution floor
  is now 29.1).
  - `:object-type 'hash-table` preserves insertion order across
    parse/serialize on Emacs 29+.
  - `:null-object :null`, `:false-object :false` are explicit so
    that a serialized output matches what Gemini CLI expects (no
    empty string surrogates).
- **Atomic write** — `make-temp-file` with the target directory as
  the parent so the subsequent `rename-file` is on the same
  filesystem and is therefore atomic on POSIX. The temp prefix
  starts with `.` so a partial file isn't picked up by anything
  scanning the directory.
- **Malformed-file refusal** — `condition-case` around
  `json-parse-buffer`; on error we signal a `user-error` naming
  the file. We do NOT overwrite the file. This is a real user-data
  protection requirement from plan §3.1.
- **Server-not-running guard** — `(or (emacs-mcp-connection-info)
  (user-error ...))` at the top of the function. The plan and FR-10
  both presume the server is up before this is called; this is
  defense in depth so a programming mistake in Task 9 surfaces
  cleanly.
- **Deep merge preserves user fields** — top-level keys other than
  `mcpServers` are untouched; entries under `mcpServers` other than
  `emacs` are untouched; fields on `mcpServers.emacs` other than
  `url` and `tools` are untouched. The `tools` key is `remhash`-ed
  when the filter is `nil` so an existing `tools` filter from a
  previous `gemini-cli-ide-mcp-allowed-tools` setting is removed
  when the user switches back to `'auto`.

## Deviations from Plan
- Plan §3.1 said "Encode with json-encode (2-space indent if
  available)". On Emacs 29, the canonical API is `json-serialize`
  which does not pretty-print. The output is single-line minimal
  JSON. This matches what Gemini CLI itself writes when it manages
  the file, and the file remains hand-editable. If a user wants
  pretty-printed output they can format the file separately —
  acceptable trade-off for using the standard library.
- Plan called the helper part of "Task 8" and the lifecycle wiring
  "Task 9". This task lands the helper only; no callers wired yet.

## Tests Added
- None in this task; Task 12 owns
  `gemini-cli-ide-test-write-settings-creates-file`,
  `gemini-cli-ide-test-write-settings-merges-existing`,
  `gemini-cli-ide-test-write-settings-rejects-malformed`.

## Build verification
- `./scripts/compile-and-test.sh` byte-compile: PASSED.
- ERT: still red (Task 11 fixes).
