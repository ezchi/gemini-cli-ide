# Task 3: Create gemini-cli-ide-tools.el — Forge Iteration 1

## Files Changed
- `gemini-cli-ide-tools.el` — created. Registers the surviving
  Gemini-specific MCP tool (`gemini-cli-ide-mcp-get-terminal-input`)
  into the external `emacs-mcp` registry at file load time.

## Key Implementation Decisions
- **No `(require 'gemini-cli-ide)` in the new file.** The tool body
  calls `gemini-cli-ide--get-terminal-input` and reads
  `gemini-cli-ide-buffer-name-function`. Both are defined in
  `gemini-cli-ide.el`. Adding a require would create a circular
  load (`gemini-cli-ide` → `gemini-cli-ide-tools` →
  `gemini-cli-ide`). Solved with `declare-function` /
  `defvar` forward declarations; at runtime, anyone using the tool
  has loaded `gemini-cli-ide` which in turn `require`d this file,
  so the symbols are bound.
- **Session-context lookup goes through emacs-mcp's protocol-layer
  internals.** Used the dynamic var `emacs-mcp--current-session-id`
  plus `emacs-mcp--session-get` and the public-ish accessor
  `emacs-mcp-session-project-dir`. These are documented in
  `~/Projects/emacs-mcp/README.org` §"Adding Custom Tools" as the
  recommended way for a tool handler to know which session it is
  servicing. The session-id var has a `--` prefix upstream, so
  this couples us to a private symbol name; the README treats this
  as a stable extension point, but if upstream changes it we will
  need to update.
- **Helper `gemini-cli-ide-tools--current-project-dir`** factors out
  the session→project-dir derivation so the tool body stays
  readable.
- **Tool registered via `emacs-mcp-deftool` macro** with the exact
  MCP-facing name `gemini-cli-ide-mcp-get-terminal-input` (preserved
  per FR-12) and the exact description text from the deleted
  `gemini-cli-ide-emacs-tools.el:369`.

## Deviations from Plan
- The plan's outline showed the `defun gemini-cli-ide--get-terminal-input`
  body being moved INTO `gemini-cli-ide-tools.el`. On inspection of
  the actual codebase, that helper is already in `gemini-cli-ide.el`
  (line 1227) — not in the deleted `gemini-cli-ide-emacs-tools.el`.
  So nothing needed to be moved across files; the new file just
  references it via `declare-function`. The plan's wording about
  "moved verbatim" was over-stated for this codebase; the actual
  effect is a new tool registration that calls into existing
  helpers. Plan §2.2's intent is preserved.

## Tests Added
- None in this task; Task 12 owns the new ERT tests including
  `gemini-cli-ide-test-tools-terminal-input-registered`.

## Build verification
```
emacs -batch -L . -L ~/Projects/emacs-mcp --eval \
  "(setq byte-compile-warnings '(not free-vars unresolved))" \
  -f batch-byte-compile gemini-cli-ide-tools.el
```
- Output: zero warnings, zero errors. The single informational
  message about emacs-mcp's own stale `.elc` is upstream noise,
  not a defect of this file.
