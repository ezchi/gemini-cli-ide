# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.3.0] - 2026-05-01

This release is a major architectural overhaul, transitioning from a built-in MCP server to the external `emacs-mcp` library.

### Added
- Standardized MCP integration via the `emacs-mcp` package (replaces built-in WebSocket server).
- Reference-counted server lifecycle management: the MCP server starts on the first session and stops when the last session is closed (if owned by this package).
- Automatic project-local settings management: `gemini-cli-ide` now writes connection URLs directly to `<projectRoot>/.gemini/settings.json`.
- New `gemini-cli-ide-tools.el` module for registering Gemini-specific MCP tools.
- New ERT test suite for verifying `emacs-mcp` integration, settings writing, and refcounting.

### Changed
- **Minimum Emacs version raised to 29.1** (required by `emacs-mcp`).
- Switched from WebSocket/JSON-RPC over raw sockets to HTTP/SSE via `emacs-mcp`.
- **Breaking**: Renamed internal MCP tools to follow `emacs-mcp` naming conventions.
- Updated `gemini-cli-ide-check-status` to report `emacs-mcp` connection info.
- Updated `gemini-cli-ide-list-sessions` and `gemini-cli-ide-switch-to-buffer` to reflect the new server model.

### Deprecated
- `gemini-cli-ide-emacs-tools-setup`: This function is now a no-op shim and will be removed in v0.4.0. Tool registration is now handled automatically.

### Removed
- `gemini-cli-ide-mcp.el`: built-in MCP server (JSON-RPC dispatcher, sessions, push notifications).
- `gemini-cli-ide-mcp-server.el`: WebSocket server lifecycle.
- `gemini-cli-ide-mcp-http-server.el`: HTTP transport.
- `gemini-cli-ide-mcp-handlers.el`: bundled tool handlers.
- `gemini-cli-ide-diagnostics.el`: flycheck/flymake → VSCode-format JSON converter (its sole consumer was the bundled MCP layer).
- `gemini-cli-ide-emacs-tools.el`: legacy tool wrappers (xref, project-info, imenu, treesit). Their replacements are `emacs-mcp`'s built-ins.
- `websocket` and `web-server` runtime dependencies.

### Renamed (MCP tool names)

The Gemini-only wrappers around generic editor functionality were duplicates of `emacs-mcp`'s built-ins; they have been removed and Gemini now calls the upstream tool names directly.

| Removed (gemini-cli-ide v0.2)             | Replaced by (emacs-mcp v0.1) |
|-------------------------------------------|------------------------------|
| `gemini-cli-ide-mcp-xref-find-references` | `xref-find-references`       |
| `gemini-cli-ide-mcp-xref-find-apropos`    | `xref-find-apropos`          |
| `gemini-cli-ide-mcp-project-info`         | `project-info`               |
| `gemini-cli-ide-mcp-imenu-list-symbols`   | `imenu-symbols`              |
| `gemini-cli-ide-mcp-treesit-info`         | `treesit-info` (see below)   |

The Gemini-specific `gemini-cli-ide-mcp-get-terminal-input` tool name is preserved verbatim.

### Regressions
- The `gemini-cli-ide-mcp-treesit-info` Gemini-only extension parameters (`whole_file`, `include_ancestors`, `include_children`) are dropped — `emacs-mcp`'s `treesit-info` does not support them. Tracked upstream.
- Real-time selection / active-editor push notifications to the connected Gemini CLI are dropped this release. `emacs-mcp` does not yet expose a public push-notification API; tracked upstream and will return in a later release.

### License
- Source files in this repository remain `GPL-3.0-or-later`. When this package is distributed or linked together with `emacs-mcp` (which is `AGPL-3.0-or-later`), the resulting combined work is governed by `AGPL-3.0-or-later`, including its section 13 network-interaction obligations. The same notice is in the `gemini-cli-ide.el` Commentary block and in the `README.md` License section.

### Tested against
- `emacs-mcp` git SHA `6c8561646b6cf0ce3ef36e4ebc4fd886068e9bfb` from `https://github.com/ezchi/emacs-mcp.git`.
- Commit subject: `fix(tools): use hash table for schema properties to ensure string keys`.
