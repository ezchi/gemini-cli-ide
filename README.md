# gemini-cli-ide.el --- Gemini CLI integration for Emacs

Gemini CLI integration for Emacs provides seamless integration with Gemini CLI through the Model Context Protocol (MCP). It supports file operations, diagnostics, and editor state management.

This package leverages the `emacs-mcp` dependency to provide a standardized MCP server, enabling real-time communication between Emacs and Gemini. It manages the server lifecycle, automatically configures project-local settings, and registers Gemini-specific tools.

**Note:** This project is a shameless copy of [claude-code-ide.el](https://github.com/ezchi/claude-code-ide.el).

## Features

- **Automatic IDE Mode**: Activates IDE integration automatically when starting Gemini.
- **Standardized MCP**: Uses `emacs-mcp` for robust, bidirectional communication via HTTP/SSE.
- **Project-Local Settings**: Automatically manages `.gemini/settings.json` for project sessions.
- **Reference-Counted Server**: Manages the underlying MCP server process automatically across multiple sessions.
- **Rich Prompt Buffer**: Compose complex prompts in a full Emacs buffer with `@` mention completion.
- **Multi-Backend Terminal**: Supports both `vterm` and `eat` terminal emulators.

## Requirements

- **Emacs 29.1** or later.
- [Gemini CLI](https://github.com/google/gemini-cli) (v0.3.0+ recommended).
- [emacs-mcp](https://github.com/ezchi/emacs-mcp) package.
- `transient` package (v0.9.0+).

## Installation

### Using `straight.el` (Recommended)

```elisp
(use-package gemini-cli-ide
  :straight (gemini-cli-ide :type git :host github :repo "ezchi/gemini-cli-ide.el")
  :after emacs-mcp)
```

Note: You must also have `emacs-mcp` installed. If using `straight.el`, you can add it as a dependency or install it separately:

```elisp
(use-package emacs-mcp
  :straight (emacs-mcp :type git :host github :repo "ezchi/emacs-mcp"))
```

Note: `emacs-mcp` is not yet on MELPA; submission is tracked upstream as future work. Until then, install from git as shown above.

### Manual Installation

Clone both repositories and add them to your `load-path`:

```bash
git clone https://github.com/ezchi/emacs-mcp.git
git clone https://github.com/ezchi/gemini-cli-ide.el.git
```

```elisp
(add-to-list 'load-path "/path/to/emacs-mcp")
(add-to-list 'load-path "/path/to/gemini-cli-ide.el")
(require 'gemini-cli-ide)
```

## Usage

- `M-x gemini-cli-ide` - Start Gemini CLI for current project.
- `M-x gemini-cli-ide-continue` - Continue most recent conversation in directory.
- `M-x gemini-cli-ide-resume` - Resume Gemini CLI with previous conversation.
- `M-x gemini-cli-ide-stop` - Stop Gemini CLI for current project.
- `M-x gemini-cli-ide-switch-to-buffer` - Switch to project's Gemini buffer.
- `M-x gemini-cli-ide-list-sessions` - List and switch between all sessions.
- `M-x gemini-cli-ide-check-status` - Check CLI availability and MCP server status.
- `M-x gemini-cli-ide-insert-at-mentioned` - Send selected text to Gemini.

## Breaking Changes (v0.3.0)

The v0.3.0 release is a major architectural overhaul. Please note the following breaking changes:

- **Emacs Requirement**: Minimum version raised from 28.1 to **29.1**.
- **External Dependency**: The bundled WebSocket and HTTP MCP server has been replaced by the [emacs-mcp](https://github.com/ezchi/emacs-mcp) package. `websocket.el` and `web-server.el` are no longer dependencies.
- **MCP Tool Renames**: The Gemini-only wrappers around generic editor functionality were duplicates of `emacs-mcp`'s built-ins; they have been removed and Gemini now calls the upstream tool names directly. Mapping (old → new):

  | Removed (gemini-cli-ide v0.2)               | Replaced by (emacs-mcp v0.1)            |
  |---------------------------------------------|-----------------------------------------|
  | `gemini-cli-ide-mcp-xref-find-references`   | `xref-find-references`                  |
  | `gemini-cli-ide-mcp-xref-find-apropos`      | `xref-find-apropos`                     |
  | `gemini-cli-ide-mcp-project-info`           | `project-info`                          |
  | `gemini-cli-ide-mcp-imenu-list-symbols`     | `imenu-symbols`                         |
  | `gemini-cli-ide-mcp-treesit-info`           | `treesit-info` (see regression below)   |

  The Gemini-specific `gemini-cli-ide-mcp-get-terminal-input` tool is preserved verbatim.

- **Regressions**:
  - The `gemini-cli-ide-mcp-treesit-info` Gemini-only extension parameters (`whole_file`, `include_ancestors`, `include_children`) are dropped — `emacs-mcp`'s `treesit-info` does not support them. Tracked upstream.
  - Real-time selection / active-editor push notifications to the Gemini CLI are dropped this release. `emacs-mcp` does not yet expose a public push-notification API; tracked upstream.

- **Settings Write**: `gemini-cli-ide` now writes the running `emacs-mcp` endpoint URL into `<project>/.gemini/settings.json` (project-local only — never to the global `~/.gemini/settings.json`). Add `.gemini/settings.json` to your `.gitignore` if you don't want the URL committed.

- **Deprecation**: `gemini-cli-ide-emacs-tools-setup` is now a one-time-warn no-op shim. It will be removed in v0.4.0. Tool registration happens automatically when `gemini-cli-ide` is loaded.

## License

This package is licensed under **GPL-3.0-or-later**.

**Note regarding combined works:** When this package is distributed or used together with `emacs-mcp`, the resulting combined work is governed by the **AGPL-3.0-or-later** license, including its section 13 obligations regarding network interaction.
