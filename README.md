# gemini-cli-ide.el --- Gemini CLI integration for Emacs

Gemini CLI integration for Emacs provides seamless integration with Gemini CLI through the Model Context Protocol (MCP). It supports file operations, diagnostics, and editor state management.

This package starts a WebSocket server that Gemini CLI connects to, enabling real-time communication between Emacs and Gemini. It supports multiple concurrent sessions per project.

**Note:** This project is a shameless copy of [claude-code-ide.el](https://github.com/ezchi/claude-code-ide.el).

## Features

- Automatic IDE mode activation when starting Gemini
- MCP WebSocket server for bidirectional communication
- Project-aware sessions with automatic working directory detection
- Clean session management with automatic cleanup on exit
- Selection and buffer state tracking
- Tool support for file operations, diagnostics, and more
- Emacs MCP tools for xref and project navigation

## Requirements

- Emacs 28.1 or later
- [Gemini CLI](https://github.com/google/gemini-cli)
- `websocket` package
- `transient` package
- `web-server` package

## Installation

### Using `use-package` with `straight.el` or `elpaca`

```elisp
(use-package gemini-cli-ide
  :straight (gemini-cli-ide :type git :host github :repo "ezchi/gemini-cli-ide.el")
  :config
  ;; Optional: Enable Emacs tools for Gemini (xref, project navigation, etc.)
  (gemini-cli-ide-emacs-tools-setup))
```

## Usage

- `M-x gemini-cli-ide` - Start Gemini CLI for current project
- `M-x gemini-cli-ide-continue` - Continue most recent conversation in directory
- `M-x gemini-cli-ide-resume` - Resume Gemini CLI with previous conversation
- `M-x gemini-cli-ide-stop` - Stop Gemini CLI for current project
- `M-x gemini-cli-ide-switch-to-buffer` - Switch to project's Gemini buffer
- `M-x gemini-cli-ide-list-sessions` - List and switch between all sessions
- `M-x gemini-cli-ide-check-status` - Check CLI availability and version
- `M-x gemini-cli-ide-insert-at-mentioned` - Send selected text to Gemini

## Prompt Buffer

The prompt buffer provides a full Emacs buffer for composing complex prompts with the following features:

- **Rich Editing:** Use all your favorite Emacs editing commands to draft your prompt.
- **@ Mentions:** Type `@` to trigger fuzzy completion for files and project symbols.
- **Multi-line Support:** Easily compose long, multi-line instructions.

### Keybindings

| Key | Action |
|-----|--------|
| `C-c '` | Open prompt buffer (from Gemini terminal) |
| `C-c C-c` | Apply prompt and send to Gemini |
| `C-c C-k` | Cancel and close prompt buffer |

## Vterm Prompt Tracking Setup (Recommended)

To ensure the most reliable terminal interaction (especially for `C-c '` and allowing Gemini to see what you are currently typing), it is highly recommended to enable **vterm's native prompt tracking**.

1. **Enable in Emacs:**
   ```elisp
   (setq vterm-use-vterm-prompt-detection-method t)
   ```

2. **Configure your shell:**
   Add the following to your shell configuration file:

   **Bash (~/.bashrc):**
   ```bash
   vterm_printf(){
       if [ -n "$TMUX" ] && { [ "${TERM%%-*}" = "tmux" ] || [ "${TERM%%-*}" = "screen" ]; }; then
           printf "\ePtmux;\e\e]%s\007\e\\" "$1"
       elif [ "${TERM%%-*}" = "screen" ]; then
           printf "\eP\e]%s\007\e\\" "$1"
       else
           printf "\e]%s\e\\" "$1"
       fi
   }
   vterm_prompt_end(){
       vterm_printf "51;A$(whoami)@$(hostname):$(pwd)"
   }
   PS1=$PS1'\[$(vterm_prompt_end)\]'
   ```

   **Zsh (~/.zshrc):**
   ```zsh
   vterm_prompt_end() {
       vterm_printf "51;A$(whoami)@$(hostname):$(pwd)"
   }
   setopt PROMPT_SUBST
   PROMPT=$PROMPT'%{$(vterm_prompt_end)%}'
   ```

## License

GPL-3.0-or-later
