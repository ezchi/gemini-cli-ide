## Project Overview

This project, `gemini-cli-ide`, is an Emacs package that provides deep integration with the Gemini CLI. It aims to create a seamless IDE-like experience for developers using Gemini within Emacs.

The core of the project is written in Emacs Lisp. It establishes a WebSocket server to facilitate real-time, bidirectional communication with the Gemini CLI through a custom Model Context Protocol (MCP). This allows Gemini to interact with the Emacs environment, enabling features like file operations, code diagnostics, and project navigation directly from the AI assistant.

**Key Technologies:**

*   **Emacs Lisp:** The primary language for the Emacs package.
*   **WebSocket:** Used for real-time communication between Emacs and the Gemini CLI.
*   **Transient:** The `transient` package is used for providing a user-friendly command interface.

**Architecture:**

The package is structured into several modules:

*   `gemini-cli-ide.el`: The main file, responsible for user-facing commands, session management, and terminal interaction.
*   `gemini-cli-ide-mcp.el`: Defines the Model Context Protocol (MCP) used for communication.
*   `gemini-cli-ide-mcp-server.el`: Implements the MCP WebSocket server within Emacs.
*   `gemini-cli-ide-mcp-handlers.el`: Contains the handlers for various MCP messages.
*   `gemini-cli-ide-emacs-tools.el`: Implements the custom Emacs-specific tools that can be called by Gemini.
*   `gemini-cli-ide-tests.el`: Contains the test suite for the package.

## Building and Running

The project includes shell scripts for common development tasks.

**To compile and run tests:**

```bash
./scripts/compile-and-test.sh
```

This script performs the following steps:
1.  Byte-compiles all `.el` files.
2.  Optionally, native-compiles the files if `--with-native-compile` is passed.
3.  Runs the test suite using `ert`.
4.  Cleans up the compiled `.elc` and `.eln` files.

**To format the code:**

```bash
./scripts/format-and-clean.sh *.el
```

This script formats the Emacs Lisp files according to the project's conventions and removes trailing whitespace.

## Development Conventions

*   **Code Formatting:** The project uses a standard Emacs Lisp indentation style, enforced by the `scripts/format-and-clean.sh` script.
*   **Testing:** Tests are written using the `ert` testing framework and are located in `gemini-cli-ide-tests.el`. All code should be tested.
*   **Commit Hooks:** The project appears to be set up to use the `compile-and-test.sh` and `format-and-clean.sh` scripts in commit hooks to ensure code quality and consistency.
