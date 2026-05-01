;;; gemini-cli-ide.el --- Gemini CLI integration for Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Enze Chi
;; Version: 0.3.0
;; Package-Requires: ((emacs "29.1") (emacs-mcp "0.1.0") (transient "0.9.0"))
;; Keywords: ai, gemini, cli, assistant, mcp
;; URL: https://github.com/ezchi/gemini-cli-ide.el

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Gemini CLI IDE integration for Emacs provides seamless integration
;; with Gemini CLI through the Model Context Protocol (MCP).
;;
;; This package leverages the `emacs-mcp' dependency to provide a
;; standardized MCP server, enabling real-time communication between
;; Emacs and Gemini.  It manages the server lifecycle, automatically
;; configures project-local settings, and registers Gemini-specific
;; tools.
;;
;; License note: this file is GPL-3.0-or-later (see header above).
;; The hard dependency `emacs-mcp' is licensed under
;; AGPL-3.0-or-later.  When this package is distributed or used
;; together with `emacs-mcp', the resulting combined work is
;; governed by AGPL-3.0-or-later, including its section 13
;; obligations regarding network interaction.  See README.md for
;; the user-facing version of this notice.
;;
;; Features:
;; - Streamable HTTP MCP transport (provided by `emacs-mcp')
;; - Project-aware sessions with automatic working directory detection
;; - Project-local `.gemini/settings.json' written on session start so
;;   Gemini CLI auto-discovers the running `emacs-mcp' endpoint
;; - Refcounted server lifecycle: this package never stops a server
;;   that the user (or another package) started independently
;; - Gemini-specific MCP tool: terminal-input reader so Gemini can see
;;   what the user is typing in the Gemini terminal buffer before they
;;   press Enter
;;
;; Usage:
;; M-x gemini-cli-ide - Start Gemini CLI for current project
;; M-x gemini-cli-ide-continue - Continue most recent conversation in directory
;; M-x gemini-cli-ide-resume - Resume Gemini CLI with previous conversation
;; M-x gemini-cli-ide-stop - Stop Gemini CLI for current project
;; M-x gemini-cli-ide-switch-to-buffer - Switch to project's Gemini buffer
;; M-x gemini-cli-ide-list-sessions - List and switch between all sessions
;; M-x gemini-cli-ide-check-status - Check CLI and MCP server status
;; M-x gemini-cli-ide-insert-at-mentioned - Send selected text to Gemini

;;; Code:

(require 'cl-lib)
(require 'project)
(require 'emacs-mcp)
(require 'gemini-cli-ide-debug)
(require 'gemini-cli-ide-transient)
(require 'gemini-cli-ide-tools)

;; External variable declarations for with-editor
(defvar with-editor-show-usage)
(defvar with-editor-finish-query-functions)

;; External function declarations for with-editor
(declare-function with-editor-mode "with-editor" (&optional arg))
(declare-function with-editor-finish "with-editor" ())
(declare-function with-editor-cancel "with-editor" ())

;; External variable declarations
(defvar eat-terminal)
(defvar eat--synchronize-scroll-function)
(defvar vterm-shell)
(defvar vterm-environment)
(defvar eat-term-name)
(defvar vterm--process)

;; External function declarations for vterm
(declare-function vterm "vterm" (&optional arg))
(declare-function vterm-send-string "vterm" (string))
(declare-function vterm-send-escape "vterm" ())
(declare-function vterm-send-return "vterm" ())
(declare-function vterm-send-key "vterm" (key &optional shift meta ctrl))
(declare-function vterm-reset-cursor-point "vterm" ())
(declare-function vterm--get-cursor-point "vterm" ())
(declare-function vterm--get-prompt-point "vterm" ())
(declare-function vterm--window-adjust-process-window-size "vterm" (&optional frame))

;; External function declarations for eat
(declare-function eat-mode "eat" ())
(declare-function eat-exec "eat" (buffer name command startfile &rest switches))
(declare-function eat-term-end "eat" (terminal))
(declare-function eat-term-send-string "eat" (terminal string))
(declare-function eat-term-display-cursor "eat" (terminal))
(declare-function eat--adjust-process-window-size "eat" (process windows))

;;; Customization

(defgroup gemini-cli-ide nil
  "Gemini CLI integration for Emacs."
  :group 'tools
  :prefix "gemini-cli-ide-")

(defcustom gemini-cli-ide-cli-path "gemini"
  "Path to the Gemini CLI executable."
  :type 'string
  :group 'gemini-cli-ide)

(defcustom gemini-cli-ide-buffer-name-function #'gemini-cli-ide--default-buffer-name
  "Function to generate buffer names for Gemini CLI sessions.
The function is called with one argument, the working directory,
and should return a string to use as the buffer name."
  :type 'function
  :group 'gemini-cli-ide)

(defcustom gemini-cli-ide-cli-debug nil
  "When non-nil, launch Gemini CLI with the -d debug flag."
  :type 'boolean
  :group 'gemini-cli-ide)

(defcustom gemini-cli-ide-cli-extra-flags ""
  "Additional flags to pass to the Gemini CLI.
This should be a string of space-separated flags, e.g. \"--model gemini-2.5-pro\"."
  :type 'string
  :group 'gemini-cli-ide)

(defcustom gemini-cli-ide-system-prompt nil
  "System prompt to append to Gemini's default system prompt.
When non-nil, the --append-system-prompt flag will be added with this value.
Set to nil to disable (default)."
  :type '(choice (const :tag "Disabled" nil)
                 (string :tag "System prompt text"))
  :group 'gemini-cli-ide)

(defcustom gemini-cli-ide-mcp-allowed-tools 'auto
  "Filter for which `emacs-mcp' tools Gemini CLI is told to use.
Written into `mcpServers.emacs.tools' in the project-local
`.gemini/settings.json' on every Gemini session start (see
`gemini-cli-ide--write-gemini-settings').

Allowed values:
  `auto'           - Omit the `tools' filter entirely; Gemini CLI
                     sees every tool the running `emacs-mcp' server
                     advertises.  Recommended default.
  nil              - Write an empty array (`tools: []').  Gemini CLI
                     will see no tools.  Useful for testing.
  A string         - Write a single-element array containing that
                     string.  The string MUST be the exact MCP-facing
                     name of an `emacs-mcp' tool.
  A list of string - Write that list verbatim.  Each string MUST be
                     the exact MCP-facing name of an `emacs-mcp'
                     tool.

Note: changes to this variable take effect on the next Gemini
session start; sessions already running pick up the new filter
only after their `.gemini/settings.json' is rewritten (which
happens automatically the next time
`gemini-cli-ide--write-gemini-settings' runs)."
  :type '(choice (const :tag "Auto (every tool)" auto)
                 (const :tag "Disabled (no tools)" nil)
                 (string :tag "Single tool name")
                 (repeat :tag "Specific tool names" string))
  :group 'gemini-cli-ide)

(defcustom gemini-cli-ide-window-side 'right
  "Side of the frame where the Gemini CLI window should appear.
Can be `left', `right', `top', or `bottom'."
  :type '(choice (const :tag "Left" left)
                 (const :tag "Right" right)
                 (const :tag "Top" top)
                 (const :tag "Bottom" bottom))
  :group 'gemini-cli-ide)

(defcustom gemini-cli-ide-window-width 90
  "Width of the Gemini CLI side window when opened on left or right."
  :type 'integer
  :group 'gemini-cli-ide)

(defcustom gemini-cli-ide-window-height 20
  "Height of the Gemini CLI side window when opened on top or bottom."
  :type 'integer
  :group 'gemini-cli-ide)

(defcustom gemini-cli-ide-focus-on-open t
  "Whether to focus the Gemini CLI window when it opens."
  :type 'boolean
  :group 'gemini-cli-ide)

(defcustom gemini-cli-ide-focus-gemini-after-ediff t
  "Whether to focus the Gemini CLI window after opening ediff.
When non-nil (default), focus returns to the Gemini CLI window
after opening ediff.  When nil, focus remains on the ediff control
window, allowing direct interaction with the diff controls."
  :type 'boolean
  :group 'gemini-cli-ide)

(defcustom gemini-cli-ide-show-gemini-window-in-ediff t
  "Whether to show the Gemini CLI side window when viewing diffs.
When non-nil (default), the Gemini CLI side window is restored
after opening ediff.  When nil, the Gemini CLI window remains
hidden during diff viewing, giving you more screen space for the
diff comparison."
  :type 'boolean
  :group 'gemini-cli-ide)

(defcustom gemini-cli-ide-use-ide-diff t
  "Whether to use IDE diff viewer for file differences.
When non-nil (default), Gemini CLI will open an IDE diff viewer
(ediff) when showing file changes.  When nil, Gemini CLI will
display diffs in the terminal instead."
  :type 'boolean
  :group 'gemini-cli-ide)

(defcustom gemini-cli-ide-switch-tab-on-ediff t
  "Whether to switch back to the original tab after closing ediff.
When non-nil (default), Emacs will switch back to the tab where
Gemini was originally opened after the ediff session is finished."
  :type 'boolean
  :group 'gemini-cli-ide)

(defcustom gemini-cli-ide-use-side-window t
  "Whether to display Gemini CLI in a side window.
When non-nil (default), Gemini CLI opens in a dedicated side window
controlled by `gemini-cli-ide-window-side' and related settings.
When nil, Gemini CLI opens in a regular buffer that follows standard
display-buffer behavior."
  :type 'boolean
  :group 'gemini-cli-ide)

(defcustom gemini-cli-ide-terminal-backend 'vterm
  "Terminal backend to use for Gemini CLI sessions.
Can be either `vterm' or `eat'.  The vterm backend is the default
and provides a fully-featured terminal emulator.  The eat backend
is an alternative terminal emulator that may work better in some
environments."
  :type '(choice (const :tag "vterm" vterm)
                 (const :tag "eat" eat))
  :group 'gemini-cli-ide)

(defcustom gemini-cli-ide-prevent-reflow-glitch t
  "Workaround for Claude Code terminal scrolling bug #1422.
When non-nil (default), prevents the terminal from reflowing on height-only
changes which can trigger uncontrollable scrolling in Claude Code.
See: https://github.com/anthropics/claude-code/issues/1422
This setting should be removed once the upstream bug is fixed."
  :type 'boolean
  :group 'gemini-cli-ide)

(defcustom gemini-cli-ide-vterm-anti-flicker t
  "Enable intelligent flicker reduction for vterm display.
When enabled, this feature optimizes terminal rendering by detecting
and batching rapid update sequences.  This provides smoother visual
output during complex terminal operations such as expanding text areas
and rapid screen updates.

This optimization applies only to vterm and uses advanced pattern
matching to maintain responsiveness while improving visual quality."
  :type 'boolean
  :group 'gemini-cli-ide)

(defcustom gemini-cli-ide-vterm-render-delay 0.005
  "Rendering optimization delay for batched terminal updates.
This parameter defines the collection window for related terminal
update sequences when anti-flicker mode is active.  The timing
balances visual smoothness with interaction responsiveness.

The 0.005 second (5ms) default delivers optimal rendering quality
with imperceptible latency."
  :type 'number
  :group 'gemini-cli-ide)

(define-obsolete-variable-alias
  'gemini-cli-ide-eat-initialization-delay
  'gemini-cli-ide-terminal-initialization-delay
  "0.2.6")

(defcustom gemini-cli-ide-terminal-initialization-delay 0.1
  "Initialization delay for terminal stability.
Provides a brief stabilization period when launching terminals
to ensure proper layout calculation and rendering.

The delay allows terminals to complete initial dimension calculations,
preventing display artifacts like prompt misalignment and cursor
positioning errors.  The 100ms default ensures reliable initialization
without noticeable latency."
  :type 'number
  :group 'gemini-cli-ide)

(defcustom gemini-cli-ide-eat-preserve-position t
  "Maintain terminal scroll position when switching windows.
When enabled, prevents the eat terminal from jumping to the top
when you switch focus to other windows and return.  This provides
a more stable viewing experience when working with multiple windows."
  :type 'boolean
  :group 'gemini-cli-ide)

;;; Constants

(defconst gemini-cli-ide--active-editor-notification-delay 0.1
  "Delay in seconds before sending active editor notification after connection.")

;;; Variables

(defvar gemini-cli-ide--cli-available nil
  "Whether Gemini CLI is available and detected.")

(defvar gemini-cli-ide--processes (make-hash-table :test 'equal)
  "Hash table mapping project/directory roots to their Gemini CLI processes.")

(defvar gemini-cli-ide--session-ids (make-hash-table :test 'equal)
  "Hash table mapping project/directory roots to their session IDs.")

;;; emacs-mcp ownership tracking

(defvar-local gemini-cli-ide--owns-mcp-server nil
  "Non-nil when this Gemini buffer started the active `emacs-mcp' server.
Buffers whose value is non-nil contribute to
`gemini-cli-ide--mcp-server-owner-count' and may stop the server
when their count drops to zero.  Buffers whose value is nil were
attached to a server that the user (or another package) had
already started; releasing them never stops that server.")

(defvar gemini-cli-ide--mcp-server-owner-count 0
  "Number of live Gemini buffers whose `emacs-mcp' server we own.
Incremented by `gemini-cli-ide--ensure-mcp-server' when this
package starts the server, decremented by
`gemini-cli-ide--release-mcp-server'.  When this counter reaches
zero the package calls `emacs-mcp-stop'.  Always non-negative —
release is guarded with `(max 0 ...)`.")

(defvar gemini-cli-ide--deprecation-shown nil
  "Non-nil after `gemini-cli-ide-emacs-tools-setup' has emitted its
deprecation warning, so the warning fires only once per Emacs
session.")

;; Forward declarations into emacs-mcp.  These are public, autoloaded,
;; or otherwise stable in the upstream API; they are required here
;; because `gemini-cli-ide.el' may byte-compile before the user has
;; ever called `(require 'emacs-mcp)' interactively.
(declare-function emacs-mcp-start "emacs-mcp" ())
(declare-function emacs-mcp-stop "emacs-mcp" ())
(declare-function emacs-mcp-connection-info "emacs-mcp" ())

;; emacs-mcp's server-wide "default project directory" used as the
;; fallback when an `initialize' request omits `projectDir'.  We pin
;; this to each Gemini buffer's project root in `--start-session'
;; so per-session routing works even when the Gemini CLI client
;; doesn't pass the param explicitly.
(defvar emacs-mcp--project-dir)

(defun gemini-cli-ide--require-emacs-mcp ()
  "Signal `user-error' if `emacs-mcp' or Emacs 29.1 is missing.
Called at the top of every `gemini-cli-ide' interactive command
(see NFR-7 / AC-6 of spec 001).  Returns nil on success."
  (cond
   ((version< emacs-version "29.1")
    (user-error
     "gemini-cli-ide requires Emacs 29.1 or later; this Emacs is %s. \
Upgrade to Emacs 29.1+ or pin gemini-cli-ide to v0.2.x"
     emacs-version))
   ((not (featurep 'emacs-mcp))
    (user-error
     "gemini-cli-ide requires the emacs-mcp package, which is not loaded. \
Install it from https://github.com/ezchi/emacs-mcp and ensure \
(require 'emacs-mcp) succeeds in your init"))))

(defun gemini-cli-ide--ensure-mcp-server ()
  "Ensure an `emacs-mcp' server is running.
Returns t when this call started the server (so the caller is
expected to mark its terminal buffer as the owner via
`gemini-cli-ide--owns-mcp-server'), or nil when a server was
already running (started by the user or another package).
Increments `gemini-cli-ide--mcp-server-owner-count' iff this call
started the server.

The caller is responsible for setting the buffer-local
`gemini-cli-ide--owns-mcp-server' flag on the appropriate Gemini
terminal buffer when this function returns t — this function
intentionally does not touch buffer-local state of the calling
buffer."
  (if (emacs-mcp-connection-info)
      nil
    (emacs-mcp-start)
    (cl-incf gemini-cli-ide--mcp-server-owner-count)
    t))

(defun gemini-cli-ide--release-mcp-server ()
  "Decrement the owner count and stop the server if we own the last one.
Idempotent — calling twice for the same buffer has no second
effect because the buffer-local flag is cleared on first release."
  (when gemini-cli-ide--owns-mcp-server
    (setq-local gemini-cli-ide--owns-mcp-server nil)
    (setq gemini-cli-ide--mcp-server-owner-count
          (max 0 (1- gemini-cli-ide--mcp-server-owner-count)))
    (when (zerop gemini-cli-ide--mcp-server-owner-count)
      (emacs-mcp-stop))))

(defun gemini-cli-ide--allowed-tools-filter ()
  "Translate `gemini-cli-ide-mcp-allowed-tools' into the Gemini settings shape.
Return value semantics:
  - nil          → no `tools' key written (Gemini sees every tool).
  - vector of strings → write that exact list as the filter.
The defcustom values are interpreted as:
  `auto'         → return nil (advertise everything; default).
  nil            → return [] (advertise nothing — testing only).
  string         → return a one-element vector containing that string.
  list of strings → return a vector of those strings."
  (cond
   ((eq gemini-cli-ide-mcp-allowed-tools 'auto) nil)
   ((null gemini-cli-ide-mcp-allowed-tools) (vector))
   ((stringp gemini-cli-ide-mcp-allowed-tools)
    (vector gemini-cli-ide-mcp-allowed-tools))
   ((listp gemini-cli-ide-mcp-allowed-tools)
    (apply #'vector gemini-cli-ide-mcp-allowed-tools))
   (t nil)))

(defun gemini-cli-ide--write-gemini-settings (project-root)
  "Merge the `emacs-mcp' endpoint URL into PROJECT-ROOT/.gemini/settings.json.
Reads the existing file (if any) into a hash table, updates
`mcpServers.emacs.url' (and optionally `mcpServers.emacs.tools'
based on `gemini-cli-ide-mcp-allowed-tools'), and writes the
result back atomically.

Signals `user-error' when:
  - no `emacs-mcp' server is currently running
    (`emacs-mcp-connection-info' returned nil), or
  - the existing settings file is malformed JSON.

Never overwrites a malformed file — that is user data."
  (let* ((info (or (emacs-mcp-connection-info)
                   (user-error "No emacs-mcp server is running; cannot \
write Gemini settings")))
         (url (alist-get :url info))
         (settings-dir (expand-file-name ".gemini" project-root))
         (settings-file (expand-file-name "settings.json" settings-dir))
         (root
          (if (file-exists-p settings-file)
              (condition-case err
                  (with-temp-buffer
                    (insert-file-contents settings-file)
                    (json-parse-buffer
                     :object-type 'hash-table
                     :array-type 'array
                     :null-object :null
                     :false-object :false))
                (error
                 (user-error
                  "Refusing to overwrite malformed JSON in %s: %s. \
Fix or delete the file and retry"
                  settings-file (error-message-string err))))
            (make-hash-table :test 'equal)))
         (mcp-servers (or (gethash "mcpServers" root)
                          (let ((h (make-hash-table :test 'equal)))
                            (puthash "mcpServers" h root)
                            h)))
         (emacs-entry (or (gethash "emacs" mcp-servers)
                          (let ((h (make-hash-table :test 'equal)))
                            (puthash "emacs" h mcp-servers)
                            h)))
         (tools-filter (gemini-cli-ide--allowed-tools-filter)))
    (puthash "url" url emacs-entry)
    (if tools-filter
        (puthash "tools" tools-filter emacs-entry)
      (remhash "tools" emacs-entry))
    (unless (file-directory-p settings-dir)
      (make-directory settings-dir t))
    ;; Atomic write: temp file in the target directory, then rename.
    (let ((tmp (make-temp-file
                (expand-file-name ".gemini-settings-" settings-dir)
                nil ".json")))
      (with-temp-buffer
        (insert (json-serialize root
                                :null-object :null
                                :false-object :false))
        (write-region (point-min) (point-max) tmp nil 'silent))
      (rename-file tmp settings-file t))))

;;; Vterm Rendering Optimization

(defvar-local gemini-cli-ide--vterm-render-queue nil
  "Queue for optimizing terminal rendering sequences.")

(defvar-local gemini-cli-ide--vterm-render-timer nil
  "Timer for executing queued rendering operations.")

(defvar-local gemini-cli-ide--session-buffer nil
  "The Gemini CLI session buffer associated with this prompt buffer.")

(defvar-local gemini-cli-ide--saved-window-configuration nil
  "Window configuration to restore when closing a prompt buffer.")

(defvar-local gemini-cli-ide--at-mention-files-cache nil
  "Cached relative file list for @ mention completion in the prompt buffer.")

(defun gemini-cli-ide--vterm-smart-renderer (orig-fun process input)
  "Smart rendering filter for optimized vterm display updates.
This advanced filter analyzes terminal output patterns to identify
rapid update sequences that benefit from batched processing.
It significantly improves visual quality during complex operations.

ORIG-FUN is the underlying filter to enhance.
PROCESS is the terminal process being optimized.
INPUT contains the terminal output stream."
  (if (or (not gemini-cli-ide-vterm-anti-flicker)
          (not (gemini-cli-ide--session-buffer-p (process-buffer process))))
      ;; Feature disabled or not a Gemini buffer, pass through normally
      (funcall orig-fun process input)
    (with-current-buffer (process-buffer process)
      ;; Detect rapid terminal redraw sequences
      ;; Pattern analysis for complex terminal updates:
      ;; - Vertical cursor movements (ESC[<n>A)
      ;; - Line clearing operations (ESC[K)
      ;; - High escape sequence density
      (let* ((complex-redraw-detected
              ;; Pattern: vertical movement + clear, repeated
              (string-match-p "\033\\[[0-9]*A.*\033\\[K.*\033\\[[0-9]*A.*\033\\[K" input))
             (clear-count (cl-count-if (lambda (s) (string= s "\033[K"))
                                       (split-string input "\033\\[K" t)))
             (escape-count (cl-count ?\033 input))
             (input-length (length input))
             ;; High escape density indicates redrawing, not normal output
             (escape-density (if (> input-length 0)
                                 (/ (float escape-count) input-length)
                               0)))
        ;; Optimize rendering for detected patterns:
        ;; 1. Complex redraw sequence detected, OR
        ;; 2. Escape sequence density exceeds threshold with line operations
        ;; 3. OR already queuing (to complete the sequence)
        (if (or complex-redraw-detected
                (and (> escape-density 0.3)
                     (>= clear-count 2))
                gemini-cli-ide--vterm-render-queue)
            (progn
              ;; Add to buffer
              (setq gemini-cli-ide--vterm-render-queue
                    (concat gemini-cli-ide--vterm-render-queue input))
              ;; Reset existing render timer
              (when gemini-cli-ide--vterm-render-timer
                (cancel-timer gemini-cli-ide--vterm-render-timer))
              ;; Schedule optimized rendering
              ;; Timing calibrated for visual quality
              (setq gemini-cli-ide--vterm-render-timer
                    (run-at-time gemini-cli-ide-vterm-render-delay nil
                                 (lambda (buf)
                                   (when (buffer-live-p buf)
                                     (with-current-buffer buf
                                       (when gemini-cli-ide--vterm-render-queue
                                         (let ((inhibit-redisplay t)
                                               (data gemini-cli-ide--vterm-render-queue))
                                           ;; Clear queue first to prevent recursion
                                           (setq gemini-cli-ide--vterm-render-queue nil
                                                 gemini-cli-ide--vterm-render-timer nil)
                                           ;; Execute queued rendering
                                           (funcall orig-fun
                                                    (get-buffer-process buf)
                                                    data))))))
                                 (current-buffer))))
          ;; Standard processing for regular output
          (funcall orig-fun process input))))))

(defun gemini-cli-ide--configure-vterm-buffer ()
  "Configure vterm for enhanced performance and visual quality.
Establishes optimal terminal settings including rendering optimizations,
cursor management, and process buffering for superior user experience."
  ;; Disable automatic scrolling to bottom on output to prevent flickering
  (setq-local vterm-scroll-to-bottom-on-output nil)
  ;; Disable immediate redraw to batch updates and reduce flickering
  (when (boundp 'vterm--redraw-immididately)
    (setq-local vterm--redraw-immididately nil))
  ;; Try to prevent cursor flickering by disabling Emacs' own cursor management
  (setq-local cursor-in-non-selected-windows nil)
  (setq-local blink-cursor-mode nil)
  (setq-local cursor-type nil)  ; Let vterm handle the cursor entirely
  ;; Increase process read buffering to batch more updates together
  (when-let* ((proc (get-buffer-process (current-buffer))))
    (set-process-query-on-exit-flag proc nil)
    ;; Try to make vterm read larger chunks at once
    (when (fboundp 'process-put)
      (process-put proc 'read-output-max 4096)))
  ;; Set up rendering optimization
  (when gemini-cli-ide-vterm-anti-flicker
    (advice-add 'vterm--filter :around #'gemini-cli-ide--vterm-smart-renderer)))


;;; Terminal Backend Abstraction

(defun gemini-cli-ide--terminal-ensure-backend ()
  "Ensure the selected terminal backend is available."
  (cond
   ((eq gemini-cli-ide-terminal-backend 'vterm)
    (unless (featurep 'vterm)
      (require 'vterm nil t))
    (unless (featurep 'vterm)
      (user-error "The package vterm is not installed.  Please install the vterm package or change `gemini-cli-ide-terminal-backend' to 'eat")))
   ((eq gemini-cli-ide-terminal-backend 'eat)
    (unless (featurep 'eat)
      (require 'eat nil t))
    (unless (featurep 'eat)
      (user-error "The package eat is not installed.  Please install the eat package or change `gemini-cli-ide-terminal-backend' to 'vterm")))
   (t
    (user-error "Invalid terminal backend: %s.  Valid options are 'vterm or 'eat" gemini-cli-ide-terminal-backend))))

(defun gemini-cli-ide--terminal-send-string (string)
  "Send STRING to the terminal in the current buffer."
  (cond
   ((eq gemini-cli-ide-terminal-backend 'vterm)
    (vterm-send-string string))
   ((eq gemini-cli-ide-terminal-backend 'eat)
    (when eat-terminal
      (eat-term-send-string eat-terminal string)))
   (t
    (error "Unknown terminal backend: %s" gemini-cli-ide-terminal-backend))))

(defun gemini-cli-ide--terminal-send-escape ()
  "Send escape key to the terminal in the current buffer."
  (cond
   ((eq gemini-cli-ide-terminal-backend 'vterm)
    (vterm-send-escape))
   ((eq gemini-cli-ide-terminal-backend 'eat)
    (when eat-terminal
      (eat-term-send-string eat-terminal "\e")))
   (t
    (error "Unknown terminal backend: %s" gemini-cli-ide-terminal-backend))))

(defun gemini-cli-ide--terminal-send-return ()
  "Send return key to the terminal in the current buffer."
  (cond
   ((eq gemini-cli-ide-terminal-backend 'vterm)
    (vterm-send-return))
   ((eq gemini-cli-ide-terminal-backend 'eat)
    (when eat-terminal
      (eat-term-send-string eat-terminal "\r")))
   (t
    (error "Unknown terminal backend: %s" gemini-cli-ide-terminal-backend))))

(defun gemini-cli-ide--setup-terminal-keybindings ()
  "Set up keybindings for the Gemini CLI terminal buffer.
This function binds:
- M-RET (Alt-Return) to insert a newline
- C-<escape> to send escape
- C-c ' to open the prompt buffer"
  (cond
   ((eq gemini-cli-ide-terminal-backend 'vterm)
    ;; For vterm, we set up local keybindings in vterm-mode-map
    (local-set-key (kbd "S-<return>") #'gemini-cli-ide-insert-newline)
    (local-set-key (kbd "C-<escape>") #'gemini-cli-ide-send-escape)
    (local-set-key (kbd "C-c '") #'gemini-cli-ide-edit-prompt))
   ((eq gemini-cli-ide-terminal-backend 'eat)
    ;; For eat, we need to modify the semi-char mode map which is the default
    ;; We use local-set-key to make it buffer-local
    (local-set-key (kbd "S-<return>") #'gemini-cli-ide-insert-newline)
    (local-set-key (kbd "C-<escape>") #'gemini-cli-ide-send-escape)
    (local-set-key (kbd "C-c '") #'gemini-cli-ide-edit-prompt))
   (t
    (error "Unknown terminal backend: %s" gemini-cli-ide-terminal-backend))))

;;; Terminal Reflow Glitch Prevention
;;
;; This section implements a workaround for Gemini CLI bug #1422
;; where terminal reflows during height-only changes can cause
;; uncontrollable scrolling. This code should be removed once
;; the upstream bug is fixed.
;; See: https://github.com/anthropics/gemini-cli/issues/1422

(defun gemini-cli-ide--terminal-resize-handler ()
  "Retrieve the terminal's resize handling function based on backend."
  (pcase gemini-cli-ide-terminal-backend
    ('vterm #'vterm--window-adjust-process-window-size)
    ('eat #'eat--adjust-process-window-size)
    (_ (error "Unsupported terminal backend: %s" gemini-cli-ide-terminal-backend))))

(defun gemini-cli-ide--terminal-scroll-mode-active-p ()
  "Determine if terminal is currently in scroll/copy mode."
  (pcase gemini-cli-ide-terminal-backend
    ('vterm (bound-and-true-p vterm-copy-mode))
    ('eat (not (bound-and-true-p eat--semi-char-mode)))
    (_ nil)))

(defun gemini-cli-ide--session-buffer-p (buffer)
  "Check if BUFFER belongs to a Gemini CLI session."
  (when-let* ((name (if (stringp buffer) buffer (buffer-name buffer))))
    (string-prefix-p "*gemini-cli[" name)))

(defun gemini-cli-ide--terminal-reflow-filter (original-fn &rest args)
  "Filter terminal reflows to prevent height-only resize triggers.
This wraps ORIGINAL-FN to suppress reflow signals unless the terminal
width has actually changed, working around the scrolling glitch."
  (let ((dimensions-stable t))
    ;; Examine each window showing a Gemini session
    (dolist (win (window-list))
      (when-let* ((buf (window-buffer win))
                  ((gemini-cli-ide--session-buffer-p buf)))
        (let* ((new-width (window-width win))
               (cached-width (window-parameter win 'gemini-cli-ide-cached-width)))
          ;; Width change detected
          (unless (eql new-width cached-width)
            (setq dimensions-stable nil)
            (set-window-parameter win 'gemini-cli-ide-cached-width new-width)))))
    ;; Decide whether to allow reflow
    (cond
     ;; Not in a Gemini buffer - pass through
     ((not (gemini-cli-ide--session-buffer-p (current-buffer)))
      (apply original-fn args))
     ;; In scroll mode - suppress reflow
     ((gemini-cli-ide--terminal-scroll-mode-active-p)
      nil)
     ;; Dimensions changed - allow reflow
     ((not dimensions-stable)
      (apply original-fn args))
     ;; No width change - suppress reflow
     (t nil))))


;;; Helper Functions

(defun gemini-cli-ide--default-buffer-name (directory)
  "Generate default buffer name for DIRECTORY."
  (format "*gemini-cli[%s]*"
          (file-name-nondirectory (directory-file-name directory))))

(defun gemini-cli-ide--get-working-directory ()
  "Get the current working directory (project root or current directory)."
  (if-let* ((project (project-current)))
      (expand-file-name (project-root project))
    (expand-file-name default-directory)))

(defun gemini-cli-ide--get-buffer-name (&optional directory)
  "Get the buffer name for the Gemini CLI session in DIRECTORY.
If DIRECTORY is not provided, use the current working directory."
  (funcall gemini-cli-ide-buffer-name-function
           (or directory (gemini-cli-ide--get-working-directory))))

(defun gemini-cli-ide--get-process (&optional directory)
  "Get the Gemini CLI process for DIRECTORY or current working directory."
  (when (and gemini-cli-ide-prevent-reflow-glitch
             (= (hash-table-count gemini-cli-ide--processes) 0))
    ;; Apply advice globally for the first session
    (advice-add (gemini-cli-ide--terminal-resize-handler)
                :around #'gemini-cli-ide--terminal-reflow-filter))
  (gethash (or directory (gemini-cli-ide--get-working-directory))
           gemini-cli-ide--processes))

(defun gemini-cli-ide--set-process (process &optional directory)
  "Set the Gemini CLI PROCESS for DIRECTORY or current working directory."
  ;; Check if this is the first session starting
  (puthash (or directory (gemini-cli-ide--get-working-directory))
           process
           gemini-cli-ide--processes))

(defun gemini-cli-ide--cleanup-dead-processes ()
  "Remove entries for dead processes from the process table."
  (maphash (lambda (directory process)
             (unless (process-live-p process)
               (remhash directory gemini-cli-ide--processes)))
           gemini-cli-ide--processes))

(defun gemini-cli-ide--cleanup-all-sessions ()
  "Clean up all active Gemini CLI sessions."
  (maphash (lambda (directory process)
             (when (process-live-p process)
               (gemini-cli-ide--cleanup-on-exit directory)))
           gemini-cli-ide--processes))

;; Ensure cleanup on Emacs exit
(add-hook 'kill-emacs-hook #'gemini-cli-ide--cleanup-all-sessions)

(defun gemini-cli-ide--display-buffer-in-side-window (buffer)
  "Display BUFFER in a side window according to customization.
The window is displayed on the side specified by
`gemini-cli-ide-window-side' with dimensions from
`gemini-cli-ide-window-width' or `gemini-cli-ide-window-height'.
If `gemini-cli-ide-focus-on-open' is non-nil, the window is selected."
  (let ((window
         (if gemini-cli-ide-use-side-window
             ;; Use side window
             (let* ((side gemini-cli-ide-window-side)
                    (slot 0)
                    (window-parameters '((no-delete-other-windows . t)))
                    (display-buffer-alist
                     `((,(regexp-quote (buffer-name buffer))
                        (display-buffer-in-side-window)
                        (side . ,side)
                        (slot . ,slot)
                        ,@(when (memq side '(left right))
                            `((window-width . ,gemini-cli-ide-window-width)))
                        ,@(when (memq side '(top bottom))
                            `((window-height . ,gemini-cli-ide-window-height)))
                        (window-parameters . ,window-parameters)))))
               (display-buffer buffer))
           ;; Use regular buffer
           (display-buffer buffer))))
    ;; Select the window to give it focus if configured to do so
    (when (and window gemini-cli-ide-focus-on-open)
      (select-window window))
    ;; For bottom/top windows, explicitly set and preserve the height
    (when (and window
               gemini-cli-ide-use-side-window
               (memq gemini-cli-ide-window-side '(top bottom)))
      (set-window-text-height window gemini-cli-ide-window-height)
      (set-window-dedicated-p window t))
    window))

(defvar gemini-cli-ide--cleanup-in-progress nil
  "Flag to prevent recursive cleanup calls.")

(defun gemini-cli-ide--cleanup-on-exit (directory)
  "Clean up tracking and `emacs-mcp' ownership when Gemini exits for DIRECTORY.
Called from the process sentinel and `kill-buffer-hook' on the
Gemini terminal buffer."
  (unless gemini-cli-ide--cleanup-in-progress
    (setq gemini-cli-ide--cleanup-in-progress t)
    (unwind-protect
        (progn
          ;; Remove from process table
          (remhash directory gemini-cli-ide--processes)
          ;; Remove global advices when no Gemini sessions remain.
          (when (and gemini-cli-ide-prevent-reflow-glitch
                     (= (hash-table-count gemini-cli-ide--processes) 0))
            (advice-remove (gemini-cli-ide--terminal-resize-handler)
                           #'gemini-cli-ide--terminal-reflow-filter))
          (when (and (eq gemini-cli-ide-terminal-backend 'vterm)
                     gemini-cli-ide-vterm-anti-flicker
                     (= (hash-table-count gemini-cli-ide--processes) 0))
            (advice-remove 'vterm--filter #'gemini-cli-ide--vterm-smart-renderer))
          ;; Release the emacs-mcp server claim from the terminal
          ;; buffer.  `--release-mcp-server' inspects the
          ;; buffer-local `--owns-mcp-server' flag, decrements the
          ;; package-global counter, and stops the server when the
          ;; counter reaches zero AND we owned it.
          (let ((buffer-name (gemini-cli-ide--get-buffer-name directory)))
            (when-let* ((buffer (get-buffer buffer-name)))
              (when (buffer-live-p buffer)
                (with-current-buffer buffer
                  (gemini-cli-ide--release-mcp-server))
                (let ((kill-buffer-hook nil)
                      (kill-buffer-query-functions nil))
                  (kill-buffer buffer)))))
          ;; Clean up the session-id mapping.
          (remhash directory gemini-cli-ide--session-ids)
          (gemini-cli-ide-debug "Cleaned up Gemini CLI session for %s"
                                (file-name-nondirectory (directory-file-name directory))))
      (setq gemini-cli-ide--cleanup-in-progress nil))))

;;; CLI Detection

(defun gemini-cli-ide--detect-cli ()
  "Detect if Gemini CLI CLI is available."
  (let ((available (condition-case nil
                       (eq (call-process gemini-cli-ide-cli-path nil nil nil "--version") 0)
                     (error nil))))
    (setq gemini-cli-ide--cli-available available)))

(defun gemini-cli-ide--ensure-cli ()
  "Ensure Gemini CLI CLI is available, detect if needed."
  (unless gemini-cli-ide--cli-available
    (gemini-cli-ide--detect-cli))
  gemini-cli-ide--cli-available)

;;; Commands

(defun gemini-cli-ide--toggle-existing-window (existing-buffer _working-dir)
  "Toggle visibility of EXISTING-BUFFER window.
If the window is visible, it will be hidden.
If the window is not visible, it will be shown in a side window.
The unused second arg is preserved for source-compatibility with
v0.2 callers that passed the project's working directory."
  (let ((window (get-buffer-window existing-buffer)))
    (if window
        ;; Window is visible, hide it
        (progn
          (delete-window window)
          (gemini-cli-ide-debug "Gemini CLI window hidden"))
      ;; Window is not visible, show it.  The "remember the current
      ;; tab" hook that lived here in v0.2 depended on the bundled
      ;; MCP session struct (now deleted); behavior is dropped in
      ;; v0.3.0 along with the other selection / active-editor
      ;; tracking that emacs-mcp does not expose a hook for (FR-14).
      (progn
        (gemini-cli-ide--display-buffer-in-side-window existing-buffer)
        (gemini-cli-ide-debug "Gemini CLI window shown")))))

(defun gemini-cli-ide--build-gemini-command (&optional continue resume)
  "Build the Gemini command with optional flags.
If CONTINUE is non-nil, add the -c flag.
If RESUME is non-nil, add the -r flag.
If `gemini-cli-ide-cli-debug' is non-nil, add the -d flag.
If `gemini-cli-ide-system-prompt' is non-nil, that text is appended
to the Emacs-context system prompt that is always included.
Additional flags from `gemini-cli-ide-cli-extra-flags' are also
included.

The `emacs-mcp' endpoint URL is communicated to Gemini through the
project-local `.gemini/settings.json' written by
`gemini-cli-ide--write-gemini-settings'; this function does NOT
shell out to `gemini mcp add' anymore."
  (let* ((gemini-cmd gemini-cli-ide-cli-path)
         (emacs-prompt "IMPORTANT: Connected to Emacs via gemini-cli-ide.el integration. Emacs uses mixed coordinates: Lines: 1-based (line 1 = first line), Columns: 0-based (column 0 = first column). Example: First character in file is at line 1, column 0. Available: xref (LSP), tree-sitter, imenu, project.el, flycheck/flymake diagnostics. Context-aware with automatic project/file/selection tracking.")
         (combined-prompt
          (if gemini-cli-ide-system-prompt
              (concat emacs-prompt "\n\n" gemini-cli-ide-system-prompt)
            emacs-prompt)))
    (when gemini-cli-ide-cli-debug
      (setq gemini-cmd (concat gemini-cmd " -d")))
    (when resume
      (setq gemini-cmd (concat gemini-cmd " -r")))
    (when continue
      (setq gemini-cmd (concat gemini-cmd " -c")))
    (setq gemini-cmd (concat gemini-cmd " -i "
                             (shell-quote-argument combined-prompt)))
    (when (and gemini-cli-ide-cli-extra-flags
               (not (string-empty-p gemini-cli-ide-cli-extra-flags)))
      (setq gemini-cmd (concat gemini-cmd " "
                               gemini-cli-ide-cli-extra-flags)))
    gemini-cmd))

(defun gemini-cli-ide--terminal-position-keeper (window-list)
  "Maintain stable terminal view position across window switches.
WINDOW-LIST contains windows requiring position synchronization.
Implements intelligent scroll management to preserve user context
when navigating between terminal and other buffers."
  (dolist (win window-list)
    (if (eq win 'buffer)
        ;; Direct buffer point update
        (goto-char (eat-term-display-cursor eat-terminal))
      ;; Window-specific position management
      (unless buffer-read-only  ; Skip when terminal is in navigation mode
        (let ((terminal-point (eat-term-display-cursor eat-terminal)))
          ;; Update window point to match terminal state
          (set-window-point win terminal-point)
          ;; Apply smart positioning strategy
          (cond
           ;; Terminal at bottom: maintain bottom alignment for active prompts
           ((>= terminal-point (- (point-max) 2))
            (with-selected-window win
              (goto-char terminal-point)
              (recenter -1)))  ; Pin to bottom
           ;; Terminal out of view: restore visibility
           ((not (pos-visible-in-window-p terminal-point win))
            (with-selected-window win
              (goto-char terminal-point)
              (recenter)))))))))

(defun gemini-cli-ide--parse-command-string (command-string)
  "Parse a command string into (program . args) for eat-exec.
COMMAND-STRING is a shell command line to parse.
Returns a cons cell (program . args) where program is the executable
and args is a list of arguments."
  (let ((parts (split-string-shell-command command-string)))
    (cons (car parts) (cdr parts))))


(defun gemini-cli-ide--create-terminal-session (buffer-name working-dir continue resume)
  "Create a new terminal session for Gemini CLI.
BUFFER-NAME is the name for the terminal buffer.
WORKING-DIR is the working directory.
CONTINUE is whether to continue the most recent conversation.
RESUME is whether to resume a previous conversation.

Gemini CLI discovers the running `emacs-mcp' endpoint by reading
`<WORKING-DIR>/.gemini/settings.json', which the caller is
expected to have written via
`gemini-cli-ide--write-gemini-settings' before invoking this
function.

Returns a cons cell of (buffer . process) on success.
Signals an error if terminal fails to initialize."
  ;; Ensure terminal backend is available before proceeding
  (gemini-cli-ide--terminal-ensure-backend)
  (let* ((gemini-cmd (gemini-cli-ide--build-gemini-command continue resume))
         (shell-cmd (format "sh -c %s" (shell-quote-argument gemini-cmd)))
         (default-directory working-dir)
         (env-vars (list "ENABLE_IDE_INTEGRATION=true"
                         "TERM_PROGRAM=emacs"
                         "FORCE_CODE_TERMINAL=true")))
    (gemini-cli-ide-debug "Starting Gemini with command: %s" gemini-cmd)
    (gemini-cli-ide-debug "Working directory: %s" working-dir)
    (gemini-cli-ide-debug "Terminal backend: %s" gemini-cli-ide-terminal-backend)

    (cond
     ;; vterm backend
     ((eq gemini-cli-ide-terminal-backend 'vterm)
      (let* ((vterm-buffer-name buffer-name)
             ;; Set vterm-shell to run Gemini directly via shell to support setup commands
             (vterm-shell shell-cmd)
             ;; vterm uses vterm-environment for passing env vars
             (vterm-environment (append env-vars vterm-environment)))
        ;; Create vterm buffer without switching to it
        (let ((buffer (save-window-excursion
                        (vterm vterm-buffer-name))))
          ;; Check if vterm successfully created a buffer
          (unless buffer
            (error "Failed to create vterm buffer.  Please ensure vterm is properly installed and compiled"))
          ;; Configure vterm buffer for optimal performance
          (with-current-buffer buffer
            (gemini-cli-ide--configure-vterm-buffer))
          ;; Get the process that vterm created
          (let ((process (get-buffer-process buffer)))
            (unless process
              (error "Failed to get vterm process.  The vterm module may not be compiled correctly"))
            ;; Check if buffer is still alive
            (unless (buffer-live-p buffer)
              (error "Vterm buffer was killed during initialization"))
            (cons buffer process)))))

     ;; eat backend
     ((eq gemini-cli-ide-terminal-backend 'eat)
      (let* ((buffer (get-buffer-create buffer-name))
             (eat-term-name "xterm-256color")
             ;; Parse command string into program and args
             (cmd-parts (gemini-cli-ide--parse-command-string shell-cmd))
             (program (car cmd-parts))
             (args (cdr cmd-parts)))
        (with-current-buffer buffer
          ;; Set up eat mode
          (unless (eq major-mode 'eat-mode)
            (eat-mode))
          ;; Configure position preservation if enabled
          (when gemini-cli-ide-eat-preserve-position
            (setq-local eat--synchronize-scroll-function
                        #'gemini-cli-ide--terminal-position-keeper))
          ;; Prepend our env vars to the buffer-local process-environment
          (setq-local process-environment
                      (append env-vars process-environment))
          (eat-exec buffer buffer-name program nil args)
          ;; Get the process
          (let ((process (get-buffer-process buffer)))
            (unless process
              (error "Failed to create eat process.  Please ensure eat is properly installed"))
            (cons buffer process)))))

     (t
      (error "Unknown terminal backend: %s" gemini-cli-ide-terminal-backend)))))

(defun gemini-cli-ide--start-session (&optional continue resume)
  "Start a Gemini CLI session for the current project.
If CONTINUE is non-nil, start Gemini with the -c (continue) flag.
If RESUME is non-nil, start Gemini with the -r (resume) flag.

This function handles:
- emacs-mcp / Emacs version availability checking
- CLI availability checking
- Dead process cleanup
- Existing session detection and window toggling
- New session creation with emacs-mcp server lifecycle
- Process and buffer lifecycle management"
  (gemini-cli-ide--require-emacs-mcp)
  (unless (gemini-cli-ide--ensure-cli)
    (user-error "Gemini CLI CLI not available.  Please install it and ensure it's in PATH"))

  ;; Clean up any dead processes first
  (gemini-cli-ide--cleanup-dead-processes)

  (let* ((working-dir (gemini-cli-ide--get-working-directory))
         (buffer-name (gemini-cli-ide--get-buffer-name))
         (existing-buffer (get-buffer buffer-name))
         (existing-process (gemini-cli-ide--get-process working-dir)))

    ;; If buffer exists and process is alive, toggle the window
    (if (and existing-buffer
             (buffer-live-p existing-buffer)
             existing-process)
        (gemini-cli-ide--toggle-existing-window existing-buffer working-dir)
      ;; Ensure the selected terminal backend is available before starting MCP
      (gemini-cli-ide--terminal-ensure-backend)
      ;; Ensure emacs-mcp server is up and write the project-local
      ;; .gemini/settings.json so Gemini CLI discovers the endpoint.
      ;; `we-started-it' is non-nil iff THIS invocation started the
      ;; server — the terminal buffer below inherits that ownership.
      (let ((we-started-it (gemini-cli-ide--ensure-mcp-server)))
        ;; Pin the server's default project dir to this Gemini
        ;; buffer's project root so the MCP session that Gemini
        ;; CLI creates via `initialize' (without an explicit
        ;; `projectDir' param) inherits the right value.
        ;; emacs-mcp's protocol-layer initialize handler falls
        ;; back to `emacs-mcp--project-dir' when the request
        ;; omits `projectDir' (see emacs-mcp-protocol.el:84-89).
        ;; This is a server-wide knob, so concurrent rapid
        ;; project starts can race; but it preserves the spec's
        ;; per-session-project-routing intent for the common
        ;; sequential workflow.
        (setq emacs-mcp--project-dir working-dir)
        (gemini-cli-ide--write-gemini-settings working-dir)
        (let ((session-id (format "gemini-%s-%s"
                                  (file-name-nondirectory (directory-file-name working-dir))
                                  (format-time-string "%Y%m%d-%H%M%S"))))
          (condition-case err
              (progn
                ;; Create new terminal session
                (let* ((buffer-and-process (gemini-cli-ide--create-terminal-session
                                            buffer-name working-dir continue resume))
                       (buffer (car buffer-and-process))
                       (process (cdr buffer-and-process)))
                  (gemini-cli-ide-debug "Gemini session started with ID: %s in %s"
                                        session-id (file-name-nondirectory (directory-file-name working-dir)))
                  (gemini-cli-ide--set-process process working-dir)
                  ;; Transfer emacs-mcp ownership from the local
                  ;; `we-started-it' to the terminal buffer.  Once
                  ;; the buffer carries the flag, clear the local
                  ;; var so the error-recovery branch below does
                  ;; NOT decrement the counter a second time when
                  ;; the buffer's kill-buffer-hook later fires.
                  (when we-started-it
                    (with-current-buffer buffer
                      (setq-local gemini-cli-ide--owns-mcp-server t))
                    (setq we-started-it nil))
                  ;; Store session ID for cleanup
                  (puthash working-dir session-id gemini-cli-ide--session-ids)
                ;; Set up process sentinel to clean up when Gemini exits
                (set-process-sentinel process
                                      (lambda (_proc event)
                                        ;; Check for abnormal exit with error code
                                        (when (string-match "exited abnormally with code \\([0-9]+\\)" event)
                                          (let ((exit-code (match-string 1 event)))
                                            (gemini-cli-ide-debug "Gemini process exited with code %s, event: %s"
                                                                  exit-code event)
                                            (message "Gemini exited with error code %s" exit-code)))
                                        (when (or (string-match "finished" event)
                                                  (string-match "exited" event)
                                                  (string-match "killed" event)
                                                  (string-match "terminated" event))
                                          (gemini-cli-ide--cleanup-on-exit working-dir))))
                  (gemini-cli-ide-debug "Gemini CLI session started in %s"
                                        (file-name-nondirectory (directory-file-name working-dir)))
                  ;; Also add buffer kill hook as a backup so we
                  ;; release the emacs-mcp server on direct
                  ;; `kill-buffer'.
                  (with-current-buffer buffer
                    (add-hook 'kill-buffer-hook
                              (lambda ()
                                (gemini-cli-ide--cleanup-on-exit working-dir))
                              nil t)
                    ;; Set up terminal keybindings
                    (gemini-cli-ide--setup-terminal-keybindings)
                    ;; Add terminal-specific exit hooks
                    (cond
                     ((eq gemini-cli-ide-terminal-backend 'vterm)
                      (add-hook 'vterm-exit-functions
                                (lambda (&rest _)
                                  (when (buffer-live-p buffer)
                                    (kill-buffer buffer)))
                                nil t))
                     ((eq gemini-cli-ide-terminal-backend 'eat)
                      (setq-local eat-kill-buffer-on-exit t))))
                  ;; Stabilization period for terminal layout initialization
                  (sleep-for gemini-cli-ide-terminal-initialization-delay)
                  ;; Display the buffer in a side window
                  (gemini-cli-ide--display-buffer-in-side-window buffer)
                  (gemini-cli-ide-log "Gemini CLI %sstarted in %s%s"
                                      (cond (continue "continued and ")
                                            (resume "resumed and ")
                                            (t ""))
                                      (file-name-nondirectory (directory-file-name working-dir))
                                      (if gemini-cli-ide-cli-debug " (debug mode enabled)" ""))))
            (error
             ;; Terminal session creation failed.  If THIS call is
             ;; what brought up the emacs-mcp server, release it so
             ;; we don't leave a zombie running.
             (when we-started-it
               (setq gemini-cli-ide--mcp-server-owner-count
                     (max 0 (1- gemini-cli-ide--mcp-server-owner-count)))
               (when (zerop gemini-cli-ide--mcp-server-owner-count)
                 (emacs-mcp-stop)))
             (signal (car err) (cdr err)))))))))

;;;###autoload
(defun gemini-cli-ide ()
  "Run Gemini CLI in a terminal for the current project or directory."
  (interactive)
  (gemini-cli-ide--require-emacs-mcp)
  (gemini-cli-ide--start-session))

;;;###autoload
(defun gemini-cli-ide-resume ()
  "Resume Gemini CLI in a terminal for the current project or directory.
This starts Gemini with the -r (resume) flag to continue the previous
conversation."
  (interactive)
  (gemini-cli-ide--require-emacs-mcp)
  (gemini-cli-ide--start-session nil t))

;;;###autoload
(defun gemini-cli-ide-continue ()
  "Continue the most recent Gemini CLI conversation in the current directory.
This starts Gemini with the -c (continue) flag to continue the most recent
conversation in the current directory."
  (interactive)
  (gemini-cli-ide--require-emacs-mcp)
  (gemini-cli-ide--start-session t))

;;;###autoload
(defun gemini-cli-ide-check-status ()
  "Check Gemini CLI and `emacs-mcp' status for the current project."
  (interactive)
  (gemini-cli-ide--require-emacs-mcp)
  (gemini-cli-ide--detect-cli)
  (let* ((cli-line
          (if gemini-cli-ide--cli-available
              (let ((version-output
                     (with-temp-buffer
                       (call-process gemini-cli-ide-cli-path nil t nil "--version")
                       (buffer-string))))
                (format "Gemini CLI: %s" (string-trim version-output)))
            "Gemini CLI: not installed"))
         (info (emacs-mcp-connection-info))
         (mcp-line (if info
                       (format "emacs-mcp: %s" (alist-get :url info))
                     "emacs-mcp: not running")))
    (gemini-cli-ide-log "%s\n%s" cli-line mcp-line)))

;;;###autoload
(defun gemini-cli-ide-stop ()
  "Stop the Gemini CLI session for the current project or directory."
  (interactive)
  (gemini-cli-ide--require-emacs-mcp)
  (let* ((working-dir (gemini-cli-ide--get-working-directory))
         (buffer-name (gemini-cli-ide--get-buffer-name)))
    (if-let* ((buffer (get-buffer buffer-name)))
        (progn
          ;; Kill the buffer.  `kill-buffer-hook' chains to
          ;; `gemini-cli-ide--cleanup-on-exit', which calls
          ;; `gemini-cli-ide--release-mcp-server' on the buffer
          ;; before the buffer is killed — that releases our claim
          ;; on the emacs-mcp server.
          (kill-buffer buffer)
          (gemini-cli-ide-log "Stopping Gemini CLI in %s..."
                              (file-name-nondirectory (directory-file-name working-dir))))
      (gemini-cli-ide-log "No Gemini CLI session is running in this directory"))))


;;;###autoload
(defun gemini-cli-ide-switch-to-buffer ()
  "Switch to the Gemini CLI buffer for the current project.
If the buffer is not visible, display it in the configured side window.
If the buffer is already visible, switch focus to it."
  (interactive)
  (gemini-cli-ide--require-emacs-mcp)
  (let ((buffer-name (gemini-cli-ide--get-buffer-name)))
    (if-let* ((buffer (get-buffer buffer-name)))
        (if-let* ((window (get-buffer-window buffer)))
            ;; Buffer is visible, just focus it
            (select-window window)
          ;; Buffer exists but not visible, display it
          (gemini-cli-ide--display-buffer-in-side-window buffer))
      (user-error "No Gemini CLI session for this project.  Use M-x gemini-cli-ide to start one"))))

;;;###autoload
(defun gemini-cli-ide-list-sessions ()
  "List all active Gemini CLI sessions and switch to selected one."
  (interactive)
  (gemini-cli-ide--require-emacs-mcp)
  (gemini-cli-ide--cleanup-dead-processes)
  (let ((sessions '()))
    (maphash (lambda (directory _)
               (push (cons (abbreviate-file-name directory)
                           directory)
                     sessions))
             gemini-cli-ide--processes)
    (if sessions
        (let ((choice (completing-read "Switch to Gemini CLI session: "
                                       sessions nil t)))
          (when choice
            (let* ((directory (alist-get choice sessions nil nil #'string=))
                   (buffer-name (funcall gemini-cli-ide-buffer-name-function directory)))
              (if-let* ((buffer (get-buffer buffer-name)))
                  (gemini-cli-ide--display-buffer-in-side-window buffer)
                (user-error "Buffer for session %s no longer exists" choice)))))
      (gemini-cli-ide-log "No active Gemini CLI sessions"))))

;;;###autoload
(defun gemini-cli-ide-insert-at-mentioned ()
  "Send the active region to the project's Gemini CLI terminal buffer.
The text is typed into the terminal as if the user had pasted it,
without sending Return.  The user can then review and submit."
  (interactive)
  (gemini-cli-ide--require-emacs-mcp)
  (unless (use-region-p)
    (user-error "No active region; mark some text first"))
  (let* ((selection (buffer-substring-no-properties
                     (region-beginning) (region-end)))
         (working-dir (gemini-cli-ide--get-working-directory))
         (buffer-name (gemini-cli-ide--get-buffer-name working-dir))
         (buffer (get-buffer buffer-name)))
    (unless (and buffer (buffer-live-p buffer))
      (user-error
       "No Gemini CLI session for this project; start one with M-x gemini-cli-ide"))
    (with-current-buffer buffer
      (gemini-cli-ide--terminal-send-string selection))
    (gemini-cli-ide-debug "Inserted %d-character selection into Gemini terminal"
                          (length selection))))

;;;###autoload
(defun gemini-cli-ide-emacs-tools-setup ()
  "Deprecation shim for the removed tool-installer (FR-13).
In v0.2.x this function registered Gemini-specific MCP tools into a
bundled MCP server.  Starting in v0.3.0 the MCP server lives in the
external `emacs-mcp' package, and this package's tools auto-register
when `gemini-cli-ide-tools' is loaded.  This shim emits a one-time
deprecation warning and does nothing else.  It will be removed in
v0.4.0."
  (interactive)
  (unless gemini-cli-ide--deprecation-shown
    (setq gemini-cli-ide--deprecation-shown t)
    (display-warning
     'gemini-cli-ide
     "gemini-cli-ide-emacs-tools-setup is deprecated. Use \
`(emacs-mcp-mode 1)' and require 'gemini-cli-ide instead. Will be \
removed in v0.4.0."
     :warning)))

;;;###autoload
(defun gemini-cli-ide-send-escape ()
  "Send escape key to the Gemini CLI terminal buffer for the current project."
  (interactive)
  (let ((buffer-name (gemini-cli-ide--get-buffer-name)))
    (if-let* ((buffer (get-buffer buffer-name)))
        (with-current-buffer buffer
          (gemini-cli-ide--terminal-send-escape))
      (user-error "No Gemini CLI session for this project"))))

;;;###autoload
(defun gemini-cli-ide-insert-newline ()
  "Send newline (backslash + return) to the Gemini CLI terminal.
This sends the newline sequence to the terminal buffer for the
current project.  This simulates typing backslash followed by
Enter, which Gemini CLI interprets as a newline."
  (interactive)
  (let ((buffer-name (gemini-cli-ide--get-buffer-name)))
    (if-let* ((buffer (get-buffer buffer-name)))
        (with-current-buffer buffer
          (gemini-cli-ide--terminal-send-string "\\")
          ;; Small delay to ensure prompt text is processed before sending return
          (sit-for 0.1)
          (gemini-cli-ide--terminal-send-return))
      (user-error "No Gemini CLI session for this project"))))

;;;###autoload
(defun gemini-cli-ide-toggle-vterm-optimization ()
  "Toggle vterm rendering optimization.
This command switches the advanced rendering optimization on or off.
Use this to balance between visual smoothness and raw responsiveness."
  (interactive)
  (setq gemini-cli-ide-vterm-anti-flicker
        (not gemini-cli-ide-vterm-anti-flicker))
  (message "Vterm rendering optimization %s"
           (if gemini-cli-ide-vterm-anti-flicker
               "enabled (smoother display with minimal latency)"
             "disabled (direct rendering, maximum responsiveness)")))

;;;###autoload
(defun gemini-cli-ide-send-prompt (&optional prompt no-return clear-line)
  "Send a prompt to the Gemini CLI terminal.
When called interactively, reads a prompt from the minibuffer.
When called programmatically, sends the given PROMPT string.
If NO-RETURN is non-nil, do not send the return key.
If CLEAR-LINE is non-nil, send C-u to clear the current line first."
  (interactive)
  (let ((buffer-name (gemini-cli-ide--get-buffer-name)))
    (if-let* ((buffer (get-buffer buffer-name)))
        (let ((prompt-to-send (or prompt (read-string "Gemini prompt: "))))
          (when (or clear-line (not (string-empty-p prompt-to-send)))
            (with-current-buffer buffer
              (when clear-line
                (if (eq gemini-cli-ide-terminal-backend 'vterm)
                    (vterm-send-key "u" nil nil t)
                  (gemini-cli-ide--terminal-send-string "\C-u"))
                (sit-for 0.1))
              (unless (string-empty-p prompt-to-send)
                (gemini-cli-ide--terminal-send-string prompt-to-send))
              (unless no-return
                (sit-for 0.1)
                (gemini-cli-ide--terminal-send-return)))
            (gemini-cli-ide-debug "Sent prompt to Gemini CLI: %s" prompt-to-send)))
      (user-error "No Gemini CLI session for this project"))))

;;;###autoload
(defun gemini-cli-ide-edit-prompt ()
  "Edit the Gemini CLI terminal prompt in a buffer.
The buffer is in `text-mode` and `with-editor-mode` (if available).
The buffer is initialized with the active region (if any) or the
current terminal input.
Press C-c C-c to update the terminal prompt (without sending) or
C-c C-k to cancel."
  (interactive)
  (let* ((working-dir (gemini-cli-ide--get-working-directory))
         (buffer-name (gemini-cli-ide--get-buffer-name))
         (target-buffer (get-buffer buffer-name))
         (window-config (current-window-configuration))
         (region-text (when (use-region-p)
                        (buffer-substring-no-properties (region-beginning) (region-end)))))
    (unless target-buffer
      (user-error "No Gemini CLI session for this project"))
    (let ((prompt-buffer (get-buffer-create
                          (format "*Gemini Prompt [%s]*"
                                  (file-name-nondirectory (directory-file-name working-dir)))))
          (initial-input (or region-text
                             (gemini-cli-ide--get-terminal-input target-buffer))))
      (with-current-buffer prompt-buffer
        (text-mode)
        (setq-local default-directory working-dir)
        (setq-local gemini-cli-ide--session-buffer target-buffer)
        (setq-local gemini-cli-ide--saved-window-configuration window-config)
        (setq-local completion-styles '(flex partial-completion basic))
        (setq-local completion-category-defaults nil)
        (setq-local completion-category-overrides '((file (styles flex partial-completion basic))))
        (setq-local tab-always-indent 'complete)
        (add-hook 'completion-at-point-functions #'gemini-cli-ide--at-mentioned-completion-at-point nil t)
        (add-hook 'post-self-insert-hook #'gemini-cli-ide--prompt-buffer-post-self-insert nil t)
        (erase-buffer)
        (when (and initial-input (not (string-empty-p initial-input)))
          (insert (string-trim initial-input)))
        ;; Use an explicit local keymap so C-c C-c always applies the prompt
        ;; instead of invoking editor/file-saving workflows.
        (use-local-map (copy-keymap text-mode-map))
        (local-set-key (kbd "C-c C-c") #'gemini-cli-ide--apply-prompt-buffer)
        (local-set-key (kbd "C-c C-k") #'gemini-cli-ide--cancel-prompt-buffer)
        (message "Type your prompt and press C-c C-c to update, or C-c C-k to cancel."))
      (pop-to-buffer prompt-buffer))))

(defun gemini-cli-ide--apply-prompt-buffer ()
  "Apply current prompt buffer content to the Gemini terminal and clean up."
  (interactive)
  (let ((prompt-buffer (current-buffer))
        (prompt (buffer-substring-no-properties (point-min) (point-max)))
        (target-buffer gemini-cli-ide--session-buffer)
        (window-config gemini-cli-ide--saved-window-configuration))
    (set-buffer-modified-p nil)
    (when window-config
      (set-window-configuration window-config))
    (when (buffer-live-p prompt-buffer)
      (kill-buffer prompt-buffer))
    (when (and target-buffer (buffer-live-p target-buffer))
      (gemini-cli-ide-send-prompt (string-trim prompt) t t))))

(defun gemini-cli-ide--get-terminal-input (buffer)
  "Try to get the current input line from the Gemini terminal in BUFFER."
  (when (and buffer (buffer-live-p buffer))
    (with-current-buffer buffer
      ;; Ensure (point) is at the actual terminal cursor for better extraction
      (cond
       ((and (derived-mode-p 'vterm-mode) (fboundp 'vterm-reset-cursor-point))
        (vterm-reset-cursor-point))
       ((and (boundp 'eat-terminal) eat-terminal (fboundp 'eat-term-display-cursor))
        (goto-char (eat-term-display-cursor eat-terminal))))
      (when-let* ((input
                  (or (gemini-cli-ide--get-terminal-input-from-vterm)
                      (gemini-cli-ide--get-terminal-input-from-eat)
                      (gemini-cli-ide--get-terminal-input-from-text))))
        (gemini-cli-ide--strip-terminal-ui-suffix
         (gemini-cli-ide--strip-terminal-prompt-prefix input))))))

(defun gemini-cli-ide--strip-terminal-prompt-prefix (input)
  "Strip a visible Gemini prompt prefix from INPUT."
  (let ((stripped (string-trim-left input "[[:space:]\u00a0]+")))
    (cond
     ((string-match "\\`[│┃[:space:]\u00a0]*\\(?:gemini[[:space:]\u00a0]+\\)?>[[:space:]\u00a0]+" stripped)
      (substring stripped (match-end 0)))
     ((string-match "\\`[[:space:]\u00a0]*[^[:alnum:]_[:space:]\u00a0]\\{1,3\\}[[:space:]\u00a0]+" stripped)
      (substring stripped (match-end 0)))
     (t stripped))))

(defun gemini-cli-ide--strip-terminal-ui-suffix (input)
  "Strip Gemini TUI footer/status content from INPUT."
  (let ((result (if (string-match "\n[▄▀━─_-╰╯╭╮│┃ ]\\{10,\\}\\(?:.\\|\n\\)*\\'" input)
                    (substring input 0 (match-beginning 0))
                  input)))
    ;; Also handle case where there's a lot of whitespace and then a TUI-like footer
    (if (string-match "\n\n+[[:space:]\u00a0]*\\(?:\\? for shortcuts\\|workspace (.*)\\)\\'" result)
        (substring result 0 (match-beginning 0))
      (string-trim-right result))))

(defun gemini-cli-ide--get-terminal-input-from-vterm ()
  "Read the active command buffer contents from the current vterm buffer.
Prioritizes native vterm prompt tracking if enabled and configured."
  (when (derived-mode-p 'vterm-mode)
    (save-excursion
      (when (fboundp 'vterm-reset-cursor-point)
        (vterm-reset-cursor-point))
      (let* ((cursor (point))
             (prompt-start (when (fboundp 'vterm--get-prompt-point)
                             (vterm--get-prompt-point)))
             (use-native (and (boundp 'vterm-use-vterm-prompt-detection-method)
                              vterm-use-vterm-prompt-detection-method
                              prompt-start
                              (<= prompt-start cursor))))
        (if use-native
            (buffer-substring-no-properties prompt-start cursor)
          ;; Fallback to text-based search for unconfigured shells
          (goto-char cursor)
          (if (re-search-backward "\\(?:gemini \\)?> " (max (point-min) (- cursor 500)) t)
              (buffer-substring-no-properties (match-end 0) cursor)
            ;; Last resort fallback to tracked prompt point even if not "native"
            (when (and (integer-or-marker-p prompt-start)
                       (<= prompt-start cursor))
              (buffer-substring-no-properties prompt-start cursor))))))))

(defun gemini-cli-ide--get-terminal-input-from-eat ()
  "Read the active command buffer contents from the current Eat buffer."
  (when (and (boundp 'eat-terminal)
             eat-terminal
             (fboundp 'eat-term-display-cursor))
    (let ((input-end (eat-term-display-cursor eat-terminal))
          (input-start (when (fboundp 'eat-term-end)
                         (eat-term-end eat-terminal))))
      (save-excursion
        (goto-char input-end)
        (if (re-search-backward "\\(?:gemini \\)?> " (max (point-min) (- input-end 500)) t)
            (buffer-substring-no-properties (match-end 0) input-end)
          ;; Fallback to eat's tracked terminal end
          (when (and (integer-or-marker-p input-start)
                     (<= input-start input-end))
            (buffer-substring-no-properties input-start input-end)))))))

(defun gemini-cli-ide--get-terminal-input-from-text ()
  "Fallback text-based extraction for terminal buffers without prompt metadata."
  (save-excursion
    ;; Start from current point (which is at the cursor for vterm/eat)
    ;; instead of point-max to avoid grabbing TUI footers.
    (let ((end (point)))
      (if (re-search-backward "\\(?:gemini \\)?> " (max (point-min) (- end 1000)) t)
          (buffer-substring-no-properties (match-end 0) end)
        ;; Fallback to just the current line
        (forward-line 0)
        (buffer-substring-no-properties (point) end)))))

(defun gemini-cli-ide--cancel-prompt-buffer ()
  "Cancel the prompt and kill the buffer."
  (interactive)
  (let ((prompt-buffer (current-buffer))
        (window-config gemini-cli-ide--saved-window-configuration))
    (set-buffer-modified-p nil)
    (when window-config
      (set-window-configuration window-config))
    (when (buffer-live-p prompt-buffer)
      (kill-buffer prompt-buffer))))

(defun gemini-cli-ide--at-mentioned-bounds ()
  "Return bounds of the @ mention at point as (START . END)."
  (let* ((pos (point))
         (start (save-excursion
                  (skip-chars-backward "^ \t\n\r")
                  (point))))
    (when (and (< start pos)
               (char-equal (char-after start) ?@))
      (cons (1+ start) pos))))

(defun gemini-cli-ide--at-mention-candidates ()
  "Return cached relative file paths for @ mention completion."
  (or gemini-cli-ide--at-mention-files-cache
      (setq gemini-cli-ide--at-mention-files-cache
            (let* ((working-dir default-directory)
                   (project (project-current nil working-dir))
                   (files (if project
                              (project-files project)
                            (directory-files-recursively working-dir ".*" nil))))
              (mapcar (lambda (f) (file-relative-name f working-dir)) files)))))

(defun gemini-cli-ide--filesystem-path-mention-p (input)
  "Return non-nil when INPUT should use filesystem path completion."
  (or (string-prefix-p "~" input)
      (file-name-absolute-p input)
      (string-prefix-p "./" input)
      (string-prefix-p "../" input)))

(defun gemini-cli-ide--at-mention-completion-table (string pred action)
  "Completion table for @ mentions using STRING, PRED, and ACTION."
  (if (gemini-cli-ide--filesystem-path-mention-p string)
      (completion-file-name-table string pred action)
    (complete-with-action action (gemini-cli-ide--at-mention-candidates) string pred)))

(defun gemini-cli-ide--at-mentioned-completion-at-point ()
  "Completion at point for '@' mentions in the prompt buffer."
  (when-let* ((bounds (gemini-cli-ide--at-mentioned-bounds)))
    (list (car bounds) (cdr bounds) #'gemini-cli-ide--at-mention-completion-table
          :exclusive 'no
          :annotation-function (lambda (_) " [File]")
          :category 'file)))

(defun gemini-cli-ide--prompt-buffer-post-self-insert ()
  "Trigger fuzzy @ mention completion after typing in the prompt buffer."
  (when (and (not (minibufferp))
             (memq this-command '(self-insert-command org-self-insert-command))
             (gemini-cli-ide--at-mentioned-bounds)
             (let ((char last-command-event))
               (and (characterp char)
                    (not (memq char '(?\s ?\t ?\n ?\r))))))
    (completion-at-point)))

;;;###autoload
(defun gemini-cli-ide-toggle ()
  "Toggle visibility of Gemini CLI window for the current project."
  (interactive)
  (let* ((working-dir (gemini-cli-ide--get-working-directory))
         (buffer-name (gemini-cli-ide--get-buffer-name))
         (buffer (get-buffer buffer-name)))
    (if buffer
        (gemini-cli-ide--toggle-existing-window buffer working-dir)
      (user-error "No Gemini CLI session for this project"))))

;;;###autoload
(defun gemini-cli-ide-toggle-recent ()
  "Toggle visibility of the most recently used Gemini CLI window."
  (interactive)
  (gemini-cli-ide--cleanup-dead-processes)
  (let ((recent-buffer
         (cl-find-if (lambda (buf)
                       (and (gemini-cli-ide--session-buffer-p buf)
                            (get-buffer-process buf)
                            (process-live-p (get-buffer-process buf))))
                     (buffer-list))))
    (if recent-buffer
        (let* ((process (get-buffer-process recent-buffer))
               (directory (let (found)
                            (maphash (lambda (dir proc)
                                       (when (eq proc process)
                                         (setq found dir)))
                                     gemini-cli-ide--processes)
                            found)))
          (gemini-cli-ide--toggle-existing-window
           recent-buffer
           (or directory (buffer-local-value 'default-directory recent-buffer))))
      (user-error "No active Gemini CLI sessions found"))))

(provide 'gemini-cli-ide)

;;; gemini-cli-ide.el ends here
