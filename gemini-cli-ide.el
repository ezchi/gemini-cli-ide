;;; gemini-cli-ide.el --- Gemini CLI integration for Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Enze Chi
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (websocket "1.12") (transient "0.9.0") (web-server "0.1.2"))
;; Keywords: ai, gemini, cli, assistant, mcp, websocket
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
;; It supports file operations, diagnostics, and editor state management.
;;
;; This package starts a WebSocket server that Gemini CLI connects to,
;; enabling real-time communication between Emacs and Gemini.  It supports
;; multiple concurrent sessions per project.
;;
;; Features:
;; - Automatic IDE mode activation when starting Gemini
;; - MCP WebSocket server for bidirectional communication
;; - Project-aware sessions with automatic working directory detection
;; - Clean session management with automatic cleanup on exit
;; - Selection and buffer state tracking
;; - Tool support for file operations, diagnostics, and more
;; - Emacs MCP tools for xref and project navigation
;;
;; Usage:
;; M-x gemini-cli-ide - Start Gemini CLI for current project
;; M-x gemini-cli-ide-continue - Continue most recent conversation in directory
;; M-x gemini-cli-ide-resume - Resume Gemini CLI with previous conversation
;; M-x gemini-cli-ide-stop - Stop Gemini CLI for current project
;; M-x gemini-cli-ide-switch-to-buffer - Switch to project's Gemini buffer
;; M-x gemini-cli-ide-list-sessions - List and switch between all sessions
;; M-x gemini-cli-ide-check-status - Check CLI availability and version
;; M-x gemini-cli-ide-insert-at-mentioned - Send selected text to Gemini
;;
;; Emacs MCP Tools:
;; To enable Emacs tools for Gemini, add to your config:
;;   (gemini-cli-ide-emacs-tools-setup)

;;; Code:

(require 'cl-lib)
(require 'project)
(require 'gemini-cli-ide-debug)
(require 'gemini-cli-ide-mcp)
(require 'gemini-cli-ide-transient)
(require 'gemini-cli-ide-mcp-server)
(require 'gemini-cli-ide-emacs-tools)

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
(declare-function vterm--window-adjust-process-window-size "vterm" (&optional frame))

;; External function declarations for eat
(declare-function eat-mode "eat" ())
(declare-function eat-exec "eat" (buffer name command startfile &rest switches))
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
  "Configuration for allowed MCP tools when MCP server is enabled.
Can be one of:
  'auto - Automatically allow all configured emacs-tools (default)
  nil - Disable the --allowedTools flag
  A string - Custom pattern/tools passed directly to --allowedTools
  A list of strings - List of specific tool names to allow"
  :type '(choice (const :tag "Auto (all emacs-tools)" auto)
                 (const :tag "Disabled" nil)
                 (string :tag "Custom pattern")
                 (repeat :tag "Specific tools" string))
  :group 'gemini-cli-ide)

(defcustom gemini-cli-ide-window-side 'right
  "Side of the frame where the Gemini CLI window should appear.
Can be `'left', `'right', `'top', or `'bottom'."
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

(define-obsolete-variable-alias
  'gemini-cli-ide-eat-initialization-delay
  'gemini-cli-ide-terminal-initialization-delay
  "0.2.6")

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

;;; Vterm Rendering Optimization

(defvar-local gemini-cli-ide--vterm-render-queue nil
  "Queue for optimizing terminal rendering sequences.")

(defvar-local gemini-cli-ide--vterm-render-timer nil
  "Timer for executing queued rendering operations.")

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
  (when-let ((proc (get-buffer-process (current-buffer))))
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
- C-<escape> to send escape"
  (cond
   ((eq gemini-cli-ide-terminal-backend 'vterm)
    ;; For vterm, we set up local keybindings in vterm-mode-map
    (local-set-key (kbd "S-<return>") #'gemini-cli-ide-insert-newline)
    (local-set-key (kbd "C-<escape>") #'gemini-cli-ide-send-escape))
   ((eq gemini-cli-ide-terminal-backend 'eat)
    ;; For eat, we need to modify the semi-char mode map which is the default
    ;; We use local-set-key to make it buffer-local
    (local-set-key (kbd "S-<return>") #'gemini-cli-ide-insert-newline)
    (local-set-key (kbd "C-<escape>") #'gemini-cli-ide-send-escape))
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
  (when-let ((name (if (stringp buffer) buffer (buffer-name buffer))))
    (string-prefix-p "*gemini-cli[" name)))

(defun gemini-cli-ide--terminal-reflow-filter (original-fn &rest args)
  "Filter terminal reflows to prevent height-only resize triggers.
This wraps ORIGINAL-FN to suppress reflow signals unless the terminal
width has actually changed, working around the scrolling glitch."
  (let* ((base-result (apply original-fn args))
         (dimensions-stable t))
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
      base-result)
     ;; In scroll mode - suppress reflow
     ((gemini-cli-ide--terminal-scroll-mode-active-p)
      nil)
     ;; Dimensions changed - allow reflow
     ((not dimensions-stable)
      base-result)
     ;; No width change - suppress reflow
     (t nil))))


;;; Helper Functions

(defun gemini-cli-ide--default-buffer-name (directory)
  "Generate default buffer name for DIRECTORY."
  (format "*gemini-cli[%s]*"
          (file-name-nondirectory (directory-file-name directory))))

(defun gemini-cli-ide--get-working-directory ()
  "Get the current working directory (project root or current directory)."
  (if-let ((project (project-current)))
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
  "Clean up MCP server and process tracking when Gemini exits for DIRECTORY."
  (unless gemini-cli-ide--cleanup-in-progress
    (setq gemini-cli-ide--cleanup-in-progress t)
    (unwind-protect
        (progn
          ;; Remove from process table
          (remhash directory gemini-cli-ide--processes)
          ;; Check if this was the last session
          (when (and gemini-cli-ide-prevent-reflow-glitch
                     (= (hash-table-count gemini-cli-ide--processes) 0))
            ;; Remove advice globally when no sessions remain
            (advice-remove (gemini-cli-ide--terminal-resize-handler)
                           #'gemini-cli-ide--terminal-reflow-filter))
          ;; Remove vterm rendering optimization if no sessions remain
          (when (and (eq gemini-cli-ide-terminal-backend 'vterm)
                     gemini-cli-ide-vterm-anti-flicker
                     (= (hash-table-count gemini-cli-ide--processes) 0))
            (advice-remove 'vterm--filter #'gemini-cli-ide--vterm-smart-renderer))
          ;; Stop MCP server for this project directory
          (gemini-cli-ide-mcp-stop-session directory)
          ;; Notify MCP tools server about session end with session ID
          (let ((session-id (gethash directory gemini-cli-ide--session-ids)))
            (gemini-cli-ide-mcp-server-session-ended session-id)
            ;; Clean up session ID mapping
            (when session-id
              (remhash directory gemini-cli-ide--session-ids)))
          ;; Kill the vterm buffer if it exists
          (let ((buffer-name (gemini-cli-ide--get-buffer-name directory)))
            (when-let ((buffer (get-buffer buffer-name)))
              (when (buffer-live-p buffer)
                (let ((kill-buffer-hook nil) ; Disable hooks to prevent recursion
                      (kill-buffer-query-functions nil)) ; Don't ask for confirmation
                  (kill-buffer buffer)))))
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

(defun gemini-cli-ide--toggle-existing-window (existing-buffer working-dir)
  "Toggle visibility of EXISTING-BUFFER window for WORKING-DIR.
If the window is visible, it will be hidden.
If the window is not visible, it will be shown in a side window."
  (let ((window (get-buffer-window existing-buffer)))
    (if window
        ;; Window is visible, hide it
        (progn
          (delete-window window)
          (gemini-cli-ide-debug "Gemini CLI window hidden"))
      ;; Window is not visible, show it
      (progn
        (gemini-cli-ide--display-buffer-in-side-window existing-buffer)
        ;; Update the original tab when showing the window
        (when-let ((session (gemini-cli-ide-mcp--get-session-for-project working-dir)))
          (when (fboundp 'tab-bar--current-tab)
            (setf (gemini-cli-ide-mcp-session-original-tab session) (tab-bar--current-tab))))
        (gemini-cli-ide-debug "Gemini CLI window shown")))))

(defun gemini-cli-ide--build-gemini-command (&optional continue resume session-id)
  "Build the Gemini command with optional flags.
If CONTINUE is non-nil, add the -c flag.
If RESUME is non-nil, add the -r flag.
If SESSION-ID is provided, it's included in the MCP server URL path.
If `gemini-cli-ide-cli-debug' is non-nil, add the -d flag.
If `gemini-cli-ide-system-prompt' is non-nil, add the --append-system-prompt flag.
Additional flags from `gemini-cli-ide-cli-extra-flags' are also included."
  (let ((gemini-cmd gemini-cli-ide-cli-path))
    ;; Add debug flag if enabled
    (when gemini-cli-ide-cli-debug
      (setq gemini-cmd (concat gemini-cmd " -d")))
    ;; Add resume flag if requested
    (when resume
      (setq gemini-cmd (concat gemini-cmd " -r")))
    ;; Add continue flag if requested
    (when continue
      (setq gemini-cmd (concat gemini-cmd " -c")))
    ;; Add append-system-prompt flag with Emacs context
    (let ((emacs-prompt "IMPORTANT: Connected to Emacs via gemini-cli-ide.el integration. Emacs uses mixed coordinates: Lines: 1-based (line 1 = first line), Columns: 0-based (column 0 = first column). Example: First character in file is at line 1, column 0. Available: xref (LSP), tree-sitter, imenu, project.el, flycheck/flymake diagnostics. Context-aware with automatic project/file/selection tracking.")
          (combined-prompt nil))
      ;; Always include the Emacs-specific prompt
      (setq combined-prompt emacs-prompt)
      ;; Append user's custom prompt if set
      (when gemini-cli-ide-system-prompt
        (setq combined-prompt (concat combined-prompt "\n\n" gemini-cli-ide-system-prompt)))
      ;; Add the combined prompt to the command
      (setq gemini-cmd (concat gemini-cmd " --prompt-interactive "
                               (shell-quote-argument combined-prompt))))
    ;; Add any extra flags
    (when (and gemini-cli-ide-cli-extra-flags
               (not (string-empty-p gemini-cli-ide-cli-extra-flags)))
      (setq gemini-cmd (concat gemini-cmd " " gemini-cli-ide-cli-extra-flags)))
    ;; Add MCP tools config if enabled
    (when (gemini-cli-ide-mcp-server-ensure-server)
      (when-let ((config (gemini-cli-ide-mcp-server-get-config session-id)))
        (let ((json-str (json-encode config)))
          (gemini-cli-ide-debug "MCP tools config JSON: %s" json-str)
          ;; For vterm, we need to escape for sh -c context
          ;; First escape backslashes, then quotes
          (setq json-str (replace-regexp-in-string "\\\\" "\\\\\\\\" json-str))
          (setq json-str (replace-regexp-in-string "\"" "\\\\\"" json-str))
          (setq gemini-cmd (concat gemini-cmd " --mcp-config \"" json-str "\""))
          ;; Add allowedTools flag if configured
          (let ((allowed-tools
                 (cond
                  ;; Auto mode: get all emacs-tools names
                  ((eq gemini-cli-ide-mcp-allowed-tools 'auto)
                   (mapconcat 'identity (gemini-cli-ide-mcp-server-get-tool-names "mcp__emacs-tools__") " "))
                  ;; List of specific tools
                  ((listp gemini-cli-ide-mcp-allowed-tools)
                   (mapconcat 'identity gemini-cli-ide-mcp-allowed-tools " "))
                  ;; String pattern or nil
                  (t gemini-cli-ide-mcp-allowed-tools))))
            (when allowed-tools
              (setq gemini-cmd (concat gemini-cmd " --allowedTools " allowed-tools)))))))
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


(defun gemini-cli-ide--create-terminal-session (buffer-name working-dir port continue resume session-id)
  "Create a new terminal session for Gemini CLI.
BUFFER-NAME is the name for the terminal buffer.
WORKING-DIR is the working directory.
PORT is the MCP server port.
CONTINUE is whether to continue the most recent conversation.
RESUME is whether to resume a previous conversation.
SESSION-ID is the unique identifier for this session.

Returns a cons cell of (buffer . process) on success.
Signals an error if terminal fails to initialize."
  ;; Ensure terminal backend is available before proceeding
  (gemini-cli-ide--terminal-ensure-backend)
  (let* ((gemini-cmd (gemini-cli-ide--build-gemini-command continue resume session-id))
         (default-directory working-dir)
         (env-vars (list (format "GEMINI_CODE_SSE_PORT=%d" port)
                         "ENABLE_IDE_INTEGRATION=true"
                         "TERM_PROGRAM=emacs"
                         "FORCE_CODE_TERMINAL=true")))
    ;; Log the command for debugging
    (gemini-cli-ide-debug "Starting Gemini with command: %s" gemini-cmd)
    (gemini-cli-ide-debug "Working directory: %s" working-dir)
    (gemini-cli-ide-debug "Environment: GEMINI_CODE_SSE_PORT=%d" port)
    (gemini-cli-ide-debug "Session ID: %s" session-id)
    (gemini-cli-ide-debug "Terminal backend: %s" gemini-cli-ide-terminal-backend)

    (cond
     ;; vterm backend
     ((eq gemini-cli-ide-terminal-backend 'vterm)
      (let* ((vterm-buffer-name buffer-name)
             ;; Set vterm-shell to run Gemini directly
             (vterm-shell gemini-cmd)
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
             (cmd-parts (gemini-cli-ide--parse-command-string gemini-cmd))
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
- CLI availability checking
- Dead process cleanup
- Existing session detection and window toggling
- New session creation with MCP server setup
- Process and buffer lifecycle management"
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
      ;; Start MCP server with project directory
      (let ((port nil)
            (session-id (format "gemini-%s-%s"
                                (file-name-nondirectory (directory-file-name working-dir))
                                (format-time-string "%Y%m%d-%H%M%S"))))
        (condition-case err
            (progn
              ;; Start MCP server
              (setq port (gemini-cli-ide-mcp-start working-dir))
              ;; Create new terminal session
              (let* ((buffer-and-process (gemini-cli-ide--create-terminal-session
                                          buffer-name working-dir port continue resume session-id))
                     (buffer (car buffer-and-process))
                     (process (cdr buffer-and-process)))
                ;; Notify MCP tools server about new session with session info
                (gemini-cli-ide-mcp-server-session-started session-id working-dir buffer)
                (gemini-cli-ide-debug "MCP session started with ID: %s in %s"
                                      session-id (file-name-nondirectory (directory-file-name working-dir)))
                (gemini-cli-ide--set-process process working-dir)
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
                (gemini-cli-ide-debug "Gemini CLI session started in %s with MCP on port %d"
                                      (file-name-nondirectory (directory-file-name working-dir))
                                      port)
                ;; Also add buffer kill hook as a backup
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
                    ;; Add vterm exit hook to ensure buffer is killed when process exits
                    ;; vterm runs Gemini directly, no shell involved
                    (add-hook 'vterm-exit-functions
                              (lambda (&rest _)
                                (when (buffer-live-p buffer)
                                  (kill-buffer buffer)))
                              nil t))
                   ((eq gemini-cli-ide-terminal-backend 'eat)
                    ;; eat uses kill-buffer-on-exit variable
                    (setq-local eat-kill-buffer-on-exit t))))
                ;; Stabilization period for terminal layout initialization
                (sleep-for gemini-cli-ide-terminal-initialization-delay)
                ;; Display the buffer in a side window
                (gemini-cli-ide--display-buffer-in-side-window buffer)
                (gemini-cli-ide-log "Gemini CLI %sstarted in %s with MCP on port %d%s"
                                    (cond (continue "continued and ")
                                          (resume "resumed and ")
                                          (t ""))
                                    (file-name-nondirectory (directory-file-name working-dir))
                                    port
                                    (if gemini-cli-ide-cli-debug " (debug mode enabled)" ""))))
          (error
           ;; Terminal session creation failed - clean up MCP server
           (when port
             (gemini-cli-ide-mcp-stop-session working-dir))
           ;; Notify MCP tools server that session ended (to decrement count)
           (gemini-cli-ide-mcp-server-session-ended session-id)
           ;; Re-signal the error with improved message
           (signal (car err) (cdr err))))))))

;;;###autoload
(defun gemini-cli-ide ()
  "Run Gemini CLI in a terminal for the current project or directory."
  (interactive)
  (gemini-cli-ide--start-session))

;;;###autoload
(defun gemini-cli-ide-resume ()
  "Resume Gemini CLI in a terminal for the current project or directory.
This starts Gemini with the -r (resume) flag to continue the previous
conversation."
  (interactive)
  (gemini-cli-ide--start-session nil t))

;;;###autoload
(defun gemini-cli-ide-continue ()
  "Continue the most recent Gemini CLI conversation in the current directory.
This starts Gemini with the -c (continue) flag to continue the most recent
conversation in the current directory."
  (interactive)
  (gemini-cli-ide--start-session t))

;;;###autoload
(defun gemini-cli-ide-check-status ()
  "Check Gemini CLI CLI status."
  (interactive)
  (gemini-cli-ide--detect-cli)
  (if gemini-cli-ide--cli-available
      (let ((version-output
             (with-temp-buffer
               (call-process gemini-cli-ide-cli-path nil t nil "--version")
               (buffer-string))))
        (gemini-cli-ide-log "Gemini CLI CLI version: %s" (string-trim version-output)))
    (gemini-cli-ide-log "Gemini CLI is not installed.")))

;;;###autoload
(defun gemini-cli-ide-stop ()
  "Stop the Gemini CLI session for the current project or directory."
  (interactive)
  (let* ((working-dir (gemini-cli-ide--get-working-directory))
         (buffer-name (gemini-cli-ide--get-buffer-name)))
    (if-let ((buffer (get-buffer buffer-name)))
        (progn
          ;; Kill the buffer (cleanup will be handled by hooks)
          ;; The process sentinel will handle cleanup when the process dies
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
  (let ((buffer-name (gemini-cli-ide--get-buffer-name)))
    (if-let ((buffer (get-buffer buffer-name)))
        (if-let ((window (get-buffer-window buffer)))
            ;; Buffer is visible, just focus it
            (select-window window)
          ;; Buffer exists but not visible, display it
          (gemini-cli-ide--display-buffer-in-side-window buffer))
      (user-error "No Gemini CLI session for this project.  Use M-x gemini-cli-ide to start one"))))

;;;###autoload
(defun gemini-cli-ide-list-sessions ()
  "List all active Gemini CLI sessions and switch to selected one."
  (interactive)
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
              (if-let ((buffer (get-buffer buffer-name)))
                  (gemini-cli-ide--display-buffer-in-side-window buffer)
                (user-error "Buffer for session %s no longer exists" choice)))))
      (gemini-cli-ide-log "No active Gemini CLI sessions"))))

;;;###autoload
(defun gemini-cli-ide-insert-at-mentioned ()
  "Insert selected text into Gemini prompt."
  (interactive)
  (if-let* ((project-dir (gemini-cli-ide-mcp--get-buffer-project))
            (session (gemini-cli-ide-mcp--get-session-for-project project-dir))
            (client (gemini-cli-ide-mcp-session-client session)))
      (progn
        (gemini-cli-ide-mcp-send-at-mentioned)
        (gemini-cli-ide-debug "Sent selection to Gemini CLI"))
    (user-error "Gemini CLI is not connected.  Please start Gemini CLI first")))

;;;###autoload
(defun gemini-cli-ide-send-escape ()
  "Send escape key to the Gemini CLI terminal buffer for the current project."
  (interactive)
  (let ((buffer-name (gemini-cli-ide--get-buffer-name)))
    (if-let ((buffer (get-buffer buffer-name)))
        (with-current-buffer buffer
          (gemini-cli-ide--terminal-send-escape))
      (user-error "No Gemini CLI session for this project"))))

;;;###autoload
(defun gemini-cli-ide-insert-newline ()
  "Send newline (backslash + return) to the Gemini CLI terminal buffer for the current project.
This simulates typing backslash followed by Enter, which Gemini CLI interprets as a newline."
  (interactive)
  (let ((buffer-name (gemini-cli-ide--get-buffer-name)))
    (if-let ((buffer (get-buffer buffer-name)))
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
(defun gemini-cli-ide-send-prompt ()
  "Read a prompt from the minibuffer and send it to the Gemini CLI terminal.
This allows you to send prompts to Gemini without typing directly in the terminal."
  (interactive)
  (let ((buffer-name (gemini-cli-ide--get-buffer-name)))
    (if-let ((buffer (get-buffer buffer-name)))
        (let ((prompt (read-string "Gemini prompt: ")))
          (when (not (string-empty-p prompt))
            (with-current-buffer buffer
              (gemini-cli-ide--terminal-send-string prompt)
              ;; Small delay to ensure prompt text is processed before sending return
              (sit-for 0.1)
              (gemini-cli-ide--terminal-send-return))
            (gemini-cli-ide-debug "Sent prompt to Gemini CLI: %s" prompt)))
      (user-error "No Gemini CLI session for this project"))))

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

(provide 'gemini-cli-ide)

;;; gemini-cli-ide.el ends here
