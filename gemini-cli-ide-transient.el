;;; gemini-cli-ide-transient.el --- Transient menus for Gemin Cli IDE  -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Enze Chi
;; Keywords: ai, gemini, transient, menu

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

;; This file provides transient menus for Gemin Cli IDE, offering
;; a convenient interface for all Gemin Cli operations.

;;; Code:

(require 'transient)
(require 'gemini-cli-ide-debug)

;; Declare functions from other files to avoid circular dependencies
(declare-function gemini-cli-ide "gemini-cli-ide" ())
(declare-function gemini-cli-ide-resume "gemini-cli-ide" ())
(declare-function gemini-cli-ide-continue "gemini-cli-ide" ())
(declare-function gemini-cli-ide-stop "gemini-cli-ide" ())
(declare-function gemini-cli-ide-list-sessions "gemini-cli-ide" ())
(declare-function gemini-cli-ide-switch-to-buffer "gemini-cli-ide" ())
(declare-function gemini-cli-ide-insert-at-mentioned "gemini-cli-ide" ())
(declare-function gemini-cli-ide-send-prompt "gemini-cli-ide" ())
(declare-function gemini-cli-ide-send-escape "gemini-cli-ide" ())
(declare-function gemini-cli-ide-insert-newline "gemini-cli-ide" ())
(declare-function gemini-cli-ide-toggle "gemini-cli-ide" ())
(declare-function gemini-cli-ide-check-status "gemini-cli-ide" ())
(declare-function gemini-cli-ide--ensure-cli "gemini-cli-ide" ())
(declare-function gemini-cli-ide-mcp--active-sessions "gemini-cli-ide-mcp" ())
(declare-function gemini-cli-ide-mcp-session-project-dir "gemini-cli-ide-mcp" (session))
(declare-function gemini-cli-ide-mcp-session-port "gemini-cli-ide-mcp" (session))
(declare-function gemini-cli-ide-mcp-session-client "gemini-cli-ide-mcp" (session))
(declare-function gemini-cli-ide-mcp-session-buffer "gemini-cli-ide-mcp" (session))
(declare-function gemini-cli-ide-mcp-session-last-buffer "gemini-cli-ide-mcp" (session))
(declare-function gemini-cli-ide-mcp--get-current-session "gemini-cli-ide-mcp" ())
(declare-function gemini-cli-ide--get-working-directory "gemini-cli-ide" ())

;; Declare variables
(defvar gemini-cli-ide-cli-path)
(defvar gemini-cli-ide-debug)
(defvar gemini-cli-ide-window-side)
(defvar gemini-cli-ide-window-width)
(defvar gemini-cli-ide-window-height)
(defvar gemini-cli-ide-focus-on-open)
(defvar gemini-cli-ide-focus-gemini-after-ediff)
(defvar gemini-cli-ide-show-gemini-window-in-ediff)
(defvar gemini-cli-ide-use-ide-diff)
(defvar gemini-cli-ide-use-side-window)
(defvar gemini-cli-ide-cli-debug)
(defvar gemini-cli-ide-cli-extra-flags)
(defvar gemini-cli-ide-system-prompt)

;;; Helper Functions

(defun gemini-cli-ide--has-active-session-p ()
  "Check if there's an active Gemin Cli session for the current buffer."
  (when (gemini-cli-ide-mcp--get-current-session) t))

(defun gemini-cli-ide--start-description ()
  "Dynamic description for start command based on session status."
  (if (gemini-cli-ide--has-active-session-p)
      (propertize "Start new Gemin Cli session (session already running)"
                  'face 'transient-inactive-value)
    "Start new Gemin Cli session"))

(defun gemini-cli-ide--start-if-no-session ()
  "Start Gemin Cli only if no session is active for current buffer."
  (interactive)
  (if (gemini-cli-ide--has-active-session-p)
      (let ((working-dir (gemini-cli-ide--get-working-directory)))
        (gemini-cli-ide-log "Gemin Cli session already running in %s"
                             (abbreviate-file-name working-dir)))
    (gemini-cli-ide)))

(defun gemini-cli-ide--continue-description ()
  "Dynamic description for continue command based on session status."
  (if (gemini-cli-ide--has-active-session-p)
      (propertize "Continue most recent conversation (session already running)"
                  'face 'transient-inactive-value)
    "Continue most recent conversation"))

(defun gemini-cli-ide--continue-if-no-session ()
  "Continue Gemin Cli only if no session is active for current buffer."
  (interactive)
  (if (gemini-cli-ide--has-active-session-p)
      (let ((working-dir (gemini-cli-ide--get-working-directory)))
        (gemini-cli-ide-log "Gemin Cli session already running in %s"
                             (abbreviate-file-name working-dir)))
    (gemini-cli-ide-continue)))

(defun gemini-cli-ide--resume-description ()
  "Dynamic description for resume command based on session status."
  (if (gemini-cli-ide--has-active-session-p)
      (propertize "Resume session (session already running)"
                  'face 'transient-inactive-value)
    "Resume session (from previous conversation)"))

(defun gemini-cli-ide--resume-if-no-session ()
  "Resume Gemin Cli only if no session is active for current buffer."
  (interactive)
  (if (gemini-cli-ide--has-active-session-p)
      (let ((working-dir (gemini-cli-ide--get-working-directory)))
        (gemini-cli-ide-log "Gemin Cli session already running in %s"
                             (abbreviate-file-name working-dir)))
    (gemini-cli-ide-resume)))

(defun gemini-cli-ide--session-status ()
  "Return a string describing the current session status."
  (if-let ((session (gemini-cli-ide-mcp--get-current-session)))
      (let* ((project-dir (gemini-cli-ide-mcp-session-project-dir session))
             (project-name (file-name-nondirectory (directory-file-name project-dir)))
             (connected (if (gemini-cli-ide-mcp-session-client session) "connected" "disconnected")))
        (propertize (format "Active session in [%s] - %s" project-name connected)
                    'face 'success))
    (propertize "No active session" 'face 'transient-inactive-value)))

(defun gemini-cli-ide-toggle-window ()
  "Toggle visibility of Gemin Cli window.
If called from a Gemini vterm buffer, toggle that window.
Otherwise, if multiple sessions exist, prompt for selection."
  (interactive)
  (gemini-cli-ide-toggle))

(defun gemini-cli-ide-show-version-info ()
  "Show detailed version information for Gemin Cli CLI."
  (interactive)
  (if (gemini-cli-ide--ensure-cli)
      (let ((version-output
             (with-temp-buffer
               (call-process gemini-cli-ide-cli-path nil t nil "--version")
               (buffer-string))))
        (with-output-to-temp-buffer "*Gemin Cli Version*"
          (princ "Gemin Cli CLI Version Information\n")
          (princ "===================================\n\n")
          (princ version-output)
          (princ "\n\nExecutable path: ")
          (princ (executable-find gemini-cli-ide-cli-path))))
    (user-error "Gemin Cli CLI not available")))

(defun gemini-cli-ide-show-mcp-sessions ()
  "Show information about active MCP sessions."
  (interactive)
  (let ((sessions (gemini-cli-ide-mcp--active-sessions)))
    (if sessions
        (with-output-to-temp-buffer "*Gemin Cli MCP Sessions*"
          (princ "Active MCP Sessions\n")
          (princ "==================\n\n")
          (dolist (session sessions)
            (princ (format "Project: %s\n" (gemini-cli-ide-mcp-session-project-dir session)))
            (princ (format "  Port: %d\n" (gemini-cli-ide-mcp-session-port session)))
            (princ (format "  Connected: %s\n"
                           (if (gemini-cli-ide-mcp-session-client session) "Yes" "No")))
            (princ (format "  Buffer: %s\n"
                           (if (gemini-cli-ide-mcp-session-last-buffer session)
                               (buffer-name (gemini-cli-ide-mcp-session-last-buffer session))
                             "None")))
            (princ "\n")))
      (gemini-cli-ide-log "No active MCP sessions"))))

(defun gemini-cli-ide-show-active-ports ()
  "Show active ports used by MCP servers."
  (interactive)
  (let ((sessions (gemini-cli-ide-mcp--active-sessions)))
    (if sessions
        (with-output-to-temp-buffer "*Gemin Cli Active Ports*"
          (princ "Active MCP Server Ports\n")
          (princ "======================\n\n")
          (dolist (session sessions)
            (princ (format "Port %d: %s\n"
                           (gemini-cli-ide-mcp-session-port session)
                           (abbreviate-file-name (gemini-cli-ide-mcp-session-project-dir session))))))
      (gemini-cli-ide-log "No active MCP servers"))))

(defun gemini-cli-ide-toggle-debug-mode ()
  "Toggle Gemin Cli debug mode."
  (interactive)
  (setq gemini-cli-ide-debug (not gemini-cli-ide-debug))
  (gemini-cli-ide-log "Debug mode %s" (if gemini-cli-ide-debug "enabled" "disabled")))

;;; Transient Infix Classes

(transient-define-suffix gemini-cli-ide--set-window-side (side)
  "Set window side."
  :description "Set window side"
  (interactive (list (intern (completing-read "Window side: "
                                              '("left" "right" "top" "bottom")
                                              nil t nil nil
                                              (symbol-name gemini-cli-ide-window-side)))))
  (setq gemini-cli-ide-window-side side)
  (gemini-cli-ide-log "Window side set to %s" side))

(transient-define-suffix gemini-cli-ide--set-window-width (width)
  "Set window width."
  :description "Set window width"
  (interactive (list (read-number "Window width: " gemini-cli-ide-window-width)))
  (setq gemini-cli-ide-window-width width)
  (gemini-cli-ide-log "Window width set to %d" width))

(transient-define-suffix gemini-cli-ide--set-window-height (height)
  "Set window height."
  :description "Set window height"
  (interactive (list (read-number "Window height: " gemini-cli-ide-window-height)))
  (setq gemini-cli-ide-window-height height)
  (gemini-cli-ide-log "Window height set to %d" height))

(transient-define-suffix gemini-cli-ide--set-cli-path (path)
  "Set CLI path."
  :description "Set CLI path"
  (interactive (list (read-file-name "Gemini CLI path: " nil gemini-cli-ide-cli-path t)))
  (setq gemini-cli-ide-cli-path path)
  (gemini-cli-ide-log "CLI path set to %s" path))

(transient-define-suffix gemini-cli-ide--set-cli-extra-flags (flags)
  "Set additional CLI flags."
  :description "Set additional CLI flags"
  (interactive (list (read-string "Additional CLI flags: " gemini-cli-ide-cli-extra-flags)))
  (setq gemini-cli-ide-cli-extra-flags flags)
  (gemini-cli-ide-log "CLI extra flags set to %s" flags))

(transient-define-suffix gemini-cli-ide--set-system-prompt (prompt)
  "Set the system prompt to append."
  :description "Set system prompt"
  (interactive (list (if gemini-cli-ide-system-prompt
                         (read-string "System prompt (leave empty to disable): "
                                      gemini-cli-ide-system-prompt)
                       (read-string "System prompt: "))))
  (setq gemini-cli-ide-system-prompt (if (string-empty-p prompt) nil prompt))
  (gemini-cli-ide-log "System prompt %s"
                       (if gemini-cli-ide-system-prompt
                           (format "set to: %s" gemini-cli-ide-system-prompt)
                         "disabled")))

;;; Transient Suffix Functions

(transient-define-suffix gemini-cli-ide--toggle-focus-on-open ()
  "Toggle focus on open setting."
  (interactive)
  (setq gemini-cli-ide-focus-on-open (not gemini-cli-ide-focus-on-open))
  (gemini-cli-ide-log "Focus on open %s" (if gemini-cli-ide-focus-on-open "enabled" "disabled")))

(transient-define-suffix gemini-cli-ide--toggle-focus-after-ediff ()
  "Toggle focus after ediff setting."
  (interactive)
  (setq gemini-cli-ide-focus-gemini-after-ediff (not gemini-cli-ide-focus-gemini-after-ediff))
  (gemini-cli-ide-log "Focus after ediff %s" (if gemini-cli-ide-focus-gemini-after-ediff "enabled" "disabled")))

(transient-define-suffix gemini-cli-ide--toggle-show-gemini-in-ediff ()
  "Toggle showing Gemini window during ediff."
  (interactive)
  (setq gemini-cli-ide-show-gemini-window-in-ediff (not gemini-cli-ide-show-gemini-window-in-ediff))
  (gemini-cli-ide-log "Show Gemini window in ediff %s" (if gemini-cli-ide-show-gemini-window-in-ediff "enabled" "disabled")))

(transient-define-suffix gemini-cli-ide--toggle-use-side-window ()
  "Toggle use side window setting."
  (interactive)
  (setq gemini-cli-ide-use-side-window (not gemini-cli-ide-use-side-window))
  (gemini-cli-ide-log "Use side window %s" (if gemini-cli-ide-use-side-window "enabled" "disabled")))

(transient-define-suffix gemini-cli-ide--toggle-use-ide-diff ()
  "Toggle IDE diff viewer setting."
  (interactive)
  (setq gemini-cli-ide-use-ide-diff (not gemini-cli-ide-use-ide-diff))
  (gemini-cli-ide-log "IDE diff viewer %s" (if gemini-cli-ide-use-ide-diff "enabled" "disabled")))

(transient-define-suffix gemini-cli-ide--toggle-cli-debug ()
  "Toggle CLI debug mode."
  (interactive)
  (setq gemini-cli-ide-cli-debug (not gemini-cli-ide-cli-debug))
  (gemini-cli-ide-log "CLI debug mode %s" (if gemini-cli-ide-cli-debug "enabled" "disabled")))

(defun gemini-cli-ide--save-config ()
  "Save current configuration to custom file."
  (interactive)
  (customize-save-variable 'gemini-cli-ide-window-side gemini-cli-ide-window-side)
  (customize-save-variable 'gemini-cli-ide-window-width gemini-cli-ide-window-width)
  (customize-save-variable 'gemini-cli-ide-window-height gemini-cli-ide-window-height)
  (customize-save-variable 'gemini-cli-ide-focus-on-open gemini-cli-ide-focus-on-open)
  (customize-save-variable 'gemini-cli-ide-focus-gemini-after-ediff gemini-cli-ide-focus-gemini-after-ediff)
  (customize-save-variable 'gemini-cli-ide-show-gemini-window-in-ediff gemini-cli-ide-show-gemini-window-in-ediff)
  (customize-save-variable 'gemini-cli-ide-use-ide-diff gemini-cli-ide-use-ide-diff)
  (customize-save-variable 'gemini-cli-ide-use-side-window gemini-cli-ide-use-side-window)
  (customize-save-variable 'gemini-cli-ide-cli-path gemini-cli-ide-cli-path)
  (customize-save-variable 'gemini-cli-ide-cli-extra-flags gemini-cli-ide-cli-extra-flags)
  (customize-save-variable 'gemini-cli-ide-system-prompt gemini-cli-ide-system-prompt)
  (gemini-cli-ide-log "Configuration saved to custom file"))

;;; Transient Menus

;;;###autoload (autoload 'gemini-cli-ide-menu "gemini-cli-ide-transient" "Gemin Cli IDE main menu." t)
(transient-define-prefix gemini-cli-ide-menu ()
  "Gemin Cli IDE main menu."
  [:description gemini-cli-ide--session-status]
  ["Gemin Cli IDE"
   ["Session Management"
    ("s" gemini-cli-ide--start-if-no-session :description gemini-cli-ide--start-description)
    ("c" gemini-cli-ide--continue-if-no-session :description gemini-cli-ide--continue-description)
    ("r" gemini-cli-ide--resume-if-no-session :description gemini-cli-ide--resume-description)
    ("q" "Stop current session" gemini-cli-ide-stop)
    ("l" "List all sessions" gemini-cli-ide-list-sessions)]
   ["Navigation"
    ("b" "Switch to Gemini buffer" gemini-cli-ide-switch-to-buffer)
    ("w" "Toggle window visibility" gemini-cli-ide-toggle-window)]
   ["Interaction"
    ("i" "Insert selection" gemini-cli-ide-insert-at-mentioned)
    ("p" "Send prompt from minibuffer" gemini-cli-ide-send-prompt)
    ("e" "Send escape key" gemini-cli-ide-send-escape)
    ("n" "Insert newline" gemini-cli-ide-insert-newline)]
   ["Submenus"
    ("C" "Configuration" gemini-cli-ide-config-menu)
    ("d" "Debugging" gemini-cli-ide-debug-menu)]])

(transient-define-prefix gemini-cli-ide-config-menu ()
  "Gemin Cli configuration menu."
  ["Gemin Cli Configuration"
   ["Window Settings"
    ("s" "Set window side" gemini-cli-ide--set-window-side)
    ("w" "Set window width" gemini-cli-ide--set-window-width)
    ("h" "Set window height" gemini-cli-ide--set-window-height)
    ("f" "Toggle focus on open" gemini-cli-ide--toggle-focus-on-open
     :description (lambda () (format "Focus on open (%s)"
                                     (if gemini-cli-ide-focus-on-open "ON" "OFF"))))
    ("e" "Toggle focus after ediff" gemini-cli-ide--toggle-focus-after-ediff
     :description (lambda () (format "Focus after ediff (%s)"
                                     (if gemini-cli-ide-focus-gemini-after-ediff "ON" "OFF"))))
    ("E" "Toggle show Gemini in ediff" gemini-cli-ide--toggle-show-gemini-in-ediff
     :description (lambda () (format "Show Gemini in ediff (%s)"
                                     (if gemini-cli-ide-show-gemini-window-in-ediff "ON" "OFF"))))
    ("i" "Toggle IDE diff viewer" gemini-cli-ide--toggle-use-ide-diff
     :description (lambda () (format "IDE diff viewer (%s)"
                                     (if gemini-cli-ide-use-ide-diff "ON" "OFF"))))
    ("u" "Toggle side window" gemini-cli-ide--toggle-use-side-window
     :description (lambda () (format "Use side window (%s)"
                                     (if gemini-cli-ide-use-side-window "ON" "OFF"))))]
   ["CLI Settings"
    ("p" "Set CLI path" gemini-cli-ide--set-cli-path)
    ("x" "Set extra CLI flags" gemini-cli-ide--set-cli-extra-flags)
    ("a" "Set system prompt" gemini-cli-ide--set-system-prompt)]]
  ["Save"
   ("S" "Save configuration" gemini-cli-ide--save-config)])

(transient-define-prefix gemini-cli-ide-debug-menu ()
  "Gemin Cli debug menu."
  ["Gemin Cli Debug"
   ["Status"
    ("S" "Check CLI status" gemini-cli-ide-check-status)
    ("v" "Show version info" gemini-cli-ide-show-version-info)]
   ["Debug Settings"
    ("d" "Toggle debug mode" gemini-cli-ide-toggle-debug-mode
     :description (lambda () (format "Debug mode (%s)"
                                     (if gemini-cli-ide-debug "ON" "OFF"))))
    ("D" "Toggle CLI debug mode" gemini-cli-ide--toggle-cli-debug
     :description (lambda () (format "CLI debug mode (%s)"
                                     (if gemini-cli-ide-cli-debug "ON" "OFF"))))]
   ["Debug Logs"
    ("l" "Show debug log" gemini-cli-ide-show-debug)
    ("c" "Clear debug log" gemini-cli-ide-clear-debug)]
   ["MCP Server"
    ("m" "Show MCP sessions" gemini-cli-ide-show-mcp-sessions)
    ("p" "Show active ports" gemini-cli-ide-show-active-ports)]])

(provide 'gemini-cli-ide-transient)

;;; gemini-cli-ide-transient.el ends here
