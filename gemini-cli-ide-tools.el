;;; gemini-cli-ide-tools.el --- Gemini-specific MCP tools  -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Enze Chi
;; Keywords: ai, gemini, mcp

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

;; Registers Gemini-specific MCP tools into the external `emacs-mcp'
;; server.  In this release the only tool is the terminal-input
;; reader, which lets Gemini see what the user is currently typing in
;; the Gemini terminal buffer before they press Enter.
;;
;; Tools are registered at file load time, so simply loading this file
;; (typically via `(require 'gemini-cli-ide)') makes them visible to
;; any subsequent `emacs-mcp' server start.

;;; Code:

(require 'emacs-mcp)
(require 'emacs-mcp-tools)   ;; for `emacs-mcp-deftool'
(require 'emacs-mcp-session) ;; for `emacs-mcp-session-project-dir'
(require 'gemini-cli-ide-debug)

;; Forward declarations into the main `gemini-cli-ide' module.  Loaded
;; transitively at runtime; this file does not (require 'gemini-cli-ide)
;; to avoid a circular load.
(declare-function gemini-cli-ide--get-terminal-input "gemini-cli-ide" (buffer))
(defvar gemini-cli-ide-buffer-name-function)

;; Forward declarations into emacs-mcp internals used here.  These
;; symbols are private (`--' prefix) but stable in the upstream
;; protocol layer; they are how a tool handler determines which
;; session it is currently servicing.
(defvar emacs-mcp--current-session-id)
(declare-function emacs-mcp--session-get "emacs-mcp-session" (session-id))
(declare-function emacs-mcp-session-project-dir "emacs-mcp-session" (session))

(defun gemini-cli-ide-tools--current-project-dir ()
  "Return the project root for the currently dispatching MCP session.
Returns nil if no session context is active (e.g. when called outside
an MCP request)."
  (when emacs-mcp--current-session-id
    (let ((session (emacs-mcp--session-get
                    emacs-mcp--current-session-id)))
      (when session
        (emacs-mcp-session-project-dir session)))))

(emacs-mcp-deftool gemini-cli-ide-mcp-get-terminal-input
  "Read what the user is currently typing in the Gemini terminal before they press Enter. Use this to provide real-time assistance or clarify context"
  ()
  (lambda (_args)
    (let ((project-dir (gemini-cli-ide-tools--current-project-dir)))
      (if (not project-dir)
          "No active Gemini session context found."
        (let* ((buffer-name (funcall gemini-cli-ide-buffer-name-function
                                     project-dir))
               (buffer (get-buffer buffer-name)))
          (if (not buffer)
              (format "Gemini terminal buffer '%s' not found."
                      buffer-name)
            (let ((input (gemini-cli-ide--get-terminal-input buffer)))
              (if (and input (not (string-empty-p input)))
                  input
                "No unsent input found in the terminal prompt."))))))))

(provide 'gemini-cli-ide-tools)
;;; gemini-cli-ide-tools.el ends here
