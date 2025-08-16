;;; gemini-cli-ide-mcp-server.el --- MCP tools server for Gemini Cli IDE  -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Enze Chi
;; Keywords: ai, gemini, mcp, tools

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

;; This module provides an MCP tools server that exposes Emacs functions
;; to Gemini Cli.  Unlike the IDE MCP server (which uses WebSocket),
;; this uses HTTP/Streamable HTTP transport and provides access to
;; general Emacs functionality like xref, project navigation, etc.
;;
;; The server lifecycle is managed automatically:
;; - Starts when the first Gemini session begins
;; - Stops when the last Gemini session ends

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'gemini-cli-ide-debug)

;; Forward declarations
(declare-function ws-process "web-server" (server))
(declare-function gemini-cli-ide-mcp-http-server-start "gemini-cli-ide-mcp-http-server" (handler &optional port))
(declare-function gemini-cli-ide-mcp-http-server-stop "gemini-cli-ide-mcp-http-server" (server))

;;; Customization

(defgroup gemini-cli-ide-mcp-server nil
  "MCP tools server settings for Gemini Cli IDE."
  :group 'gemini-cli-ide
  :prefix "gemini-cli-ide-mcp-server-")

(defcustom gemini-cli-ide-enable-mcp-server nil
  "Enable MCP tools server for exposing Emacs functions to Gemini.
When enabled, a separate MCP server will be started to provide
Gemini with access to configured Emacs functions."
  :type 'boolean
  :group 'gemini-cli-ide-mcp-server)

(defcustom gemini-cli-ide-mcp-server-port nil
  "Port for the MCP tools server.
If nil, a random available port will be selected automatically."
  :type '(choice (const :tag "Auto-select" nil)
                 (integer :tag "Fixed port"))
  :group 'gemini-cli-ide-mcp-server)

(defcustom gemini-cli-ide-mcp-server-tools nil
  "Alist of Emacs functions to expose via MCP tools server.
Each entry is (FUNCTION . PLIST) where PLIST contains:
  :description - Human-readable description of the function
  :parameters - List of parameter specifications, each with:
    :name - Parameter name
    :type - Parameter type (string, number, boolean)
    :required - Whether parameter is required
    :description - Parameter description"
  :type '(alist :key-type symbol
                :value-type (plist :key-type keyword
                                   :value-type sexp))
  :group 'gemini-cli-ide-mcp-server)

;;; State Management

(defvar gemini-cli-ide-mcp-server--server nil
  "The MCP tools server process.")

(defvar gemini-cli-ide-mcp-server--port nil
  "The port the MCP tools server is running on.")

(defvar gemini-cli-ide-mcp-server--session-count 0
  "Number of active Gemini sessions using the MCP tools server.")

(defvar gemini-cli-ide-mcp-server--sessions (make-hash-table :test 'equal)
  "Hash table mapping session IDs to session contexts.
Each entry contains a plist with session information:
  :project-dir - The project directory for the session
  :buffer - The Gemini Cli buffer
  :start-time - When the session was started")

(defvar gemini-cli-ide-mcp-server--current-session-id nil
  "The session ID for the current MCP tool request.
This is dynamically bound during tool execution.")

;;; Tool Definition Functions

(defun gemini-cli-ide-make-tool (&rest slots)
  "Make a Gemini Cli IDE tool for MCP use.

The following keyword arguments are available:

NAME: The name of the tool, recommended to be in snake_case.

FUNCTION: The function itself (lambda or symbol) that runs the tool.

DESCRIPTION: A verbose description of what the tool does, how to
call it and what it returns.

ARGS: A list of plists specifying the arguments, or nil for a function that
takes no arguments.  Each plist in ARGS should have the following keys:
- :name - Argument name (string)
- :type - Argument type (symbol: string, number, integer, boolean, array, object, null)
- :description - Argument description (string)
- :optional - Whether the argument is optional (boolean, default nil)
- :enum - For enumerated types, a vector of allowed values
- :items - For array types, a plist describing the array items
- :properties - For object types, a plist of property specifications

CATEGORY: A string indicating a category for the tool (optional).

The tool is automatically added to `gemini-cli-ide-mcp-server-tools'.
Returns the tool specification for convenience."
  (let ((function (plist-get slots :function))
        (name (plist-get slots :name))
        (description (plist-get slots :description))
        (args (plist-get slots :args))
        (category (plist-get slots :category)))
    ;; Validate required parameters
    (unless function
      (error "Tool :function is required"))
    (unless name
      (error "Tool :name is required"))
    (unless description
      (error "Tool :description is required"))

    ;; Build the tool specification
    (let ((spec (list :function function
                      :name name
                      :description description)))
      (when args
        (setq spec (plist-put spec :args args)))
      (when category
        (setq spec (plist-put spec :category category)))
      ;; Add to the tools list
      (add-to-list 'gemini-cli-ide-mcp-server-tools spec)
      ;; Return the spec for convenience
      spec)))

;;; Format Detection and Conversion

(defun gemini-cli-ide--tool-format-p (tool-spec)
  "Determine format of TOOL-SPEC.
Returns 'old for (symbol . plist) format, 'new for plist format."
  (cond
   ;; Old format: (function-symbol :description "..." :parameters ...)
   ((and (consp tool-spec)
         (symbolp (car tool-spec))
         (not (keywordp (car tool-spec))))
    'old)
   ;; New format: (:function fn :name "..." :description "..." :args ...)
   ((and (listp tool-spec)
         (keywordp (car tool-spec)))
    'new)
   (t
    (error "Unknown tool format: %S" tool-spec))))

(defun gemini-cli-ide--normalize-tool-spec (tool-spec)
  "Convert TOOL-SPEC to normalized format for processing.
Handles both old format: (func :description ... :parameters ...)
and new format: (:function func :name ... :args ...).
Returns a consistent plist format with :args."
  (let ((format (gemini-cli-ide--tool-format-p tool-spec)))
    (cond
     ((eq format 'old)
      ;; Convert old format to new normalized format with :args
      (let* ((func (car tool-spec))
             (plist (cdr tool-spec))
             (description (plist-get plist :description))
             (parameters (plist-get plist :parameters)))
        ;; Emit deprecation warning
        (message "Warning: Tool '%s' is using deprecated format. Please use `gemini-cli-ide-make-tool' instead."
                 (symbol-name func))
        (list :function func
              :name (symbol-name func)
              :description description
              :args (gemini-cli-ide--parameters-to-args parameters))))
     ((eq format 'new)
      ;; New format - already in the right format, just return it
      tool-spec)
     (t
      (error "Cannot normalize tool spec: %S" tool-spec)))))

(defun gemini-cli-ide--parameters-to-args (parameters)
  "Convert PARAMETERS (old format) to :args (new format).
PARAMETERS is a list of plists with :name, :type, :description, :required.
Returns a list of plists with :name, :type, :description, :optional."
  (mapcar (lambda (param)
            (let ((name (plist-get param :name))
                  (type (plist-get param :type))
                  (description (plist-get param :description))
                  (required (plist-get param :required)))
              ;; Build arg spec
              (let ((arg (list :name name
                               :type (if (stringp type)
                                         (intern type)
                                       type))))
                (when description
                  (setq arg (plist-put arg :description description)))
                (unless required
                  (setq arg (plist-put arg :optional t)))
                ;; Handle additional properties
                (when-let ((enum (plist-get param :enum)))
                  (setq arg (plist-put arg :enum enum)))
                (when-let ((items (plist-get param :items)))
                  (setq arg (plist-put arg :items items)))
                (when-let ((properties (plist-get param :properties)))
                  (setq arg (plist-put arg :properties properties)))
                arg)))
          parameters))

;;; Public Functions

(defun gemini-cli-ide-mcp-server-ensure-server ()
  "Ensure the MCP tools server is running.
Starts the server if not already running.
Returns the port number on success, nil on failure."
  (when gemini-cli-ide-enable-mcp-server
    (unless (and gemini-cli-ide-mcp-server--server
                 gemini-cli-ide-mcp-server--port
                 (gemini-cli-ide-mcp-server--server-alive-p))
      (gemini-cli-ide-mcp-server--start-server))
    gemini-cli-ide-mcp-server--port))

(defun gemini-cli-ide-mcp-server-get-port ()
  "Get the port number of the running MCP tools server.
Returns nil if server is not running."
  (when (and gemini-cli-ide-mcp-server--server
             gemini-cli-ide-mcp-server--port
             (gemini-cli-ide-mcp-server--server-alive-p))
    gemini-cli-ide-mcp-server--port))

(defun gemini-cli-ide-mcp-server-session-started (&optional session-id project-dir buffer)
  "Notify that a Gemini session has started.
If SESSION-ID, PROJECT-DIR and BUFFER are provided, register the session.
Increments the session counter."
  (cl-incf gemini-cli-ide-mcp-server--session-count)
  (gemini-cli-ide-debug "MCP session started. Count: %d"
                         gemini-cli-ide-mcp-server--session-count)
  (when (and session-id project-dir buffer)
    (gemini-cli-ide-mcp-server-register-session session-id project-dir buffer)))

(defun gemini-cli-ide-mcp-server-session-ended (&optional session-id)
  "Notify that a Gemini session has ended.
If SESSION-ID is provided, unregister that specific session.
Decrements the session counter and stops server if no sessions remain."
  (when session-id
    (gemini-cli-ide-mcp-server-unregister-session session-id))
  (when (> gemini-cli-ide-mcp-server--session-count 0)
    (cl-decf gemini-cli-ide-mcp-server--session-count)
    (gemini-cli-ide-debug "MCP session ended. Count: %d"
                           gemini-cli-ide-mcp-server--session-count)
    (when (= gemini-cli-ide-mcp-server--session-count 0)
      (gemini-cli-ide-mcp-server--stop-server))))

(defun gemini-cli-ide-mcp-server-get-config (&optional session-id)
  "Get the MCP configuration for the tools server.
If SESSION-ID is provided, includes it in the URL path.
Returns an alist suitable for JSON encoding."
  (when-let ((port (gemini-cli-ide-mcp-server-get-port)))
    (let* ((path (if session-id
                     (format "/mcp/%s" session-id)
                   "/mcp"))
           (url (format "http://localhost:%d%s" port path))
           (config `((type . "http")
                     (url . ,url))))
      `((mcpServers . ((emacs-tools . ,config)))))))

(defun gemini-cli-ide-mcp-server-get-tool-names (&optional prefix)
  "Get a list of all registered MCP tool names.
If PREFIX is provided, prepend it to each tool name.
This is useful for generating --allowedTools lists."
  (mapcar (lambda (tool-spec)
            (let* ((normalized (gemini-cli-ide--normalize-tool-spec tool-spec))
                   (tool-name (or (plist-get normalized :name)
                                  (symbol-name (plist-get normalized :function)))))
              (if prefix
                  (concat prefix tool-name)
                tool-name)))
          gemini-cli-ide-mcp-server-tools))

;;; Session Management Functions

(defun gemini-cli-ide-mcp-server-register-session (session-id project-dir buffer)
  "Register a new session with SESSION-ID, PROJECT-DIR, and BUFFER."
  (puthash session-id
           (list :project-dir project-dir
                 :buffer buffer
                 :last-active-buffer nil
                 :start-time (current-time))
           gemini-cli-ide-mcp-server--sessions)
  (gemini-cli-ide-debug "Registered MCP session %s for project %s" session-id project-dir))

(defun gemini-cli-ide-mcp-server-unregister-session (session-id)
  "Unregister the session with SESSION-ID."
  (when (gethash session-id gemini-cli-ide-mcp-server--sessions)
    (remhash session-id gemini-cli-ide-mcp-server--sessions)
    (gemini-cli-ide-debug "Unregistered MCP session %s" session-id)))

(defun gemini-cli-ide-mcp-server-get-session-context (&optional session-id)
  "Get the context for SESSION-ID or the current session.
Returns a plist with :project-dir and :buffer, or nil if not found."
  (let ((id (or session-id gemini-cli-ide-mcp-server--current-session-id)))
    (when id
      (gethash id gemini-cli-ide-mcp-server--sessions))))

(defun gemini-cli-ide-mcp-server-update-last-active-buffer (session-id buffer)
  "Update the last active buffer for SESSION-ID to BUFFER.
This should be called when the user switches to a different buffer
in the project to ensure MCP tools execute in the correct context."
  (when-let ((session (gethash session-id gemini-cli-ide-mcp-server--sessions)))
    (plist-put session :last-active-buffer buffer)
    (gemini-cli-ide-debug "Updated last active buffer for session %s to %s"
                           session-id (buffer-name buffer))))

(defmacro gemini-cli-ide-mcp-server-with-session-context (session-id &rest body)
  "Execute BODY with the context of SESSION-ID.
Sets the default-directory to the session's project directory
and makes the session's buffer current if it exists.
Prefers the last active buffer over the registered buffer."
  (declare (indent 1))
  `(let* ((context (gemini-cli-ide-mcp-server-get-session-context ,session-id))
          (project-dir (plist-get context :project-dir))
          (last-active-buffer (plist-get context :last-active-buffer))
          (registered-buffer (plist-get context :buffer))
          ;; Prefer last active buffer, fall back to registered buffer
          (buffer (or (and last-active-buffer
                           (buffer-live-p last-active-buffer)
                           last-active-buffer)
                      (and registered-buffer
                           (buffer-live-p registered-buffer)
                           registered-buffer))))
     (if (not context)
         (error "No session context found for session %s" ,session-id)
       (let ((default-directory (or project-dir default-directory)))
         (if buffer
             (with-current-buffer buffer
               ,@body)
           ,@body)))))

;;; Internal Functions

(defun gemini-cli-ide-mcp-server--server-alive-p ()
  "Check if the MCP tools server is still alive."
  (when gemini-cli-ide-mcp-server--server
    (condition-case nil
        (let ((process (ws-process gemini-cli-ide-mcp-server--server)))
          (and process (process-live-p process)))
      (error nil))))

(defun gemini-cli-ide-mcp-server--start-server ()
  "Start the MCP HTTP server."
  (condition-case err
      (progn
        ;; Load the server implementation
        (require 'gemini-cli-ide-mcp-http-server)
        ;; Check if web-server is available
        (unless (featurep 'web-server)
          (error "The web-server package is required for MCP tools support. Please install it with: M-x package-install RET web-server RET"))
        ;; Start the server
        (let ((result (gemini-cli-ide-mcp-http-server-start
                       gemini-cli-ide-mcp-server-port)))
          (setq gemini-cli-ide-mcp-server--server (car result)
                gemini-cli-ide-mcp-server--port (cdr result))
          gemini-cli-ide-mcp-server--port))
    (error
     (gemini-cli-ide-debug "Failed to start MCP server: %s"
                            (error-message-string err))
     (message "Warning: Failed to start MCP server: %s"
              (error-message-string err))
     nil)))

(defun gemini-cli-ide-mcp-server--stop-server ()
  "Stop the MCP HTTP server."
  (when gemini-cli-ide-mcp-server--server
    (condition-case err
        (progn
          (gemini-cli-ide-mcp-http-server-stop gemini-cli-ide-mcp-server--server)
          (setq gemini-cli-ide-mcp-server--server nil
                gemini-cli-ide-mcp-server--port nil)
          ;; Clear all registered sessions
          (clrhash gemini-cli-ide-mcp-server--sessions)
          (gemini-cli-ide-debug "MCP server stopped"))
      (error
       (gemini-cli-ide-debug "Error stopping MCP server: %s"
                              (error-message-string err))))))

(provide 'gemini-cli-ide-mcp-server)
;;; gemini-cli-ide-mcp-server.el ends here
