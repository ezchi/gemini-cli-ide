;;; gemini-cli-ide-mcp.el --- MCP server for Gemini Cli IDE  -*- lexical-binding: t; -*-

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

;; This file implements an MCP (Model Context Protocol) server for Gemini Cli IDE.
;; It provides a WebSocket server that Gemini CLI can connect to, handling JSON-RPC
;; messages and exposing Emacs functionality through MCP tools.

;;; Code:

;; Declare external functions for byte-compilation
(declare-function websocket-server "websocket" (port &rest plist))
(declare-function websocket-server-close "websocket" (server))
(declare-function websocket-server-filter "websocket" (proc string))
(declare-function websocket-send-text "websocket" (ws text))
(declare-function websocket-send "websocket" (ws frame))
(declare-function websocket-ready-state "websocket" (websocket))
(declare-function websocket-url "websocket" (websocket))
(declare-function websocket-frame-text "websocket" (frame))
(declare-function websocket-frame-opcode "websocket" (frame))
(declare-function make-websocket-frame "websocket" (&rest args))

;; Require websocket at runtime to avoid batch mode issues
(unless (featurep 'websocket)
  (condition-case err
      (require 'websocket)
    (error
     (gemini-cli-ide-debug "Failed to load websocket package: %s" (error-message-string err)))))
(require 'json)
(require 'cl-lib)
(require 'project)
(require 'url-parse)
(require 'gemini-cli-ide-debug)
(require 'gemini-cli-ide-mcp-handlers)
(require 'gemini-cli-ide-mcp-server)

;; External declarations
(defvar gemini-cli-ide--session-ids)
(declare-function gemini-cli-ide-mcp--build-tool-list "gemini-cli-ide-mcp-handlers" ())
(declare-function gemini-cli-ide-mcp--build-tool-schemas "gemini-cli-ide-mcp-handlers" ())
(declare-function gemini-cli-ide-mcp--build-tool-descriptions "gemini-cli-ide-mcp-handlers" ())

;;; Constants

(defconst gemini-cli-ide-mcp-version "2024-11-05"
  "MCP protocol version.")

(defconst gemini-cli-ide-mcp-port-range '(10000 . 65535)
  "Port range for WebSocket server.")

(defconst gemini-cli-ide-mcp-max-port-attempts 100
  "Maximum number of attempts to find a free port.")

(defconst gemini-cli-ide-mcp-ping-interval 30
  "Interval in seconds between ping messages to keep connection alive.")

(defconst gemini-cli-ide-mcp-selection-delay 0.05
  "Delay in seconds before sending selection changes to avoid flooding.")

(defconst gemini-cli-ide-mcp-initial-notification-delay 0.1
  "Delay in seconds before sending initial notifications after connection.")

;;; Variables

;; Only keep the global sessions table
(defvar gemini-cli-ide-mcp--sessions (make-hash-table :test 'equal)
  "Hash table mapping project directories to MCP sessions.")

;; Buffer-local cache variables for performance optimization
(defvar-local gemini-cli-ide-mcp--buffer-project-cache nil
  "Cached project directory for the current buffer.
This avoids repeated project lookups on every cursor movement.")

(defvar-local gemini-cli-ide-mcp--buffer-session-cache nil
  "Cached MCP session for the current buffer.
This avoids repeated session lookups on every cursor movement.")

(defvar-local gemini-cli-ide-mcp--buffer-cache-tick nil
  "Modification tick when the cache was last updated.
Used to invalidate cache when buffer is modified.")

;;; Error Definition

(define-error 'mcp-error "MCP Error" 'error)

;;; Session Management

(cl-defstruct gemini-cli-ide-mcp-session
  "Structure to hold all state for a single MCP session."
  server           ; WebSocket server instance
  client           ; Connected WebSocket client
  port             ; Server port
  project-dir      ; Project directory
  deferred         ; Hash table of deferred responses
  ping-timer       ; Ping timer
  selection-timer  ; Selection tracking timer
  last-selection   ; Last selection state
  last-buffer      ; Last active buffer
  active-diffs     ; Hash table of active diffs
  original-tab)    ; Original tab-bar tab where Gemini was opened

(defun gemini-cli-ide-mcp--get-buffer-project ()
  "Get the project directory for the current buffer.
Returns the expanded project root path if a project is found,
otherwise returns nil.
Uses buffer-local cache to avoid repeated project lookups."
  ;; Check if we have a valid cache
  (if (and gemini-cli-ide-mcp--buffer-project-cache
           gemini-cli-ide-mcp--buffer-cache-tick
           (= gemini-cli-ide-mcp--buffer-cache-tick (buffer-modified-tick)))
      ;; Cache is valid, return cached value
      gemini-cli-ide-mcp--buffer-project-cache
    ;; Cache is invalid or doesn't exist, recalculate
    (let ((project-dir (when-let ((project (project-current)))
                         (expand-file-name (project-root project)))))
      ;; Update cache
      (setq gemini-cli-ide-mcp--buffer-project-cache project-dir
            gemini-cli-ide-mcp--buffer-cache-tick (buffer-modified-tick))
      project-dir)))

(defun gemini-cli-ide-mcp--get-session-for-project (project-dir)
  "Get the MCP session for PROJECT-DIR.
Returns the session structure if found, nil otherwise."
  (when project-dir
    (gethash project-dir gemini-cli-ide-mcp--sessions)))

(defun gemini-cli-ide-mcp--get-current-session ()
  "Get the MCP session for the current buffer's project.
This is a convenience function that combines
`gemini-cli-ide-mcp--get-buffer-project' and
`gemini-cli-ide-mcp--get-session-for-project'."
  (when-let ((project-dir (gemini-cli-ide-mcp--get-buffer-project)))
    (gemini-cli-ide-mcp--get-session-for-project project-dir)))

(defun gemini-cli-ide-mcp--find-session-by-websocket (ws)
  "Find the MCP session that owns the WebSocket WS.
Searches through all active sessions to find the one with matching client.
Returns the session if found, nil otherwise."
  (let ((found-session nil))
    (maphash (lambda (_project-dir session)
               (when (eq (gemini-cli-ide-mcp-session-client session) ws)
                 (setq found-session session)))
             gemini-cli-ide-mcp--sessions)
    found-session))

(defun gemini-cli-ide-mcp--active-sessions ()
  "Return a list of all active MCP sessions."
  (let ((sessions '()))
    (maphash (lambda (_project-dir session)
               (push session sessions))
             gemini-cli-ide-mcp--sessions)
    sessions))

;;; Backward Compatibility Layer

;;; Lockfile Management

(defun gemini-cli-ide-mcp--lockfile-directory ()
  "Return the directory for MCP lockfiles."
  (expand-file-name "~/.gemini/ide/"))

(defun gemini-cli-ide-mcp--lockfile-path (port)
  "Return the lockfile path for PORT."
  (format "%s%d.lock" (gemini-cli-ide-mcp--lockfile-directory) port))

(defun gemini-cli-ide-mcp--create-lockfile (port project-dir)
  "Create a lockfile for PORT with server information for PROJECT-DIR."
  (let* ((lockfile-dir (gemini-cli-ide-mcp--lockfile-directory))
         (lockfile-path (gemini-cli-ide-mcp--lockfile-path port))
         (workspace-folders (vector project-dir))
         (lockfile-content `((pid . ,(emacs-pid))
                             (workspaceFolders . ,workspace-folders)
                             (ideName . "Emacs")
                             (transport . "ws"))))
    ;; Ensure directory exists
    (make-directory lockfile-dir t)
    ;; Write lockfile directly without temp file
    (condition-case err
        (with-temp-file lockfile-path
          (insert (json-encode lockfile-content)))
      (error
       (gemini-cli-ide-debug "Failed to create lockfile: %s" err)
       (signal 'mcp-error (list (format "Failed to create lockfile: %s" (error-message-string err))))))))

(defun gemini-cli-ide-mcp--remove-lockfile (port)
  "Remove the lockfile for PORT."
  (when port
    (let ((lockfile-path (gemini-cli-ide-mcp--lockfile-path port)))
      (gemini-cli-ide-debug "Attempting to remove lockfile: %s" lockfile-path)
      (if (file-exists-p lockfile-path)
          (progn
            (delete-file lockfile-path)
            (gemini-cli-ide-debug "Lockfile deleted: %s" lockfile-path))
        (gemini-cli-ide-debug "Lockfile not found: %s" lockfile-path)))))


;;; JSON-RPC Message Handling

(defun gemini-cli-ide-mcp--make-response (id result)
  "Create a JSON-RPC response with ID and RESULT."
  `((jsonrpc . "2.0")
    (id . ,id)
    (result . ,result)))

(defun gemini-cli-ide-mcp--make-error-response (id code message &optional data)
  "Create a JSON-RPC error response with ID, CODE, MESSAGE and optional DATA."
  `((jsonrpc . "2.0")
    (id . ,id)
    (error . ((code . ,code)
              (message . ,message)
              ,@(when data `((data . ,data)))))))

(defun gemini-cli-ide-mcp--send-notification (method params)
  "Send a JSON-RPC notification with METHOD and PARAMS to the current session."
  ;; Try to find the session for the current buffer
  (when-let* ((project-dir (gemini-cli-ide-mcp--get-buffer-project))
              (session (gemini-cli-ide-mcp--get-session-for-project project-dir))
              (client (gemini-cli-ide-mcp-session-client session)))
    (let ((message `((jsonrpc . "2.0")
                     (method . ,method)
                     (params . ,params))))
      (gemini-cli-ide-debug "Sending notification: %s" (json-encode message))
      (condition-case err
          (progn
            (websocket-send-text client (json-encode message))
            (gemini-cli-ide-debug "Sent %s notification" method))
        (error
         (gemini-cli-ide-debug "Failed to send notification %s: %s" method err))))))

(defun gemini-cli-ide-mcp--handle-initialize (id _params)
  "Handle the initialize request with ID."
  (gemini-cli-ide-debug "Handling initialize request with id: %s" id)
  ;; Start ping timer after successful initialization
  ;; DISABLED: Ping causing connection issues - needs investigation
  ;; (gemini-cli-ide-mcp--start-ping-timer)

  ;; Send tools/list_changed notification after initialization
  (gemini-cli-ide-debug "Scheduling tools/list_changed notification")
  (run-with-timer gemini-cli-ide-mcp-initial-notification-delay nil
                  (lambda ()
                    (gemini-cli-ide-debug "Sending tools/list_changed notification")
                    (gemini-cli-ide-mcp--send-notification
                     "notifications/tools/list_changed"
                     (make-hash-table :test 'equal))))

  (let ((response `((protocolVersion . ,gemini-cli-ide-mcp-version)
                    (capabilities . ((tools . ((listChanged . t)))
                                     (resources . ((subscribe . :json-false)
                                                   (listChanged . :json-false)))
                                     (prompts . ((listChanged . t)))
                                     (logging . ,(make-hash-table :test 'equal))))
                    (serverInfo . ((name . "gemini-cli-ide-mcp")
                                   (version . "0.1.0"))))))
    (gemini-cli-ide-debug "Initialize response capabilities: tools.listChanged=%s, resources.subscribe=%s, resources.listChanged=%s, prompts.listChanged=%s"
                           t :json-false :json-false t)
    (gemini-cli-ide-mcp--make-response id response)))

(defun gemini-cli-ide-mcp--prepare-schema-for-json (schema)
  "Prepare SCHEMA for JSON encoding.
Converts :json-empty to empty hash tables which json-encode will
turn into {}. Recursively processes nested structures."
  (cond
   ;; If it's :json-empty, return an empty alist which json-encode will convert to {}
   ((eq schema :json-empty)
    (make-hash-table :test 'equal))
   ;; If it's a list, recursively process each element
   ((listp schema)
    (mapcar (lambda (item)
              (if (consp item)
                  (cons (car item) (gemini-cli-ide-mcp--prepare-schema-for-json (cdr item)))
                item))
            schema))
   ;; Otherwise return as-is
   (t schema)))

(defun gemini-cli-ide-mcp--handle-tools-list (id _params)
  "Handle the tools/list request with ID."
  (gemini-cli-ide-debug "Handling tools/list request with id: %s" id)
  ;; Rebuild tool lists to respect current settings
  (setq gemini-cli-ide-mcp-tools (gemini-cli-ide-mcp--build-tool-list))
  (setq gemini-cli-ide-mcp-tool-schemas (gemini-cli-ide-mcp--build-tool-schemas))
  (setq gemini-cli-ide-mcp-tool-descriptions (gemini-cli-ide-mcp--build-tool-descriptions))
  ;; Ensure handlers are loaded
  (gemini-cli-ide-debug "Building tools list from %d registered tools"
                         (length gemini-cli-ide-mcp-tools))
  (let ((tools '()))
    (dolist (tool-entry gemini-cli-ide-mcp-tools)
      (let* ((name (car tool-entry))
             (schema (alist-get name gemini-cli-ide-mcp-tool-schemas nil nil #'string=))
             (prepared-schema (gemini-cli-ide-mcp--prepare-schema-for-json schema))
             (description (alist-get name gemini-cli-ide-mcp-tool-descriptions nil nil #'string=)))
        (gemini-cli-ide-debug "  Tool: %s (has schema: %s, has description: %s)"
                               name
                               (if schema "yes" "no")
                               (if description "yes" "no"))
        (push `((name . ,name)
                (description . ,description)
                (inputSchema . ,prepared-schema))
              tools)))
    (let* ((tools-array (vconcat (nreverse tools)))
           (response `((tools . ,tools-array))))
      (gemini-cli-ide-debug "Returning %d tools in response" (length tools-array))
      (gemini-cli-ide-mcp--make-response id response))))

(defun gemini-cli-ide-mcp--handle-prompts-list (id _params)
  "Handle the prompts/list request with ID."
  ;; Return empty prompts list for now - Gemini Cli doesn't require any prompts
  (gemini-cli-ide-mcp--make-response id '((prompts . []))))

(defun gemini-cli-ide-mcp--handle-tools-call (id params &optional session)
  "Handle the tools/call request with ID and PARAMS.
Optional SESSION contains the MCP session context."
  (gemini-cli-ide-debug "Handling tools/call request with id: %s" id)
  ;; Ensure handlers are loaded
  (let* ((tool-name (alist-get 'name params))
         (arguments (alist-get 'arguments params))
         (handler (alist-get tool-name gemini-cli-ide-mcp-tools nil nil #'string=)))
    (gemini-cli-ide-debug "Tool call: %s with arguments: %S" tool-name arguments)
    (if handler
        (condition-case err
            (progn
              (gemini-cli-ide-debug "Found handler for tool: %s" tool-name)
              (let ((result (if (member tool-name '("getDiagnostics"))
                                ;; Pass session to handlers that need it
                                (funcall handler arguments session)
                              (funcall handler arguments))))
                ;; Check if this is a deferred response
                (if (alist-get 'deferred result)
                    (progn
                      (gemini-cli-ide-debug "Tool %s returned deferred, storing id %s" tool-name id)
                      ;; Store the request ID for later in the current session
                      (let* ((unique-key (alist-get 'unique-key result))
                             (storage-key (if unique-key
                                              (format "%s-%s" tool-name unique-key)
                                            tool-name))
                             ;; Get session from result or try to find current session
                             (result-session (alist-get 'session result))
                             (session (or result-session
                                          (gemini-cli-ide-mcp--get-current-session))))
                        (if session
                            (let ((session-deferred (gemini-cli-ide-mcp-session-deferred session)))
                              (puthash storage-key id session-deferred)
                              (gemini-cli-ide-debug "Stored deferred response in session for %s"
                                                     (gemini-cli-ide-mcp-session-project-dir session)))
                          (gemini-cli-ide-debug "Warning: No session found, cannot store deferred response")))
                      ;; Don't send a response yet
                      nil)
                  ;; Normal response
                  (gemini-cli-ide-debug "Tool %s returned result: %S" tool-name result)
                  (gemini-cli-ide-mcp--make-response id `((content . ,result))))))
          (mcp-error
           (gemini-cli-ide-debug "Tool %s threw MCP error: %S" tool-name err)
           (gemini-cli-ide-mcp--make-error-response
            id -32603 (if (listp (cdr err))
                          (car (cdr err))
                        (cdr err))))
          (error
           (gemini-cli-ide-debug "Tool %s threw error: %S" tool-name err)
           (gemini-cli-ide-mcp--make-error-response
            id -32603 (format "Tool execution failed: %s" (error-message-string err)))))
      (progn
        (gemini-cli-ide-debug "Unknown tool requested: %s" tool-name)
        (gemini-cli-ide-mcp--make-error-response
         id -32601 (format "Unknown tool: %s" tool-name))))))

(defun gemini-cli-ide-mcp--handle-message (message &optional session)
  "Handle incoming JSON-RPC MESSAGE from SESSION."
  (when message
    (gemini-cli-ide-debug "Processing message with method: %s, id: %s"
                           (alist-get 'method message)
                           (alist-get 'id message))
    (let* ((method (alist-get 'method message))
           (id (alist-get 'id message))
           (params (alist-get 'params message))
           (response
            (cond
             ;; Request handlers
             ((string= method "initialize")
              (gemini-cli-ide-debug "Handling initialize request")
              (gemini-cli-ide-mcp--handle-initialize id params))
             ((string= method "tools/list")
              (gemini-cli-ide-debug "Handling tools/list request")
              (gemini-cli-ide-mcp--handle-tools-list id params))
             ((string= method "tools/call")
              (gemini-cli-ide-debug "Handling tools/call request")
              (gemini-cli-ide-mcp--handle-tools-call id params session))
             ((string= method "prompts/list")
              (gemini-cli-ide-debug "Handling prompts/list request")
              (gemini-cli-ide-mcp--handle-prompts-list id params))
             ;; Unknown method
             (id
              (gemini-cli-ide-debug "Unknown method: %s (sending error response)" method)
              (gemini-cli-ide-mcp--make-error-response
               id -32601 (format "Method not found: %s" method)))
             ;; Notification (no id) - ignore
             (t
              (gemini-cli-ide-debug "Received notification (no response needed): %s" method)
              nil))))
      ;; Send response if we have one
      (cond
       ;; We have a response to send
       (response
        (let ((client (if session
                          (gemini-cli-ide-mcp-session-client session)
                        ;; Fallback: try to find session from current buffer
                        (when-let* ((project-dir (gemini-cli-ide-mcp--get-buffer-project))
                                    (s (gemini-cli-ide-mcp--get-session-for-project project-dir)))
                          (gemini-cli-ide-mcp-session-client s)))))
          (if client
              (let ((response-text (json-encode response)))
                (gemini-cli-ide-debug "Sending response for method %s (id %s): %s" method id response-text)
                (gemini-cli-ide-debug "MCP sending response for %s: %s" method response-text)
                (condition-case err
                    (websocket-send-text client response-text)
                  (error
                   (gemini-cli-ide-debug "Error sending response: %s" err)
                   (gemini-cli-ide-debug "Error sending MCP response: %s" err))))
            (gemini-cli-ide-debug "No client connected, cannot send response"))))
       ;; No response but we have an ID (deferred response)
       ((and id (not response))
        (gemini-cli-ide-debug "No response generated for method %s (id %s) - likely deferred" method id)
        ;; Check if it's stored as deferred in any session
        (let ((tool-name (alist-get 'name params))
              (found nil))
          (when tool-name
            (maphash (lambda (_proj-dir s)
                       (when (gethash tool-name (gemini-cli-ide-mcp-session-deferred s))
                         (setq found t)))
                     gemini-cli-ide-mcp--sessions)
            (when found
              (gemini-cli-ide-debug "Confirmed: %s is waiting for deferred response" tool-name)))))
       ;; No response and no ID (notification)
       (t
        (gemini-cli-ide-debug "No response needed for notification: %s" method))))))

;;; WebSocket Server


(defun gemini-cli-ide-mcp--find-free-port ()
  "Find a free port in the configured range."
  (let ((min-port (car gemini-cli-ide-mcp-port-range))
        (max-port (cdr gemini-cli-ide-mcp-port-range))
        (max-attempts gemini-cli-ide-mcp-max-port-attempts)
        (attempts 0)
        (found-port nil))
    (gemini-cli-ide-debug "Starting port search in range %d-%d" min-port max-port)
    (while (and (< attempts max-attempts) (not found-port))
      (let* ((port (+ min-port (random (- max-port min-port))))
             (server (condition-case err
                         (progn
                           (gemini-cli-ide-debug "Trying to bind to port %d" port)
                           (let ((ws-server (websocket-server
                                             port
                                             :host "127.0.0.1"
                                             :on-open #'gemini-cli-ide-mcp--on-open
                                             :on-message #'gemini-cli-ide-mcp--on-message
                                             :on-error #'gemini-cli-ide-mcp--on-error
                                             :on-close #'gemini-cli-ide-mcp--on-close
                                             :on-ping #'gemini-cli-ide-mcp--on-ping
                                             :protocol '("mcp"))))
                             ;; Add debug filter to see raw data (only if debugging)
                             (when (and ws-server gemini-cli-ide-debug)
                               (set-process-filter ws-server
                                                   (lambda (proc string)
                                                     ;; Only log if it looks like text (not binary WebSocket frames)
                                                     (if (string-match-p "^[[:print:][:space:]]+$" string)
                                                         (gemini-cli-ide-debug "Server received text data: %S" string)
                                                       (gemini-cli-ide-debug "Server received binary frame (%d bytes)" (length string)))
                                                     (websocket-server-filter proc string))))
                             ws-server))
                       (error
                        (gemini-cli-ide-debug "Failed to bind to port %d: %s" port err)
                        (gemini-cli-ide-debug "Failed to start server on port %d: %s" port err)
                        nil))))
        (if server
            (progn
              (setq found-port (cons server port))
              (gemini-cli-ide-debug "Successfully bound to port %d" port)
              (gemini-cli-ide-debug "Server object: %S" server)
              (gemini-cli-ide-debug "WebSocket server started on port %d" port))
          (cl-incf attempts))))
    (or found-port
        (error "Could not find free port in range %d-%d" min-port max-port))))

(defun gemini-cli-ide-mcp--on-open (ws)
  "Handle new WebSocket connection WS."
  (gemini-cli-ide-debug "=== WebSocket connection opened ===")
  (gemini-cli-ide-debug "WebSocket object: %S" ws)
  (gemini-cli-ide-debug "WebSocket state: %s" (websocket-ready-state ws))
  (gemini-cli-ide-debug "WebSocket URL: %s" (websocket-url ws))

  ;; Find the session that owns this connection
  ;; We need to extract the port from the websocket connection info
  (let ((session nil)
        (port nil))
    ;; Try to extract port from the websocket string representation
    ;; Format: "websocket server on port XXXXX <127.0.0.1:YYYYY>"
    (let ((ws-string (format "%s" ws)))
      (gemini-cli-ide-debug "WebSocket string representation: %s" ws-string)
      (when (string-match "on port \\([0-9]+\\)" ws-string)
        (setq port (string-to-number (match-string 1 ws-string)))
        (gemini-cli-ide-debug "Extracted port: %d" port)))
    ;; If we couldn't extract port from string, we'll have to search all sessions
    ;; Find session by matching port
    (when port
      (maphash (lambda (_project-dir s)
                 (when (eq (gemini-cli-ide-mcp-session-port s) port)
                   (setq session s)))
               gemini-cli-ide-mcp--sessions))

    (if session
        (progn
          ;; Update session with client
          (setf (gemini-cli-ide-mcp-session-client session) ws)
          (gemini-cli-ide-debug "Gemini Cli connected to MCP server for %s"
                                 (file-name-nondirectory
                                  (directory-file-name (gemini-cli-ide-mcp-session-project-dir session))))

          ;; Send initial active editor notification if we have one in the project
          (let ((file-path (buffer-file-name))
                (project-dir (gemini-cli-ide-mcp-session-project-dir session)))
            (when (and file-path
                       project-dir
                       (string-prefix-p (expand-file-name project-dir)
                                        (expand-file-name file-path)))
              (setf (gemini-cli-ide-mcp-session-last-buffer session) (current-buffer))
              ;; Update MCP tools server's last active buffer
              (when-let ((session-id (gethash project-dir gemini-cli-ide--session-ids)))
                (gemini-cli-ide-mcp-server-update-last-active-buffer session-id (current-buffer)))
              (run-at-time gemini-cli-ide-mcp-initial-notification-delay nil
                           (lambda ()
                             (when-let ((s (gethash project-dir gemini-cli-ide-mcp--sessions)))
                               (let ((file-path (buffer-file-name)))
                                 (gemini-cli-ide-mcp--send-notification
                                  "workspace/didChangeActiveEditor"
                                  `((uri . ,(concat "file://" file-path))
                                    (path . ,file-path)
                                    (name . ,(buffer-name))))))))))
          (gemini-cli-ide-debug "Warning: Could not find session for WebSocket connection")))))

(defun gemini-cli-ide-mcp--on-message (ws frame)
  "Handle incoming WebSocket message from WS in FRAME."
  (gemini-cli-ide-debug "=== Received WebSocket frame ===")

  ;; Check if frame is actually a frame struct
  ;; In some edge cases, the websocket library might pass something else
  (condition-case err
      (progn
        ;; Try to get the opcode - this will fail if frame is not a proper struct
        (gemini-cli-ide-debug "Frame opcode: %s" (websocket-frame-opcode frame))

        ;; Find the session for this websocket
        (let ((session (gemini-cli-ide-mcp--find-session-by-websocket ws)))
          (if session
              (progn
                (let* ((text (websocket-frame-text frame))
                       (message (condition-case err
                                    (json-read-from-string text)
                                  (error
                                   (gemini-cli-ide-debug "JSON parse error: %s" err)
                                   (gemini-cli-ide-debug "Raw text: %s" text)
                                   (gemini-cli-ide-debug "Failed to parse JSON: %s" err)
                                   nil))))
                  (gemini-cli-ide-debug "Received: %s" text)
                  (gemini-cli-ide-debug "MCP received: %s" text)
                  (when message
                    (gemini-cli-ide-mcp--handle-message message session))))
            (gemini-cli-ide-debug "Warning: Could not find session for WebSocket message"))))
    (error
     ;; If we get an error accessing frame properties, log it and continue
     (gemini-cli-ide-debug "Error processing WebSocket frame: %s" err)
     (gemini-cli-ide-debug "Frame type: %s, Frame value: %S" (type-of frame) frame)
     ;; If frame is a string, it might be raw text data
     (when (stringp frame)
       (gemini-cli-ide-debug "Received raw string instead of frame: %s" frame))
     ;; Don't crash the connection, just skip this message
     nil)))

(defun gemini-cli-ide-mcp--on-error (ws type err)
  "Handle WebSocket error from WS of TYPE with ERR."
  (gemini-cli-ide-debug "=== WebSocket error ===")
  (gemini-cli-ide-debug "Error type: %s" type)
  (gemini-cli-ide-debug "Error details: %S" err)
  (gemini-cli-ide-debug "WebSocket state: %s" (websocket-ready-state ws))
  (gemini-cli-ide-log "MCP WebSocket error (%s): %s" type err))

(defun gemini-cli-ide-mcp--on-close (ws)
  "Handle WebSocket close for WS."
  (gemini-cli-ide-debug "=== WebSocket connection closed ===")

  ;; Find the session for this websocket
  (let ((session (gemini-cli-ide-mcp--find-session-by-websocket ws)))
    (when session
      ;; Clear the client in the session
      (setf (gemini-cli-ide-mcp-session-client session) nil)
      ;; Stop the ping timer for this session
      (gemini-cli-ide-mcp--stop-ping-timer session)
      (gemini-cli-ide-debug "Final WebSocket state: %s" (websocket-ready-state ws))
      (gemini-cli-ide-debug "Gemini Cli disconnected from MCP server for %s"
                             (file-name-nondirectory
                              (directory-file-name (gemini-cli-ide-mcp-session-project-dir session)))))))

(defun gemini-cli-ide-mcp--on-ping (_ws _frame)
  "Handle WebSocket ping from WS in FRAME."
  (gemini-cli-ide-debug "Received ping frame, sending pong")
  ;; websocket.el automatically sends pong response, we just log it
  )

;;; Ping/Pong Keepalive

(defun gemini-cli-ide-mcp--start-ping-timer (session)
  "Start the ping timer for keepalive for SESSION."
  (gemini-cli-ide-mcp--stop-ping-timer session)
  (let ((timer (run-with-timer gemini-cli-ide-mcp-ping-interval gemini-cli-ide-mcp-ping-interval
                               (lambda ()
                                 (gemini-cli-ide-mcp--send-ping session)))))
    (setf (gemini-cli-ide-mcp-session-ping-timer session) timer)))

(defun gemini-cli-ide-mcp--stop-ping-timer (session)
  "Stop the ping timer for SESSION."
  (when-let ((timer (gemini-cli-ide-mcp-session-ping-timer session)))
    (cancel-timer timer)
    (setf (gemini-cli-ide-mcp-session-ping-timer session) nil)))

(defun gemini-cli-ide-mcp--send-ping (session)
  "Send a ping frame to keep connection alive for SESSION."
  (when-let ((client (gemini-cli-ide-mcp-session-client session)))
    (condition-case err
        (websocket-send client
                        (make-websocket-frame :opcode 'ping
                                              :payload ""))
      (error
       (gemini-cli-ide-debug "Failed to send ping: %s" err)))))

;;; Cache Management

(defun gemini-cli-ide-mcp--invalidate-buffer-cache ()
  "Invalidate the buffer-local cache for project and session.
This should be called when the buffer's context might have changed."
  (setq gemini-cli-ide-mcp--buffer-project-cache nil
        gemini-cli-ide-mcp--buffer-session-cache nil
        gemini-cli-ide-mcp--buffer-cache-tick nil))

(defun gemini-cli-ide-mcp--setup-buffer-cache-hooks ()
  "Set up hooks to invalidate cache when buffer context changes."
  ;; Invalidate cache when file is saved to a new location
  (add-hook 'after-save-hook #'gemini-cli-ide-mcp--invalidate-buffer-cache nil t)
  ;; Invalidate cache when buffer's file association changes
  (add-hook 'after-change-major-mode-hook #'gemini-cli-ide-mcp--invalidate-buffer-cache nil t))

;;; Selection and Buffer Tracking

(defun gemini-cli-ide-mcp--track-selection ()
  "Track selection changes and notify Gemini for the current buffer's project."
  ;; Early exit for non-file buffers
  (when (buffer-file-name)
    ;; Try to use cached session first
    (let ((session (if (and gemini-cli-ide-mcp--buffer-session-cache
                            gemini-cli-ide-mcp--buffer-cache-tick
                            (= gemini-cli-ide-mcp--buffer-cache-tick (buffer-modified-tick)))
                       ;; Use cached session
                       gemini-cli-ide-mcp--buffer-session-cache
                     ;; No valid cache, look up session
                     (when-let* ((project-dir (gemini-cli-ide-mcp--get-buffer-project))
                                 (found-session (gemini-cli-ide-mcp--get-session-for-project project-dir)))
                       ;; Cache the session
                       (setq gemini-cli-ide-mcp--buffer-session-cache found-session
                             gemini-cli-ide-mcp--buffer-cache-tick (buffer-modified-tick))
                       found-session))))
      ;; Only proceed if we have a session
      (when session
        ;; Cancel any existing timer for this session
        (when-let ((timer (gemini-cli-ide-mcp-session-selection-timer session)))
          (cancel-timer timer))
        ;; Set new timer for this session
        (let ((project-dir (gemini-cli-ide-mcp-session-project-dir session)))
          (setf (gemini-cli-ide-mcp-session-selection-timer session)
                (run-with-timer gemini-cli-ide-mcp-selection-delay nil
                                (lambda ()
                                  (gemini-cli-ide-mcp--send-selection-for-project project-dir)))))))))

(defun gemini-cli-ide-mcp--send-selection-for-project (project-dir)
  "Send current selection to Gemini for PROJECT-DIR."
  (when-let ((session (gemini-cli-ide-mcp--get-session-for-project project-dir)))
    ;; Clear the timer in the session
    (setf (gemini-cli-ide-mcp-session-selection-timer session) nil)

    (let ((file-path (buffer-file-name)))
      ;; Only process if we have a client and a file-backed buffer
      (when (and (gemini-cli-ide-mcp-session-client session)
                 file-path)
        ;; Check if file is within project
        (let ((file-in-project (string-prefix-p (expand-file-name project-dir)
                                                (expand-file-name file-path))))
          (if file-in-project
              ;; File is in project - check cursor/selection changes
              (let* ((cursor-pos (point))
                     (current-state (if (use-region-p)
                                        (list cursor-pos (region-beginning) (region-end))
                                      (list cursor-pos cursor-pos cursor-pos)))
                     (last-state (gemini-cli-ide-mcp-session-last-selection session))
                     (state-changed (not (equal current-state last-state))))
                ;; Send notification if cursor or selection changed
                (when state-changed
                  (setf (gemini-cli-ide-mcp-session-last-selection session) current-state)
                  (let ((selection (gemini-cli-ide-mcp-handle-get-current-selection nil)))
                    (gemini-cli-ide-mcp--send-notification "selection_changed" selection))))
            ;; File outside project - reset selection state
            (setf (gemini-cli-ide-mcp-session-last-selection session) nil))))
      ;; Reset selection state for non-file buffers
      (unless file-path
        (setf (gemini-cli-ide-mcp-session-last-selection session) nil)))))

(defun gemini-cli-ide-mcp--send-selection ()
  "Send current selection to Gemini."
  ;; Try to find appropriate session based on current buffer
  (when-let* ((project-dir (gemini-cli-ide-mcp--get-buffer-project))
              (session (gemini-cli-ide-mcp--get-session-for-project project-dir)))
    (gemini-cli-ide-mcp--send-selection-for-project project-dir)))

(defun gemini-cli-ide-mcp--track-active-buffer ()
  "Track active buffer changes and notify Gemini for the current buffer's project."
  (let ((current-buffer (current-buffer))
        (file-path (buffer-file-name)))
    ;; Early exit for non-file buffers
    (when file-path
      ;; Try to use cached session first
      (let ((session (if (and gemini-cli-ide-mcp--buffer-session-cache
                              gemini-cli-ide-mcp--buffer-cache-tick
                              (= gemini-cli-ide-mcp--buffer-cache-tick (buffer-modified-tick)))
                         ;; Use cached session
                         gemini-cli-ide-mcp--buffer-session-cache
                       ;; No valid cache, look up session
                       (when-let* ((project-dir (gemini-cli-ide-mcp--get-buffer-project))
                                   (found-session (gemini-cli-ide-mcp--get-session-for-project project-dir)))
                         ;; Cache the session
                         (setq gemini-cli-ide-mcp--buffer-session-cache found-session
                               gemini-cli-ide-mcp--buffer-cache-tick (buffer-modified-tick))
                         found-session))))
        ;; Only proceed if we have a session with a client
        (when (and session (gemini-cli-ide-mcp-session-client session))
          (let ((project-dir (gemini-cli-ide-mcp-session-project-dir session)))
            ;; Check if this is a different buffer than last tracked
            (when (and (not (eq current-buffer (gemini-cli-ide-mcp-session-last-buffer session)))
                       ;; Only track files within the project directory
                       (string-prefix-p (expand-file-name project-dir)
                                        (expand-file-name file-path)))
              (setf (gemini-cli-ide-mcp-session-last-buffer session) current-buffer)
              ;; Update MCP tools server's last active buffer
              (when-let ((session-id (gethash project-dir gemini-cli-ide--session-ids)))
                (gemini-cli-ide-mcp-server-update-last-active-buffer session-id current-buffer))
              ;; Send notification
              (gemini-cli-ide-mcp--send-notification
               "workspace/didChangeActiveEditor"
               `((uri . ,(concat "file://" file-path))
                 (path . ,file-path)
                 (name . ,(buffer-name current-buffer)))))))))))

;;; Public API

(defun gemini-cli-ide-mcp-start (&optional project-directory)
  "Start the MCP server for PROJECT-DIRECTORY."
  (gemini-cli-ide-debug "=== Starting MCP server ===")

  (let* ((project-dir (expand-file-name (or project-directory default-directory)))
         (existing-session (gethash project-dir gemini-cli-ide-mcp--sessions)))

    ;; If there's an existing session for this project, return its port
    (if existing-session
        (progn
          (gemini-cli-ide-debug "Reusing existing session for %s" project-dir)
          (gemini-cli-ide-mcp-session-port existing-session))

      ;; Create new session
      (let* ((session (make-gemini-cli-ide-mcp-session
                       :project-dir project-dir
                       :deferred (make-hash-table :test 'equal)
                       :active-diffs (make-hash-table :test 'equal)
                       :original-tab (when (fboundp 'tab-bar--current-tab)
                                       (tab-bar--current-tab))))
             (server-and-port (gemini-cli-ide-mcp--find-free-port))
             (server (car server-and-port))
             (port (cdr server-and-port)))

        ;; Set port and server in session
        (setf (gemini-cli-ide-mcp-session-port session) port
              (gemini-cli-ide-mcp-session-server session) server)

        ;; Store session
        (puthash project-dir session gemini-cli-ide-mcp--sessions)

        (gemini-cli-ide-debug "Project directory: %s" project-dir)
        (gemini-cli-ide-debug "Creating lockfile for port %d" port)
        (gemini-cli-ide-mcp--create-lockfile port project-dir)

        ;; Set up hooks for selection and buffer tracking
        (add-hook 'post-command-hook #'gemini-cli-ide-mcp--track-selection)
        (add-hook 'post-command-hook #'gemini-cli-ide-mcp--track-active-buffer)

        (gemini-cli-ide-debug "MCP server ready on port %d" port)
        (gemini-cli-ide-debug "MCP server started on port %d for %s" port
                               (file-name-nondirectory (directory-file-name project-dir)))
        port))))

(defun gemini-cli-ide-mcp-stop-session (project-dir)
  "Stop the MCP session for PROJECT-DIR."
  (when-let ((session (gethash project-dir gemini-cli-ide-mcp--sessions)))
    (gemini-cli-ide-debug "Stopping MCP session for %s" project-dir)

    ;; Close server and client
    (when-let ((server (gemini-cli-ide-mcp-session-server session)))
      (websocket-server-close server))

    ;; Stop timers
    (when-let ((ping-timer (gemini-cli-ide-mcp-session-ping-timer session)))
      (cancel-timer ping-timer))
    (when-let ((sel-timer (gemini-cli-ide-mcp-session-selection-timer session)))
      (cancel-timer sel-timer))

    ;; Remove lockfile
    (when-let ((port (gemini-cli-ide-mcp-session-port session)))
      (gemini-cli-ide-debug "Removing lockfile for port %d" port)
      (gemini-cli-ide-mcp--remove-lockfile port))

    ;; Remove session from registry
    (remhash project-dir gemini-cli-ide-mcp--sessions)

    ;; Invalidate cache in all buffers that belong to this project
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (and gemini-cli-ide-mcp--buffer-project-cache
                   (string= gemini-cli-ide-mcp--buffer-project-cache project-dir))
          (gemini-cli-ide-mcp--invalidate-buffer-cache))))

    ;; Remove hooks if no more sessions
    (when (= 0 (hash-table-count gemini-cli-ide-mcp--sessions))
      (remove-hook 'post-command-hook #'gemini-cli-ide-mcp--track-selection)
      (remove-hook 'post-command-hook #'gemini-cli-ide-mcp--track-active-buffer))

    (gemini-cli-ide-debug "MCP server stopped for %s"
                           (file-name-nondirectory (directory-file-name project-dir)))))

(defun gemini-cli-ide-mcp-stop ()
  "Stop the MCP server for the current project or directory."
  (gemini-cli-ide-debug "Stopping MCP server...")

  ;; Try to determine which session to stop
  (let ((project-dir (gemini-cli-ide-mcp--get-buffer-project)))

    (if project-dir
        (gemini-cli-ide-mcp-stop-session project-dir)
      ;; No specific project - stop all sessions (backward compatibility)
      (let ((sessions (hash-table-keys gemini-cli-ide-mcp--sessions)))
        (if sessions
            (dolist (dir sessions)
              (gemini-cli-ide-mcp-stop-session dir))
          (gemini-cli-ide-debug "No MCP servers running"))))))

(defun gemini-cli-ide-mcp-send-at-mentioned ()
  "Send at-mentioned notification."
  ;; Only send if there's an actual region selected
  (when (use-region-p)
    (let* ((file-path (or (buffer-file-name) ""))
           (start-line (line-number-at-pos (region-beginning)))
           (end-line (line-number-at-pos (region-end))))
      (gemini-cli-ide-mcp--send-notification
       "at_mentioned"
       `((filePath . ,file-path)
         (lineStart . ,start-line)
         (lineEnd . ,end-line))))))

(defun gemini-cli-ide-mcp-complete-deferred (session tool-name result &optional unique-key)
  "Complete a deferred response for SESSION and TOOL-NAME with RESULT.
SESSION is the MCP session that owns the deferred response.
If UNIQUE-KEY is provided, it's used to disambiguate multiple deferred
responses."
  (let* ((lookup-key (if unique-key
                         (format "%s-%s" tool-name unique-key)
                       tool-name)))
    (gemini-cli-ide-debug "Complete deferred for %s" lookup-key)
    (if (not session)
        (gemini-cli-ide-debug "No session provided for completing deferred response %s" lookup-key)
      ;; Use the provided session directly
      (let* ((session-deferred (gemini-cli-ide-mcp-session-deferred session))
             (id (gethash lookup-key session-deferred)))
        (if id
            (let ((client (gemini-cli-ide-mcp-session-client session)))
              (gemini-cli-ide-debug "Found deferred response id %s in session for %s"
                                     id (gemini-cli-ide-mcp-session-project-dir session))
              (remhash lookup-key session-deferred)
              (if client
                  (let* ((response (gemini-cli-ide-mcp--make-response id `((content . ,result))))
                         (json-response (json-encode response)))
                    (gemini-cli-ide-debug "Sending deferred response: %s" json-response)
                    (websocket-send-text client json-response)
                    (gemini-cli-ide-debug "Deferred response sent"))
                (gemini-cli-ide-debug "No client connected for session, cannot send deferred response")))
          (gemini-cli-ide-debug "No deferred response found for %s" lookup-key))))))

;;; Cleanup on exit

(defun gemini-cli-ide-mcp--cleanup ()
  "Cleanup all MCP sessions on Emacs exit."
  ;; Stop all sessions
  (maphash (lambda (project-dir _session)
             (gemini-cli-ide-mcp-stop-session project-dir))
           gemini-cli-ide-mcp--sessions))

(add-hook 'kill-emacs-hook #'gemini-cli-ide-mcp--cleanup)

(provide 'gemini-cli-ide-mcp)

;;; gemini-cli-ide-mcp.el ends here
