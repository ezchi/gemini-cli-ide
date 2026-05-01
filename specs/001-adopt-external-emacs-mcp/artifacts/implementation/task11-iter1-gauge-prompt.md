# Gauge Code Review — Task 11 Iteration 1

## Task
Delete obsolete tests and the WebSocket mock module from `gemini-cli-ide-tests.el`.

## Verification Criteria
1. No reference to `gemini-cli-ide-mcp-...`, `websocket-...`, `json-rpc`, `gemini-cli-ide-diagnostics`, or `gemini-cli-ide-emacs-tools` remains.
2. The embedded WebSocket mock blocks are gone.
3. `(require 'gemini-cli-ide-mcp-handlers)` is removed.
4. Orphaned `defun` helpers are pruned.
5. Byte-compile and ERT tests are green.

## Git Diff
```diff
diff --git a/gemini-cli-ide-tests.el b/gemini-cli-ide-tests.el
index 0694c48..cc58a7f 100644
--- a/gemini-cli-ide-tests.el
+++ b/gemini-cli-ide-tests.el
@@ -22,18 +22,6 @@
 ;; these packages during testing. This allows the tests to run in any environment
 ;; without external dependencies.
 ;;
-;; CRITICAL DISCOVERY: Gemini Cli tools only work when launched from VS Code/editor terminals
-;; because the extensions set these environment variables:
-;; - GEMINI_CLI_SSE_PORT: The WebSocket server port created by the extension
-;; - ENABLE_IDE_INTEGRATION: Set to "true" to enable MCP tools
-;; - FORCE_CODE_TERMINAL: Set to "true" to enable terminal features
-;;
-;; Workflow:
-;; 1. Extension creates WebSocket/MCP server on random port
-;; 2. Extension sets environment variables in terminal
-;; 3. Extension launches 'gemini' command
-;; 4. Gemini CLI reads env vars and connects to WebSocket server
-;; 5. CLI and extension communicate via WebSocket/JSON-RPC for tool calls
 
 ;;; Code:
 
@@ -61,47 +49,6 @@
   "")
 (provide 'gemini-cli-ide-debug)
 
-;; === Mock websocket module ===
-;; Try to load real websocket, otherwise provide comprehensive mocks
-(condition-case nil
-    (progn
-      (add-to-list 'load-path (expand-file-name "~/.emacs.d/.cache/straight/build/websocket/"))
-      (require 'websocket))
-  (error
-   ;; Comprehensive websocket mock implementation
-   (defun websocket-server (&rest _args)
-     "Mock websocket-server function."
-     ;; Return something that looks like a server but isn't a process
-     '(:mock-server t))
-   (defun websocket-server-close (_server)
-     "Mock websocket-server-close function."
-     nil)
-   (defun websocket-send-text (_ws _text)
-     "Mock websocket-send-text function."
-     nil)
-   (defun websocket-ready-state (_ws)
-     "Mock websocket-ready-state function."
-     'open)
-   (defun websocket-url (_ws)
-     "Mock websocket-url function."
-     "ws://localhost:12345")
-   (defun websocket-frame-text (_frame)
-     "Mock websocket-frame-text function."
-     "{}")
-   (defun websocket-frame-opcode (_frame)
-     "Mock websocket-frame-opcode function."
-     'text)
-   (defun websocket-send (_ws _frame)
-     "Mock websocket-send function."
-     nil)
-   (defun websocket-server-filter (_proc _string)
-     "Mock websocket-server-filter function."
-     nil)
-   ;; Define the structure accessors to avoid free variable warnings
-   (defvar websocket-frame nil)
-   (cl-defstruct websocket-frame opcode payload)
-   (provide 'websocket)))
-
 ;; === Mock vterm module ===
 (defvar vterm--process nil)
 (defvar vterm-buffer-name nil)
@@ -156,16 +103,6 @@
     (set-window-buffer (selected-window) buffer)
     (selected-window)))
 
-;; === Additional test-specific websocket mocks ===
-(unless (featurep 'websocket)
-  ;; Only define these if websocket wasn't loaded above
-  (defvar websocket--test-server nil
-    "Mock server for testing.")
-  (defvar websocket--test-client nil
-    "Mock client for testing.")
-  (defvar websocket--test-port 12345
-    "Mock port for testing."))
-
 ;; === Mock flycheck module ===
 ;; Mock flycheck before loading any modules that require it
 (defvar flycheck-mode nil
@@ -204,10 +141,7 @@ executes TEST-BODY, and ensures cleanup even if TEST-BODY fails."
 (defun gemini-cli-ide-tests--clear-processes ()
   "Clear the process hash table for testing.
 Ensures a clean state before each test that involves process management."
-  (clrhash gemini-cli-ide--processes)
-  ;; Also clear MCP sessions
-  (when (boundp 'gemini-cli-ide-mcp--sessions)
-    (clrhash gemini-cli-ide-mcp--sessions)))
+  (clrhash gemini-cli-ide--processes))
 
 (defun gemini-cli-ide-tests--wait-for-process (buffer)
   "Wait for the process in BUFFER to finish.
@@ -353,13 +287,15 @@ have completed before cleanup.  Waits up to 5 seconds."
   (let ((gemini-cli-ide--cli-available t)
         (gemini-cli-ide-cli-path "echo")
         (gemini-cli-ide-terminal-backend 'vterm)
-        (orig-featurep (symbol-function 'featurep)))
+        (orig-featurep (symbol-function 'featurep))
+        (orig-require (symbol-function 'require)))
     (cl-letf (((symbol-function 'featurep)
-               (lambda (sym &rest _) (if (eq sym 'vterm) nil (funcall orig-featurep sym))))
+               (lambda (sym &rest args) (if (eq sym 'vterm) nil (apply orig-featurep sym args))))
               ((symbol-function 'require)
                (lambda (feature &optional filename noerror)
-                 (unless (eq feature 'vterm)
-                   (require feature filename noerror)))))
+                 (if (eq feature 'vterm)
+                     (unless noerror (signal 'file-error (list "Cannot load" "vterm")))
+                   (funcall orig-require feature filename noerror)))))
       (should-error (gemini-cli-ide)
                     :type 'user-error))))
 
@@ -368,13 +304,15 @@ have completed before cleanup.  Waits up to 5 seconds."
   (let ((gemini-cli-ide--cli-available t)
         (gemini-cli-ide-cli-path "echo")
         (gemini-cli-ide-terminal-backend 'eat)
-        (orig-featurep (symbol-function 'featurep)))
+        (orig-featurep (symbol-function 'featurep))
+        (orig-require (symbol-function 'require)))
     (cl-letf (((symbol-function 'featurep)
-               (lambda (sym &rest _) (if (eq sym 'eat) nil (funcall orig-featurep sym))))
+               (lambda (sym &rest args) (if (eq sym 'eat) nil (apply orig-featurep sym args))))
               ((symbol-function 'require)
                (lambda (feature &optional filename noerror)
-                 (unless (eq feature 'eat)
-                   (require feature filename noerror)))))
+                 (if (eq feature 'eat)
+                     (unless noerror (signal 'file-error (list "Cannot load" "eat")))
+                   (funcall orig-require feature filename noerror)))))
       (should-error (gemini-cli-ide)
                     :type 'user-error))))
 
@@ -1094,15 +1032,6 @@ have completed before cleanup.  Waits up to 5 seconds."
 
 ;;; Run all tests
 
-(ert-deftest gemini-cli-ide-test-tab-bar-tracking ()
-  "Test that tab-bar tabs are tracked correctly."
-... (and many more deletions)
```

## Full File Content
### gemini-cli-ide-tests.el
(Content already read in previous turn)

## Verification
- Run `./scripts/compile-and-test.sh`.

## Output
Standard review format. End with VERDICT.
