;;; launcher-portal-gui-tests.el --- Launcher in Portal's minibuffer panel -*- lexical-binding: t; -*-
;; GUI ONLY: bash test/portal-vm.sh. Native keys exercise the panel;
;; application and browser actions are stubbed.
(require 'portal-launcher-gui-tests)
(require 'portal-acceptance-config)
(require 'launcher)
(require 'launcher-test-support)
(declare-function portal--launcher-inspect "portal-launcher" ())
(declare-function portal-test-native-post-key "test-terminal-input" (code flags text &optional windowless))
(declare-function portal-test-native-target-frame "test-terminal-input" (title))

(defconst launcher-portal--apps
  '(("Activity Monitor" . "/System/Applications/Utilities/Activity Monitor.app")
    ("Calculator" . "/System/Applications/Calculator.app")
    ("Calendar" . "/System/Applications/Calendar.app")
    ("Chess" . "/System/Applications/Chess.app")
    ("Contacts" . "/System/Applications/Contacts.app")
    ("Dictionary" . "/System/Applications/Dictionary.app")
    ("Mail" . "/System/Applications/Mail.app")
    ("Maps" . "/System/Applications/Maps.app")
    ("Notes" . "/System/Applications/Notes.app"))
  "Fixed application index for panel checks.")

(defconst launcher-portal--keys
  '((return 36 0 "\r") (escape 53 0 "\e"))
  "Named keys the checks press, as (NAME CODE FLAGS TEXT).")

(defvar launcher-portal--actions nil "Launches and searches, newest first.")

(defun launcher-portal--drive (command strokes)
  "Run COMMAND with STROKES typed into Portal's panel; return how it ended.
Strings type characters, symbols press keys, and functions run via C-].
Return `returned', `quit' or (error MESSAGE), checking that the panel closed."
  (let* ((actions nil)
         (queue (mapcan (lambda (stroke)
                          (cond ((stringp stroke) (portal-launcher-gui--text stroke))
                                ((eq stroke 'quit) (list stroke))
                                ((assq stroke launcher-portal--keys)
                                 (list (cdr (assq stroke launcher-portal--keys))))
                                ((functionp stroke)
                                 (setq actions (append actions (list stroke)))
                                 (list '(30 262144 "\C-]")))
                                (t (error "Unknown stroke %S" stroke))))
                        strokes))
         (inhibit-quit nil)
         (quit-flag nil)
         (map (make-sparse-keymap))
         (global (current-global-map))
         (depth (recursion-depth))
         (timer (run-with-timer
                 0.1 0.1
                 (lambda ()
                   (when (and queue (> (recursion-depth) depth))
                     (portal-view--with-frame-title portal-launcher--frame
                                                    #'portal-test-native-target-frame)
                     (pcase (pop queue)
                       ('quit (setq unread-command-events (append unread-command-events '(7))))
                       (stroke (apply #'portal-test-native-post-key stroke))))))))
    (set-keymap-parent map global)
    (keymap-set map "C-]" (lambda () (interactive) (funcall (pop actions))))
    (use-global-map map)
    (unwind-protect
        (prog1 (condition-case err
                   (with-timeout (60 (error "Launcher check timed out; pending: %S" queue))
                     (call-interactively command)
                     'returned)
                 (quit 'quit)
                 (error (list 'error (error-message-string err))))
          (should-not queue)
          (should-not actions)
          (should (= depth (recursion-depth)))
          (should-not (active-minibuffer-window))
          (should-not portal-launcher--session)
          (should-not (plist-get (portal--launcher-inspect) :visible)))
      (cancel-timer timer)
      (portal-test-native-target-frame nil)
      (use-global-map global))))

(defun launcher-portal--leaks ()
  "Global state that a presentation must leave as it found it."
  (list emulation-mode-map-alists
        (default-value 'post-command-hook)
        (default-value 'pre-redisplay-functions)
        minibuffer-setup-hook
        (default-value 'minibuffer-exit-hook)
        window-buffer-change-functions
        display-buffer-alist
        display-buffer-overriding-action
        (default-value 'vertico-buffer-mode)
        (default-value 'vertico-count)
        (default-value 'mode-line-format)
        (current-global-map)))

(defun launcher-portal--window-state (window)
  "Return WINDOW's buffer, start, point and dedication."
  (list (window-buffer window) (window-start window) (window-point window)
        (window-dedicated-p window)))

(defmacro launcher-portal--with (&rest body)
  "Run BODY with a panel and stubbed application and browser actions."
  (declare (indent 0))
  `(portal-launcher-gui--with-panel
     (let ((launcher--apps launcher-portal--apps)
           (launcher-show-icons nil)
           (launcher-tools launcher-test--search-tools)
           (launcher-portal--actions nil)
           (state (launcher-portal--window-state ordinary))
           (leaks (launcher-portal--leaks)))
       (cl-letf (((symbol-function 'launcher--launch)
                  (lambda (path) (push (list 'launch path) launcher-portal--actions)))
                 ((symbol-function 'browse-url)
                  (lambda (url &rest _) (push (list 'browse url) launcher-portal--actions))))
         ,@body)
       (should (equal leaks (launcher-portal--leaks)))
       (should (equal state (launcher-portal--window-state ordinary))))))

(ert-deftest launcher-portal-app-and-search ()
  (launcher-portal--with
    (let (kind)
      (should (eq 'returned
                  (launcher-portal--drive
                   'my/app-launcher
                   (list "calc"
                         (lambda ()
                           (setq kind (portal-launcher--kind portal-launcher--frame))
                           (should (equal "calc" (minibuffer-contents-no-properties)))
                           (portal-launcher-gui--capture "panel-minibuffer"))
                         'return))))
      (should (eq 'minibuffer kind)))
    (should (eq 'returned
                (launcher-portal--drive 'my/app-launcher '("!gh portal launcher" return))))
    (should (eq 'returned
                (launcher-portal--drive 'my/app-launcher '("zz top" return))))
    (should (equal (reverse launcher-portal--actions)
                   '((launch "/System/Applications/Calculator.app")
                     (browse "https://github.com/search?q=portal%20launcher")
                     (browse "https://www.google.com/search?q=zz%20top"))))))

(ert-deftest launcher-portal-cancel-and-reenter ()
  (launcher-portal--with
    (dolist (strokes '(("calc" escape) ("calc" quit)))
      (should (eq 'returned (launcher-portal--drive 'my/app-launcher strokes))))
    (should-not launcher-portal--actions)
    (should (eq 'returned
                (launcher-portal--drive 'my/app-launcher '("calc" return))))
    (should (equal '((launch "/System/Applications/Calculator.app"))
                   launcher-portal--actions))))

(defun launcher-portal-run-and-exit ()
  "Run the selected Portal checks and exit successfully only if all pass."
  (let ((status 1))
    (unwind-protect
        (condition-case err
            (let ((stats (ert-run-tests-batch (getenv "LAUNCHER_TEST_SELECTOR"))))
              (when (and (> (ert-stats-total stats) 0)
                         (= (ert-stats-completed-expected stats) (ert-stats-total stats)))
                (setq status 0)))
          (error (message "Launcher Portal checks failed: %S\n%s" err
                          (with-output-to-string (backtrace)))))
      (with-current-buffer "*Messages*"
        (princ (buffer-string) #'external-debugging-output))
      (kill-emacs status))))

(provide 'launcher-portal-gui-tests)
;;; launcher-portal-gui-tests.el ends here
