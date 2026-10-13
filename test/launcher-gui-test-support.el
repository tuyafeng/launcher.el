;;; launcher-gui-test-support.el --- Graphical launcher test fixture -*- lexical-binding: t; -*-

;; GUI ONLY: run in the test VM with `bash test/vm.sh', which runs
;; test/gui.sh.  Keys go through AppKit's event queue into a disposable
;; graphical Emacs with neither Portal nor a user init.  Launching and
;; browsing are stubbed: nothing is opened.

(require 'ert)
(require 'cl-lib)
(require 'launcher)
(require 'launcher-test-support)
(require 'vertico)

(declare-function launcher-test-key "native-input" (code flags text))
(declare-function launcher-test-capture "native-input" (file))

(defconst launcher-gui--apps
  (mapcar (lambda (name) (cons name (format "/Applications/%s.app" name)))
          '("Activity Monitor" "Calculator" "Calendar" "Chess" "Contacts"
            "Dictionary" "Mail" "Maps" "Notes"))
  "Fixed application index for graphical checks.")

(defconst launcher-gui--codes
  '((?a . 0) (?b . 11) (?c . 8) (?d . 2) (?e . 14) (?f . 3) (?g . 5)
    (?h . 4) (?i . 34) (?j . 38) (?k . 40) (?l . 37) (?m . 46) (?n . 45)
    (?o . 31) (?p . 35) (?q . 12) (?r . 15) (?s . 1) (?t . 17) (?u . 32)
    (?v . 9) (?w . 13) (?x . 7) (?y . 16) (?z . 6) (?\s . 49) (?! . 18)
    (?0 . 29))
  "macOS virtual key codes of the characters the checks type.")

(defconst launcher-gui--keys
  `((return 36 0 "\r") (escape 53 0 "\e") (backspace 51 0 "\177")
    (down 125 0 ,(string #xf701)) (up 126 0 ,(string #xf700)))
  "Named keys the checks press, as (NAME CODE FLAGS TEXT).")

(defvar launcher-gui--launched nil "Apps the stubbed launcher opened.")
(defvar launcher-gui--searched nil "URLs the stubbed browser opened.")

(defun launcher-gui--strokes (strokes)
  "Expand STROKES into native key presses, quits and actions.
A string types its characters, a symbol presses a named key or quits,
and a function is an action called between key presses."
  (mapcan (lambda (stroke)
            (cond ((stringp stroke)
                   (mapcar (lambda (char)
                             (list (alist-get (downcase char) launcher-gui--codes)
                                   (if (or (<= ?A char ?Z) (= char ?!)) 131072 0)
                                   (string char)))
                           stroke))
                  ((eq stroke 'quit) (list 'quit))
                  ((symbolp stroke)
                   (list (or (cdr (assq stroke launcher-gui--keys))
                             (error "Unknown key %s" stroke))))
                  (t (list stroke))))
          strokes))

(defun launcher-gui--drive (start strokes)
  "Call START with STROKES delivered by the event loop; return its value.
Strokes go one at a time, by timer, while START waits for input.  Native
C-g is held by this Emacs build, so `quit' queues the Emacs event."
  (let* ((queue (launcher-gui--strokes strokes))
         ;; ERT runs from a timer, where quitting is inhibited.
         (inhibit-quit nil)
         (quit-flag nil)
         (timer (run-with-timer
                 0.15 0.15
                 (lambda ()
                   (when queue
                     (let ((stroke (pop queue)))
                       (cond ((functionp stroke)
                              (redisplay t)
                              (funcall stroke))
                             ((eq stroke 'quit)
                              (setq unread-command-events
                                    (append unread-command-events '(7))))
                             (t (apply #'launcher-test-key stroke)))))))))
    (unwind-protect
        (with-timeout (30 (error "Launcher check timed out; pending: %S" queue))
          (funcall start))
      (cancel-timer timer)
      (should-not queue))))

(defun launcher-gui--capture (name)
  "Save the frame's drawing as NAME.png in $LAUNCHER_TEST_SCREENSHOTS."
  (when-let* ((directory (getenv "LAUNCHER_TEST_SCREENSHOTS")))
    (redisplay t)
    (launcher-test-capture (expand-file-name (concat name ".png") directory))))

(defun launcher-gui--selected-row (string)
  "Return the line number of Vertico's selected candidate in STRING."
  (let ((pos 0) found)
    (while (and pos (not found) (< pos (length string)))
      (if (memq 'vertico-current (ensure-list (get-text-property pos 'face string)))
          (setq found pos)
        (setq pos (next-single-property-change pos 'face string))))
    (and found (cl-count ?\n string :end found))))

(defmacro launcher-gui--with-fixture (&rest body)
  "Run BODY in a fresh window layout with stubbed apps and browser.
Afterwards, check that no query hooks, timers or keymaps remain."
  (declare (indent 0))
  `(progn
     (skip-unless (eq window-system 'ns))
     (let* ((frames (frame-list))
            (frame (selected-frame))
            (size (list (frame-pixel-width frame) (frame-pixel-height frame)))
            (position (frame-position frame))
            (origin (get-buffer-create "*launcher GUI origin*"))
            (mode-line (default-value 'mode-line-format))
            (setup-hook minibuffer-setup-hook)
            (exit-hook (default-value 'minibuffer-exit-hook))
            (emulation emulation-mode-map-alists)
            (launcher--apps launcher-gui--apps)
            ;; The fake apps have no icons; launcher-icons-gui-tests.el
            ;; checks real ones.
            (launcher-show-icons nil)
            (launcher-gui--launched nil)
            (launcher-gui--searched nil)
            (vertico-count 6))
       (delete-other-windows)
       (with-current-buffer origin
         (erase-buffer)
         (dotimes (i 200) (insert (format "Origin line %d\n" i)))
         (goto-char (point-min))
         (forward-line 40))
       (switch-to-buffer origin)
       (set-window-start nil (save-excursion (forward-line -10) (point)))
       (unwind-protect
           (cl-letf (((symbol-function 'launcher--launch)
                      (lambda (path) (push path launcher-gui--launched)))
                     ((symbol-function 'browse-url)
                      (lambda (url &rest _) (push url launcher-gui--searched))))
             ,@body)
         (delete-other-windows)
         (switch-to-buffer origin))
       (should (equal emulation emulation-mode-map-alists))
       (should (equal setup-hook minibuffer-setup-hook))
       (should (equal exit-hook (default-value 'minibuffer-exit-hook)))
       (should (eq mode-line (default-value 'mode-line-format)))
       (should-not (active-minibuffer-window))
       (should (zerop (recursion-depth)))
       (should (zerop (window-vscroll (minibuffer-window frame) t)))
       ;; No query timers may outlive the reader.
       (should-not (cl-some #'launcher-gui--launcher-timer-p
                            (append timer-list timer-idle-list)))
       (should (equal frames (frame-list)))
       (should (equal size (list (frame-pixel-width frame) (frame-pixel-height frame))))
       (should (equal position (frame-position frame))))))

(defun launcher-gui--launcher-timer-p (timer)
  "Return non-nil if TIMER would call a Launcher function."
  (let ((function (timer--function timer)))
    (and (symbolp function)
         (string-prefix-p "launcher-" (symbol-name function))
         (not (string-prefix-p "launcher-gui-" (symbol-name function))))))

(defun launcher-gui-run-and-exit ()
  "Run the graphical checks matching $LAUNCHER_TEST_SELECTOR, then exit.
Exit 0 only if all ran as expected, with none skipped or quit."
  (let ((status 1))
    (message "Environment: %s; repository %s; %s; Vertico %s; frame %sx%s chars, %sx%s px, font %s"
             (emacs-version) emacs-repository-version
             (string-trim (shell-command-to-string "sw_vers -productVersion"))
             (with-temp-buffer
               (insert-file-contents (locate-library "vertico.el"))
               (and (re-search-forward "^;; Version: \\(.*\\)" nil t) (match-string 1)))
             (frame-width) (frame-height) (frame-pixel-width) (frame-pixel-height)
             (face-attribute 'default :family))
    (unwind-protect
        (condition-case err
            (let ((stats (ert-run-tests-batch (getenv "LAUNCHER_TEST_SELECTOR"))))
              (when (and (> (ert-stats-total stats) 0)
                         (= (ert-stats-completed-expected stats)
                            (ert-stats-total stats)))
                (setq status 0)))
          (error (message "Launcher GUI checks failed: %S\n%s" err
                          (with-output-to-string (backtrace)))))
      (with-current-buffer "*Messages*"
        (princ (buffer-string) #'external-debugging-output))
      (kill-emacs status))))

(provide 'launcher-gui-test-support)
;;; launcher-gui-test-support.el ends here
