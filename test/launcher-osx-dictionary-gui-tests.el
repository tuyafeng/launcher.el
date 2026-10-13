;;; launcher-osx-dictionary-gui-tests.el --- Real Apple Dictionary checks -*- lexical-binding: t; -*-

;; GUI ONLY, OPT-IN: run in the test VM with
;;
;;   bash test/vm.sh sh test/gui.sh '^launcher-dictionary-real-'
;;
;; These checks look words up in the VM's real Apple dictionaries with
;; the pinned osx-dictionary package, which `sh test/elpa.sh' fetches.
;; They copy the package to a temporary directory and load it from there,
;; so the first lookup builds its native helper there with the VM's
;; compiler.  Keys go through AppKit's event queue, as in the other
;; graphical checks; launching apps and browsing stay stubbed.  The log
;; records the environment and the definitions found.

(require 'launcher-tools-gui-tests)
(require 'launcher-osx-dictionary)

(defconst launcher-dictionary-real--revision "655bca5cea78440a1ac41f9cd78711b9c8aff8f3"
  "The pinned osx-dictionary commit.")

(defvar osx-dictionary-last-dictionary-file)
(defvar osx-dictionary-current-dictionary)
(defvar osx-dictionary-previous-window-configuration)
(defvar osx-dictionary--current-word)

(defvar launcher-dictionary-real--dir nil
  "Temporary copy of the package the checks load, once loaded.")

(defun launcher-dictionary-real--load ()
  "Load a temporary copy of the pinned package, once; return its directory."
  (or launcher-dictionary-real--dir
      (let ((source (expand-file-name
                     (concat ".cache/elpa/osx-dictionary.el-" launcher-dictionary-real--revision)
                     (getenv "LAUNCHER_TEST_ROOT")))
            (copy (make-temp-file "launcher-dictionary-real" t)))
        (copy-directory source (file-name-as-directory copy) nil nil t)
        ;; No choice saved by a user: search the active dictionaries.
        (setq osx-dictionary-last-dictionary-file nil)
        (load (expand-file-name "osx-dictionary.el" copy) nil 'nomessage)
        (setq launcher-dictionary-real--dir copy))))

(defun launcher-dictionary-real--command (program &rest args)
  "Return the first line PROGRAM prints with ARGS, or its failure."
  (with-temp-buffer
    (let ((status (apply #'call-process program nil t nil args)))
      (format "%s%s" (car (split-string (buffer-string) "\n"))
              (if (eql status 0) "" (format " (exit %s)" status))))))

(defmacro launcher-dictionary-real--with (&rest body)
  "Run BODY in the graphical fixture with a real dictionary tool \"d\"."
  (declare (indent 0))
  `(progn
     (launcher-dictionary-real--load)
     (launcher-gui-tool--with
       (let ((launcher-tools '(("d" :name "Dictionary" :query launcher-osx-dictionary-query :debounce 0.2
                                :query launcher-osx-dictionary-query :submit launcher-osx-dictionary-submit)))
             (osx-dictionary-current-dictionary nil)
             (osx-dictionary-previous-window-configuration nil))
         (unwind-protect (progn ,@body)
           (when (get-buffer launcher-osx-dictionary-buffer-name)
             (kill-buffer launcher-osx-dictionary-buffer-name)))
         (should-not (get-buffer "*osx-dictionary*"))
         (should-not osx-dictionary-previous-window-configuration)))))

(defun launcher-dictionary-real--result ()
  "Return the dictionary result's word and text."
  (when-let* ((buffer (get-buffer launcher-osx-dictionary-buffer-name)))
    (with-current-buffer buffer
      (list osx-dictionary--current-word
            (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest launcher-dictionary-real-result-and-keys ()
  "Show a real definition through normal display; search and quit its result."
  (launcher-dictionary-real--with
    (let ((frames (frame-list)))
      (should (eq 'returned
                  (launcher-gui-tool--run (list "d hello" #'ignore 'return))))
      (should-not (active-minibuffer-window))
      (should (eq (current-buffer) (get-buffer launcher-osx-dictionary-buffer-name)))
      (should (derived-mode-p 'osx-dictionary-mode))
      (pcase-let ((`(,word ,text) (launcher-dictionary-real--result)))
        (should (equal "hello" word))
        (should (string-match-p "greeting" text)))
      (launcher-gui--drive
       (lambda () (execute-kbd-macro "s"))
       (list (lambda () (should (equal "d " (minibuffer-contents))))
             "ice cream" #'ignore 'return))
      (pcase-let ((`(,word ,text) (launcher-dictionary-real--result)))
        (should (equal "ice cream" word))
        (should (string-match-p "frozen" text)))
      (launcher-gui--capture "dictionary-result")
      (execute-kbd-macro "q")
      (should-not (get-buffer-window launcher-osx-dictionary-buffer-name))
      (should (get-buffer launcher-osx-dictionary-buffer-name))
      (should (equal frames (frame-list))))))

(ert-deftest launcher-dictionary-real-unknown-and-pasted-unicode ()
  "Unknown words retain input; pasted Unicode displays a normal result."
  (launcher-dictionary-real--with
    (let (state)
      (should (eq 'quit
                  (launcher-gui-tool--run
                   (list "d qwxzv" #'ignore #'ignore
                         (lambda () (setq state (launcher-gui-query--state))) 'escape))))
      (should (equal "d qwxzv" (plist-get state :input)))
      (should (string-match-p "No definition" (plist-get state :notice)))
      (kill-new "d café")
      (should (eq 'returned
                  (launcher-gui-tool--run (list 's-v #'ignore 'return))))
      (should (eq (current-buffer) (get-buffer launcher-osx-dictionary-buffer-name)))
      (pcase-let ((`(,word ,text) (launcher-dictionary-real--result)))
        (should (equal "café" word))
        (should (string-match-p "restaurant" text)))
      (should-not launcher-gui--launched)
      (should-not launcher-gui--searched))))

(ert-deftest launcher-dictionary-real-environment ()
  "Record the environment of the real lookups, after them."
  (launcher-dictionary-real--load)
  (let* ((helper (launcher-osx-dictionary--find-helper))
         (dictionaries (and helper (launcher-osx-dictionary--dictionaries helper))))
    (message "Dictionary environment: osx-dictionary %s; macOS %s; %s; clang: %s; \
developer tools: %s; helper: %s; restricted to: %S; %d dictionaries listed, \
English ones: %S"
             launcher-dictionary-real--revision
             (launcher-dictionary-real--command "sw_vers" "-productVersion")
             (emacs-version)
             (launcher-dictionary-real--command "clang" "--version")
             (launcher-dictionary-real--command "xcode-select" "-p")
             helper osx-dictionary-current-dictionary (length dictionaries)
             (seq-filter (lambda (name) (string-match-p "English\\|American" name))
                         dictionaries))
    (should helper)
    (should (string-prefix-p (file-name-as-directory launcher-dictionary-real--dir)
                             helper))
    (should dictionaries)))

(provide 'launcher-osx-dictionary-gui-tests)
;;; launcher-osx-dictionary-gui-tests.el ends here
