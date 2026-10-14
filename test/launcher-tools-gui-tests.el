;;; launcher-tools-gui-tests.el --- Graphical tool routing checks -*- lexical-binding: t; -*-

;; GUI ONLY: run in the test VM with `bash test/vm.sh', which runs
;; test/gui.sh.  Keys go through AppKit's event queue into a disposable
;; graphical Emacs with neither Portal nor a user init, as in
;; launcher-gui-test-support.el, whose fixture these checks share.
;; Handlers are fakes returning real buffers: no dictionary, network,
;; app launch or browser is involved.

(require 'launcher-gui-test-support)

(defconst launcher-gui-tool--keys
  (append launcher-gui--keys
          '((s-v 9 1048576 "v") (M-p 35 524288 "p") (M-n 45 524288 "n")))
  "Shared keys plus paste and query history.")

(defvar launcher-gui-tool--calls nil "Queries the fake handlers got, newest first.")

(define-derived-mode launcher-gui-tool-result-mode special-mode "Definition"
  "Read-only synthetic definition.")

(defun launcher-gui-tool--definition (query)
  "Fake dictionary handler: log QUERY, return a long read-only result."
  (push query launcher-gui-tool--calls)
  (pcase query
    ("bad" (error "No definition for %s" query))
    (_ (with-current-buffer (get-buffer-create "*launcher GUI definition*")
         (let ((inhibit-read-only t))
           (erase-buffer)
           (insert (format "Definition of %s\n\n" query))
           (dotimes (i 60) (insert (format "Sense %d of %s\n" i query))))
         (launcher-gui-tool-result-mode)
         (goto-char (point-min))
         (current-buffer)))))

(defconst launcher-gui-tool--tools
  '(("d" :name "Dictionary" :query launcher-test--text-query :submit (lambda (candidate) (launcher-gui-tool--definition (plist-get candidate :data)))))
  "Tools of these checks.")

(defmacro launcher-gui-tool--with (&rest body)
  "Run BODY in the shared fixture, with this file's tools and keys."
  (declare (indent 0))
  `(launcher-gui--with-fixture
     (let ((launcher-tools launcher-gui-tool--tools)
           (launcher-gui--keys launcher-gui-tool--keys)
           (launcher-gui-tool--calls nil)
           (minibuffer-history nil)
           (kill-ring nil)
           (kill-ring-yank-pointer nil))
       (vertico-mode 1)
       (unwind-protect (progn ,@body)
         (vertico-mode -1)
         (when-let* ((buffer (get-buffer "*launcher GUI definition*")))
           (kill-buffer buffer))))))

(defun launcher-gui-tool--run (strokes)
  "Drive Launcher with STROKES; return `returned' or `quit'."
  (condition-case nil
      (progn (launcher-gui--drive #'launcher strokes) 'returned)
    (quit 'quit)))

(defun launcher-gui-query--state ()
  "Return query titles and selection from the active minibuffer."
  (let ((buffer (window-buffer (active-minibuffer-window))))
    (with-current-buffer buffer
      (list :input (minibuffer-contents-no-properties)
            :titles (mapcar (lambda (r) (plist-get r :title))
                            (launcher--query-session-results launcher--query-session))
            :selected (plist-get (launcher--results-current) :id)
            :notice (or (overlay-get (launcher--query-session-status-overlay
                                      launcher--query-session) 'after-string) "")))))

(ert-deftest launcher-gui-tool-unified-input-and-result ()
  (launcher-gui-tool--with
    (let (state)
      (should (eq 'returned
                  (launcher-gui-tool--run
                   (list "d hello" (lambda () (setq state (launcher-gui-query--state)))
                         'return))))
      (should (equal "d hello" (plist-get state :input)))
      (should (equal '("Dictionary: hello") (plist-get state :titles)))
      (should (equal '("hello") launcher-gui-tool--calls))
      (should-not (active-minibuffer-window))
      (should (eq (current-buffer) (get-buffer "*launcher GUI definition*")))
      (should (derived-mode-p 'launcher-gui-tool-result-mode))
      (should-not launcher-gui--searched))))

(ert-deftest launcher-gui-tool-buffer-display-action ()
  (launcher-gui-tool--with
    (let* ((origin (selected-window))
           (origin-buffer (window-buffer origin))
           (submits 0) (displays 0)
           (action `((,(lambda (buffer alist)
                         (cl-incf displays)
                         (should-not (active-minibuffer-window))
                         (or (display-buffer-reuse-window buffer alist)
                             (display-buffer-pop-up-window buffer alist))))
                     (inhibit-same-window . t)))
           (launcher-tools
            `(("d" :name "Dictionary" :query launcher-test--text-query
                    :submit ,(lambda (candidate)
                               (cl-incf submits)
                               (list :buffer (launcher-gui-tool--definition
                                              (plist-get candidate :data))
                                     :action action))))))
      (dotimes (_ 2)
        (select-window origin)
        (should (eq 'returned (launcher-gui-tool--run '("d hello" return))))
        (should (eq (window-buffer origin) origin-buffer))
        (should-not (eq (selected-window) origin))
        (should (eq (current-buffer) (get-buffer "*launcher GUI definition*")))
        (should (= 2 (length (window-list)))))
      (should (= submits 2))
      (should (= displays 2)))))

(ert-deftest launcher-gui-tool-debounce-and-navigation ()
  (launcher-gui-tool--with
    (let* ((queries nil)
           (launcher-tools
            `(("d" :name "Dictionary" :submit launcher-test--submit-action :debounce 0.2
                    :query ,(lambda (text)
                              (unless (string-blank-p text)
                                (push text queries)
                                (list (list :id text :title text :subtitle "Definition preview" :data #'ignore)))))))
           before after navigation changed)
      (should (eq 'quit
                  (launcher-gui-tool--run
                   (list "d hello"
                         (lambda () (setq before (launcher-gui-query--state)))
                         #'ignore
                         (lambda () (setq after (launcher-gui-query--state)))
                         'down 'up
                         (lambda () (setq navigation (length queries)))
                         "x" (lambda () (setq changed (launcher-gui-query--state)))
                         'escape))))
      (should-not (plist-get before :titles))
      (should (string-match-p "Querying Dictionary" (plist-get before :notice)))
      (should (equal '("hello") (plist-get after :titles)))
      (should (= 1 navigation))
      (should-not (member "hello" (plist-get changed :titles))))))

(ert-deftest launcher-gui-tool-duplicate-title-actions ()
  (launcher-gui-tool--with
    (let* ((executed nil)
           (launcher-tools
            `((nil :name "First" :submit launcher-test--submit-action :query ,(lambda (_) (list (list :id 'same :title "Same" :data (lambda () (push 'first executed) nil)))))
              (nil :name "Second" :submit launcher-test--submit-action :query ,(lambda (_) (list (list :id 'same :title "Same" :data (lambda () (push 'second executed) nil))))))))
      (should (eq 'returned (launcher-gui-tool--run '("z" down return))))
      (should (equal '(second) executed)))))

(ert-deftest launcher-gui-tool-selection-survives-input-refresh ()
  (launcher-gui-tool--with
    (let* ((launcher-tools
            '((nil :name "Stable" :submit launcher-test--submit-action :query
                   (lambda (text)
                     (list (list :id 'one :title (concat "One " text) :data #'ignore)
                           (list :id 'two :title (concat "Two " text) :data #'ignore))))))
           before after)
      (should (eq 'quit
                  (launcher-gui-tool--run
                   (list "z" 'down (lambda () (setq before (launcher-gui-query--state)))
                         "x" (lambda () (setq after (launcher-gui-query--state))) 'escape))))
      (should (eq 'two (plist-get before :selected)))
      (should (eq 'two (plist-get after :selected)))
      (should (equal '("One zx" "Two zx") (plist-get after :titles))))))

(ert-deftest launcher-gui-tool-action-failure-and-edit ()
  (launcher-gui-tool--with
    (let (state)
      (should (eq 'returned
                  (launcher-gui-tool--run
                   (list "d bad" 'return (lambda () (setq state (launcher-gui-query--state)))
                         'backspace 'backspace 'backspace "hello" 'return))))
      (should (equal "d bad" (plist-get state :input)))
      (should (string-match-p "No definition" (plist-get state :notice)))
      (should (equal '("bad" "hello") (reverse launcher-gui-tool--calls)))
      (should (eq (current-buffer) (get-buffer "*launcher GUI definition*"))))))

(ert-deftest launcher-gui-tool-history-restores-prefix ()
  (launcher-gui-tool--with
    (let (state)
      (should (eq 'returned (launcher-gui-tool--run '("d hello" return))))
      (should (eq 'quit
                  (launcher-gui-tool--run
                   (list "d " 'M-p (lambda () (setq state (launcher-gui-query--state)))
                         'escape))))
      (should (equal "d hello" (plist-get state :input)))
      (should (equal '("hello") launcher-gui-tool--calls)))))

(ert-deftest launcher-gui-tool-cancelled-history ()
  (dolist (input '("1+2" "d hello" "d "))
    (launcher-gui-tool--with
      (let ((launcher-selection-history nil) recalled restored)
        (should (eq 'quit (launcher-gui-tool--run (list input 'quit))))
        (should (eq 'quit
                    (launcher-gui-tool--run
                     (list 'M-p (lambda () (setq recalled (launcher-gui-query--state)))
                           'M-n (lambda () (setq restored (launcher-gui-query--state)))
                           'escape))))
        (should (equal input (plist-get recalled :input)))
        (should (equal "" (plist-get restored :input)))
        (should-not launcher-selection-history)
        (should-not launcher-gui-tool--calls)
        (should-not kill-ring)))))

(ert-deftest launcher-gui-tool-history-crosses-prefixes ()
  (launcher-gui-tool--with
    (let ((minibuffer-history '("d hello" "1+2" "d ")))
      (should
       (eq 'quit
           (launcher-gui-tool--run
            (list "d "
                  'M-p (lambda () (should (equal "d hello" (plist-get (launcher-gui-query--state) :input))))
                  'M-p (lambda () (should (equal "1+2" (plist-get (launcher-gui-query--state) :input))))
                  'M-p (lambda () (should (equal "d " (plist-get (launcher-gui-query--state) :input))))
                  'M-n 'M-n 'M-n
                  (lambda () (should (equal "d " (plist-get (launcher-gui-query--state) :input))))
                  'quit)))))))

(ert-deftest launcher-gui-tool-calculator ()
  (launcher-gui-tool--with
    (let (state)
      (should (eq 'returned
                  (launcher-gui-tool--run
                   (list (lambda () (insert "1 + 2 * 3"))
                         (lambda () (setq state (launcher-gui-query--state))) 'return))))
      (should (equal "= 7" (car (plist-get state :titles))))
      (should (equal "7" (car kill-ring))))))

(provide 'launcher-tools-gui-tests)
;;; launcher-tools-gui-tests.el ends here
