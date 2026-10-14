;;; launcher-icons-tests.el --- App icon cache checks -*- lexical-binding: t; -*-

;; Deterministic: fake apps and a cache in a temporary directory, and a
;; pipe process in place of the worker.  Needs neither macOS nor a
;; display.  test/launcher-icons-worker-tests.el checks the real worker
;; on macOS.

(require 'ert)
(require 'cl-lib)
(require 'launcher)
(require 'launcher-icons)
(require 'launcher-test-support)

(defvar launcher-icons-test--root nil
  "The temporary directory of a check's fake apps and cache.")

(defvar launcher-icons-test--spawned nil
  "Commands of the fake worker processes, latest first.")

(defun launcher-icons-test--make-process (&rest args)
  "Record the command in ARGS, and return a live pipe process.
It keeps the sentinel of ARGS, which `launcher-icons-test--exit' runs."
  (push (plist-get args :command) launcher-icons-test--spawned)
  (let ((process (make-pipe-process :name "launcher-icons-test" :noquery t)))
    (process-put process 'sentinel (plist-get args :sentinel))
    process))

(defun launcher-icons-test--exit (&optional event)
  "End the running fake worker, and run its sentinel as Emacs would.
EVENT is the sentinel's, \"finished\\n\" by default."
  (let ((process launcher-icons--process))
    (delete-process process)
    (funcall (process-get process 'sentinel) process (or event "finished\n"))))

(defmacro launcher-icons-test--with-cache (&rest body)
  "Run BODY with icons enabled, a fake worker, and a temporary
`launcher-icons-test--root', whose \"cache/\" is the icon cache."
  (declare (indent 0))
  `(let* ((launcher-icons-test--root (file-name-as-directory (make-temp-file "launcher-icons-test" t)))
          (launcher-icon-cache-directory
           (expand-file-name "cache/" launcher-icons-test--root))
          (launcher-show-icons t)
          (launcher-icons--requested (make-hash-table :test #'equal))
          (launcher-icons--process nil)
          (launcher-icons-test--spawned nil))
     (cl-letf (((symbol-function 'default-font-height) (lambda () 20))
               ((symbol-function 'launcher-icons-enabled-p) (lambda (&rest _) t))
               ((symbol-function 'make-process) #'launcher-icons-test--make-process))
       (unwind-protect
           (progn ,@body)
         (when (process-live-p launcher-icons--process)
           (delete-process launcher-icons--process))
         (delete-directory launcher-icons-test--root t)))))

(defun launcher-icons-test--app (name &optional time)
  "Make a fake app NAME under `launcher-icons-test--root', with an Info.plist modified at TIME.
Return its path."
  (let* ((app (expand-file-name name launcher-icons-test--root))
         (plist (expand-file-name "Contents/Info.plist" app)))
    (make-directory (file-name-directory plist) t)
    (write-region "<plist/>" nil plist nil 'silent)
    (when time
      (set-file-times plist (seconds-to-time time)))
    app))

(defun launcher-icons-test--draw (&optional command)
  "Do what the worker COMMAND, the latest by default, does: write its PNGs."
  (let ((pairs (nthcdr 5 (or command (car launcher-icons-test--spawned)))))
    (while pairs
      (write-region "png" nil (cadr pairs) nil 'silent)
      (setq pairs (cddr pairs)))))

(defun launcher-icons-test--foreign-png ()
  "Return a PNG name of another program's, in the configured directory.
It is named like the launcher's own, as a shared directory may hold."
  (expand-file-name (concat (make-string 40 ?a) ".png") launcher-icon-cache-directory))

(defun launcher-icons-test--apps-of (command)
  "Return the app paths COMMAND, a worker's, asks icons of."
  (seq-filter (lambda (arg) (string-suffix-p ".app" arg)) (nthcdr 5 command)))

(defun launcher-icons-test--display (path)
  "Return the display property of the prefix of the app at PATH."
  (get-text-property 0 'display (launcher-icons-prefix (launcher-icons-app path))))

(declare-function marginalia-mode "marginalia" (&optional arg))
(declare-function nerd-icons-completion-mode "nerd-icons-completion" (&optional arg))
(defvar marginalia-mode)
(defvar nerd-icons-completion-mode)

(defconst launcher-icons-test--elpa
  (expand-file-name "../.cache/elpa" (file-name-directory (or load-file-name buffer-file-name)))
  "Where `sh test/elpa.sh' puts the pinned packages.")

(dolist (package '("marginalia-2.13" "nerd-icons.el-17faac7977242b470732efd417d3bcc8eb5a830e"
                   "nerd-icons-completion-f924dd490c8c4c1066fd97a76e0dc31e303fca30"))
  (let ((directory (expand-file-name package launcher-icons-test--elpa)))
    (when (file-directory-p directory)
      (add-to-list 'load-path directory))))

;;; Cache

(ert-deftest launcher-icons-file-follows-the-app ()
  (launcher-icons-test--with-cache
    (let* ((calc (launcher-icons-test--app "Calc.app" 1000))
           (notes (launcher-icons-test--app "Notes.app" 1000))
           (file (launcher-icons--file calc)))
      (should (string-match-p launcher-icons--png-regexp (file-name-nondirectory file)))
      (should (equal (file-name-directory file) (launcher-icons--directory)))
      (should (string-prefix-p launcher-icon-cache-directory file))
      (should (equal (launcher-icons--file calc) file))
      (should-not (equal (launcher-icons--file notes) file))
      ;; An updated app gets a new icon.
      (set-file-times (expand-file-name "Contents/Info.plist" calc) (seconds-to-time 2000))
      (should-not (equal (launcher-icons--file calc) file))
      ;; Without an Info.plist, the bundle's time; without an app, none.
      (delete-file (expand-file-name "Contents/Info.plist" calc))
      (should (launcher-icons--file calc))
      (should-not (launcher-icons--file (expand-file-name "Gone.app" launcher-icons-test--root))))))

(ert-deftest launcher-icons-prepare-makes-only-missing-icons ()
  (launcher-icons-test--with-cache
    (let ((calc (launcher-icons-test--app "Calc.app"))
          (notes (launcher-icons-test--app "Notes.app"))
          (gone (expand-file-name "Gone.app" launcher-icons-test--root)))
      (launcher-icons-prepare (list calc notes gone))
      (should (= (length launcher-icons-test--spawned) 1))
      (let ((command (car launcher-icons-test--spawned)))
        (should (equal (seq-take command 5)
                       (list launcher-icons--osascript "-l" "JavaScript"
                             launcher-icons--script "64")))
        (should (equal (nthcdr 5 command)
                       (list calc (launcher-icons--file calc)
                             notes (launcher-icons--file notes)))))
      ;; While the worker runs, nothing starts another.
      (launcher-icons-prepare (list calc notes))
      (should (= (length launcher-icons-test--spawned) 1))
      ;; The worker makes Calc's icon, and fails on Notes'.
      (write-region "png" nil (launcher-icons--file calc) nil 'silent)
      (launcher-icons-test--exit)
      ;; Made icons, and those that failed, are not asked for again.
      (launcher-icons-prepare (list calc notes))
      (should (= (length launcher-icons-test--spawned) 1))
      ;; An updated app is.
      (set-file-times (expand-file-name "Contents/Info.plist" calc) (seconds-to-time 5000))
      (launcher-icons-prepare (list calc notes))
      (should (equal (launcher-icons-test--apps-of (car launcher-icons-test--spawned))
                     (list calc))))))

(ert-deftest launcher-icons-a-failed-start-is-retried ()
  (launcher-icons-test--with-cache
    (let ((calc (launcher-icons-test--app "Calc.app")))
      (cl-letf (((symbol-function 'make-process)
                 (lambda (&rest _) (error "Too many processes"))))
        (should-error (launcher-icons-prepare (list calc))))
      (should-not launcher-icons-test--spawned)
      (launcher-icons-prepare (list calc))
      (should (equal (launcher-icons-test--apps-of (car launcher-icons-test--spawned))
                     (list calc))))))

(ert-deftest launcher-icons-a-deleted-icon-is-made-again ()
  ;; As when another Emacs, sharing the cache, deletes it in a refresh.
  (launcher-icons-test--with-cache
    (let ((calc (launcher-icons-test--app "Calc.app"))
          (broken (launcher-icons-test--app "Broken.app")))
      (launcher-icons-prepare (list calc broken))
      ;; The worker makes Calc's icon, and fails on Broken's.
      (write-region "png" nil (launcher-icons--file calc) nil 'silent)
      (launcher-icons-test--exit)
      (delete-file (launcher-icons--file calc))
      (launcher-icons-prepare (list calc broken))
      (should (equal (launcher-icons-test--apps-of (car launcher-icons-test--spawned))
                     (list calc))))))

(ert-deftest launcher-icons-a-crashed-worker-is-retried ()
  (launcher-icons-test--with-cache
    (let ((calc (launcher-icons-test--app "Calc.app"))
          (notes (launcher-icons-test--app "Notes.app")))
      (launcher-icons-prepare (list calc notes))
      ;; The worker makes Calc's icon, and dies before Notes'.
      (write-region "png" nil (launcher-icons--file calc) nil 'silent)
      (launcher-icons-test--exit "killed\n")
      (launcher-icons-prepare (list calc notes))
      (should (equal (launcher-icons-test--apps-of (car launcher-icons-test--spawned))
                     (list notes))))))

(ert-deftest launcher-icons-a-stuck-worker-is-stopped ()
  ;; Even when the next launcher finds no icon it has not asked for yet.
  (launcher-icons-test--with-cache
    (let ((calc (launcher-icons-test--app "Calc.app")))
      (launcher-icons-prepare (list calc))
      (let ((stuck launcher-icons--process))
        (launcher-icons-prepare (list calc))
        (should (eq launcher-icons--process stuck))
        (process-put stuck 'start (- (float-time) launcher-icons--timeout 1))
        (launcher-icons-prepare (list calc))
        (should-not (process-live-p stuck))
        (should (process-live-p launcher-icons--process))
        ;; Its icons are asked for again.
        (should (equal (launcher-icons-test--apps-of (car launcher-icons-test--spawned))
                       (list calc)))))))

(ert-deftest launcher-icons-refresh-deletes-only-icons-of-gone-apps ()
  (launcher-icons-test--with-cache
    (let* ((calc (launcher-icons-test--app "Calc.app"))
           (notes (launcher-icons-test--app "Notes.app"))
           (other (expand-file-name "notes.txt" launcher-icon-cache-directory))
           (foreign (launcher-icons-test--foreign-png)))
      (launcher-icons-prepare (list calc notes))
      (launcher-icons-test--draw)
      (delete-process launcher-icons--process)
      (write-region "mine" nil other nil 'silent)
      (write-region "theirs" nil foreign nil 'silent)
      ;; A failed Spotlight query deletes nothing.
      (launcher-icons-index-refreshed nil)
      (should (= (length (launcher-icons--pngs)) 2))
      (launcher-icons-index-refreshed (list calc))
      (should (equal (launcher-icons--pngs) (list (launcher-icons--file calc))))
      (should (file-exists-p other))
      (should (file-exists-p foreign))
      ;; Icons that failed are tried again.
      (should (= (hash-table-count launcher-icons--requested) 0)))))

(ert-deftest launcher-icons-clear-deletes-only-icons ()
  (launcher-icons-test--with-cache
    (let ((calc (launcher-icons-test--app "Calc.app"))
          (other (expand-file-name "notes.txt" launcher-icon-cache-directory))
          (foreign (launcher-icons-test--foreign-png)))
      (launcher-icons-prepare (list calc))
      (launcher-icons-test--draw)
      (write-region "mine" nil other nil 'silent)
      (write-region "theirs" nil foreign nil 'silent)
      (let ((worker launcher-icons--process))
        (launcher-clear-icon-cache)
        (should-not (process-live-p worker)))
      (should-not (launcher-icons--pngs))
      (should (file-exists-p other))
      (should (file-exists-p foreign))
      (launcher-icons-prepare (list calc))
      (should (= (length launcher-icons-test--spawned) 2)))))

;;; Display and completion

(ert-deftest launcher-icons-app-resolves-names-and-paths-without-work ()
  (launcher-icons-test--with-cache
    (let* ((calc (launcher-icons-test--app "Calc.app"))
           (a (launcher-icons-test--app "a/Notes.app"))
           (b (launcher-icons-test--app "b/Notes.app"))
           (label (concat "Notes  (" a ")"))
           (launcher--apps (list (cons "Calculator" calc)
                                (cons label a) (cons "Notes (b)" b)))
           (icon (list :file (launcher-icons--file calc))))
      (cl-letf (((symbol-function 'launcher-refresh)
                 (lambda () (ert-fail "Icon lookup must not discover apps"))))
        (should (equal (launcher-icons-app "Calculator") icon))
        (should (equal (launcher-icons-app "Calc") icon))
        (should (equal (launcher-icons-app calc) icon))
        (should (equal (launcher-icons-app (concat calc "/")) icon))
        (let ((default-directory launcher-icons-test--root))
          (should (equal (launcher-icons-app "Calc.app") icon))
          (should (equal (launcher-icons-app "./Calc.app") icon)))
        (should (equal (launcher-icons-app label)
                       (list :file (launcher-icons--file a))))
        (should-not (launcher-icons-app "Notes"))
        (should-not (launcher-icons-app "calc"))
        (should-not (launcher-icons-app "Unknown"))
        (should-not (launcher-icons-app (concat calc "/missing.app")))
        (let ((launcher--apps nil))
          (should (equal (launcher-icons-app calc) icon))
          (should-not (launcher-icons-app "Calc")))
        (let ((launcher--apps (list (cons "Calc" calc) (cons "Calc" calc))))
          (should (equal (launcher-icons-app "Calc") icon))))
      (should-not (file-exists-p (cadr icon)))
      (should-not launcher-icons-test--spawned))))

(ert-deftest launcher-icons-pending-image-appears-without-rebuilding-candidate ()
  (launcher-icons-test--with-cache
    (let* ((app (launcher-icons-test--app "Calc.app"))
           (candidate (car (launcher--apps-candidates (list (cons "Calc" app)))))
           (icon (plist-get candidate :icon)))
      (should (equal icon (launcher-icons-app app)))
      (should (eq (car (get-text-property 0 'display (launcher-icons-prefix icon))) 'space))
      (launcher-icons-prepare (list app))
      (launcher-icons-test--draw)
      (should (eq (car (get-text-property 0 'display (launcher-icons-prefix icon))) 'image)))))

(ert-deftest launcher-icons-text-and-custom-images ()
  (launcher-icons-test--with-cache
    (let* ((text (propertize "T" 'face '(:family "Icon Font" :foreground "red")))
           (file (expand-file-name "custom.png" launcher-icons-test--root))
           (icon (list :file file)))
      (cl-letf (((symbol-function 'string-pixel-width) (lambda (&rest _) 10)))
        (let ((prefix (launcher-icons-prefix text)))
          (should (equal-including-properties (substring prefix 1 2) text))
          (should (equal (get-text-property 0 'display prefix) '(space :width (5.0))))
          (should (equal (get-text-property 2 'display prefix)
                         '(space :width (5.0) :height (20))))))
      (should (equal (launcher-icons-prefix "") (launcher-icons-prefix nil)))
      (should (equal (get-text-property 0 'display (launcher-icons-prefix icon))
                     '(space :width (20) :height (20))))
      (write-region "png" nil file nil 'silent)
      (let ((image (get-text-property 0 'display (launcher-icons-prefix icon))))
        (should (equal (plist-get (cdr image) :file) file))
        (should (= (plist-get (cdr image) :width) 20))
        (should (= (plist-get (cdr image) :height) 20)))
      (cl-letf (((symbol-function 'create-image) (lambda (&rest _) (error "Invalid PNG"))))
        (should (equal (get-text-property 0 'display (launcher-icons-prefix icon))
                       '(space :width (20) :height (20)))))
      (cl-letf (((symbol-function 'file-readable-p) #'ignore))
        (should (equal (get-text-property 0 'display (launcher-icons-prefix icon))
                       '(space :width (20) :height (20)))))
      (launcher-clear-icon-cache)
      (should (file-exists-p file)))))

(ert-deftest launcher-icons-follow-font-height ()
  "Every prefix follows the current font height, without caching its size."
  (launcher-icons-test--with-cache
    (let* ((file (expand-file-name "custom.png" launcher-icons-test--root))
           (icon (list :file file))
           (text (propertize "T" 'face '(:family "Icon Font" :height 1.5
                                                 :foreground "red")))
           (height 12))
      (write-region "png" nil file nil 'silent)
      (cl-letf (((symbol-function 'default-font-height) (lambda () height))
                ((symbol-function 'string-pixel-width) (lambda (&rest _) 10)))
        (dolist (value '(12 30 18))
          (setq height value)
          (let* ((image (get-text-property 0 'display (launcher-icons-prefix icon)))
                 (prefix (launcher-icons-prefix text))
                 (half (/ (- height 10) 2.0)))
            (should (= (plist-get (cdr image) :width) height))
            (should (= (plist-get (cdr image) :height) height))
            (should (eq (plist-get (cdr image) :ascent) 'center))
            (should (equal-including-properties (substring prefix 1 2) text))
            (should (equal (get-text-property 0 'display prefix)
                           `(space :width (,half))))
            (should (equal (get-text-property 2 'display prefix)
                           `(space :width (,half) :height (,height))))
            (dolist (blank (list nil "" '(:file "/missing/launcher-icon.png")))
              (should (equal (get-text-property 0 'display (launcher-icons-prefix blank))
                             `(space :width (,height) :height (,height)))))))))))

(ert-deftest launcher-icons-wide-text-keeps-properties ()
  "Text wider than the font-sized slot is neither clipped nor scaled."
  (launcher-icons-test--with-cache
    (let ((text (propertize "Wide" 'face '(:height 2.0 :foreground "red"))))
      (cl-letf (((symbol-function 'string-pixel-width) (lambda (&rest _) 40)))
        (let ((prefix (launcher-icons-prefix text)))
          (should (equal-including-properties (substring prefix 1 5) text))
          (should (equal (get-text-property 0 'display prefix) '(space :width (0.0))))
          (should (equal (get-text-property 5 'display prefix)
                         '(space :width (0.0) :height (20)))))))))

(ert-deftest launcher-icons-builtin-references-resolve-when-displayed ()
  (launcher-icons-test--with-cache
    (let* ((text (propertize "S" 'face '(:foreground "red")))
           (file (expand-file-name "custom.png" launcher-icons-test--root))
           (launcher-builtin-icons nil))
      (should (equal (launcher-icons-prefix '(:builtin search))
                     (launcher-icons-prefix nil)))
      (setq launcher-builtin-icons (list (cons 'search text)))
      (cl-letf (((symbol-function 'string-pixel-width) (lambda (&rest _) 10)))
        (should (equal-including-properties (launcher-icons-prefix '(:builtin search))
                                            (launcher-icons-prefix text))))
      (write-region "png" nil file nil 'silent)
      (setq launcher-builtin-icons (list (cons 'search (list :file file))))
      (should (equal-including-properties (launcher-icons-prefix '(:builtin search))
                                          (launcher-icons-prefix (list :file file))))
      ;; References in configuration values are not followed recursively.
      (setq launcher-builtin-icons '((search . (:builtin url)) (url . "U")))
      (should (equal (launcher-icons-prefix '(:builtin search))
                     (launcher-icons-prefix nil))))))

(ert-deftest launcher-icons-builtin-actions-and-search-defaults ()
  (let ((launcher-builtin-icons nil)
        (launcher-show-icons t))
    (cl-letf (((symbol-function 'string-pixel-width) (lambda (&rest _) 10)))
      (launcher-test--with-session nil
          (append launcher-test--search-tools
                  (list (launcher-search-tool "nil" :name "Default"
                                              :url "https://example/?q=%s" :icon nil)
                        (launcher-search-tool "own" :name "Own"
                                              :url "https://example/?q=%s" :icon "O")
                        (launcher-search-tool "blank" :name "Blank"
                                              :url "https://example/?q=%s" :icon "")
                        '("plain" :name "Plain" :icon nil)))
        ;; Set icons after constructing the tools and session.
        (setq launcher-builtin-icons '((calculator . "C") (search . "S") (url . "U")))
        (dolist (case '(("1+2" . "C") ("= (" . "C")
                        ("unmatched" . "S") ("HTTPS://example.org" . "U")
                        ("gh" . "S") ("gh " . "S") ("gh emacs" . "S")
                        ("nil emacs" . "S") ("own emacs" . "O")
                        ("blank emacs" . "") ("plain " . nil)))
          (launcher--query-update session (car case))
          (let ((results (launcher--query-session-results session)))
            (should (= 1 (length results)))
            (should (equal-including-properties
                     (launcher-icons-prefix (plist-get (car results) :icon))
                     (launcher-icons-prefix (cdr case))))))
        (setq launcher-builtin-icons '((search . "N")))
        (launcher--query-update session "gh emacs")
        (should (equal-including-properties
                 (launcher-icons-prefix
                  (plist-get (car (launcher--query-session-results session)) :icon))
                 (launcher-icons-prefix "N")))))))

(ert-deftest launcher-icons-builtin-references-load-alone ()
  (with-temp-buffer
    (should (zerop
             (call-process (expand-file-name invocation-name invocation-directory) nil t nil
                           "--batch" "-Q" "-L" (file-name-directory (locate-library "launcher"))
                           "--eval"
                           (prin1-to-string
                            '(progn
                               (require 'launcher-icons)
                               (prin1 (list (featurep 'launcher)
                                            (boundp 'launcher-builtin-icons)
                                            (and (launcher-icons-valid-p '(:builtin search)) t)
                                            (equal (launcher-icons-prefix '(:builtin search))
                                                   (launcher-icons-prefix nil)))))))))
    (should (equal (buffer-string) "(nil nil t t)"))))

(ert-deftest launcher-icons-global-switch-disables-all-icons ()
  (let ((enabled-p (symbol-function 'launcher-icons-enabled-p)))
    (launcher-icons-test--with-cache
      (let* ((app (launcher-icons-test--app "Calc.app"))
             (launcher-show-icons nil)
             (launcher--apps (list (cons "Calc" app)))
             (launcher-tools
              '(("text" :name "Text" :candidates ((:id text :title "Text" :icon "T"))
                 :submit ignore)))
             (completing-read-function
              (launcher-test--reader
               (list (lambda ()
                       (should (equal completion-extra-properties
                                      '(:affixation-function launcher--results-affixation)))
                       (launcher-test--input "text ")
                       (launcher-results-submit)
                       nil)))))
        (should (equal (launcher-icons-prefix "T") ""))
        (should (equal (launcher-icons-prefix (launcher-icons-app app)) ""))
        (should (equal (launcher-icons-prefix nil) ""))
        (let ((launcher-builtin-icons '((search . "S"))))
          (should (equal (launcher-icons-prefix '(:builtin search)) "")))
        (launcher-icons-prepare (list app))
        (cl-letf (((symbol-function 'launcher-icons-enabled-p) enabled-p))
          (launcher))
        (should-not launcher-icons-test--spawned)
        (should-not (file-exists-p launcher-icon-cache-directory))))))

(ert-deftest launcher-icons-candidate-icon-validation ()
  (let ((tool (launcher--tool-make :name "Test")))
    (dolist (icon (list nil "T" (propertize "T" 'face 'bold)
                     '(:file "/tmp/icon.png") '(:builtin calculator)
                     '(:builtin search) '(:builtin url)))
      (launcher--query-validate-results (list (list :id 'test :title "Test" :icon icon)) tool))
    (dolist (icon '(42 (:file) (:file nil) (:file "x" :extra t)
                    (:other "x") (image "x") (:builtin) (:builtin nil)
                    (:builtin unknown) (:builtin "search") (:builtin search :extra t)))
      (should-error (launcher--query-validate-results
                     (list (list :id 'test :title "Test" :icon icon)) tool)))
    (should-error (launcher--query-validate-results
                   '((:id test :title "Test" :icon-path "/tmp/Test.app")) tool))))

(ert-deftest launcher-icons-tool-icon-validation ()
  (dolist (icon (list nil "" "T" (propertize "T" 'face 'bold)
                     '(:file "/tmp/icon.png") '(:builtin calculator)
                     '(:builtin search) '(:builtin url)))
    (let ((tool (launcher--tool-from-entry (list "t" :name "Test" :icon icon))))
      (should (equal-including-properties (launcher--tool-icon tool) icon))))
  (dolist (icon '(42 (:file) (:file nil) (:file "x" :extra t)
                    (:other "x") (image "x") (:builtin) (:builtin nil)
                    (:builtin unknown) (:builtin "search") (:builtin search :extra t)))
    (should-error (launcher--tool-from-entry (list "t" :name "Test" :icon icon))
                  :type 'user-error)))

(ert-deftest launcher-icons-tool-inheritance-and-submission ()
  (let* ((icon (propertize "T" 'face '(:foreground "red")))
         (items '((:id inherit :title "Inherited")
                  (:id override :title "Override" :icon "O")
                  (:id blank :title "Blank" :icon nil)
                  (:id empty :title "Empty" :icon "")))
         (before (copy-tree items))
         submitted)
    (launcher-test--with-session nil
        (list (list "t" :name "Test" :icon icon :candidates items
                    :query (lambda (_) items)
                    :submit (lambda (item) (setq submitted item) nil)))
      ;; Prefix entry, then blank static query, then dynamic replacement.
      (launcher--query-update session "t")
      (should (equal-including-properties
               (plist-get (car (launcher--query-session-results session)) :icon) icon))
      (dolist (input '("t " "t query"))
        (launcher--query-update session input)
        (let ((results (launcher--query-session-results session)))
          (should (= (length results) 4))
          (should (equal-including-properties (plist-get (car results) :icon) icon))
          (should (equal (mapcar (lambda (r) (plist-get r :icon)) (cdr results))
                         '("O" nil "")))
          (should (eq (plist-get (car results) :item) (car items)))
          (launcher--result-execute (car results))
          (should (eq submitted (car items)))
          (should-not (plist-member submitted :icon))))
      (should (equal items before)))
    (launcher-test--with-session nil (list (list "t" :name "Empty" :icon icon))
      (launcher--query-update session "t ")
      (should (equal-including-properties
               (plist-get (car (launcher--query-session-results session)) :icon) icon)))))

(ert-deftest launcher-icons-inherited-icon-affixation ()
  (launcher-icons-test--with-cache
    (let* ((app (launcher-icons-test--app "Calc.app"))
           (file (launcher-icons--file app))
           (text (propertize "T" 'face 'bold)))
      (launcher-test--with-session nil
          (list (list "t" :name "Test" :icon (launcher-icons-app app)
                      :candidates '((:id one :title "One")) :submit #'ignore))
        (launcher--query-update session "t ")
        (let* ((result (car (launcher--query-session-results session)))
               (key (plist-get result :candidate))
               (prefix (lambda () (cadar (launcher--results-affixation (list key))))))
          (should (equal (get-text-property 0 'display (funcall prefix))
                         '(space :width (20) :height (20))))
          (launcher-icons-prepare (list app))
          (launcher-icons-test--draw)
          (should (equal (plist-get (cdr (get-text-property 0 'display (funcall prefix)))
                                   :file) file))
          (setf (launcher--tool-icon (plist-get result :tool)) text)
          (launcher--query-publish session)
          (cl-letf (((symbol-function 'string-pixel-width) (lambda (&rest _) 10)))
            (should (equal-including-properties (substring (funcall prefix) 1 2) text)))
          (let ((launcher-show-icons nil))
            (should (equal (funcall prefix) ""))))))))

(ert-deftest launcher-icons-prefix-shows-the-icon-or-space ()
  (launcher-icons-test--with-cache
    (let ((calc (launcher-icons-test--app "Calc.app")))
      (launcher-icons-prepare (list calc))
      (should (equal (launcher-icons-test--display calc) '(space :width (20) :height (20))))
      (should (equal (launcher-icons-test--display nil) '(space :width (20) :height (20))))
      (launcher-icons-test--draw)
      (let ((image (launcher-icons-test--display calc)))
        (should (eq (car image) 'image))
        (should (equal (plist-get (cdr image) :file) (launcher-icons--file calc)))
        (should (equal (plist-get (cdr image) :width) 20))
        (should (equal (plist-get (cdr image) :height) 20)))
      (should (equal (substring-no-properties (launcher-icons-prefix (launcher-icons-app calc))) "  ")))))

(ert-deftest launcher-icons-disabled-does-nothing ()
  (launcher-icons-test--with-cache
    (cl-letf (((symbol-function 'launcher-icons-enabled-p) #'ignore))
      (let* ((calc (launcher-icons-test--app "Calc.app"))
             (launcher--apps (list (cons "Calc" calc)))
             (completing-read-function
              (launcher-test--reader
               (list (lambda ()
                       (should (equal completion-extra-properties
                                      '(:affixation-function launcher--results-affixation)))
                       "Calc")))))
        (cl-letf (((symbol-function 'launcher--launch) #'ignore)) (launcher))
        (should-not launcher-icons-test--spawned)
        (should-not (file-exists-p launcher-icon-cache-directory))))))

(ert-deftest launcher-icons-load-starts-nothing ()
  (should-not (and (boundp 'launcher-icons--process) launcher-icons--process))
  (dolist (option '(launcher-show-icons launcher-icon-cache-directory
                   launcher-builtin-icons))
    (should (custom-variable-p option))
    (should (assq option (get 'launcher 'custom-group))))
  (should (commandp 'launcher-clear-icon-cache))
  (should (file-readable-p launcher-icons--script)))

(defun launcher-icons-test--query-all (entries)
  "Return a session with all ENTRIES and the search entry g for affixation checks."
  (let* ((session (launcher--query-create
                   entries (list (launcher--tool-from-entry
                                  (launcher-search-tool "g" :name "Google"
                                                        :url "https://google.example/?q=%s")))))
         (launcher--query-session session))
    (launcher-icons-prepare (mapcar #'cdr entries))
    (launcher--static-run session 0 (car (launcher--query-session-tools session)) "")
    (puthash 2 (list (launcher--prefix-result (nth 2 (launcher--query-session-tools session))))
             (launcher--query-session-slots session))
    (launcher--query-publish session)
    session))

(ert-deftest launcher-icons-affixation-keeps-candidates ()
  (launcher-icons-test--with-cache
    (let* ((odd (launcher-icons-test--app "It's $(rm -rf) ; `x` 名前.app"))
           (entries (list (cons "Calc" (launcher-icons-test--app "Calc.app"))
                          (cons "Notes (a)" (launcher-icons-test--app "a/Notes.app"))
                          (cons "Notes (b)" (launcher-icons-test--app "b/Notes.app"))
                          (cons "It's" odd)))
           (launcher--query-session (launcher-icons-test--query-all entries))
           (candidates (mapcar (lambda (result) (plist-get result :candidate))
                               (launcher--query-session-results launcher--query-session))))
      (launcher-icons-test--draw)
      (let ((affixed (launcher--results-affixation candidates)))
        (should (equal (mapcar #'car affixed) candidates))
        (should (string-prefix-p " " (nth 2 (car affixed))))
        (should (eq (get-text-property 1 'face (nth 2 (car affixed)))
                    'completions-annotations))
        (should (equal (substring-no-properties (nth 2 (car (last affixed)))) " Google"))
        (should (equal (get-text-property 0 'display (nth 1 (car (last affixed))))
                       '(space :width (20) :height (20))))
        (let ((files (mapcar (lambda (entry)
                              (plist-get (cdr (get-text-property 0 'display (nth 1 entry))) :file))
                            (butlast affixed))))
          (should (seq-every-p #'stringp files))
          (should (= 4 (length (delete-dups (copy-sequence files))))))
        (dolist (entry affixed) (should (equal (substring-no-properties (nth 1 entry)) "  ")))))))

(ert-deftest launcher-icons-reader-gets-affixation ()
  (launcher-icons-test--with-cache
    (let* ((calc (launcher-icons-test--app "Calc.app"))
           (launcher--apps (list (cons "Calc" calc))) launched properties metadata
           (completing-read-function
            (launcher-test--reader
             (list (lambda ()
                     (setq properties completion-extra-properties
                           metadata (completion-metadata "" minibuffer-completion-table nil))
                     "Calc")))))
      ;; A fake reader does not normally bind the table, unlike completing-read.
      (let* ((reader completing-read-function)
             (completing-read-function
              (lambda (prompt table &rest args)
                (let ((minibuffer-completion-table table)) (apply reader prompt table args)))))
        (cl-letf (((symbol-function 'launcher--launch) (lambda (path) (push path launched)))) (launcher)))
      (should (equal properties '(:affixation-function launcher--results-affixation)))
      (should (eq (completion-metadata-get metadata 'category) 'launcher-result))
      (should (equal launched (list calc)))
      (should (equal (launcher-icons-test--apps-of (car launcher-icons-test--spawned)) (list calc))))))

(ert-deftest launcher-icons-errors-never-fail-the-refresh ()
  (launcher-icons-test--with-cache
    (let ((calc (launcher-icons-test--app "Calc.app"))
          (launcher--apps nil))
      (cl-letf (((symbol-function 'launcher--collect-paths) (lambda () (list calc)))
                ((symbol-function 'launcher-icons-index-refreshed)
                 (lambda (_) (error "Broken"))))
        (launcher-refresh))
      (should (equal launcher--apps (list (cons "Calc" calc)))))))

(ert-deftest launcher-icons-coexist-with-marginalia-and-nerd-icons ()
  (skip-unless (and (require 'marginalia nil t) (require 'nerd-icons-completion nil t)))
  (launcher-icons-test--with-cache
    (let* ((calc (launcher-icons-test--app "Calc.app"))
           (launcher--query-session (launcher-icons-test--query-all (list (cons "Calc" calc))))
           (completion-extra-properties '(:affixation-function launcher--results-affixation))
           (metadata (completion-metadata "" (launcher--results-collection launcher--query-session) nil)))
      (launcher-icons-test--draw)
      (unwind-protect
          (progn
            (marginalia-mode 1) (nerd-icons-completion-mode 1)
            (let* ((fun (completion-metadata-get metadata 'affixation-function))
                   (candidates (mapcar (lambda (r) (plist-get r :candidate))
                                       (launcher--query-session-results launcher--query-session)))
                   (affixed (funcall fun candidates)))
              (should (equal (mapcar #'car affixed) candidates))
              (should (eq (car (get-text-property 0 'display (nth 1 (car affixed)))) 'image))
              (dolist (entry affixed) (should (equal (substring-no-properties (nth 1 entry)) "  ")))))
        (marginalia-mode -1) (nerd-icons-completion-mode -1)))))

;;; Annotation layout

(defun launcher-icons-test--layout-session (&rest titles)
  "Return a session with TITLES and long subtitles, without app discovery."
  (let ((session (launcher--query-session-create :candidates (make-hash-table :test #'equal))))
    (cl-loop for title in titles for index from 0 do
             (let ((candidate (propertize (format "opaque-%d" index) 'display title)))
               (puthash candidate (list :title title :subtitle (make-string 150 ?x))
                        (launcher--query-session-candidates session))))
    session))

(defun launcher-icons-test--layout-column (row)
  "Return ROW's annotation alignment coordinate."
  (get-text-property 0 'display (nth 2 row)))

(ert-deftest launcher-icons-annotations-stable-and-session-local ()
  "Filtering never shrinks the column; new long titles grow it in steps."
  (let* ((launcher--query-session (launcher-icons-test--layout-session "A" "Long name" "Much longer name"))
         (launcher-show-icons nil)
         (keys '("opaque-0" "opaque-1" "opaque-2")))
    (cl-letf (((symbol-function 'display-graphic-p) #'ignore)
              ((symbol-function 'window-body-width) (lambda (&rest _) 100)))
      (let* ((rows (launcher--results-affixation (seq-take keys 2)))
             (column (launcher-icons-test--layout-column (car rows))))
        (should (equal column '(space :align-to 12)))
        (should (equal column (launcher-icons-test--layout-column (cadr rows))))
        (should (equal column (launcher-icons-test--layout-column
                               (car (launcher--results-affixation (list (car keys)))))))
        (should (equal '(space :align-to 20)
                       (launcher-icons-test--layout-column
                        (car (launcher--results-affixation (list (nth 2 keys)))))))
        (let ((launcher--query-session (launcher-icons-test--layout-session "A")))
          (should (equal '(space :align-to 4)
                         (launcher-icons-test--layout-column
                          (car (launcher--results-affixation '("opaque-0")))))))))))

(ert-deftest launcher-icons-annotations-truncate-display-only ()
  "Wide titles and annotations fit; identities and source titles survive."
  (let* ((title (concat "中文" (make-string 80 ?a)))
         (launcher--query-session (launcher-icons-test--layout-session title))
         (launcher-show-icons nil)
         (key (car (hash-table-keys (launcher--query-session-candidates launcher--query-session)))))
    (cl-letf (((symbol-function 'display-graphic-p) #'ignore)
              ((symbol-function 'window-body-width) (lambda (&rest _) 100)))
      (let* ((row (car (launcher--results-affixation (list key))))
             (visible (get-text-property 0 'display (car row)))
             (suffix (nth 2 row)))
        (should (equal (car row) key))
        (should (eq (launcher--result-for-candidate (car row)) (launcher--result-for-candidate key)))
        (should (equal (get-text-property 0 'display key) title))
        (should (equal (plist-get (launcher--result-for-candidate key) :title) title))
        (should (<= (string-width visible) 38))
        (should (string-suffix-p "…" visible))
        (should (equal '(space :align-to 40) (launcher-icons-test--layout-column row)))
        (should (<= (string-width (substring suffix 1)) 59))
        (should (eq (get-text-property 1 'face suffix) 'completions-annotations))
        (should (eq (get-text-property (1- (length suffix)) 'face suffix)
                    'completions-annotations))
        (should (string-suffix-p "…" suffix))
        (should (equal (nth 1 row) ""))))))

(ert-deftest launcher-icons-annotations-reset-on-resize-and-font-change ()
  "Changing rendering metrics resets the session's remembered column."
  (let* ((launcher--query-session (launcher-icons-test--layout-session "A" "Long name"))
         (launcher-show-icons nil) (width 100) (height 20))
    (cl-letf (((symbol-function 'display-graphic-p) #'ignore)
              ((symbol-function 'window-body-width) (lambda (&rest _) width))
              ((symbol-function 'default-font-height) (lambda () height)))
      (launcher--results-affixation '("opaque-0" "opaque-1"))
      (setq width 80)
      (should (equal '(space :align-to 4) (launcher-icons-test--layout-column
                                         (car (launcher--results-affixation '("opaque-0"))))))
      (launcher--results-affixation '("opaque-1"))
      (setq height 30)
      (should (equal '(space :align-to 4) (launcher-icons-test--layout-column
                                         (car (launcher--results-affixation '("opaque-0")))))))))

(ert-deftest launcher-icons-annotations-pixel-prefix-and-no-subtitle ()
  "Pixel layout accounts for image slots and text prefixes, and omits absent notes."
  (let* ((launcher--query-session (launcher-icons-test--layout-session "A" "B"))
         (table (launcher--query-session-candidates launcher--query-session)))
    (setf (plist-get (gethash "opaque-0" table) :icon) '(:file "image.png")
          (plist-get (gethash "opaque-1" table) :icon) "TTTT"
          (plist-get (gethash "opaque-1" table) :subtitle) nil)
    (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
              ((symbol-function 'launcher-icons-enabled-p) (lambda (&rest _) t))
              ((symbol-function 'default-font-height) (lambda () 20))
              ((symbol-function 'window-body-width) (lambda (&rest _) 400))
              ((symbol-function 'string-pixel-width)
               (lambda (text &rest _)
                 (if (eq (car-safe (get-text-property 0 'display text)) 'image)
                     30
                   (* 10 (string-width text)))))
              ((symbol-function 'launcher-icons-prefix)
               (lambda (icon) (if (stringp icon) (concat icon " ")
                               (concat (propertize " " 'display '(image :width 20 :height 20)) " ")))))
      (let ((rows (launcher--results-affixation '("opaque-0" "opaque-1"))))
        (should (equal '(space :align-to (80)) (launcher-icons-test--layout-column (car rows))))
        (should (equal (nth 2 (cadr rows)) ""))
        (should (eq (car (get-text-property 0 'display (nth 1 (car rows)))) 'image))))))

(ert-deftest launcher-icons-titles-without-subtitles-use-row-width ()
  "Absent or blank notes release the row without moving annotation columns."
  (dolist (subtitle '(nil "" " \t\n"))
    (let* ((title (make-string 60 ?a))
           (launcher--query-session (launcher-icons-test--layout-session "A" title))
           (table (launcher--query-session-candidates launcher--query-session))
           (keys (sort (hash-table-keys table) #'string<))
           (launcher-show-icons nil)
           (width 80))
      (setf (plist-get (gethash "opaque-1" table) :subtitle) subtitle)
      (cl-letf (((symbol-function 'display-graphic-p) #'ignore)
                ((symbol-function 'window-body-width) (lambda (&rest _) width)))
        (let ((rows (launcher--results-affixation keys)))
          (should (equal '(space :align-to 4)
                         (launcher-icons-test--layout-column (car rows))))
          (should (equal title (get-text-property 0 'display (car (cadr rows)))))
          (should (equal "" (nth 2 (cadr rows)))))
        (launcher--results-affixation (cdr keys))
        (should (= 4 (launcher--query-session-annotation-column launcher--query-session)))
        (setq width 61)
        (should (equal title (get-text-property
                             0 'display (caar (launcher--results-affixation (cdr keys))))))
        (setq width 60)
        (let* ((row (car (launcher--results-affixation (cdr keys))))
               (visible (get-text-property 0 'display (car row))))
          (should (= 59 (string-width visible)))
          (should (string-suffix-p "…" visible))
          (should (equal title (plist-get (gethash "opaque-1" table) :title))))))))

(ert-deftest launcher-icons-titles-without-subtitles-account-for-prefix-and-wide-text ()
  "Both column and pixel layouts reserve the icon prefix and right margin."
  (dolist (pixels '(nil t))
    (let* ((title (make-string 40 ?中))
           (launcher--query-session (launcher-icons-test--layout-session title))
           (table (launcher--query-session-candidates launcher--query-session))
           (keys (hash-table-keys table))
           (width 100)
           (unit (if pixels 10 1)))
      (setf (plist-get (gethash "opaque-0" table) :subtitle) nil)
      (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) pixels))
                ((symbol-function 'default-font-height) (lambda () 20))
                ((symbol-function 'launcher-icons-enabled-p) (lambda () t))
                ((symbol-function 'launcher-icons-prefix) (lambda (_) "XX "))
                ((symbol-function 'string-pixel-width)
                 (lambda (text &rest _) (* 10 (string-width text))))
                ((symbol-function 'window-body-width) (lambda (&rest _) (* width unit))))
        (let ((row (car (launcher--results-affixation keys))))
          (should (equal title (get-text-property 0 'display (car row))))
          (should (equal "XX " (nth 1 row)))
          (should (equal "" (nth 2 row)))
          (should (= 0 (launcher--query-session-annotation-column launcher--query-session))))
        (setq width 60)
        (let* ((row (car (launcher--results-affixation keys)))
               (visible (get-text-property 0 'display (car row))))
          (should (<= (string-width visible) 56))
          (should (> (string-width visible) 54))
          (should (string-suffix-p "…" visible)))
        (setq width 1)
        (should (equal "" (get-text-property
                           0 'display (caar (launcher--results-affixation keys)))))))))

(ert-deftest launcher-icons-annotations-too-narrow ()
  "Extremely narrow windows do not emit annotations which cannot fit."
  (let ((launcher--query-session (launcher-icons-test--layout-session "中文"))
        (launcher-show-icons nil))
    (cl-letf (((symbol-function 'display-graphic-p) #'ignore)
              ((symbol-function 'window-body-width) (lambda (&rest _) 1)))
      (let ((row (car (launcher--results-affixation '("opaque-0")))))
        (should (equal (get-text-property 0 'display (car row)) ""))
        (should (equal (nth 2 row) ""))))))

(ert-deftest launcher-icons-truncated-title-submits-original-item ()
  "A shortened display still resolves and executes the full source item."
  (dolist (subtitle '(nil "Explanation"))
    (let* ((title (make-string 100 ?a))
           (launcher-show-icons nil) submitted)
      (launcher-test--with-session nil
          `(("t" :name "Truncation"
             :candidates ((:id target :title ,title :subtitle ,subtitle :data full-data))
             :submit ,(lambda (item) (setq submitted item) nil)))
        (launcher--query-update session "t ")
        (cl-letf (((symbol-function 'display-graphic-p) #'ignore)
                  ((symbol-function 'window-body-width) (lambda (&rest _) 40)))
          (let* ((result (car (launcher--query-session-results session)))
                 (candidate (plist-get result :candidate))
                 (row (car (launcher--results-affixation (list candidate)))))
            (should-not (equal title (get-text-property 0 'display (car row))))
            (launcher--result-execute (launcher--result-for-candidate (car row)))
            (should (equal (plist-get submitted :title) title))
            (should (eq (plist-get submitted :id) 'target))
            (should (eq (plist-get submitted :data) 'full-data))))))))

(ert-deftest launcher-icons-stock-layout-is-scoped ()
  "Single-column completion applies only while reading Launcher results."
  (let ((completions-format 'horizontal)
        (launcher-show-icons nil)
        (completing-read-function
         (launcher-test--reader
          (list (lambda ()
                  (should (eq completions-format 'one-column))
                  (should (equal completion-extra-properties
                                 '(:affixation-function launcher--results-affixation)))
                  nil)))))
    (should (eq (condition-case nil
                    (launcher--read nil (launcher--tools))
                  (quit 'quit))
                'quit))
    (should (eq completions-format 'horizontal))))

(ert-deftest launcher-icons-annotations-fixed-cap-in-narrow-windows ()
  "The forty-column cap shrinks when needed to leave annotation room."
  (let ((launcher--query-session (launcher-icons-test--layout-session (make-string 80 ?a)))
        (launcher-show-icons nil))
    (cl-letf (((symbol-function 'display-graphic-p) #'ignore)
              ((symbol-function 'window-body-width) (lambda (&rest _) 20)))
      (let ((row (car (launcher--results-affixation '("opaque-0")))))
        (should (equal '(space :align-to 10) (launcher-icons-test--layout-column row)))
        (should (<= (string-width (get-text-property 0 'display (car row))) 8))
        (should (<= (string-width (substring (nth 2 row) 1)) 9))))))

;;; launcher-icons-tests.el ends here
