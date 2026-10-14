;;; launcher-icons-gui-tests.el --- Graphical app icon checks -*- lexical-binding: t; -*-

;; GUI ONLY: run in the test VM with `bash test/vm.sh', which runs
;; test/gui.sh.  Real icons of the VM's system apps, made by the real
;; worker into a temporary cache, shown by Vertico and stock completion
;; using launcher-gui-test-support.el.
;; Launching and browsing are stubbed.

(require 'launcher-gui-test-support)
(require 'marginalia)

(defconst launcher-gui-icons--names
  '("Calculator" "Calendar" "Chess" "Clock" "Contacts" "Maps" "Notes" "Photos")
  "System apps whose icons the checks show, those the VM has.")

(defvar launcher-gui-icons--fake-notes nil
  "A second, fake Notes.app, so that two apps share a name.")

(defun launcher-gui-icons--apps (directory)
  "Return an app index of real system apps, and a fake Notes in DIRECTORY."
  (let* ((paths (seq-filter #'file-directory-p
                            (mapcar (lambda (name) (format "/System/Applications/%s.app" name))
                                    launcher-gui-icons--names)))
         (fake (expand-file-name "Notes.app" directory)))
    (make-directory (expand-file-name "Contents" fake) t)
    (with-temp-file (expand-file-name "Contents/Info.plist" fake)
      (insert "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<plist version=\"1.0\"><dict>"
              "<key>CFBundlePackageType</key><string>APPL</string></dict></plist>\n"))
    (setq launcher-gui-icons--fake-notes fake)
    (launcher--build-entries (append paths (list fake)))))

(defun launcher-gui-icons--wait ()
  "Wait until no icon worker runs."
  (with-timeout (60 (error "Icon worker did not finish"))
    (while (process-live-p launcher-icons--process)
      (accept-process-output launcher-icons--process 0.05))))

(defmacro launcher-gui-icons--with (&rest body)
  "Run BODY in the fixture, with icons of real apps in a temporary cache.
`apps' is bound to the index, warmed before BODY."
  (declare (indent 0))
  `(launcher-gui--with-fixture
     (let* ((directory (file-name-as-directory (make-temp-file "launcher-icons-gui" t)))
            (launcher-icon-cache-directory (expand-file-name "cache/" directory))
            (launcher-show-icons t)
            (launcher-icons--requested (make-hash-table :test #'equal))
            (launcher-icons--process nil)
            (apps (launcher-gui-icons--apps directory))
            (launcher--apps apps))
       (ignore apps)
       (unwind-protect
           (progn
             (should (launcher-icons-enabled-p))
             (let ((start (float-time)))
               (launcher-icons-prepare (mapcar #'cdr apps))
               (launcher-gui-icons--wait)
               (message "Icons: warmed %d apps in %.3fs at scale %s"
                        (length apps) (- (float-time) start)
                        (frame-scale-factor)))
             ,@body)
         (launcher-gui-icons--wait)
         (delete-directory directory t)))))

(defun launcher-gui-icons--rows (string)
  "Return the rows of STRING, a candidate list, as (TEXT IMAGES WIDTH).
IMAGES lists the files of images in the row's display properties, and
WIDTH is the pixel width of the row's text before its candidate."
  (mapcar (lambda (line)
            (let (images (pos 0))
              (while (< pos (length line))
                (let ((display (get-text-property pos 'display line)))
                  (when (eq (car-safe display) 'image)
                    (push (plist-get (cdr display) :file) images)))
                (setq pos (or (next-single-property-change pos 'display line) (length line))))
              (list (substring-no-properties line) (nreverse images)
                    (string-pixel-width (substring line 0 (min 2 (length line)))))))
          (seq-remove #'string-blank-p (split-string string "\n" t))))

(defun launcher-gui-icons--vertico-string ()
  "Return the candidates Vertico shows in the active minibuffer."
  (when-let* ((window (active-minibuffer-window))
              (buffer (window-buffer window))
              (overlay (buffer-local-value 'vertico--candidates-ov buffer)))
    (or (overlay-get overlay 'after-string) (overlay-get overlay 'before-string) "")))

(defun launcher-gui-icons--check-rows (rows)
  "Check ROWS of a list of launcher candidates: one icon per app, aligned."
  (should rows)
  (dolist (row rows)
    (let ((app (and (not (string-match-p "Search web:" (car row))) (string-match-p "\\.app\\|Calculator\\|Calendar\\|Chess\\|Clock\\|Contacts\\|Maps\\|Notes\\|Photos"
                               (car row)))))
      (if app
          (progn
            (should (= (length (nth 1 row)) 1))
            (should (string-match-p launcher-icons--png-regexp
                                    (file-name-nondirectory (car (nth 1 row))))))
        (should-not (nth 1 row)))))
  ;; Icons, and the space of search tools, are equally wide.
  (should (= (length (delete-dups (mapcar (lambda (row) (nth 2 row)) rows))) 1)))

(defun launcher-gui-icons--quietly (start strokes)
  "Drive START with STROKES, as `launcher-gui--drive', which end by quitting."
  (should (eq (condition-case nil
                  (progn (launcher-gui--drive start strokes) 'done)
                (quit 'quit))
              'quit)))

(defun launcher-gui-icons--line-heights (window)
  "Return the pixel heights of WINDOW's screen lines."
  (let (heights)
    (dotimes (row (window-body-height window))
      (when-let* ((height (car (window-line-height row window))))
        (push height heights)))
    (nreverse heights)))

(ert-deftest launcher-gui-icons-vertico-minibuffer ()
  "Vertico with Marginalia shows one aligned native icon per app row,
the selected row included, without changing what is launched."
  (launcher-gui-icons--with
    (let (states)
      (vertico-mode 1)
      (marginalia-mode 1)
      (unwind-protect
          (launcher-gui--drive
           #'launcher
           (list "C" (lambda ()
                   (redisplay t)
                   (launcher-gui--capture "icons-vertico")
                   (push (list (launcher-gui-icons--vertico-string)
                               (launcher-gui-icons--line-heights (active-minibuffer-window)))
                         states))
                 'backspace "No"
                 (lambda ()
                   (redisplay t)
                   (launcher-gui--capture "icons-vertico-notes")
                   (push (list (launcher-gui-icons--vertico-string)
                               (launcher-gui-icons--line-heights (active-minibuffer-window)))
                         states))
                 'down 'return))
        (marginalia-mode -1)
        (vertico-mode -1))
      (setq states (nreverse states))
      (pcase-dolist (`(,string ,heights) states)
        (let ((rows (launcher-gui-icons--rows string)))
          (message "Icons: Vertico rows %S; line heights %S, default %d"
                   (mapcar (lambda (row) (list (car row) (length (nth 1 row)) (nth 2 row))) rows)
                   heights (default-line-height))
          (launcher-gui-icons--check-rows rows)
          ;; Rows of icons and of search tools are equally tall.
          (should (= (length (delete-dups (seq-take (cdr heights) (length rows)))) 1))
          ;; The selected row keeps its icon and its highlighting.
          (should (launcher-gui--selected-row string))))
      ;; Two apps named Notes, each with its own icon.
      (let ((rows (launcher-gui-icons--rows (car (nth 1 states)))))
        (should (= (length rows) 2))
        (should-not (equal (nth 1 (nth 0 rows)) (nth 1 (nth 1 rows)))))
      (should (equal launcher-gui--launched (list launcher-gui-icons--fake-notes)))
      (should-not launcher-gui--searched))))

(ert-deftest launcher-gui-icons-tool-defaults ()
  "Tool entries and inherited images align with text and blank overrides."
  (launcher-gui-icons--with
    (let* ((icon (launcher-icons-app (cdar apps)))
           (text (propertize "T" 'face '(:foreground "red")))
           (launcher-tools
            (list (list "tool" :name "Icon tool" :icon icon
                        :candidates (list '(:id image :title "Inherited image")
                                          (list :id text :title "Text override" :icon text)
                                          '(:id blank :title "Blank override" :icon nil))
                        :submit #'ignore))))
      (vertico-mode 1)
      (unwind-protect
          (launcher-gui-icons--quietly
           #'launcher
           (list "tool"
                 (lambda ()
                   (redisplay t)
                   (let ((rows (launcher-gui-icons--rows (launcher-gui-icons--vertico-string))))
                     (should (= (length rows) 1))
                     (should (equal (nth 1 (car rows)) (list (cadr icon)))))
                   (launcher-gui--capture "icons-tool-entry"))
                 " "
                 (lambda ()
                   (redisplay t)
                   (let* ((string (launcher-gui-icons--vertico-string))
                          (rows (launcher-gui-icons--rows string))
                          (pos (string-match "T.*Text override" string))
                          (heights (launcher-gui-icons--line-heights (active-minibuffer-window))))
                     (should (= (length rows) 3))
                     (should (equal (nth 1 (car rows)) (list (cadr icon))))
                     (should-not (nth 1 (nth 1 rows)))
                     (should-not (nth 1 (nth 2 rows)))
                     (should pos)
                     (should (equal (get-text-property pos 'face string)
                                    (get-text-property 0 'face text)))
                     (should (= (length (delete-dups (seq-take (cdr heights) 3))) 1)))
                   (launcher-gui--capture "icons-tool-defaults"))
                 'quit))
        (vertico-mode -1)))))

(ert-deftest launcher-gui-icons-follow-buffer-font ()
  "Mixed prefixes follow remapped fonts and ignore extra line spacing."
  (launcher-gui-icons--with
    (save-window-excursion
      (with-temp-buffer
        (switch-to-buffer (current-buffer))
        (let ((icon (launcher-icons-app (cdar apps)))
              (text (propertize "T" 'face '(:foreground "red")))
              heights)
          (dolist (scale '(-2 0 2))
            (text-scale-set scale)
            (let ((size (default-font-height)))
              (push size heights)
              (dolist (spacing '(nil 8))
                (setq-local line-spacing spacing)
                (erase-buffer)
                (let ((prefixes (mapcar #'launcher-icons-prefix (list icon text nil))))
                  (let ((image (get-text-property 0 'display (car prefixes))))
                    (should (eq (car image) 'image))
                    (should (= (plist-get (cdr image) :width) size))
                    (should (= (plist-get (cdr image) :height) size)))
                  (let ((widths (mapcar #'string-pixel-width prefixes)))
                    (should (<= (- (apply #'max widths) (apply #'min widths)) 1)))
                  (dolist (prefix prefixes)
                    (insert prefix "Candidate\n")))
                (goto-char (point-min))
                (redisplay t)
                (let ((rows (seq-take (launcher-gui-icons--line-heights (selected-window)) 3)))
                  (should (= (length rows) 3))
                  (should (= (length (delete-dups rows)) 1))))))
          ;; The same buffer produces larger and smaller icons after remapping.
          (should (apply #'> heights)))))))

(ert-deftest launcher-gui-icons-stock-completion ()
  "Stock completion's *Completions* shows the icons too."
  (launcher-gui-icons--with
    (let ((launcher-gui--keys (append launcher-gui--keys '((tab 48 0 "\t"))))
          (completions-format 'one-column)
          rows)
      (launcher-gui-icons--quietly
       #'launcher
       (list "C" 'tab
             (lambda ()
               (redisplay t)
               (launcher-gui--capture "icons-stock-completion")
               (with-current-buffer "*Completions*"
                 (setq rows (launcher-gui-icons--rows (buffer-string)))))
             'quit))
      (message "Icons: stock rows %S"
               (mapcar (lambda (row) (list (car row) (length (nth 1 row)))) rows))
      (let ((apps (seq-filter (lambda (row)
                                (string-match-p "Calculator\\|Calendar\\|Chess\\|Clock\\|Contacts"
                                                (car row)))
                              rows)))
        (should (>= (length apps) 4))
        (dolist (row apps)
          (should (= (length (nth 1 row)) 1))))
      (should-not launcher-gui--launched))))

(provide 'launcher-icons-gui-tests)
;;; launcher-icons-gui-tests.el ends here
