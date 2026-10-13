;;; launcher-candidates-tests.el --- Static sources and unified submit -*- lexical-binding: t; -*-
(require 'launcher-test-support)

(defun launcher-candidates-test--titles (session)
  "Return SESSION's result titles."
  (mapcar (lambda (item) (plist-get item :title))
          (launcher--query-session-results session)))

(ert-deftest launcher-candidates-cache-refilter-and-session-lifetime ()
  (let ((calls 0) (completion-styles '(substring)) selected)
    (dotimes (_ 2)
      (launcher-test--with-session nil
          `(("play" :name "Music"
             :candidates ,(lambda ()
                            (cl-incf calls)
                            '((:id 12 :title "Quiet" :data (:index 12))
                              (:id 24 :title "Quiet" :data (:index 24))
                              (:id 30 :title "Sun" :subtitle "Quiet directory")))
             :submit ,(lambda (item) (setq selected item) nil)))
        (launcher--query-update session "")
        (should (= calls _))
        (launcher--query-update session "play ")
        (should (equal '("Quiet" "Quiet" "Sun") (launcher-candidates-test--titles session)))
        (launcher--query-update session "play UI")
        (should (equal '("Quiet" "Quiet") (launcher-candidates-test--titles session)))
        (launcher--result-execute (cadr (launcher--query-session-results session)))
        (should (equal '(:id 24 :title "Quiet" :data (:index 24)) selected))
        (launcher--query-update session "play QuietXYZ")
        (should-not (launcher--query-session-results session))
        (launcher--query-update session "play ")
        (should (= 3 (length (launcher--query-session-results session))))
        (should (= calls (1+ _)))))))

(ert-deftest launcher-candidates-empty-and-failed-loads-are-cached ()
  (dolist (fails '(nil t))
    (let ((calls 0) queries)
      (launcher-test--with-session nil
          `(("t" :name "Both"
             :candidates ,(lambda () (cl-incf calls) (when fails (error "Static failure")))
             :query ,(lambda (text) (push text queries) '((:id dynamic :title "Other")))
             :submit ignore))
        (launcher--query-update session "t ")
        (should-not (launcher--query-session-results session))
        (should-not queries)
        (dolist (input '("t a" "t b" "t ")) (launcher--query-update session input))
        (should (= calls 1))
        (should (equal '("b" "a") queries))
        (should (eq fails (and (gethash 2 (launcher--query-session-errors session)) t)))
        (launcher--query-update session "t c")
        (should (equal '("Other") (launcher-candidates-test--titles session)))))))

(ert-deftest launcher-candidates-union-and-dynamic-debounce ()
  (let ((completion-styles '(substring)) callbacks queries)
    (launcher-test--with-session nil
        `(("t" :name "Both" :debounce 0.15
           :candidates ((:id a :title "Match A" :data old)
                        (:id b :title "Match B") (:id c :title "Other"))
           :query ,(lambda (text)
                     (push text queries)
                     '((:id a :title "Replacement" :data new)
                       (:id d :title "Dynamic only")))
           :submit ignore))
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (_delay _repeat fn &rest args) (push (cons fn args) callbacks)))
                ((symbol-function 'cancel-timer) #'ignore))
        (launcher--query-update session "t Match")
        (should (equal '("Match A" "Match B") (launcher-candidates-test--titles session)))
        (let ((obsolete (car callbacks)))
          (launcher--query-update session "t Match A")
          (apply (car obsolete) (cdr obsolete))
          (should-not queries)
          (apply (caar callbacks) (cdar callbacks))
          (should (equal '("Replacement" "Dynamic only") (launcher-candidates-test--titles session)))
          (should (eq 'new (plist-get (car (launcher--query-session-results session)) :data))))))))

(ert-deftest launcher-candidates-input-change-discards-previous-dynamic-results ()
  (dolist (delay '(0 0.15))
    (let ((completion-styles '(substring)) callback)
      (launcher-test--with-session nil
          `(("t" :name "Both" :submit ignore :debounce ,delay
             :candidates ((:id a :title "Match A" :data static)
                          (:id b :title "Match B"))
             :query ,(lambda (text)
                       (if (equal text "Match")
                           '((:id a :title "Replacement" :data old)
                             (:id obsolete :title "Obsolete"))
                         '((:id fresh :title "Fresh"))))))
        (cl-letf (((symbol-function 'run-at-time)
                   (lambda (_delay _repeat function &rest args)
                     (setq callback (cons function args))))
                  ((symbol-function 'cancel-timer) #'ignore))
          (launcher--query-update session "t Match")
          (when callback (apply (car callback) (cdr callback)))
          (should (equal '("Replacement" "Match B" "Obsolete")
                         (launcher-candidates-test--titles session)))
          (launcher--query-update session "t Match A")
          (when callback
            (should (equal '("Match A") (launcher-candidates-test--titles session)))
            (apply (car callback) (cdr callback)))
          (should (equal '("Match A" "Fresh") (launcher-candidates-test--titles session)))
          (should (eq 'static (plist-get (car (launcher--query-session-results session)) :data))))))))

(ert-deftest launcher-candidates-union-retains-static-order ()
  (let ((completion-styles '(substring)))
    (launcher-test--with-session nil
        '(("t" :name "Both" :submit ignore
           :candidates ((:id a :title "Match A") (:id b :title "Match B"))
           :query (lambda (_) '((:id b :title "New B") (:id c :title "C")))))
      (launcher--query-update session "t ")
      (should (equal '("Match A" "Match B") (launcher-candidates-test--titles session)))
      ;; Replacement retains B's position; dynamic titles need not match.
      (launcher--query-update session "t Match")
      (should (equal '("Match A" "New B" "C") (launcher-candidates-test--titles session))))))

(ert-deftest launcher-candidates-source-errors-are-independent ()
  (dolist (static-bad '(nil t))
    (dolist (dynamic-bad '(nil t))
      (launcher-test--with-session nil
          `(("t" :name "Both" :submit ignore
             :candidates ,(lambda () (if static-bad (error "Static broken")
                                       '((:id a :title "A"))))
             :query ,(lambda (_) (if dynamic-bad (error "Dynamic broken")
                                   '((:id b :title "B"))))))
        (launcher--query-update session "t A")
        (should (equal (append (unless static-bad '("A")) (unless dynamic-bad '("B")))
                       (launcher-candidates-test--titles session)))
        (let ((error-text (gethash 2 (launcher--query-session-errors session))))
          (when static-bad (should (string-match-p "Static broken" error-text)))
          (when dynamic-bad (should (string-match-p "Dynamic broken" error-text))))))))

(ert-deftest launcher-candidates-duplicate-ids-and-reserved-fields ()
  (dolist (bad '(((:id a :title "A") (:id a :title "B"))
                 ((:id a :title "A" :completion "t "))
                 ((:id a :title "A" :function ignore))))
    (launcher-test--with-session nil
        `(("t" :name "Both" :submit ignore :candidates ,bad
           :query (lambda (_) '((:id b :title "B")))))
      (launcher--query-update session "t A")
      (should (equal '("B") (launcher-candidates-test--titles session)))
      (should (gethash 2 (launcher--query-session-errors session))))))

(ert-deftest launcher-candidates-submit-only-has-no-content ()
  (launcher-test--with-session nil '(("t" :name "Empty" :submit ignore))
    (launcher--query-update session "t ")
    (should (equal '("t") (launcher-candidates-test--titles session)))
    (launcher--query-update session "t hello")
    (should-not (launcher--query-session-results session))))

(ert-deftest launcher-candidates-completion-base-restores-full-title ()
  (launcher-test--with-session nil
      '(("t" :name "Static" :submit ignore
         :candidates ((:id a :title "FooBar") (:id b :title "Other"))))
    (cl-letf (((symbol-function 'completion-all-completions)
               (lambda (_input _table _pred _point) '("Bar" . 3))))
      (launcher--query-update session "t foo")
      (should (equal '("FooBar") (launcher-candidates-test--titles session))))))

(provide 'launcher-candidates-tests)
