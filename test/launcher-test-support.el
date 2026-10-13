;;; launcher-test-support.el --- Batch input fixture -*- lexical-binding: t; -*-
(require 'launcher)
(require 'ert)

(defconst launcher-test--search-tools
  (list (launcher-search-tool "gh" :name "GitHub"
                              :url "https://github.com/search?q=%s"))
  "Explicit GitHub search configuration shared by batch and GUI tests.")

(defun launcher-test--submit-action (candidate)
  "Execute a no-argument test action carried as CANDIDATE data."
  (let ((action (plist-get candidate :data)))
    (when action (funcall action))))

(defun launcher-test--text-query (text)
  "Return a text submission candidate for TEXT."
  (list (list :id 'execute :title text :data text)))

(defmacro launcher-test--with-session (entries tools &rest body)
  "Evaluate BODY with a fresh query session and clean it up."
  (declare (indent 2))
  `(let* ((session (launcher--query-create ,entries (let ((launcher-tools ,tools)) (launcher--tools))))
          (launcher--query-session session))
     (unwind-protect (progn ,@body) (launcher--query-close session))))

(defun launcher-test--input (text)
  "Type TEXT in the fake minibuffer and run the real query hook."
  (erase-buffer)
  (insert text)
  (launcher--results-post-command))

(defun launcher-test--submit (text &optional index)
  "Query TEXT and submit result INDEX (default first) through the real action path."
  (launcher-test--input text)
  (let* ((result (nth (or index 0) (launcher--query-session-results launcher--query-session)))
         (candidate (plist-get result :candidate)))
    (cl-letf (((symbol-function 'launcher--results-current) (lambda () result))
              ((symbol-function 'exit-minibuffer) #'ignore))
      (launcher-results-submit))
    candidate))

(defun launcher-test--reader (answers)
  "Return a fake completion reader driving actual queries/actions with ANSWERS.
Strings query and submit; functions can inspect the input or drive a more
complex sequence; symbols signal a condition."
  (lambda (&rest args)
    (with-temp-buffer
      (use-local-map (copy-keymap minibuffer-local-completion-map))
      (when (stringp (nth 4 args)) (insert (nth 4 args)))
      (run-hooks 'minibuffer-setup-hook)
      (unwind-protect
          (let ((answer (pop answers)))
            (cond ((stringp answer) (launcher-test--submit answer))
                  ((functionp answer)
                   (let ((value (funcall answer)))
                     (when (stringp value) (launcher-test--submit value))))
                  (t (signal (or answer 'quit) nil))))
        (run-hooks 'minibuffer-exit-hook))
      "")))

(provide 'launcher-test-support)
