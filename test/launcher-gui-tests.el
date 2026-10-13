;;; launcher-gui-tests.el --- Graphical minibuffer checks -*- lexical-binding: t; -*-
(require 'launcher-gui-test-support)

(ert-deftest launcher-gui-app-and-web-actions ()
  (launcher-gui--with-fixture
    (let ((launcher-tools launcher-test--search-tools))
      (vertico-mode 1)
      (unwind-protect
          (progn
            (launcher-gui--drive #'launcher
                                 (list "Calculator"
                                       (lambda ()
                                         (should (active-minibuffer-window))
                                         (should-not
                                          (cl-some (lambda (window)
                                                     (minibufferp (window-buffer window)))
                                                   (window-list nil 'never))))
                                       'return))
            (launcher-gui--drive #'launcher '("some words" return))
            (launcher-gui--drive #'launcher '("!gh portal launcher" return))
            (should (equal '("/Applications/Calculator.app") launcher-gui--launched))
            (should (equal '("https://github.com/search?q=portal%20launcher"
                             "https://www.google.com/search?q=some%20words")
                           launcher-gui--searched)))
        (vertico-mode -1)))))

(ert-deftest launcher-gui-cancel-and-reenter ()
  (launcher-gui--with-fixture
    (vertico-mode 1)
    (unwind-protect
        (progn
          (dolist (strokes '(("calc" escape) ("calc" quit)))
            (should (eq 'quit
                        (condition-case nil
                            (launcher-gui--drive #'launcher strokes)
                          (quit 'quit)))))
          (launcher-gui--drive #'launcher '("Calculator" return))
          (should (equal '("/Applications/Calculator.app") launcher-gui--launched))
          (should-not launcher-gui--searched))
      (vertico-mode -1))))

(provide 'launcher-gui-tests)
