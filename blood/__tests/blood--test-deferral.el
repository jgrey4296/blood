;; blood--test.el -*- mode: emacs-lisp; lexical-binding: t; -*-
;;
;; General ERT tests for blood
;;
(require 'f)

(defun blood--test-setup-deferral ()
  (add-to-list 'load-path (f-parent default-directory))
  (load "blood-deferral")
  )

(defun blood-test-h ()
  (ilog! "TODO")
  )

(ert-deftest blood-test-deferral-setup ()
  "Tests the rendering of `quote' symbols in `pp-to-string'."
  (blood--test-setup-deferral)
  (should (boundp 'blood-defer--load-queue))
  (should (boundp 'blood-defer--load-timer))
  (should (boundp 'blood-defer--skip-loads))
  (should (fboundp 'blood-defer--start-h))
  (should (fboundp 'file!))
  (should (fboundp 'dir!))
  (should (fboundp 'local-load!))
  (should (fboundp 'defer!))
  (should (fboundp 'defer-load!))
  (should (fboundp 'defer-require!))
  )

(ert-deftest blood-test-deferral-local-load-expand ()
  "Tests local load expands properly"
  (should (equal (macroexpand '(local-load! "test"))
                 '(load (file-name-concat (dir!) "test") nil 'nomessage)
                 ))
  )

(ert-deftest blood-test-deferral-local-load-skip-expand ()
  "tests skipping local load if blood-defer--skip-loads is set"
  (let ((blood-defer--skip-loads t))
    (should (equal (macroexpand '(local-load! "test"))
                   nil))
    )
  )

(ert-deftest blood-test-deferral-defer-macro-expand ()
  "Tests the defer! macro"
  (should (equal (macroexpand '(defer! 10 "test"))
                 '(run-with-idle-timer 10 nil (lambda nil "test"))
                 ))
)

(ert-deftest blood-test-deferral-defer-load-expand ()
  "check defer-load!"
  (should nil)
  )

(ert-deftest blood-test-deferral-defer-require-expand ()
  "check defer-require"
  (should nil)
  )

(ert-deftest blood-test-deferral-queue-pop-load ()
  "Test popping a load instruction from the queue"
  (should nil)
  )

(ert-deftest blood-test-deferral-queue-pop-require ()
  "Test popping a require instruction from the queue"
  (should nil)
  )

(ert-deftest blood-test-deferral-queue-pop-eval()
  "Test popping an eval instruction from the queue"
  (should nil)
  )

(ert-deftest blood-test-deferral-queue-pop-after ()
  "Test popping an after instruction from the queue"
  (should nil)
  )

(ert-deftest blood-test-deferral-queue-pop-unknown ()
  "Test popping an after instruction from the queue"
  (should nil)
  )

(ert-deftest blood-test-deferral-queue-pop-empty ()
  "check queue-pop cancels the timer when empty"
  )
