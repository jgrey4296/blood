;; blood--test.el -*- mode: emacs-lisp; lexical-binding: t; -*-
;;
;; General ERT tests for blood
;;
(require 'f)

(defun blood--test-setup-core ()
  (add-to-list 'load-path (f-parent default-directory))
  (load "blood-defs")
  (load "blood-log")
  (load "blood-utils")
  (load "blood-core")
  )

(ert-deftest blood-test-core-blood-macro ()
  "Tests the core blood! macro for profile definition"
  (should nil)
)

(ert-deftest blood-test-core-blood-macro-with-sync-cmd ()
  "Tests the core blood! macro for profile definition"
  (should nil)
)

(ert-deftest blood-test-core-blood-macro-with-clean-cmd ()
  "Tests the core blood! macro for profile definition"
  (should nil)
)

(ert-deftest blood-test-core-blood-macro-with-report-cmd ()
  "Tests the core blood! macro for profile definition"
  (should nil)
)

(ert-deftest blood-test-core-blood-macro-with-stub-cmd ()
  "Tests the core blood! macro for profile definition"
  (should nil)
)

(ert-deftest blood-test-core-use-macro ()
  "Tests the core use! macro for module definition"
  (should nil)
)

(ert-deftest blood-test-core-use-macro-fail  ()
  "Tests the core use! macro for module definition"
  (should nil)
)

(ert-deftest blood-test-core-install-macro ()
  "Tests the core install! macro for module agonostic package definition"
  (should nil)
)

(ert-deftest blood-test-core-install-macro-fail ()
  "Tests the core install! macro for module agonostic package definition"
  (should nil)
)
