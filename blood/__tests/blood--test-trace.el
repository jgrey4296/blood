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
  (load "blood-trace")
  )

;; blood-trace-package-require
;; blood-tracing?
;; hooks: memory
;; hooks: time
;; hooks: dependents

;; report
