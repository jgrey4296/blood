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
  (load "blood-sync")
  )

;; hook
;; complete hook
;; module spec retrieval
;; module collection
;; native-comp
;; byte-comp
;; autoloads
;;
