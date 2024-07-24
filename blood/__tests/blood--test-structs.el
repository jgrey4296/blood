;; blood--test.el -*- mode: emacs-lisp; lexical-binding: t; -*-
;;
;; General ERT tests for blood
;;
(require 'f)

(defun blood--test-setup-core ()
  (add-to-list 'load-path (f-parent default-directory))
  (load "blood-structs")
  )


;; test profile building
;; test module
;; test package
;; test constraint
;; test paths
;; test recipes
;; test identifiers
;; test backends
;; test advice

;; test uniq id's
