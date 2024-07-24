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
  )

;; expand loadpath
;; find-with-fd
;; bloody-lazy!
;; blood--lambda!
