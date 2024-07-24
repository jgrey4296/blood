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
  (load "blood--straight")
  )

;; in-straight!
;; straight build dir
;; straight bootstrap
;; straight init
;; build-dir advice
;; straight sync

;; package to straight recipe
