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
  (load "blood-profile")
  )

;; test the setup
;; test the start hook
;; profile registration
;; current profile id
;; use files setup
;; auto saves setup
