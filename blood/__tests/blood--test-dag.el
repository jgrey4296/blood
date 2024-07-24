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
  (load "blood-modules")
  (load "blood-dag")
  )

(defun blood--test-setup-straight-mock ()
  "dag needs straight, so mock it here"
  )

;; hook
;; dag build
;; dag dfs
