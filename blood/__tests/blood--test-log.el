;; blood--test.el -*- mode: emacs-lisp; lexical-binding: t; -*-
;;
;; General ERT tests for blood
;;
(require 'f)

(defun blood--test-setup-core ()
  (add-to-list 'load-path (f-parent default-directory))
  (load "blood-defs")
  (load "blood-log")
  )

(ert-deftest blood-test-log-setup ()
  "Tests the vars and consts of blood-log "
  (should nil)
)


;; test log! - basic
;; test llog! - loading
;; test dlog! - debug level
;; test hlog! - header
;; test ghlog! - group
;; test glog! - group
;; test glogx! - group
;; test glogxs! - group level reset
;; test ilog! - indented
