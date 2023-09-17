;;; blood-utils.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- header
;;
;; Copyright (C) 2023 John Grey
;;
;; Author: John Grey <https://github.com/jgrey4296>
;; Maintainer: John Grey <johngrey@Johns-Mac-mini.local>
;; Created: September 07, 2023
;; Modified: September 07, 2023
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 28.2))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:
;;-- end header
(require 'subr-x)
(message "Loading Utils")

(defun blood--call (prog &rest args)
  "Call a program with arguments, returns (retcode . msg)"
    (with-temp-buffer
      (cons (or (format "%d" (apply #'call-process prog nil t nil args))
                -1)
            (string-trim (buffer-string)))
      )
  )

(defun blood--update-loadpath ()
  (setq load-path (append
                   ;; TODO ELN cache
                   ;; TODO profile build dir
                   (directory-files blood-profile--installation-dir 'full)
                   load-path
                   )
        )
  )

(defun inhibit! (&rest inhibited)
  "WARNING: will break stuff. Add a feature to `features', so any `require's for it become no-ops. "
  (mapcar #'provide inhibited)
  )

(defun blood--lambda! (data)
  "convert a list into a lambda, handling a list of sexps or just a single sexp"
  `(lambda () ,@(if (symbolp (car data))
                    (list data)
                  data)))

(defmacro mquote! (e)
  (if (eq (car-safe e) 'quote)
      e
    (list 'quote e)
    )
  )

;;-- logging
(defmacro log! (text &rest args)
  " A Simple, debug message when 'debug-on-error is true"
  `(when debug-on-error
     (let ((inhibit-message t))
       (funcall #'message ,text ,@args)
       )
     )
  )

(defvar blood--log-group-level 0)
(defmacro glog! (entermsg &rest args)
  (declare (indent defun))
  `(progn
     (cl-incf blood--log-group-level)
     (message "%s> Blood (%s): %s" (make-string blood--log-group-level ?-) blood--log-group-level
              (format ,entermsg ,@args)))
  )

(defmacro glogx! ()
  `(progn (cl-decf blood--log-group-level)
          (if (< blood--log-group-level 0)
              (setq blood--log-group-level 0))
          ;; (message "<%s Exit (%s)" (string-join (make-list (max 0 (- 10 blood--log-group-level)) "-")) blood--log-group-level)
          )
  )

(defmacro ilog! (msg &rest args)
  `(message "%s> (%s): %s" (make-string blood--log-group-level ? ) blood--log-group-level (format ,msg ,@args))
  )

;;-- end logging

(provide 'blood-utils)
;;; blood-utils.el ends here
