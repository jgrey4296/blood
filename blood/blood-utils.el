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

(defconst WIN-TYPES '(cygwin windows-ms ms-dos))

(defconst MAC-TYPES '(darwin))

(defconst BSD-TYPES '(darwin berkeley-unix gnu/kfreebsd))

(defconst blood--hook-laziness '(;; Not Lazy
                         :cold-start   -99
                                 :bootstrap    -95
                                 :clean        -90
                                 :sync         -85
                                 :build        -80
                                 :run          -70
                                 :profile-init -60
                                 :module-init  -50
                                 :module-config 25
                                 :user-min      50
                                 :user-max      90
                                 :finalize      99
                                 ) ;; Lazy
  "Standard laziness values for hooks.
if you're lazy, you run later than others.
use `bloody-lazy!' to convert the values
"
  )

;;-- logging

(defvar blood--log-group-level 2)

(defconst blood--log-header-line "--------------------")

(defconst blood--log-group-entry-line "----------")

(defmacro log! (text &rest args)
  " A Simple, debug message when 'debug-on-error is true"
  `(let ((inhibit-message t))
       (funcall #'message ,text ,@args)
       )
  )

(defmacro llog! (text &rest args)
  "A Load message"
  `(when debug-on-error
       (funcall #'message ,text ,@args)
       )
  )

(defmacro dlog! (text &rest args)
  " A Simple, debug message when 'debug-on-error is true"
  `(when debug-on-error
       (funcall #'message ,text ,@args)
     )
  )

(defmacro hlog! (text &rest args)
  "A Header Log"
  `(message "\n%s Blood: %s %s" blood--log-header-line (format ,text,@args) blood--log-header-line)
  )

(defmacro ghlog! (entermsg &rest args)
  "Enter a group"
  (declare (indent defun))
  `(progn
     (cl-incf blood--log-group-level 2)
     (message "%s >> Blood: %s" (make-string blood--log-group-level ?-) (format ,entermsg ,@args)))
  )

(defmacro glog! (entermsg &rest args)
  "Enter a group"
  (declare (indent defun))
  `(progn
     (cl-incf blood--log-group-level 2)
     (message "%s >> Blood: %s" (make-string blood--log-group-level ? ) (format ,entermsg ,@args)))
  )

(defmacro glogx! (&rest rest)
  "Exit the last group"
  `(prog1
       (progn ,@rest)
     (glogxs! (format "%s <<" (make-string blood--log-group-level ? )))
     )
  )

(defun glogxs! (&optional msg)
  (message "%s" (or msg ""))
  (cl-decf blood--log-group-level 2)
  (if (< blood--log-group-level 2) (setq blood--log-group-level 2))
  )

(defmacro ilog! (msg &rest args)
  "Indented log line"
  `(message "%s > %s" (make-string (+ 2 blood--log-group-level) ? ) (format ,msg ,@args))
  )

;;-- end logging

(llog! "Utils")

(defun blood--call (prog &rest args)
  "Call a program with arguments, returns (retcode . msg)"
    (with-temp-buffer
      (cons (or (format "%d" (apply #'call-process prog nil t nil args))
                -1)
            (string-trim (buffer-string)))
      )
  )

(defun blood--ecall (prog &rest args)
  "Error if the call returns non-zero"
  (let ((result (apply 'blood--call prog args)))
    (cond ((zerop (string-to-number (car-safe result)))
           result
           )
          (t (error "Blood Call Failed: %s %s : %s" prog args result))
           )
    )
  )

(defun blood--expand-loadpath ()
  "Expand the Loadpath completely"
  (glog! "Expanding Loadpath")
  (setq load-path (append
           ;; ELN cache
                   (mapcar (lambda (x)
                             (when (file-exists-p x)
                               (ilog! "Expanding: %s" x)
                               (directory-files x 'full)))
                           native-comp-eln-load-path)
                   ;; profile build dir
                   (directory-files blood-profile--installation-dir 'full)
                   load-path
                   )
        )
  (glogxs!)
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

(defun blood--force-terminal ()
  (select-frame (make-terminal-frame `((tty-type . "xterm"))))
  (tty-run-terminal-initialization (selected-frame) nil t)
  )

(defmacro bloody-lazy! (kw &optional mod)
  (if (not (plist-get blood--hook-laziness kw))
      `(error "Bad hook laziness value: %s" ,kw)
    (+ (plist-get blood--hook-laziness kw) (or mod 0))
    )
  )

;;-- Message control

;;-- end Message control

(provide 'blood-utils)
;;; blood-utils.el ends here
