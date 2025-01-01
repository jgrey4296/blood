;; blood-log.el -*- mode: emacs-lisp; lexical-binding: t; -*-

(require 'seq)
(require 'subr-x)

(defvar blood--log-group-level 2)

(defconst blood--log-header-line "--------------------")

(defconst blood--log-group-entry-line "----------")

(defconst blood--log-levels (list :debug :info :warn :nolog))

(defvar blood--current-log-level :info)

(defvar blood--level-log-fmt "(%s) : %s\n")

(defvar blood--log-prefix "Blood")

(defun blood-set-loglevel (level)
  (unless (seq-contains-p blood--log-levels level)
    (error "Bad Error Level: %s" level))
  (setq blood--current-log-level level)
  )

(defmacro log! (level text &rest args)
  " A Simple, debug message sent to *Messages*"
  (declare (indent defun))
  (if (and (seq-position blood--log-levels level)
           (<= (seq-position blood--log-levels blood--current-log-level) (seq-position blood--log-levels level)))
      `(let ((inhibit-message t))
         (if (not noninteractive)
             (funcall #'message ,text ,@args)
           (funcall #'princ (format ,text ,@args) #'external-debugging-output)
           (flush-standard-output)
           )
         ;; (append-to-file (format ,(format blood--level-log-fmt level text) ,@args)
         ;;                 nil
         ;;                 (expand-file-name "~/.blood.log")
         ;;                 )
         )
    )
  )

(defmacro llog! (text &rest args)
  "A Load message log"
  (declare (indent defun))
  (when debug-on-error
    `(log! :debug "Loading: %s" (format ,text ,@args))
    )
  )

(defmacro dlog! (text &rest args)
  " A Simple, debug message when 'debug-on-error is true at :debug level"
  (declare (indent defun))
  `(when debug-on-error
       (log! :debug ,text ,@args)
     )
  )

(defmacro hlog! (text &rest args)
  "A Header Log"
  `(log! :info "\n%s %s %s"
     blood--log-header-line
     (format ,text ,@args)
     blood--log-header-line)
  )

(defmacro ghlog! (entermsg &rest args)
  "Enter a group log"
  (declare (indent defun))
  `(progn
     (cl-incf blood--log-group-level 2)
     (log! :info "%s >> %s : %s"
       (make-string blood--log-group-level ?-)
       blood--log-prefix
       (format ,entermsg ,@args)))
  )

(defmacro glog! (entermsg &rest args)
  "Enter a group"
  (declare (indent defun))
  `(progn
     (cl-incf blood--log-group-level 2)
     (log! :info "%s >> %s : %s"
       (make-string blood--log-group-level ? )
       blood--log-prefix
       (format ,entermsg ,@args)))
  )

(defmacro glogx! (&rest rest)
  "Exit the last group after executing &rest"
  (declare (indent defun))
  `(prog1
       (progn ,@rest)
     (glogxs! (format "%s <<" (make-string blood--log-group-level ? )))
     )
  )

(defun glogxs! (&optional msg)
  "log a message and reset the group level"
  (log! :info "%s" (or msg ""))
  (cl-decf blood--log-group-level 2)
  (if (< blood--log-group-level 2) (setq blood--log-group-level 2))
  )

(defmacro ilog! (msg &rest args)
  "Indented log line"
  (declare (indent defun))
  `(log! :info "%s > %s" (make-string (+ 2 blood--log-group-level) ? ) (format ,msg ,@args))
  )

(defun blood--clear-log-prefix ()
  "Clear the logging prefix"
  (setq blood--log-prefix ""))

(provide 'blood-log)
