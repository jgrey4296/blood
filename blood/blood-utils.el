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
(require 'blood-log)

(defconst WIN-TYPES '(cygwin windows-ms ms-dos))

(defconst MAC-TYPES '(darwin))

(defconst BSD-TYPES '(darwin berkeley-unix gnu/kfreebsd))

(defconst blood--eln-cache-name "eln-cache")

(defvar   blood--caches (make-hash-table))

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


(llog! "Utils")

;;-- external calls
(defun blood--dcall (dir prog &rest args)
  (unless (file-exists-p dir)
    (error "%s doesn't exist for call of %s" dir prog))
  (let ((default-directory dir))
    (apply #'blood-call prog args)
    )
  )

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

;;-- end external calls

(defun blood--expand-loadpath ()
  "Expand the Loadpath completely"
  (glog! "Expanding Loadpath")
  (setq load-path (append
                   ;; ELN cache
                   (apply #'append (mapcar #'(lambda (x) (blood--find-with-fd x nil nil "d")) native-comp-eln-load-path))
                   ;; profile build dir
                   (blood--find-with-fd blood-profile--build-dir nil nil "d")
                   ;; installation directory
                   (blood--find-with-fd blood-profile--installation-dir nil nil "d")
                   ;; existing load-path
                   load-path
                   )
        )
  (ilog! "New Loadpath: %s" load-path)
  (glogxs!)
  )

(defun blood--find-with-fd (base source &optional pattern type)
  (ilog! "Searching: %s" (expand-file-name base source))
  (cond ((executable-find "fdfind")
 (split-string (cdr (blood--ecall "fdfind"
                      "-t" (or type "f")
                      (or pattern ".")
                      (expand-file-name base source)))
                       "\n" t " +")
         )
        ((executable-find "fd")
         (split-string (cdr (blood--ecall "fd"
                                          "-t" (or type "f")
                                          (or pattern ".") (expand-file-name base source)))
                       "\n" t " +")
         )
        ((executable-find "find")
         (split-string (cdr (blood-ecall "find"
                                         "-t" (or type "f")
                                         (expand-file-name base source)
                                         "-regex" (or pattern ".")))
                       "\n" t " +"))
        (t (cons 0 (directory-files (expand-file-name base source) 'full)))
        )
  )

(defun inhibit! (&rest inhibited)
  "WARNING: will break stuff. Add a feature to `features', so any `require's for it become no-ops.
TODO advice load instead
"
  (mapcar #'provide inhibited)
  )

;;-- macro utils
(defun blood--lambda! (data)
  "convert a list into a lambda, handling a list of sexps or just a single sexp.
This isn't a macro though, because we don't want to actually construct it yet.
"
  `(lambda () ,@(if (symbolp (car data))
                    (list data)
                  data)))

(defmacro mquote! (e)
  (declare (indent defun))
  (if (eq (car-safe e) 'quote)
      e
    (list 'quote e)
    )
  )

;;-- end macro utils

;;-- terminal forcing
(defun blood--force-terminal ()
  "by default blood sets `initial-window-system' to nil, avoiding the startup of a gui.
this ensures the terminal setup code runs.
this is done in the early-init file, so startup's call to `tty-run-terminal-initialization'
progresses as normal.
"
  (ilog! "Forcing a Terminal Frame")
  (setq initial-window-system nil)
  (select-frame (make-terminal-frame `((tty-type . ,(or (getenv "TERM") "xterm")))))
  (tty-run-terminal-initialization (selected-frame) nil t)
  )

;;-- end terminal forcing

(defmacro bloody-lazy! (kw &optional mod)
  (declare (indent defun))
  (if (not (plist-get blood--hook-laziness kw))
      `(error "Bad hook laziness value: %s" ,kw)
    (+ (plist-get blood--hook-laziness kw) (or mod 0))
    )
  )

;;-- caching

(cl-defun blood-register-cache! (kw save-lambda &key read-lambda header incremental)
  "Register a cache. Must have a lambda to convert data to text,
but can also have a read lambda, a header for the cache file,
and an incremental flag"
  (unless (keywordp kw) (error "Blood Cache Registration Uses Keywords"))
  (unless save-lambda (error "Caches need a save lambda"))
  (ilog! "Registering Cache: %s" kw)
  (puthash kw (list :save save-lambda :read read-lambda :header header :incremental incremental) blood--caches)
  )

(defun blood-cache! (kw &optional data incremental)
  "Get the file path of a cache, or write to that cache if you provide data.
Pass a third argument to add to the cache instead of completely re-write
"
  (unless (gethash kw blood--caches)
    (error "Unrecognzed Blood Cache: %s" kw))
  (if (not data)
      (expand-file-name (format "./%s.cache" (string-replace ":" "" (symbol-name kw))) user-emacs-directory)
    (let ((header (plist-get (gethash kw blood--caches) :header))
          (writer (plist-get (gethash kw blood--caches) :save))
          )
      (ilog! "Caching: %s" (blood-cache! kw))
      (with-temp-buffer
        (when (and (not incremental) header)
          (mapc #'(lambda (x) (insert "## " x "\n")) (split-string header "\n" t " +")))
        (insert (funcall writer data) "\n")
        (if (and incremental (plist-get (gethash kw blood--caches) :incremental))
            (append-to-file (point-min) (point-max) (blood-cache! kw))
          (write-file (blood-cache! kw)))
        )
      )
    )
  )

(defun blood-read-cache! (kw)
  "Read a cache"
  (ilog! "Reading Cached Data: %s" (blood-cache! kw))
  (with-temp-buffer
    (insert-file-contents-literally (blood-cache! kw))
    (when (plist-get (gethash kw blood--caches) :header)
      (require 'replace)
      (goto-char (point-min))
      (flush-lines "^## "))
    (if (plist-get (gethash kw blood--caches) :read)
        (funcall (plist-get (gethash kw blood--caches) :read) (buffer-string))
      (buffer-string))
    )
  )

;;-- end caching

;;-- Message control

;;-- end Message control

(provide 'blood-utils)
;;; blood-utils.el ends here
