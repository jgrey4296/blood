 ;;; blood-utils.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- header
;;
;; Copyright (C) 2023 John Grey
;;
;; Author: John Grey <https://github.com/jgrey4296>
;; Maintainer: John Grey <johngrey@Johns-Mac-mini.local>
;; Created: September 07, 2023
;; Modified: September 07, 2023
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
(loaded? blood-defs blood-log subr-x)
(llog! "Utils")

;;-- external calls

(defun blood--dcall (dir prog &rest args)
  "call an external program in dir"
  (unless (file-exists-p dir)
    (error "%s doesn't exist for call of %s" dir prog))
  (let ((default-directory dir))
    (apply #'blood--call prog args)
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
  (let ((eln-cache (apply #'append (mapcar #'(lambda (x) (blood--find-with-fd x nil nil "d" 1)) native-comp-eln-load-path)))
        (profile-build (blood--find-with-fd blood-profile--build-dir nil nil "d" 1))
        (profile-install (blood--find-with-fd blood-profile--installation-dir nil nil "d" 1))
        )
    (log! :debug "From eln-cache: %s" eln-cache)
    (log! :debug "From Build: %s" profile-build)
    (log! :debug "From install: %s" profile-install)
    (setq load-path (cl-remove-duplicates (append eln-cache profile-build profile-install load-path)))
  )
  (ilog! "New Loadpath: %s" load-path)
  (glogxs!)
  )

(defun blood--find-with-fd (base source &optional pattern type depth) ;; -> list
  "Find directories to add to the loadpath"
  (ilog! "Searching: %s" (expand-file-name base source))
  (cond ((not (file-directory-p (expand-file-name base source)))
         (ilog! "Search location does not exist: %s" (expand-file-name base source))
         nil
         )
        ((executable-find "fdfind")
         (split-string (cdr (blood--ecall "fdfind"
                                          "-d" (format "%s" (or depth 4))
                                          "-t" (or type "f")
                                          (or pattern ".")
                                          (expand-file-name base source)))
                       "\n" t " +")
         )
        ((executable-find "fd")
         (split-string (cdr (blood--ecall "fd"
                                          "-d" (format "%s" (or depth 4))
                                          "-t" (or type "f")
                                          (or pattern ".") (expand-file-name base source)))
                       "\n" t " +")
         )
        ((executable-find "find")
         (split-string (cdr (blood-ecall "find"
                                         "-maxdepth" (format "%s" (or depth 4))
                                         "-t" (or type "f")
                                         (expand-file-name base source)
                                         "-regex" (or pattern ".")))
                       "\n" t " +"))
        (t (cons 0 (directory-files (expand-file-name base source) 'full)))
        )
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
  "util for quoting"
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

this might be broken
"
  (ilog! "Forcing a Terminal Frame")
  (setq initial-window-system nil
        frame-initial-frame t
        inhibit-message t
        )
  (let ((curr (selected-frame)))
    (select-frame (make-terminal-frame `((tty-type . ,(or (getenv "TERM") "xterm")))))
    (tty-run-terminal-initialization (selected-frame) nil t)
    (delete-frame curr))
)

;;-- end terminal forcing

(defmacro bloody-lazy! (kw &optional mod)
  " a quick way to convert laziness kwords to priority values "
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
  (log! :debug "Registering Cache: %s" kw)
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
      (log! :debug "Caching: %s" (blood-cache! kw))
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
  (if (not (file-exists-p (blood-cache! kw)))
      (log! :warn "Can't read cache, it does not exist: %s : %s" kw (blood-cache! kw))
    (log! :debug "Reading Cached Data: %s" (blood-cache! kw))
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
  )

;;-- end caching

(provide 'blood-utils)
;;; blood-utils.el ends here
