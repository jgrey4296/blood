;;; blood--packages.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header
(loaded? blood-defs blood-log blood-utils blood-structs blood-deferral)
(llog! "Packages lib")

(defun blood-packages--init (package-spec)
  "The standard way of loading a package spec"
  (cl-assert (blood--package-s-p package-spec))
  (let ((package-sym (blood--identifier-s-package (blood--package-s-id package-spec))))
    (ghlog! "Initialising Package: %s" package-sym)
    (funcall (blood--package-s-on-init package-spec))
    (blood-packages--load-autoloads package-spec)
    (blood-packages--set-advice package-spec)
    (blood-packages--set-hooks package-spec)
    (glogxs!)
    )
  )

(defun blood-packages--config (package-spec)
    (let* ((package-sym (blood--identifier-s-package (blood--package-s-id package-spec))))
      (ghlog! "Loading: %s" package-sym)
      (cond ((blood--package-s-defer package-spec) ;; deferred
             (blood-packages--defer-package package-spec))
            ((blood-tracing? package-sym) ;; traced
             (blood-trace-wrap
              (require package-sym)
              (funcall (blood--package-s-on-load package-spec))
              )
             )
            (t ;; otherwise just require it
             (require package-sym)
             (funcall (blood--package-s-on-load package-spec)))
            )
      (glogxs!)
      )
  )

(defun blood-packages--load-autoloads (package-spec)
  "a possibly backend-specific autoload file loading fn"
  ;; also register autoloads mentioned in the package spec
  (warn "TODO autoload prep")
  )

(defun blood-packages--defer-package (package-spec)
  "instead of requiring the package now, defer it
"
  (warn "TODO straight--load-package-autoloads")
  )

(defun blood-packages--set-advice (package-spec)
  "setup a packages registered advice functions"
  (warn "TODO: package advice setup")
  )

(defun blood-packages--set-hooks (package-spec)
  "add functions to appropriate hooks"
  )

(defun blood-packages--set-repeatable-specs (package-spec)
  "register the various repeatable specs associated with the package"
  (warn "TODO: package repeatable specs")
  )

(provide 'blood-packages)

;;-- Footer
;; Copyright (C) 2024 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    July 24, 2024
;; Modified:   July 24, 2024
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 29.1))
;;
;; This file is not part of GNU Emacs.
;;
;;-- end Footer
;;; blood--packages.el ends here
