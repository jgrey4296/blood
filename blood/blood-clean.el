;;; blood-clean.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- header
;;
;; Copyright (C) 2023 John Grey
;;
;; Author: John Grey <https://github.com/jgrey4296>
;; Maintainer: John Grey <johngrey@Johns-Mac-mini.local>
;; Created: September 08, 2023
;; Modified: September 08, 2023
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
(cl-assert (featurep 'blood-defs))
(cl-assert (featurep 'blood-log))
(cl-assert (featurep 'blood-utils))
(llog! "Clean")

(defvar blood--clean-queue nil "a list of profile names to clean, or t if all")

;; TODO: Enable cleaning all profiles
;;

(defun blood--clean-h ()
  "clean the current profile (or if arg, all profiles) build directory
type, as a symbol, can be: 'elc 'eln ...?

"
  (hlog! "Cleaning")
  (run-hooks 'blood-clean-hook)
  (hlog! "Clean Complete")
  )

(defun blood--clean--cache-h ()
  "Delete the blood cache"
  (ilog! "Removing ELN Cache: %s" (expand-file-name blood--eln-cache-name blood-cache-dir))
  (delete-directory (expand-file-name blood--eln-cache-name blood-cache-dir) t)
  )

(defun blood--clean--backend-h ()
  "Clean a backend's files"
  (ghlog! "Running Backend Clean")
  (cond ((and blood--backend-active (functionp (blood--backend-s-clean blood--backend-default)))
         (funcall (blood--backend-s-clean blood--backend-default))
         )
        (t
         (warn "No Clean function for current backend", blood--backend-default))
        )
  (glogx!)
  )

(defun blood--clean--package-src-h ()
  "clean a package source code"
  (warn "TODO: single package cleaning")
  )

(defun blood--clean--package-compilation-h ()
  "clean compiled code for a package"
  (warn "TODO: single package compilation cleaning")
  )

(defun blood--clean--autoloads-h ()
  "clean generated autoloads"
  (warn "TODO: autoload cleaning")
  )

(provide 'blood-clean)
;;; blood-clean.el ends here
