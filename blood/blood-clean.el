;;; blood-clean.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- header
;;
;; Copyright (C) 2023 John Grey
;;
;; Author: John Grey <https://github.com/jgrey4296>
;; Maintainer: John Grey <johngrey@Johns-Mac-mini.local>
;; Created: September 08, 2023
;; Modified: September 08, 2023
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
(llog! "Clean")
(defvar blood--clean-queue nil)

(defun blood--clean-h ()
  "clean the current profile (or if arg, all profiles) build directory
type, as a symbol, can be: 'elc 'eln ...?

"
  (hlog! "Cleaning")
  (ilog! "TODO: Enable cleaning all profiles")
  (ilog! "Removing ELN Cache: %s" (expand-file-name blood--eln-cache-name blood-cache-dir))
  (delete-directory (expand-file-name blood--eln-cache-name blood-cache-dir) t)
  (cond (blood--straight-initialised
         (ilog! "Removing Build: %s" (straight--build-dir))
         (delete-directory (straight--build-dir) t)
         (delete-directory (straight--modified-dir))
         (delete-file (straight--build-cache-file))
         )
        (t
         (error "TODO: clean for non-straight installs")
         )
        )
  (hlog! "Clean Complete")
  )

(provide 'blood-clean)
;;; blood-clean.el ends here
