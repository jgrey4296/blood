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

(defun blood--clean-h (type &optional all)
  "TODO clean the current profile (or if arg, all profiles) build directory
type, as a symbol, can be:
'elc
'eln
...?

"
  (hlog! "Cleaning")
  )

(provide 'blood-clean)
;;; blood-clean.el ends here
