;;; re-doom-installer.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- header
;;
;; Copyright (C) 2023 John Grey
;;
;; Author: John Grey <https://github.com/jgrey4296>
;; Maintainer: John Grey <johngrey@Johns-Mac-mini.local>
;; Created: September 01, 2023
;; Modified: September 01, 2023
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

(defun re-doom-sync ()
  "Trigger the syncing of packages, "
  (princ "-------------------- Re-Doom: Syncing")
  (unless RE-DOOM-BOOTSTRAPPED
    (error "Tried to sync re-doom without it having bootstrapped"))

  (let ((module-specs (re-doom--sync-module-specs)))

    )
  )

(defun re-doom--sync-module-specs ()
  "get all active modules"

    )

(defun re-doom--sync-package-specs ()
  ""
  )

(defun re-doom--sync-straight ()

  )

(defun re-doom--clean-installation ()

  )

(provide 're-doom-sync)
;;; re-doom-installer.el ends here
