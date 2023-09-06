;;; blood-installer.el -*- lexical-binding: t; no-byte-compile: t; -*-
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

(defun blood-sync ()
  "Trigger the syncing of packages, "
  (princ "-------------------- Blood: Syncing")
  (unless BLOOD-BOOTSTRAPPED
    (error "Tried to sync blood without it having bootstrapped"))

  (dolist (spec blood-profile-spec-list)
    ;; get active modules
    (blood--sync-module-specs spec)
    )
  ;; Install
  (funcall blood--sync-backend (blood--sync-collect-package-specs))
  ;; TODO Build if cli arg says so
  )

(defun blood--sync-module-specs (spec)
  "get all active modules"
  (let ((search-base (plist-get (plist-get spec :paths) :modules))
        (modules (plist-get spec :modules))
        )

    (dolist (module modules)
      ;; seach locs for BLOOD-MODULE-FILE-PATTERN s
      ;; load each found module file, using defer--skip-loads
      nil
      )
    )
)

(defun blood--sync-collect-package-specs ()
  "flatten package specs from blood--module-packages"
  )

(provide 'blood-sync)
;;; blood-installer.el ends here
