;;; blood-profile.el -*- lexical-binding: t; no-byte-compile: t; -*-
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
(loaded? blood-defs blood-log blood-utils)
(llog! "Profile Lib")
(require 'blood-structs)

(defvar blood-profile--declared-ht     (make-hash-table) "All declared profiles, which can be activated later. Use `blood-profile--register' to add. keys are profile names as symbols ")

(defvar blood-profile-active-specs nil "The Specs of the active profiles, as a stack. the original profile is last.")

(defvar blood-profile--installation-dir nil "The current profile's package installation directory")

(defvar blood-profile--build-dir nil "The current profile's build directory")

(defvar blood-profile--clear-hook nil "Functions to run when clearing the profile stack")

(defvar blood-profile--post-activate-hook nil "Functions to run when activating a profile")

(defun blood-profile-start-h (&optional profile clear quiet)
  "Start the cli specified / default profile
Clears the profile stack,
sets paths for profile,
"
  (interactive)
  (when clear
    (ilog! "Clearing Profile Stack")
    (setq blood-profile-active-specs nil
          native-comp-eln-load-path (ensure-list (expand-file-name blood--eln-cache-name blood-cache-dir))
          load-path blood--original-load-path
          )
    (run-hooks 'blood-profile--clear-hook)
    )
  (unless (or profile blood-profile--default) (error "No Profile Provided, no default profile set"))

  (let* ((profile (or profile blood-profile--default))
         (spec (cond ((stringp profile) (gethash (intern profile) blood-profile--declared-ht))
                     ((symbolp profile) (gethash profile blood-profile--declared-ht))
                     (t profile)
                     ))
         (backend-sym (cond ((blood--profile-s-p spec)
                             (or (blood--profile-s-backend spec) blood--backend-default))
                            (t blood--backend-default)
                            ))
         )
    (blood--set-backend backend-sym)
    (unless quiet (hlog! "Activating Profile Spec: %s" (blood--profile-s-name spec)))
    (push spec blood-profile-active-specs)
    (ghlog! "Bootstapping Backend")
    (funcall (blood--backend-s-bootstrap blood--backend-active) spec)
    (glogx!)
    (ghlog! "Initialising Backend")
    (funcall (blood--backend-s-activator blood--backend-active) spec)
    (glogx!)
    (run-hooks 'blood-profile--post-activate-hook)
    (unless quiet (glogx!))
    )
  )

(defun blood-profile--register (spec)
  "Register a new profile"
  (let ((profile-name (blood--profile-s-name spec))
        )
    ;; Handle non-interactive startup variations:
    (when-let ((profile (gethash profile-name blood-profile--declared-ht)))
      (log! :warn "Duplicated profile name '%s', overriding" profile-name)
      (log! :warn "Existing profile: %s" profile)
      )
    (ilog! "+ Profile: %s" profile-name)
    (puthash profile-name spec blood-profile--declared-ht)
    )
  )

(defun blood-profile-current ()
  (car blood-profile-active-specs)
  )

(defun blood-user-files-h ()
  "set the location of the user emacs directory from the current profile"
  (setq user-emacs-directory (expand-file-name (file-name-concat "profiles" (blood--profile-s-name (blood-profile-current)) "user-files") blood-cache-dir))
  (ilog! "User Emacs Directory Set to: %s" user-emacs-directory)
  (unless (file-exists-p user-emacs-directory)
    (make-directory user-emacs-directory t)
      )
  )

(defun blood-auto-saves-h ()
  "set the location of the auto-save-dir using the current profile"
  (ilog! "Setting Auto Save location to profile: %s" (blood--profile-s-name (blood-profile-current)))
  (setq auto-save-dir (expand-file-name (file-name-concat
                                         (symbol-name (blood--profile-s-name (blood-profile-current)))
                                         "auto-save") blood-cache-dir)
        auto-save-list-file-prefix (file-name-concat auto-save-dir "save-")
        )
  )

(add-hook 'blood-profile--post-activate-hook #'blood-user-files-h)
(add-hook 'blood-profile--post-activate-hook #'blood-auto-saves-h)

(provide 'blood-profile)
;;; blood-profile.el ends here
