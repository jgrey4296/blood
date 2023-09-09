;;; blood-profile.el -*- lexical-binding: t; no-byte-compile: t; -*-
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
(message "----- Loading Profile")

(defvar blood-profile--declared-ht     (make-hash-table) "All declared profiles, which can be activated later")

(defvar blood-profile-active-specs nil "The Specs of the active profiles, as a stack. the original profile is last.")

(defvar blood-profile--installation-dir nil "The current profile's package installation directory")

(defvar blood-profile--clear-hook nil "Functions to run when clearing the profile stack")

(defvar blood-profile--post-activate-hook nil "Functions to run when activating a profile")

(defun blood-profile-start (&optional profile clear)
  "Start the cli specified / default profile"
  (interactive)
  (when clear
    (message "Clearing Profile Stack")
    (setq blood-profile-active-specs nil
          native-comp-eln-load-path (expand-file-name "eln-cache" blood-cache-dir)
          load-path blood--original-load-path
          )
    (run-hooks 'blood-profile--clear-hook)
    )
  (unless (or profile blood-profile--default)
    (error "No Profile Provided, no default profile set"))

  (let* ((profile (or profile blood-profile--default))
         (spec (if (stringp profile) (gethash (intern profile) blood-profile--declared-ht) profile))
         (backend (or (plist-get spec :backend) blood--backend-default))
         )
    (if (not spec)
        (error "No Matching Spec: %s" profile)
      (message "Activating Profile Spec: %s" (plist-get spec :name))
      (push spec blood-profile-active-specs)
      (funcall (plist-get backend :activator) spec)
      (run-hooks 'blood-profile--post-activate-hook)
      )
    )
  )

(defun blood-profile--register (spec)
  ""
  (let ((profile-name (intern (plist-get spec :name)))
        )
    ;; Handle non-interactive startup variations:
    (when (gethash profile-name blood-profile--declared-ht)
      (warn "Duplicated profile name, as the profile spec list is an alist, only the last profile of this name will be usable" 'profile profile-name))
    (message "Registering profile: %s" profile-name)
    (puthash profile-name spec blood-profile--declared-ht)
    )
  )

(defun blood--sync-profile (spec)
  (message "\n--- Syncing profile: %s" (plist-get spec :name))
  (let ((active-mods (blood--sync-module-specs spec)) ;; get active modules
        (packages (blood--sync-collect-package-specs))
        )
    (funcall (plist-get (or (plist-get spec :backend) blood--backend-default) :sync) packages)
    ;; TODO Build if cli arg says so
    )
  )

(defun blood-current-profile ()
  (car blood-profile-active-specs)
  )

(defmacro blood--args-to-profile-spec (profile-name default disabled args)
  "is a macro to allow expansion and checking of init.el"
  `(list
    :name                 ,profile-name
    :source               (file!)
    :default              ,default
    :disabled             ,disabled
    :bootstrap            ,(or (plist-get args :bootstrap) (list))
    :modules              ,(blood--args-to-module-decs (memq :active-modules args))
    :constraints          (list :system        ,(plist-get args :on-system)
                                :emacs-version ,(plist-get args :on-emacs)
                                )
    :paths                (list :install ,(plist-get args :install-to)
                                :build   ,(plist-get args :build-to)
                                :modules ,(cons 'list (plist-get args :modules-from))
                                )
    :recipes              ,(plist-get args :recipes)
    :block-compile-of     ,(plist-get args :block-compile-of)
    :post-activation      ,(when (plist-get args :on-activation)
                             `(lambda () ,@(plist-get args :on-activation)))
    )
  )

(defun blood-user-files! ()
  (setq user-emacs-directory (expand-file-name (file-name-concat (plist-get (blood-current-profile) :name) "user-files") blood-cache-dir))
  )

(defun blood-auto-saves! ()
  (setq auto-save-dir (expand-file-name (file-name-concat (plist-get (blood-current-profile) :name) "auto-saves") blood-cache-dir)
        auto-save-list-file-prefix (file-name-concat auto-save-dir "save-"))
  )

(add-hook 'blood-profile--post-activate-hook #'blood-user-files!)
(add-hook 'blood-profile--post-activate-hook #'blood-auto-saves!)

(provide 'blood-profile)
;;; blood-profile.el ends here
