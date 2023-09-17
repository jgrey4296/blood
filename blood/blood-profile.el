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
(ilog! "Loading Profile Lib")

(defconst BLOOD-PROFILE-FILE-PATTERN "profile\\(-.+\\)?.el" "blood will search and load all profiles in files with this name")

(defvar blood-profile--declared-ht     (make-hash-table) "All declared profiles, which can be activated later")

(defvar blood-profile-active-specs nil "The Specs of the active profiles, as a stack. the original profile is last.")

(defvar blood-profile--installation-dir nil "The current profile's package installation directory")

(defvar blood-profile--clear-hook nil "Functions to run when clearing the profile stack")

(defvar blood-profile--post-activate-hook nil "Functions to run when activating a profile")

(defun blood-profile-start (&optional profile clear)
  "Start the cli specified / default profile"
  (interactive)
  (glog! "Starting Profile")
  (when clear
    (ilog! "Clearing Profile Stack")
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
      (ilog! "Activating Profile Spec: %s" (plist-get spec :name))
      (push spec blood-profile-active-specs)
      (funcall (plist-get backend :activator) spec)
      (run-hooks 'blood-profile--post-activate-hook)
      )
    )
  (glogx!)
  )

(defun blood-profile--register (spec)
  ""
  (let ((profile-name (intern (plist-get spec :name)))
        )
    ;; Handle non-interactive startup variations:
    (when (gethash profile-name blood-profile--declared-ht)
      (warn "Duplicated profile name, as the profile spec list is an alist, only the last profile of this name will be usable" 'profile profile-name))
    (ilog! "Registering profile: %s" profile-name)
    (puthash profile-name spec blood-profile--declared-ht)
    )
  )

(defun blood-profile-current ()
  (car blood-profile-active-specs)
  )

(defmacro blood-profile--build-spec (profile-name default disabled args)
  "is a macro to allow expansion and checking of init.el"
  `(prog2
     (glog! "Building Profile Spec")
     (list
      :name                 ,profile-name
      :source               (file!)
      :default              ,default
      :disabled             ,disabled
      :bootstrap            ,(or (plist-get args :bootstrap) (list))
      :modules              ,(blood-profile--build-active-modules (memq :active-modules: args))
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
     (glogx!)
     )
  )

(defun blood-profile--build-active-modules (lst)
  "Convert the remaining list into a list of (:group %s :module %s :allow () :disallow ())"
  (let ((source (cl-copy-list lst))
        curr res)
    (ilog! "Parsing Module List: %s" lst)
    (while source
      (pcase (pop source)
        (:active-modules: nil)
        ((and kw (pred keywordp) (guard (memq kw '(:allow :disallow))))
         (setq curr (append curr (list kw (pop source))))
         )
        ((and kw (pred keywordp))
         (when curr (push curr res) (setq curr nil))
         (ilog! "Handling: %s" kw)
         (setq curr (append curr (list :group (substring (symbol-name kw) 1)
                                       :module (symbol-name (pop source))
                                       )))
         )
        (other
         (ilog! "Unknown module dec: %s" other)
         )
        )
      )
    (push curr res)
    (ilog! "Active Modules Parsed: %s" res)
    (list 'quote res)
    )
  )

(defun blood-user-files! ()
  (setq user-emacs-directory (expand-file-name (file-name-concat (plist-get (blood-profile-current) :name) "user-files") blood-cache-dir))
  )

(defun blood-auto-saves! ()
  (setq auto-save-dir (expand-file-name (file-name-concat (plist-get (blood-profile-current) :name) "auto-saves") blood-cache-dir)
        auto-save-list-file-prefix (file-name-concat auto-save-dir "save-"))
  )

(add-hook 'blood-profile--post-activate-hook #'blood-user-files!)
(add-hook 'blood-profile--post-activate-hook #'blood-auto-saves!)

(provide 'blood-profile)
;;; blood-profile.el ends here
