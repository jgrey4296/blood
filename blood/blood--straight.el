 ;;; blood--straight.el -*- lexical-binding: t; no-byte-compile: t; -*-
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
;;  integrates straight into the blood bootstrap and sync api
;;
;;; Code:
;;-- end header
(message "----- Loading Straight")
(require 'env)

(defvar blood--bootstrap-straight-location "repos/straight.el" "relative path for straight to isntall into")

(defvar blood--bootstrap-straight-repo     "https://github.com/radian-software/straight.el" "the url for straight")

(defvar blood--bootstrap-straight-branch   "develop" "the branch to use for straight")

(defvar blood--bootstrap-straight-depth    1 "the depth straight should clone repos")

(defvar blood--bootstrap-straight-single-branch t "whether straight should clone only the branch used")

(defconst blood--bootstrap-straight-default-recipes
  '((org-elpa :local-repo nil)
    (melpa              :type git :host github
                        :repo "melpa/melpa"
                        :build nil)
    (nongnu-elpa        :type git
                        :repo "https://git.savannah.gnu.org/git/emacs/nongnu.git"
                        :local-repo "nongnu-elpa"
                        :build nil)
    (gnu-elpa-mirror    :type git :host github
                        :repo "emacs-straight/gnu-elpa-mirror"
                        :build nil)
    (el-get             :type git :host github
                        :repo "dimitri/el-get"
                        :build nil)
    (emacsmirror-mirror :type git :host github
                        :repo "emacs-straight/emacsmirror-mirror"
                        :build nil))

  )

(defvar blood--straight-initialised nil)

(defun in-straight! (&rest args)
  (expand-file-name (apply #'file-name-concat "straight" args) blood-cache-dir))

(defun blood--bootstrap-straight (spec)
  " download and initialize straight for package management
adapted from doom--ensure-straight
"
  (with-environment-variables (("GIT_CONFIG" nil)
                               ("GIT_CONFIG_NOSYSTEM" "1")
                               ("GIT_CONFIG_GLOBAL" (or (getenv "DOOMGITCONFIG") "/dev/null"))
                               )
    (let* ((spec-paths (plist-get spec :paths))
           (repo-dir (in-straight! (or (plist-get spec-paths :install) blood--bootstrap-straight-location)))
           (repo-url blood--bootstrap-straight-repo)
           (branch blood--bootstrap-straight-branch)
           (vc-depth blood--bootstrap-straight-depth)
           (branch-switch (if blood--bootstrap-straight-single-branch "--single-branch" "--no-single-branch"))
           )
      (if (file-directory-p repo-dir) ;; Do nothing if straight already exists for this profile spec
          (princ (format "-- Straight Exists: %s\n" repo-dir))
        (princ "-- Bootstrapping straight...\n")
        (cond ((eq 'full vc-depth)
               (princ (format "- Cloning Full Depth Straight into: %s\n" repo-dir))
               (blood--call "git" "clone" "--origin" "origin" branch-switch repo-url repo-dir))
              ((integerp vc-depth)
               (princ (format "- Cloning Depth %s Straight into: %s\n" vc-depth repo-dir))
               (make-directory repo-dir 'recursive)
               (let ((default-directory repo-dir))
                 (blood--call "git" "clone" "--origin" "origin"
                                          repo-url
                                          repo-dir
                                          "--depth" (number-to-string vc-depth)
                                          branch-switch
                                          "--no-tags"
                                          "--branch" branch)
                 )
               )
              )
        )
      )
    )
  )

(defun blood--bootstrap-straight-init (spec)
  "Now that straight is bootstrapped, start it"
  (message "-- Initializing Straight: %s" (plist-get spec :name))
  (let* ((paths (plist-get spec :paths))
         (recipes (plist-get spec :recipes))
         (repo-dir (or (plist-get paths :install) blood--bootstrap-straight-location))
         (profile-build-dir (or (plist-get paths :build)
                                (format "build-%s-%s" emacs-version (plist-get spec :name))))
         (straight-file-loc (in-straight! repo-dir "straight"))
         (profile-sym (intern (plist-get spec :name)))
         (profile-cache-dir (expand-file-name (file-name-concat "profiles" (plist-get spec :name)) blood-cache-dir))
         )
    (require 'straight straight-file-loc)
    (unless (file-directory-p profile-cache-dir) (make-directory profile-cache-dir 'recursive))
    (setq straight-base-dir  blood-cache-dir
          straight-build-dir profile-build-dir
          straight-build-cache-fixed-name nil
          straight-check-for-modifications nil
          straight-current-profile (intern (plist-get spec :name))
          straight--packages-not-to-rebuild (make-hash-table)

          straight-disable-native-compile t
          straight-disable-autoloads t

          blood-profile--installation-dir (in-straight! "repos")
          )
    (unless (alist-get profile-sym straight-profiles nil)
      (push `(,profile-sym . ,(file-name-concat profile-cache-dir "straight-lockfile.el")) straight-profiles))
    ;; (clrhash straight--success-cache)

    (unless (file-directory-p (in-straight! profile-build-dir))
      (make-directory (in-straight! profile-build-dir) 'recursive))
    (blood--bootstrap-straight-add-advice)
    (mapc #'straight-use-recipes blood--bootstrap-straight-default-recipes)
    (mapc #'straight-use-recipes recipes)
    (setq blood--straight-initialised t)
    )
  )

(defun blood--bootstrap-straight-build-dir-advice (&rest segments)
  " for overriding 'straight--build-dir to build packages
in a profile specific subdirectory

eg: emacs.d/straight/build-28.2
->  emacs.d/straight/build-28.2-{profile}
"
  (let* ((spec (blood-current-profile))
         (profile-build-dir (or (plist-get (plist-get spec :paths) :build)
                                (format "build-%s-%s" emacs-version (plist-get spec :name))))
         )

    (apply #'straight--dir profile-build-dir segments)
    )
  )

(defun blood--bootstrap-straight-add-advice ()
  (advice-add 'straight--build-dir :override #'blood--bootstrap-straight-build-dir-advice)
  )

(defun blood--sync-straight (packages)
  "call straight-usej-package on each member of the input list"
  (message "-- Straight syncing packages: %s : %s" (plist-get (blood-current-profile) :name) (straight--build-cache-file))
  (message "- Build Dir: %s" (straight--build-dir))

  (dolist (spec packages)
    (let (recipe)
      (message "- Straight installing package: %s" recipe)
      ;; convert package specs to straight recipes
      (straight-use-package recipe nil t)
      )
    )
  ;; (let ((straight--packages-to-rebuild :all))
    (message "Using evil in %s : %s" (plist-get (blood-current-profile) :name) straight-current-profile)
    (straight-use-package 'evil nil nil)
    (straight--transaction-finalize)
    ;; )
  )

(provide 'blood--straight)
;;; blood--straight.el ends here
