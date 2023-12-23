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
(llog! "Straight boostrapper")
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

(defun blood--bootstrap-straight-profile-build-dir (name-or-spec)
  (let* ((name (if (stringp name-or-spec) name-or-spec (plist-get name-or-spec :name)))
         (spec (if (stringp name-or-spec) (gethash name-or-spec blood-profile--declared-ht) name-or-spec))
         (paths (plist-get spec :paths))
         )
    (cond ((and paths (plist-get paths :build))
           (plist-get paths :build))
          ((plist-get spec :name)
           (format "build-%s-%s" emacs-version (plist-get spec :name)))
          (t
           (format "build-%s-%s" emacs-version name))
          )
    )
  )

(defun blood--bootstrap-straight (spec)
  " download and initialize straight for package management
adapted from doom--ensure-straight
"
  (with-environment-variables (("GIT_CONFIG" nil)
                               ("GIT_CONFIG_NOSYSTEM" "1")
                               ("GIT_CONFIG_GLOBAL" (or (getenv "BLOODGITCONFIG") "/dev/null"))
                               )
    (let* ((spec-paths (plist-get spec :paths))
           (repo-dir (in-straight! (or (plist-get spec-paths :install) blood--bootstrap-straight-location)))
           (repo-url blood--bootstrap-straight-repo)
           (branch blood--bootstrap-straight-branch)
           (vc-depth blood--bootstrap-straight-depth)
           (branch-switch (if blood--bootstrap-straight-single-branch "--single-branch" "--no-single-branch"))
           )
      (if (file-directory-p repo-dir) ;; Do nothing if straight already exists for this profile spec
          (ilog! "Straight Exists: %s" repo-dir)
        (ilog! "Bootstrapping straight...")
        (cond ((eq 'full vc-depth)
               (ilog! "- Cloning Full Depth Straight into: %s\n" repo-dir)
               (blood--call "git" "clone" "--origin" "origin" branch-switch repo-url repo-dir))
              ((integerp vc-depth)
               (ilog! " Cloning Depth %s Straight into: %s" vc-depth repo-dir)
               (make-directory repo-dir 'recursive)
               (blood--dcall repo-dir "git" "clone" "--origin" "origin"
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

(defun blood--bootstrap-straight-init (spec)
  "Now that straight is bootstrapped, start it"
  (glog! "Initializing Straight: %s" (plist-get spec :name))
  (let* ((paths (plist-get spec :paths))
         (recipes (plist-get spec :recipes))
         (repo-dir (or (plist-get paths :install) blood--bootstrap-straight-location))
         (profile-build-dir (blood--bootstrap-straight-profile-build-dir spec))
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
          straight-cache-autoloads nil

          straight-disable-native-compile t
          straight-disable-compile        t
          straight-disable-autoloads      t

          blood-profile--installation-dir (in-straight! repo-dir)
          blood-profile--build-dir        (in-straight! profile-build-dir)
          )
    (unless (alist-get profile-sym straight-profiles nil)
      (push `(,profile-sym . ,(file-name-concat profile-cache-dir "straight-lockfile.el")) straight-profiles))

    (unless (file-directory-p (in-straight! profile-build-dir))
      (make-directory (in-straight! profile-build-dir) 'recursive))
    (blood--bootstrap-straight-add-advice)
    (mapc #'straight-use-recipes blood--bootstrap-straight-default-recipes)
    (mapc #'straight-use-recipes recipes)
    (setq blood--straight-initialised t)
    )
  (glogx!)
  )

(defun blood--bootstrap-straight-build-dir-advice (&rest segments)
  " for overriding 'straight--build-dir to build packages
in a profile specific subdirectory

eg: emacs.d/straight/build-28.2
->  emacs.d/straight/build-28.2-{profile}
"
  (let* ((spec (blood-profile-current))
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
  "call straight-use-package on each member of the input list"
  (ghlog! "Sync: Installing Module Components using Straight: %s" (plist-get (blood-profile-current) :name))
  (ilog! "Cache File: %s" (straight--build-cache-file))
  (ilog! "Build Dir: %s" (straight--build-dir))
  (unless (and (fboundp 'straight-use-package) (fboundp 'straight--transaction-finalize))
    (error 'blood 'missing-straight-fns))

  (dolist (spec packages)
    (let ((package (plist-get (plist-get spec :id) :package))
          (recipe  (plist-get spec :recipe))
          )
      (ilog! "Straight installing package: %s" package)
      ;; TODO convert package specs to straight recipes
      (cond ((eq recipe 'melpa)
             (straight-use-package package nil)
             )
            ((and (plistp recipe) (eq (plist-get recipe :host) 'github))
             (ilog! "Handling Github recipe")
             (straight-use-package (append (list package :type 'git)
                                           recipe))
             )
            (t (straight-use-package recipe nil))
            )
      )
    )
  (ilog! "Finalizing Straight Transaction")
  (straight--transaction-finalize)
  (glogx!)
  )

;; TODO handle straight's popups that don't work in noninteractive

(provide 'blood--straight)
;;; blood--straight.el ends here
