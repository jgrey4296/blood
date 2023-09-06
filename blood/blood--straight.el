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
(require 'env)
(require 'blood-bootstrap)

(defvar blood--bootstrap-straight-location "straight/repos/straight.el" "relative path for straight to isntall into")

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

(defun blood--bootstrap-straight (spec)
  " download and initialize straight for package management
adapted from doom--ensure-straight
"
  (with-environment-variables (("GIT_CONFIG" nil)
                               ("GIT_CONFIG_NOSYSTEM" "1")
                               ("GIT_CONFIG_GLOBAL" (or (getenv "DOOMGITCONFIG") "/dev/null"))
                               )
    (let* ((spec-paths (plist-get spec :paths))
           (repo-dir (expand-file-name (or (plist-get spec-paths :install) blood--bootstrap-straight-location)
                                       blood-cache-dir))
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
               (blood--bootstrap-call "git" "clone" "--origin" "origin" branch-switch repo-url repo-dir))
              ((integerp vc-depth)
               (princ (format "- Cloning Depth %s Straight into: %s\n" vc-depth repo-dir))
               (make-directory repo-dir 'recursive)
               (let ((default-directory repo-dir))
                 (blood--bootstrap-call "git" "clone" "--origin" "origin"
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
  (princ "-- Initializing Straight")
  (let* ((paths (plist-get spec :paths))
         (recipes (plist-get spec :recipes))
         (repo-dir (expand-file-name (or (plist-get paths :install) blood--bootstrap-straight-location) blood-cache-dir))
         )
    (require 'straight (expand-file-name "straight" repo-dir))
    (setq straight-base-dir  blood-cache-dir
          straight-build-dir (plist-get paths :build)
          straight-build-cache-fixed-name ""
          straight-use-version-specific-build-dir t
          )
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
->  emacs.d/straight/{profile}/build-28.2
"
  (apply #'straight--dir (car (last blood-active-profile-specs)) straight-build-dir segments)
  )

(defun blood--bootstrap-straight-add-advice ()
  (advice-add 'straight--build-dir :override #'blood--bootstrap-straight-build-dir-advice)
  )

(defun blood--sync-straight (packages)
  "call straight-usej-package on each member of the input list"
  (dolist (spec packages)
    (let (recipe)
      ;; convert package specs to straight recipes
      (straight-use-package recipe nil t)
      )
    )
  )

(provide 'blood--straight)
;;; blood--straight.el ends here
