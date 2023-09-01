;;; re-doom--straight.el -*- lexical-binding: t; no-byte-compile: t; -*-
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
;;  integrates straight into the re-doom bootstrap and sync api
;;
;;; Code:
;;-- end header
(defvar re-doom--bootstrap-straight-location "repos/straight.el" "relative path for straight to isntall into" :type 'string)
(defvar re-doom--bootstrap-straight-repo     "https://github.com/radian-software/straight.el" "the url for straight" :type 'string)
(defvar re-doom--bootstrap-straight-branch   "develop" "the branch to use for straight")
(defvar re-doom--bootstrap-straight-depth    1 "the depth straight should clone repos")
(defvar re-doom--bootstrap-straight-single-branch t "whether straight should clone only the branch used")

(defun re-doom--bootstrap-straight (spec)
  " download and initialize straight for package management
adapted from doom--ensure-straight
"
  (letenv! (("GIT_CONFIG" nil)
            ("GIT_CONFIG_NOSYSTEM" "1")
            ("GIT_CONFIG_GLOBAL" (or (getenv "DOOMGITCONFIG") "/dev/null"))
            )
    (let* ((spec-paths (plist-get spec :paths))
           (repo-dir (expand-file-name (or (plist-get spec-paths :install) re-doom--bootstrap-straight-location)
                                       re-doom-cache-dir))
           (repo-url re-doom--bootstrap-straight-repo)
           (branch re-doom--bootstrap-straight-branch)
           (vc-depth re-doom--bootstrap-straight-depth)
           (branch-switch (if re-doom--bootstrap-straight-single-branch "--single-branch" "--no-single-branch"))
           )
      (unless (file-directory-p repo-dir) ;; Do nothing if straight already exists for this profile spec
        (princ "-- Bootstrapping straight...")
        (cond ((eq 'full vc-depth)
               (princ (format "- Cloning Full Depth Straight into: %s" repo-dir))
               (re-doom--bootstrap-call "git" "clone" "--origin" "origin" branch-switch repo-url repo-dir))
              ((integerp vc-depth)
               (princ (format "- Cloning %s Depth Straight into: %s" vc-depth repo-dir))
               (make-directory repo-dir 'recursive)
               (let ((default-directory repo-dir))
                 (re-doom--bootstrap-call "git" "init")
                 (re-doom--bootstrap-call "git" "remote" "add" "origin" repo-url
                                          "--master" re-doom--bootstrap-straight-branch)
                 (re-doom--bootstrap-call "git" "fetch" "origin" pin
                                          "--depth" (number-to-string vc-depth)
                                          "--no-tags")
                 (re-doom--bootstrap-call "git" "branch" "-m" re-doom--bootstrap-straight-branch)
                 (re-doom--bootstrap-call "git" "reset" "--hard" pin)))
              )
        )
      )
    )
  )

(defun re-doom--bootstrap-straight-init (spec)
  "Now that straight is bootstrapped, start it"
  (require 'straight (expand-file-name "" re-doom-cache-dir))
  (princ "-- Initializing Straight")
  (let ((paths (plist-get spec :paths))
        (recipes (plist-get spec :recipes)))
    (setq straight-base-dir  re-doom-cache-dir
          straight-build-dir (plist-get paths :build)
          straight-build-cache-fixed-name ""
          straight-use-version-specific-build-dir t
          )
    (mapc #'straight-use-recipes
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
                                :build nil)))
    (mapc #'straight-use-recipes recipes)
    )
  )

(defun re-doom--bootstrap-straight-build-dir-advice (&rest segments)
  " for overriding 'straight--build-dir to build packages
in a profile specific subdirectory

eg: emacs.d/straight/build-28.2
->  emacs.d/straight/{profile}/build-28.2
"
  (apply #'straight--dir (car (last re-doom-active-profile-specs)) straight-build-dir segments)
  )

(defun re-doom--bootstrap-straight-add-advice ()
  (advice-add 'straight--build-dir :override #'re-doom--bootstrap-straight-build-dir-advice)
  )

(provide 're-doom--straight)
;;; re-doom--straight.el ends here
