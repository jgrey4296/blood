 ;;; blood--straight.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- header
;;
;; Copyright (C) 2023 John Grey
;;
;; Author: John Grey <https://github.com/jgrey4296>
;; Maintainer: John Grey <johngrey@Johns-Mac-mini.local>
;; Created: September 01, 2023
;; Modified: September 01, 2023
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
  '((org-elpa           :local-repo nil)
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

(defvar blood--straight-recipes nil
  "Additional registered recipes for straight"
  )

(defvar blood--straight-initialised nil)

(defun in-straight! (&rest args)
  (expand-file-name (apply #'file-name-concat "straight" args) blood-cache-dir))

(defun blood--bootstrap-straight-profile-build-dir (name-or-spec)
  " Get the build directory of the profile "
  (let* ((name (blood-uniq-id name-or-spec))
         (spec (gethash name blood-profile--declared-ht))
         (paths (blood--profile-s-paths spec))
         )
    (cond ((and paths (blood--paths-s-build paths))
           (blood--paths-s-build paths))
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
    (let* ((spec-paths (blood--profile-s-paths spec))
           (repo-dir (in-straight! (or (blood--paths-s-instal spec-paths) blood--bootstrap-straight-location)))
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
  (glog! "Initializing Straight: %s" (blood-uniq-id spec))
  (let* ((paths (blood--profile-s-paths spec))
         (recipes (blood--profile-s-recipes spec))
         (repo-dir (or (blood--paths-s-install paths) blood--bootstrap-straight-location))
         (profile-build-dir (blood--bootstrap-straight-profile-build-dir spec))
         (straight-file-loc (in-straight! repo-dir "straight"))
         (profile-sym (blood-uniq-id spec))
         (profile-cache-dir (expand-file-name (file-name-concat "profiles" (blood-uniq-id spec)) blood-cache-dir))
         )
    (require 'straight straight-file-loc)
    (unless (file-directory-p profile-cache-dir) (make-directory profile-cache-dir 'recursive))
    (setq straight-base-dir  blood-cache-dir
          straight-build-dir profile-build-dir
          straight-build-cache-fixed-name nil
          straight-check-for-modifications nil
          straight-current-profile (blood-uniq-id spec)
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
         (profile-build-dir (or (blood--paths-s-build (blood--profile-s-paths spec))
                                (format "build-%s-%s" emacs-version (blood-uniq-id spec))))
         )

    (apply #'straight--dir profile-build-dir segments)
    )
  )

(defun blood--bootstrap-straight-add-advice ()
  (advice-add 'straight--build-dir :override #'blood--bootstrap-straight-build-dir-advice)
  )

(defun blood--sync-straight (packages)
  "call straight-use-package on each member of the input list"
  (ghlog! "Sync: Installing Module Components using Straight: %s" (blood-uniq-id (blood-profile-current)))
  (ilog! "Cache File: %s" (straight--build-cache-file))
  (ilog! "Build Dir: %s" (straight--build-dir))
  (unless (and (fboundp 'straight-use-package) (fboundp 'straight--transaction-finalize))
    (error 'blood 'missing-straight-fns))

  (dolist (spec packages)
    (let* ((package (blood--identifier-s-package (blood--package-s-id spec)))
           (recipe  (blood--package-to-straight-recipe spec))
           )
      (ilog! "Straight installing package: %s" package)
      (straight-use-package recipe)
      )
    (ilog! "Finalizing Straight Transaction")
    (straight--transaction-finalize)
    (glogx!)
    )
  )

(defun blood--package-to-straight-recipe (package-spec)
  " Create a recipe from a package spec,
returns a list for straight"
  (let ((recipe (blood--package-s-recipe package-spec))
        (package (blood--identifier-s-package (blood--package-s-id package-spec)))
        )
    (cond ((null recipe)
           (append (list pacakge (assq 'melpa blood--bootstrap-straight-default-recipes))))
          ((and (symbolp recipe)
                (assq recipe (append blood--bootstrap-straight-default-recipes
                                     blood--straight-recipes)))
           (append (list package)
                   (assq recipe (append blood--bootstrap-straight-default-recipes
                                        blood--straight-recipes))))
          ((blood--recipe-s-p recipe)
           (append (list package)

                   )
           )
          (t (error "Unknown recipe format for package" package-spec))
          )
    )
  )


;; TODO handle straight's popups that don't work in noninteractive

(provide 'blood--straight)
;;; blood--straight.el ends here
