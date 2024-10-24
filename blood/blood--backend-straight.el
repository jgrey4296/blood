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
(llog! "Straight backend")
(loaded? blood-backend)
(require 'env)

(defvar blood--backend-straight-location "repos/straight.el" "relative path for straight to install into")

(defvar blood--backend-straight-repo     "https://github.com/radian-software/straight.el" "the url for straight")

(defvar blood--backend-straight-branch   "develop" "the branch to use for straight")

(defvar blood--backend-straight-depth    1 "the depth straight should clone repos")

(defvar blood--backend-straight-single-branch t "whether straight should clone only the branch used")

(defconst blood--backend-straight-default-recipes
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
  "adapated from doom"
  )

(defvar blood--backend-straight-recipes nil
  "User registered recipes for straight"
  )

(defvar blood--backend-straight-initialised nil "marker variable for marking that straight is initialised")

(defconst blood--backend-straight-default
  (blood-build-backend 'straight
                      :requires 'straight
                      :bootstrap #'blood--backend-straight-bootstrap
                      :activator #'blood--backend-straight-activate
                      :sync      #'blood--backend-straight-sync
                      :clean     #'blood--backend-straight-clean
                      )
  "The default blood--backend-s for straight"
  )

(defun blood--backend-straight-bootstrap (spec)
  " download and initialise straight for package management
adapted from doom--ensure-straight
"
  (with-environment-variables (("GIT_CONFIG" nil)
                               ("GIT_CONFIG_NOSYSTEM" "1")
                               ("GIT_CONFIG_GLOBAL" (or (getenv "BLOODGITCONFIG") "/dev/null"))
                               )
    (let* ((spec-paths (blood--profile-s-paths spec))
           (repo-dir (file-name-concat (blood--paths-s-install spec-paths) "straight" "repos"))
           (repo-url blood--backend-straight-repo)
           (branch blood--backend-straight-branch)
           (vc-depth blood--backend-straight-depth)
           (branch-switch (if blood--backend-straight-single-branch "--single-branch" "--no-single-branch"))
           )
      (if (file-directory-p (file-name-concat repo-dir "straight")) ;; Do nothing if straight already exists
          (ilog! "Straight Exists: %s" repo-dir)
        (cond ((eq 'full vc-depth)
               (ilog! "- Cloning Full Depth Straight into: %s\n" repo-dir)
               (blood--dcall repo-dir "git" "clone" "--origin" "origin" branch-switch repo-url "straight"))
              ((integerp vc-depth)
               (ilog! "- Cloning Depth %s Straight into: %s" vc-depth repo-dir)
               (make-directory repo-dir 'recursive)
               (blood--dcall repo-dir "git" "clone" "--origin" "origin"
                             repo-url
                             "straight"
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

(defun blood--backend-straight-activate (spec)
  "Now that straight is bootstrapped, start it"
  (let* ((paths             (blood--profile-s-paths spec))
         (recipes           (blood--profile-s-recipes spec))
         (repo-dir (file-name-concat (blood--paths-s-install paths) "straight" "repos"))
         (profile-build-dir (blood--backend-straight-profile-build-dir spec))
         (straight-file-loc (file-name-concat repo-dir "straight" "straight.el"))
         (profile-sym (blood-uniq-id spec))
         (profile-cache-dir (expand-file-name (file-name-concat "profiles" (symbol-name profile-sym)) blood-cache-dir))
         )
    (ilog! "Loading straight from: %s" straight-file-loc)
    (require 'straight straight-file-loc)
    (unless (file-directory-p profile-cache-dir)
      (ilog! "Building Profile Cache Dir: %s" profile-cache-dir)
      (make-directory profile-cache-dir 'recursive))
    (ilog! "Setting Straight Vars")
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

          blood-profile--installation-dir repo-dir
          blood-profile--build-dir        profile-build-dir
          )
    (ilog! "- Set blood-profile--installation-dir : %s" blood-profile--installation-dir)
    (ilog! "- Set blood-profile--build-dir : %s" blood-profile--build-dir)
    (unless (alist-get profile-sym straight-profiles nil)
      (ilog! "- Adding profile to straight-profiles")
      (push `(,profile-sym . ,(file-name-concat profile-cache-dir "straight-lockfile.el")) straight-profiles))
    (unless (file-directory-p profile-build-dir)
      (ilog! "Building Profile Build Dir: %s" profile-build-dir)
      (make-directory profile-build-dir 'recursive))
    ;; (blood--backend-straight-add-advice)
    (ilog! "Setting Default Recipes")
    (mapc #'straight-use-recipes blood--backend-straight-default-recipes)
    (ilog! "Adding Profile Recipes")
    (mapc #'straight-use-recipes recipes)
    (setq blood--backend-straight-initialised t)
    )
  )

(defun blood--backend-straight-sync (packages)
  "call straight-use-package on each member of the input list"
  (ilog! "[straight] Cache File: %s" (straight--build-cache-file))
  (ilog! "[straight] Build Dir: %s" (straight--build-dir))
  (unless (and (fboundp 'straight-use-package) (fboundp 'straight--transaction-finalize))
    (error 'blood 'missing-straight-fns))

  (dolist (spec packages)
    (let* ((package (blood--identifier-s-package (blood--package-s-id spec)))
           (recipe  (blood--package-to-straight-recipe spec))
           )
      (ghlog! "Installing package: %s" package)
      (ilog! "Recipe: %s" recipe)
      (straight-use-package recipe)
      (glogx!)
      )
    )
  (ilog! "[straight] Finalizing Transaction")
  (straight--transaction-finalize)
  )

(defun blood--backend-straight-clean ()
  (ilog! "[straight] Cleaning : %s" (straight--build-dir))
  (delete-directory (straight--build-dir) t)
  (delete-directory (straight--modified-dir))
  (delete-file (straight--build-cache-file))
  )

(defun blood--backend-straight-profile-build-dir (name-or-spec)
  " Get the build directory of the profile,
relative to straight-dase-dir (ie: blood-cache-dir)
eg: profile:simple -> [.cache/blood]/build/emacs-29/simple
 "
  (let* ((name (blood-uniq-id name-or-spec))
         (spec (gethash name blood-profile--declared-ht))
         (paths (blood--profile-s-paths spec))
         )
    (cond ((and paths (blood--paths-s-build paths))
           (blood--paths-s-build paths))
          (t
           (format "build/emacs-%s/%s" emacs-version name))
          )
    )
  )

(defun blood--backend-straight-build-dir-ad (&rest segments)
  " for overriding 'straight--build-dir to build packages
in a profile specific subdirectory

eg: emacs.d/straight/build-28.2
->  emacs.d/straight/build/emacs-28.2/{profile}
"
  (let* ((spec (blood-profile-current))
         (name (blood-uniq-id spec))
         (paths (blood--profile-s-paths spec))
         (profile-build-dir (or (blood--paths-s-build paths)
                                (format "build/emacs-%s/%s" emacs-version name)))
         )
    (apply #'straight--dir profile-build-dir segments)
    )
  )

(defun blood--backend-straight-add-advice ()
  (ilog! "Adding Advice to Straight")
  (advice-add 'straight--build-dir :override #'blood--backend-straight-build-dir-ad)
  )

(defun blood--package-to-straight-recipe (package-spec)
  " Create a recipe from a blood--package-s,
returns a list for straight"
  (let ((recipe (blood--package-s-recipe package-spec))
        (package (blood--identifier-s-package (blood--package-s-id package-spec)))
        )
    (cond ((null recipe)
           (append (list pacakge (cdr (assq 'melpa blood--backend-straight-default-recipes)))))
          ((and (symbolp recipe) (assq recipe blood--backend-straight-recipes))
           (append (list package)
                   (cdr (assq recipe blood--backend-straight-recipes))))
          ((and (symbolp recipe) (assq recipe blood--backend-straight-default-recipes))
           (append (list package)
                   (cdr (assq recipe blood--backend-straight-default-recipes))))
          ((blood--recipe-s-p recipe)
           (append (list package)
                   (when-let ((host (blood--recipe-s-host recipe))) (list :host host))
                   (when-let ((repo (blood--recipe-s-repo recipe))) (list :repo repo))
                   (when-let ((files (blood--recipe-s-files recipe))) (list :files files))
                   (when-let ((local (blood--recipe-s-local-repo recipe))) (list :local-repo local))
                   (when-let ((comp (blood--recipe-s-compilation recipe))) (list :compilation comp))
                   )
           )
          ((listp recipe)
           (append (list package) recipe))
          (t (error "Unknown recipe format for package" package-spec))
          )
    )
  )

(defun in-straight! (base &rest args)
  "util to create a path string inside the blood straight dir"
  (expand-file-name (apply #'file-name-concat "straight" args) base))

;; TODO handle straight's popups that don't work in noninteractive ?

(provide 'blood--backend-straight)
;;; blood--straight.el ends here
