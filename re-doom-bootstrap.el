;;; bootstrap.el -*- lexical-binding: t; -*-
;;-- vars

(defgroup 're-doom--bootstrap)

(defcustom re-doom--bootstrap-straight-location "straight/repos/straight.el" "where to bootstrap straight to" :type 'string)
(defcustom re-doom--bootstrap-straight-repo     "https://github.com/radian-software/straight.el" "the url for straight" :type 'string)
(defcustom re-doom--bootstrap-straight-branch   "develop")
(defcustom re-doom--bootstrap-straight-depth    1)
(defcustom re-doom--bootstrap-straight-single-branch t)

(defcustom re-doom--additional-bootstrappers nil "functions to run as part of the bootstrap process" :type 'list)

(defcustom re-doom-cache-dir (expand-file-name "~/.config/emacs/"))

(defvar re-doom--bootstrap-queue nil)
;;-- end vars

(defun re-doom--boostrap ()
  " "
  (re-doom--bootstrap-git-check)
  (dolist (spec re-doom--bootstrap-queue)
    (princ (format "*Boostrapping Profile: %s*" (plist-get spec :name)))
    (re-doom--bootstrap-paths    spec)
    (re-doom--bootstrap-straight spec)
    (re-doom--init-straight      spec)
    )
  )

(defun re-doom--bootstrap-paths (specs)
  " setup the cache if it doesn't exist yet"
  (princ "-- Bootstrapping Paths")
  (let ((straight-dir (expand-file-name re-doom--bootstrap-straight-location re-doom-cache-dir)))
    (unless (file-exists-p straight-dir)
      (make-directory straight-dir 'recursive))
    )
  )

(defun re-doom--bootstrap-straight (specs)
  " download and initialize straight for package management
adapted from doom--ensure-straight
"
  (letenv! (("GIT_CONFIG" nil)
            ("GIT_CONFIG_NOSYSTEM" "1")
            ("GIT_CONFIG_GLOBAL" (or (getenv "DOOMGITCONFIG") "/dev/null"))
            )
    (let* ((repo-dir (expand-file-name (or (plist-get specs :straight-dir) re-doom--bootstrap-straight-location) re-doom-cache-dir))
           (repo-url re-doom--bootstrap-straight-repo)
           (branch re-doom--bootstrap-straight-branch)
           (vc-depth re-doom--boostrap-straight-depth)
           (branch-switch (if re-doom--bootstrap-straight-single-branch "--single-branch" "--no-single-branch"))
           )
      (unless (file-directory-p repo-dir)
        (princ "-- Bootstrapping straight...")
        (cond
         ((eq 'full vc-depth)
          (funcall call "git" "clone" "--origin" "origin"
                   branch-switch repo-url repo-dir))
         ((and (integerp vc-depth) (null pin))
          (when (file-directory-p repo-dir)
            (delete-directory repo-dir 'recursive))
          (re-doom--bootstrap-call "git" "clone" "--origin" "origin" repo-url
                                   "--no-checkout" repo-dir
                                   "--depth" (number-to-string vc-depth)
                                   branch-switch
                                   "--no-tags"
                                   "--branch" straight-repository-branch))
         ((integerp vc-depth)
          (make-directory repo-dir 'recursive)
          (let ((default-directory repo-dir))
            (re-doom--bootstrap-call "git" "init")
            (re-doom--bootstrap-call "git" "branch" "-m" straight-repository-branch)
            (re-doom--bootstrap-call "git" "remote" "add" "origin" repo-url
                     "--master" straight-repository-branch)
            (re-doom--bootstrap-call "git" "fetch" "origin" pin
                     "--depth" (number-to-string vc-depth)
                     "--no-tags")
            (re-doom--bootstrap-call "git" "reset" "--hard" pin)))
         )
        )
      )
    )
  )

(defun re-doom--init-straight ()
  (require 'straight (expand-file-name "" re-doom-cache-dir))
  (princ "-- Initializing Straight")
  (setq straight-base-dir  re-doom-cache-dir
        straight-build-dir
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
  )

(defun re-doom--bootstrap-git-check ()
  (princ "-- Checking for Git")
  (unless (executable-find "git")
    (user-error "Git isn't present on your system. Cannot proceed."))
  (let* ((version (cdr (re-doom--bootstrap-call "git" "version")))
         (version
          (and (string-match "\\_<[0-9]+\\.[0-9]+\\(\\.[0-9]+\\)\\_>" version)
               (match-string 0 version))))
    (if version
        (when (version< version "2.23")
          (user-error "Git %s detected! re-doom requires git 2.23 or newer!"
                      version))))
  )

(defun re-doom--bootstrap-call (prog &rest args)
  (with-temp-buffer
    (cons (or (format "%d" (apply #'call-process prog nil t nil args))
              -1)
          (string-trim (buffer-string)))
    )
  )

(defun re-doom--bootstrap-straight-build-dir-advice (&rest segments)
  " for overriding 'straight--build-dir to build packages
in a profile specific subdirectory

eg: emacs.d/straight/build-28.2
->  emacs.d/straight/{profile}/build-28.2
"
  )


(provide 're-doom-bootstrap)
