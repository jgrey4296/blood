;;; bootstrap.el -*- lexical-binding: t; -*-
(llog! "Bootstrap")

(defvar BLOOD-BOOTSTRAPPED nil)

(defvar blood--bootstrap-queue nil)

(defcustom blood-additional-bootstrappers nil "functions to run as part of the bootstrap process" :type 'list)

(defcustom blood--backend-default nil
  "The plist describing the default backend to use. comprised of :name, :require, :bootstrap and :actviator ")

(defun blood-bootstrap-h ()
  "For each declared profile in the quue, bootstrap it, using blood--bootstrap-defaults
if the profile doesn't provide its own bootstrappers.
will always run blood--bootstrap-git-check and blood--bootstrap-core-paths "
  (hlog! "Bootstrapping")
  (unless blood--backend-default
    (require 'blood--straight)
    (setq blood--backend-default (list :name 'straight
                                       :require 'straight
                                       :bootstrap (list
                                                   #'blood--bootstrap-straight
                                                   #'blood--bootstrap-straight-init
                                                   )
                                       :activator #'blood--bootstrap-straight-init
                                       :sync #'blood--sync-straight
                                       ))
    )

  (blood--bootstrap-git-check)
  (blood--bootstrap-core-paths)
  (if (not blood--bootstrap-queue)
      (ilog! "Nothing More to bootstrap")
    (dolist (spec blood--bootstrap-queue)
      (ilog! "Bootstrapping Profile: %s" (plist-get spec :name))
      (dolist (fn (or (plist-get spec :bootstrap) (plist-get blood--backend-default :bootstrap)))
        (ilog! "Calling: %s" fn)
        (funcall fn spec)
        )
      )
    (setq BLOOD-BOOTSTRAPPED t)
    )
  )

(defun blood--bootstrap-core-paths ()
  "Make the core paths needed for running blood"
  (ghlog! "Bootstrapping Paths")
  (let ((cache-dir (expand-file-name  blood-cache-dir)))
    (unless (file-exists-p cache-dir)
      (ilog! "Making cache Directory")
      (make-directory cache-dir 'recursive))
    )
  (glogx!)
  )

(defun blood--bootstrap-git-check ()
  "bootstrap or complain about git"
  (ghlog! "Checking for Git")
  (unless (executable-find "git")
    (user-error "Git isn't present on your system. Cannot proceed."))
  (let* ((version (cdr (blood--call "git" "version")))
         (version
          (and (string-match "\\_<[0-9]+\\.[0-9]+\\(\\.[0-9]+\\)\\_>" version)
               (match-string 0 version))))
    (if version
        (when (version< version "2.23")
          (user-error "Git %s detected! blood requires git 2.23 or newer!"
                      version)))
    )
  (glogx!)
  )

(defun blood--bootstrap-env ()
  (ghlog! "Bootstrapping Env")

  (glogx!)
  )

(provide 'blood-bootstrap)
