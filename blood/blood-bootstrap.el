;;; bootstrap.el -*- lexical-binding: t; -*-
(cl-assert (featurep 'blood-defs))
(cl-assert (featurep 'blood-log))
(cl-assert (featurep 'blood-utils))
(cl-assert (featurep 'blood-structs))
(llog! "Bootstrap")


(defvar blood--bootstrap-queue nil "A queue of profile specs to bootstrap")

(defcustom blood-additional-bootstrappers nil "functions to run as part of the bootstrap process" :type 'list)

(defcustom blood--backend-default nil
  "The plist describing the default backend to use. comprised of :name, :require, :bootstrap and :actviator ")

(defun blood-bootstrap-h ()
  "For each declared profile in the quue, bootstrap it, using blood--bootstrap-defaults
if the profile doesn't provide its own bootstrappers.
will always run blood--bootstrap-git-check-h and blood--bootstrap-core-paths-h "
  (hlog! "Bootstrapping")
  (when BLOOD-BOOTSTRAPPED
    (error "Trying to bootstrap blood when it's already been bootstrapped"))
  (unless blood--backend-default
    (require 'blood--straight)
    (setq blood--backend-default blood--bootstrap-straight-backend-default)
    )
  (add-hook 'blood-bootstrap-hook #'blood--bootstrap-env-h -90)
  (add-hook 'blood-bootstrap-hook #'blood--bootstrap-core-paths-h -89)
  (add-hook 'blood-bootstrap-hook #'blood--bootstrap-git-check-h - 88)

  (run-hooks 'blood-bootstrap-hook)
  (if (not blood--bootstrap-queue)
      (ilog! "Nothing More to bootstrap")
    (dolist (spec blood--bootstrap-queue)
      (ilog! "Bootstrapping Profile: %s" (blood--profile-s-name spec))
      (dolist (fn (or (blood--profile-s-bootstrap spec) (blood--backend-s-bootstrap blood--backend-default)))
        (ilog! "Calling: %s" fn)
        (funcall fn spec)
        )
      )
    )
  (setq BLOOD-BOOTSTRAPPED t)
  )

(defun blood--bootstrap-git-check-h ()
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

(defun blood--bootstrap-core-paths-h ()
  "Make the core paths needed for running blood"
  (ghlog! "Bootstrapping Paths")
  (let ((cache-dir (expand-file-name  blood-cache-dir)))
    (if (file-exists-p cache-dir)
        (ilog! "Cache Exists: %s" cache-dir)
      (ilog! "Making cache Directory: %s" cache-dir)
      (make-directory cache-dir 'recursive))
    )
  (glogx!)
  )

(defun blood--bootstrap-env-h ()
  (ghlog! "Bootstrapping Env")
  (warn "TODO: bootstrap blood env")
  (glogx!)
  )

(provide 'blood-bootstrap)
