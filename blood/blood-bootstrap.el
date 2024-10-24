;;; bootstrap.el -*- lexical-binding: t; -*-
(loaded? blood-defs blood-log blood-utils blood-structs blood-backend)
(llog! "Bootstrap")

(defvar blood--bootstrap-queue nil "A queue of profile specs to bootstrap")

(defcustom blood-additional-bootstrappers nil "functions to run as part of the bootstrap process" :type 'list)

(defun blood-bootstrap-h ()
  "For each declared profile in the queue, bootstrap it.
will always run blood--bootstrap-git-check-h and blood--bootstrap-core-paths-h "
  (hlog! "Bootstrapping")
  (when BLOOD-BOOTSTRAPPED (error "Trying to bootstrap blood when it's already been bootstrapped"))
  (add-hook 'blood-bootstrap-hook #'blood--bootstrap-env-h -90)
  (add-hook 'blood-bootstrap-hook #'blood--bootstrap-core-paths-h -89)
  (add-hook 'blood-bootstrap-hook #'blood--bootstrap-git-check-h  -88)
  (add-hook 'blood-bootstrap-hook #'blood--backend-activate-h -87)
  (add-hook 'blood-bootstrap-hook #'blood--bootstrap-keybindings-h)
  ;; TODO bootstrap a backend
  (run-hooks 'blood-bootstrap-hook)

  (if (not blood--bootstrap-queue)
      (ilog! "No Profiles Queued to bootstrap")
    (dolist (spec blood--bootstrap-queue)
      (ghlog! "Bootstrapping Profile: %s" (blood--profile-s-name spec))
      (dolist (fn (blood--profile-s-bootstrap spec))
        (ilog! "Calling: %s" fn)
        (funcall fn spec)
        )
      (glogx!)
      )
    )
  (glogxs!)
  (setq BLOOD-BOOTSTRAPPED t)
  )

(defun blood--bootstrap-git-check-h ()
  "bootstrap or complain about git"
  (ghlog! "Checking for Git")
  (if (executable-find "git")
      (ilog! "Found Git")
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
