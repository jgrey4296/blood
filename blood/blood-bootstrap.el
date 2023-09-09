;;; bootstrap.el -*- lexical-binding: t; -*-
(message "----- Loading bootstrap")

(defvar BLOOD-BOOTSTRAPPED nil)

(defvar blood--bootstrap-queue nil)

(defcustom blood-additional-bootstrappers nil "functions to run as part of the bootstrap process" :type 'list)

(defcustom blood--backend-default nil
  "The plist describing the default backend to use. comprised of :name, :require, :bootstrap and :actviator ")

(defun blood--bootstrap ()
  "For each declared profile in the quue, bootstrap it, using blood--bootstrap-defaults
if the profile doesn't provide its own bootstrappers.
will always run blood--bootstrap-git-check and blood--bootstrap-core-paths "
  (princ "-------------------- Bootstrapping\n")
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
      (princ "- Nothing More to bootstrap")
    (dolist (spec blood--bootstrap-queue)
      (princ (format "-- Bootstrapping Profile: %s\n" (plist-get spec :name)))
      (dolist (fn (or (plist-get spec :bootstrap) (plist-get blood--backend-default :bootstrap)))
        (princ (format "- Calling: %s : %s\n" fn spec))
        (funcall fn spec)
        )
      )
    (setq BLOOD-BOOTSTRAPPED t)
    )
  )

(defun blood--bootstrap-core-paths ()
  " build  "
  (princ "-- Bootstrapping Paths\n")
  (let ((cache-dir (expand-file-name  blood-cache-dir)))
    (unless (file-exists-p cache-dir)
      (princ "- Making cache Directory")
      (make-directory cache-dir 'recursive))
    )
  )

(defun blood--bootstrap-git-check ()
  "bootstrap or complain about git"
  (princ "-- Checking for Git\n")
  (unless (executable-find "git")
    (user-error "Git isn't present on your system. Cannot proceed."))
  (let* ((version (cdr (blood--call "git" "version")))
         (version
          (and (string-match "\\_<[0-9]+\\.[0-9]+\\(\\.[0-9]+\\)\\_>" version)
               (match-string 0 version))))
    (if version
        (when (version< version "2.23")
          (user-error "Git %s detected! blood requires git 2.23 or newer!"
                      version))))
  )

(defun blood--bootstrap-env ()
  ;; TODO
  )

(provide 'blood-bootstrap)
