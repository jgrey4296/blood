;;; bootstrap.el -*- lexical-binding: t; -*-
(defvar blood--bootstrap-queue nil)
(defvar blood--bootstrap-call-noop nil)

(defun blood--bootstrap ()
  "For each declared profile in the quue, bootstrap it, using blood--bootstrap-defaults
if the profile doesn't provide its own bootstrappers.
will always run blood--bootstrap-git-check and blood--bootstrap-core-paths "
  (princ "-------------------- Bootstrapping\n")
  (blood--bootstrap-git-check)
  (blood--bootstrap-core-paths)
  (unless blood--bootstrap-queue
    (princ "- Nothing More to bootstrap"))
  (dolist (spec blood--bootstrap-queue)
    (princ (format "-- Bootstrapping Profile: %s\n" (plist-get spec :name)))
    (unless (plist-get spec :bootstrap)
      (require 'blood--straight))
    (dolist (fn (or (plist-get spec :bootstrap) blood--bootstrap-defaults))
      (princ (format "- Calling: %s : %s\n" fn spec))
      (funcall fn spec)
      )
    )
  (setq BLOOD-BOOTSTRAPPED t)
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
  (let* ((version (cdr (blood--bootstrap-call "git" "version")))
         (version
          (and (string-match "\\_<[0-9]+\\.[0-9]+\\(\\.[0-9]+\\)\\_>" version)
               (match-string 0 version))))
    (if version
        (when (version< version "2.23")
          (user-error "Git %s detected! blood requires git 2.23 or newer!"
                      version))))
  )

(defun blood--bootstrap-call (prog &rest args)
  "Call a program with arguments, returns (retcode . msg)"
  (if blood--bootstrap-call-noop
      (cons 0 (princ (format "No-op call: %s : %s\n" prog args)))
    (with-temp-buffer
      (cons (or (format "%d" (apply #'call-process prog nil t nil args))
                -1)
            (string-trim (buffer-string)))
      )
    )
  )

(defun blood--bootstrap-env ()
  ;; TODO
  )

(provide 'blood-bootstrap)
