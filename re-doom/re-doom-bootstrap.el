;;; bootstrap.el -*- lexical-binding: t; -*-
(defvar re-doom--bootstrap-queue nil)
(defvar re-doom--bootstrap-call-noop nil)

(defun re-doom--bootstrap ()
  "For each declared profile in the quue, bootstrap it, using re-doom--bootstrap-defaults
if the profile doesn't provide its own bootstrappers.
will always run re-doom--bootstrap-git-check and re-doom--bootstrap-core-paths "
  (princ "-------------------- Bootstrapping\n")
  (re-doom--bootstrap-git-check)
  (re-doom--bootstrap-core-paths)
  (dolist (spec re-doom--bootstrap-queue)
    (princ (format "*Bootstrapping Profile: %s*\n" (plist-get spec :name)))
    (dolist (fn (or (plist-get spec :bootstrap) re-doom--bootstrap-defaults))
      (princ (format "- %s\n" (funcall fn spec)))
      )
    )
  (setq RE-DOOM-BOOTSTRAPPED t)
  )

(defun re-doom--bootstrap-core-paths ()
  " build  "
  (princ "-- Bootstrapping Paths\n")
  (let ((cache-dir (expand-file-name  re-doom-cache-dir)))
    (unless (file-exists-p cache-dir)
      (make-directory cache-dir 'recursive))
    )
  )

(defun re-doom--bootstrap-git-check ()
  "bootstrap or complain about git"
  (princ "-- Checking for Git\n")
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
  "Call a program with arguments"
  (if re-doom--bootstrap-call-noop
      (princ (format "No-op call: %s : %s\n" prog args))
    (with-temp-buffer
      (cons (or (format "%d" (apply #'call-process prog nil t nil args))
                -1)
            (string-trim (buffer-string)))
      )
    )
  )

(defun re-doom--bootstrap-env ()
  ;; TODO
  )

(provide 're-doom-bootstrap)
