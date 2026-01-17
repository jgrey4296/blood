;;; blood--native.el -*- lexical-binding: t; no-byte-compile: t; -*-
(loaded? blood-defs blood-log blood-utils blood-structs)
(llog! "Native")

;; TODO: maybe cache some of this?

(defun blood-native--setup-h ()
  "blood hook to setup native comp"
  (hlog! "Setting up Native Compilation")
  (let* ((profile (blood-profile-current))
         (profile-id (blood-uniq-id profile))
         (target-dir (expand-file-name (file-name-concat blood--eln-cache-name
                                                         (symbol-name profile-id)
                                                         comp-native-version-dir)
                                       blood-cache-dir))
         )
    (log! "Native Target Dir: %s" target-det)
    (setq native-compile-target-directory (file-name-directory target-dir)
          native-comp-eln-load-path (append (list native-compile-target-directory) native-comp-eln-load-path)

          native-comp-jit-compilation                t
          native-comp-enable-subr-trampolines        t
          native-comp-async-query-on-exit            t

          no-native-compile                          nil
          no-byte-compile                            nil
          comp-no-spawn                              nil
          async-bytecomp-allowed-packages            nil

          native-comp-always-compile                 nil
          native-comp-bootstrap-deny-list            nil
          native-comp-compiler-options               nil
          native-comp-jit-compilation-deny-list      (list "/with-editor\\.el\\'" "/vterm\\.el\\'" "/evil-collection-vterm\\.el\\'" "/emacs-jupyter.*\\.el\\'")
          native-comp-async-jobs-number   1
          native-comp-verbose             0
          native-comp-debug               0
          native-comp-speed               2
          native-comp-async-report-warnings-errors init-file-debug
          native-comp-warning-on-missing-source    init-file-debug
          )
    (add-hook 'blood-profile--post-activate-hook #'blood-native--update-eln-cache-h)

    (when init-file-debug
      (add-hook 'native-comp-async-cu-done-functions #'(lambda (file) (log! :debug "Native Comp Success: %s -> %s" file (comp-el-to-eln-filename file))))
      (add-hook 'native-comp-async-all-done-hook     #'(lambda () (log! :debug "All Native Compilations Complete")))
      (advice-add 'native-compile         :before    #'(lambda (fn &optional out) (log! :debug "Native Compile: %s : %s" fn out)))
      (advice-add 'native-compile-async   :before    #'(lambda (fls &rest args) (log! :debug "Async Native Compile: %s" fls)))
      (advice-add 'comp-run-async-workers :before    #'(lambda () (log! :debug "Starting async compilation: %s : %s : %s" comp-files-queue comp-native-version-dir native-compile-target-directory)))
      )
    ;; (add-hook 'kill-emacs-query-functions #'blood-native--clear-compilation-queue-on-exit-h)
    ;; (add-hook 'kill-emacs-hook #'blood-native--noninteractive-clear-compilation-queue-h)

    (ilog! "Native Compilation Activated")
    (ilog! "Deny Lists have been cleared")
    (ilog! "native-comp-eln-load-path: %s" native-comp-eln-load-path)
    (ilog! "native-comp-async-jobs-number: %s" native-comp-async-jobs-number)
    (glogx!)
    )
  )

(defun blood-native--update-eln-cache-h ()
  "For adding to blood-profile--post-activate-hook"
  (setq native-compile-target-directory (file-name-directory (expand-file-name
                                                              (file-name-concat blood--eln-cache-name
                                                                                (blood-uniq-id (blood-profile-current))
                                                                                comp-native-version-dir)
                                                              blood-cache-dir))
        )
  )

(defun blood-native--clear-compilation-queue-on-exit-h ()
  "An exit hook for catching unfinished compilation "
  (let ((read-answer-short 'auto))
    (pcase (read-answer (format "There are %s entries remaining in the comp-files-queue, what do you want to do?" (length comp-files-queue))
                        '(("clear" ?x "Clear the Queue")
                          ("cancel" ?c "Cancel the quit command")
                          ("ignore" ?i "Ignore and quit anyway")
                          )
                        )
      ("clear" (setq comp-files-queue nil) t)
      ("cancel" nil)
      ("ignore" t)
      )
    )
  )

(defun blood-native--noninteractive-clear-compilation-queue-h ()
  "hook to clear compilation queue on non-interactive exit"
  (if noninteractive
      (setq comp-files-queue nil)
      )
  )

(provide 'blood--native)
;;; blood--native.el ends here
