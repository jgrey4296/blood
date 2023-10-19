;;; blood--native.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- header
;;
;; Copyright (C) 2023 John Grey
;;
;; Author: John Grey <https://github.com/jgrey4296>
;; Maintainer: John Grey <johngrey@Johns-Mac-mini.local>
;; Created: September 08, 2023
;; Modified: September 08, 2023
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 28.2))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:
;;-- end header
(llog! "Native")

(defun blood-native--setup-h ()
  (ghlog! "Setting up Native Compilation")
  (setq native-compile-target-directory (file-name-directory (expand-file-name
                                                              (file-name-concat blood--eln-cache-name
                                                                                (plist-get (blood-profile-current) :name)
                                                                                comp-native-version-dir)
                                                              blood-cache-dir))
        native-comp-eln-load-path (append (list native-compile-target-directory) native-comp-eln-load-path)

        native-comp-jit-compilation                         t
        native-comp-enable-subr-trampolines                 t
        native-comp-async-query-on-exit                     t

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
  ;; (add-hook 'kill-emacs-query-functions #'blood-native--clear-compilation-queue-on-exit)
  ;; (add-hook 'kill-emacs-hook #'blood-native--noninteractive-clear-compilation-queue)

  (ilog! "Native Compilation Activated")
  (ilog! "Deny Lists have been cleared")
  (ilog! "native-comp-eln-load-path: %s" native-comp-eln-load-path)
  (ilog! "native-comp-async-jobs-number: %s" native-comp-async-jobs-number)
  (glogx!)
  )

(defun blood-native--update-eln-cache-h ()
  "For adding to blood-profile--post-activate-hook"
  (setq native-compile-target-directory (file-name-directory (expand-file-name
                                                              (file-name-concat blood--eln-cache-name
                                                                                (plist-get (blood-profile-current) :name)
                                                                                comp-native-version-dir)
                                                              blood-cache-dir))
        )
  )

(defun blood-native--clear-compilation-queue-on-exit ()
  " "
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

(defun blood-native--noninteractive-clear-compilation-queue ()
  (if noninteractive
      (setq comp-files-queue nil)
      )
  )

(provide 'blood--native)
;;; blood--native.el ends here
