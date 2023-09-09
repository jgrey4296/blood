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

(defun blood-native--setup ()
  (setq native-compile-target-directory (file-name-directory (expand-file-name
                                                              (file-name-concat "eln-cache"
                                                                                (plist-get (blood-current-profile) :name)
                                                                                comp-native-version-dir)
                                                              blood-cache-dir))
        native-comp-eln-load-path (append (list native-compile-target-directory) native-comp-eln-load-path)

        no-native-compile nil
        native-comp-deferred-compilation             t
        comp-enable-subr-trampolines                 t
        package-native-compile t
        native-comp-async-jobs-number 1
        native-comp-async-report-warnings-errors init-file-debug
        native-comp-warning-on-missing-source    init-file-debug

        native-comp-deferred-compilation-deny-list nil
        native-comp-bootstrap-deny-list            nil
        native-comp-verbose 0
        )

  (when init-file-debug
    (add-hook 'native-comp-async-cu-done-functions #'(lambda (file) (message "Native Comp Success: %s -> %s" file (comp-el-to-eln-filename file))))
    (add-hook 'native-comp-async-all-done-hook     #'(lambda () (message "All Native Compilations Complete")))
    (advice-add 'native-compile         :before    #'(lambda (fn &optional out) (message "Native Compile: %s : %s" fn out)))
    (advice-add 'native-compile-async   :before    #'(lambda (fls &rest args) (message "Async Native Compile: %s" fls)))
    (advice-add 'comp-run-async-workers :before    #'(lambda () (message "Starting async compilation: %s : %s : %s" comp-files-queue comp-native-version-dir native-compile-target-directory)))
    )
  (message "Native Compilation Activated")
)


(provide 'blood--native)
;;; blood--native.el ends here
