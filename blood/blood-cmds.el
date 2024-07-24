;;; blood-cmds.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header
(cl-assert (featurep 'blood-defs))
(cl-assert (featurep 'blood-log))
(cl-assert (featurep 'blood-utils))

(defun blood--cmd-sync ()
  " Cmd for triggering a sync of packages, using the chosen backend "
  (require 'blood-sync)
  (require 'blood-dag)
  (require 'blood-modules)
  (add-hook 'after-init-hook #'blood-bootstrap-h (bloody-lazy! :bootstrap))
  (add-hook 'after-init-hook #'blood-sync-h               (bloody-lazy! :sync))
  (add-hook 'after-init-hook #'blood--setup-default-h     (bloody-lazy! :run))
  (add-hook 'after-init-hook #'blood-sync--complete-h     (bloody-lazy! :finalize))
  ;;(when ('build in cli-args) (add-hook 'after-init-hook #'blood--build-packages (plist-get blood-hook-priorities :build)))
  (add-hook 'after-init-hook #'(lambda () (ilog! "TODO: build version report")) (bloody-lazy! :build))
  (when ,is-interactive (add-hook 'after-init-hook #'(lambda () (require 'evil) (evil-mode)) (bloody-lazy! :finalize 1)))

  )

(defun blood--cmd-clean ()
  " cmd to clean bloods cache and installation "
  (require 'blood-clean)
  ;; load pincushion
  ;; Queue the packages for cleaning
  (add-hook 'after-init-hook #'blood--clean-h (bloody-lazy! :clean))
  )

(defun blood--cmd-report ()
  " blood to trigger a report on installed packages "
  (require 'blood-report)
  (add-hook 'after-init-hook #'blood--report-h (bloody-lazy! :report))
  )

(defun blood--cmd-stub ()
  " cmd to generate a profile/module stub "
  (require 'blood-stub)
  (add-hook 'after-init-hook #'blood-stub-h (bloody-lazy! :run))
  )

(defun blood--cmd-run ()
  " blood's main cmd: trigger loading a profile and its modules/packages "
  ;; (ilog! "Setting up activation hooks: %s" ,profile-name)
  (require 'blood--native)
  (require 'blood-modules)
  (add-hook 'after-init-hook #'blood-bootstrap-h                     (bloody-lazy! :bootstrap))
  (add-hook 'after-init-hook #'blood--setup-default-h                (bloody-lazy! :run))
  (add-hook 'after-init-hook #'blood-profile-start-h                 (bloody-lazy! :profile-init))
  (add-hook 'after-init-hook #'blood-native--setup-h                 (bloody-lazy! :profile-init 5))
  (add-hook 'after-init-hook #'blood-modules--init-packages-h        (bloody-lazy! :module-init))
  (add-hook 'after-init-hook #'blood-modules--config-packages-h      (bloody-lazy! :module-config))
  (when blood--trace-memory
    (add-hook 'after-init-hook #'blood-trace--memory-report-h         (bloody-lazy! :module-config 2))
    )
  (add-hook 'after-init-hook #'blood-defer--start-h                  (bloody-lazy! :finalize))
  )

(provide 'blood-cmds)

;;-- Footer
;; Copyright (C) 2024 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    July 24, 2024
;; Modified:   July 24, 2024
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 29.1))
;;
;; This file is not part of GNU Emacs.
;;
;;-- end Footer
;;; blood-cmds.el ends here
