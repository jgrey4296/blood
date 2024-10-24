;;; blood-hooks.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- header
;;
;; Copyright (C) 2023 John Grey
;;
;; Author: John Grey <https://github.com/jgrey4296>
;; Maintainer: John Grey <johngrey@Johns-Mac-mini.local>
;; Created: September 07, 2023
;; Modified: September 07, 2023
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 28.2))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;  General hooks for blood, intended for 'after-init-hook.
;;  Other modules, like blood-profile, declare their own hooks.
;;
;;; Code:
;;-- end header
(llog! "Hooks")
;;;; Installed on after-init-hook:

(defun blood--setup-default-h ()
  "Set Core Settings for blood"
  (hlog! "Setting up core defaults")
  (set-language-environment "UTF-8")
  ;; General Startup Settings
  (setq default-input-method nil
        inhibit-startup-screen                       t ;; Startup Control
        inhibit-startup-echo-area-message            "Blood for the Blood God"
        inhibit-message nil

        jka-compr-verbose                            init-file-debug
        ;; Garbage collection
        gc-cons-threshold                            (* 16 1024 1024)
        gcmh-idle-delay                              'auto  ; default is 15s
        gcmh-auto-idle-delay-factor                  10
        gcmh-high-cons-threshold                     (* 16 1024 1024)  ; 16mb
        inhibit-compacting-font-caches               t

        ;; Bidirectional text
        bidi-display-reordering                      'left-to-right
        bidi-paragraph-direction                     'left-to-right

        ;; Misc UI
        cursor-in-non-selected-windows               nil
        highlight-nonselected-windows                nil
        fast-but-imprecise-scrolling                 t
        idle-update-delay                            1.0
        redisplay-skip-fontification-on-input        t
        menu-bar-mode                  nil
        tool-bar-mode                  nil
        scroll-bar-mode                nil

        ffap-machine-p-known                         'reject
        read-process-output-max                      (* 64 1024)  ; 64kb
        )

  (push '(menu-bar-lines . 0)   default-frame-alist)
  (push '(tool-bar-lines . 0)   default-frame-alist)
  (push '(vertical-scroll-bars) default-frame-alist)
  (when (eq system-type 'darwin)
    (add-hook 'window-setup-hook          #'doom-restore-menu-bar-in-gui-frames-h)
    (add-hook 'after-make-frame-functions #'doom-restore-menu-bar-in-gui-frames-h)
    )
  (ilog! "TODO - setup load timing")
  (ilog! "TODO - set auth-sources , encrypted")
  (ilog! "TODO - handle customized variables")
  ;; todo - on mac regrab focus: (when (display-graphic-p (selected-frame)) (set-frame-parameter frame 'menu-bar-lines 1))
  (glogx!)
  )

(defun blood-menu-bar-fix-h (&optional frame)
  "adapted from doom-restore-menu-bar-in-gui-frames-h"
  (ilog! "Handling menu bar")
  (when-let (frame (or frame (selected-frame)))
    (when (display-graphic-p frame)
      (set-frame-parameter frame 'menu-bar-lines 1)))
  )

(defun blood--report-after-init-h ()
  "A hook fn to report key values and registered after-init-hook fn's that are going to be run"
  (hlog! "Init Loaded, Processing...")
  (ilog! "Active Profile: %s" blood-profile--default)
  (ghlog! "Current Variable Assignments:")
  (dolist (loc '(native-comp-eln-load-path data-directory doc-directory exec-directory
	         installation-directory invocation-directory invocation-name source-directory
                 shared-game-score-directory noninteractive blood--cmd
                 blood-profile--build-dir blood-profile--installation-dir

                 ))
    (ilog! "%-30s : %s" (symbol-name loc) (symbol-value loc))
    )
  (glogx!)
  (ghlog! "Hooks to Run: ")
  (dolist (hook after-init-hook)
    (when (symbolp hook) (ilog! "-- : %s" (symbol-name hook))))
  (glogx!)
  )

(defun blood--final-report-h ()
  "Hook fn to provide some final information at the end of running after-init-hook,
and switch to the *Messages* buffer
"
  (hlog! "BLOOD")
  (ilog! "Profile: %s" blood-profile--default)
  (ilog! "Command: %s" blood--cmd)
  (ilog! "Remaining CLI Args:  %s"  command-line-args)
  (ilog! "Initial Buffer Choice: %s" initial-buffer-choice)
  (hlog! "BLOOD")
  (switch-to-buffer "*Messages*")
  )

(defun blood--write-messages-to-log-file-h ()
  "Runs at the end of post-init-hook to write startup messages to blood-cache-dir"
  (ilog! "Writing messages to blood log")
  (with-temp-buffer
    (insert-buffer "*Messages*")
    (write-file (file-name-concat blood-cache-dir "startup.log"))
    )
  )

(provide 'blood-hooks)
;;; blood-hooks.el ends here
