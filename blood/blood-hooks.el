;;; blood-hooks.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- header
;;
;; Copyright (C) 2023 John Grey
;;
;; Author: John Grey <https://github.com/jgrey4296>
;; Maintainer: John Grey <johngrey@Johns-Mac-mini.local>
;; Created: September 07, 2023
;; Modified: September 07, 2023
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
(llog! "Hooks")
;;;; Installed on after-init-hook:

(defun blood--setup-default-h ()
  "set core settings"
  (ghlog! "Setting up core Blood customisations")
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
  ;; todo - setup load timing
  ;; todo - set auth-sources , encrypted
  ;; todo - handle customized variables
  ;; todo - on mac regrab focus: (when (display-graphic-p (selected-frame)) (set-frame-parameter frame 'menu-bar-lines 1))
  (glogx!)
  )

(defun doom-restore-menu-bar-in-gui-frames-h (&optional frame)
  "from doom for handling GUI's on mac with no menu bar"
  (when-let (frame (or frame (selected-frame)))
    (when (display-graphic-p frame)
      (set-frame-parameter frame 'menu-bar-lines 1)))
  )

(provide 'blood-hooks)
;;; blood-hooks.el ends here
