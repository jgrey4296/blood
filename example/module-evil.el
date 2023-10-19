;;; module-example.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- header
;;
;; Copyright (C) 2023 John Grey
;;
;; Author: John Grey <https://github.com/jgrey4296>
;; Maintainer: John Grey <johngrey@Johns-Mac-mini.local>
;; Created: August 01, 2023
;; Modified: August 01, 2023
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
;;  Would be named 'config.el'.
;;  Everything should be declarative so re-doom and load it safely during sync
;;
;;; Code:
;;-- end header

;; (local-load!! "+spec-defs" "+defs" "+vars") ;; Local package files
;; (defer-load! "bindings" :after re-doom-bindings-init) ;; deferred local loading till feature

(local-load! "+test1")
(defer-load! "+test2")

(use! evil
      :recipe      'melpa
      :autoloads   '(evil-mode)
      :pre-load (message "Prior to loading evil")
      :advice ()
      :hooks ()
      :post-load ((message "evil has loaded from: %s" (dir!))
                  (evil-mode 1)
                  (evil-ex-define-cmd "q[uit]" #'evil-quit-all)
                  )
      )

(use! evil-escape
      ;; :disabled t
      :recipe (:host github :repo "jgrey4296/evil-escape-hook")
      ;; :after (blah)
      :pre-load (
                 (setq evil-escape-inhibit-functions nil ;; '(evil-ex-p)
                       evil-escape-excluded-states '(normal multiedit emacs motion)
                       evil-escape-excluded-major-modes '(neotree-mode treemacs-mode vterm-mode)
                       evil-escape-key-sequence "jk"
                       evil-escape-delay 0.15
                       )
                 )
      :post-load (
                  (evil-escape-mode)
                  )

      )
