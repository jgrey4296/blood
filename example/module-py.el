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

(use! python
      :recipe      'python-mode
      :autoloads   '(python-mode)
      :pre-load (message "pre-python")
      :advice ()
      :hooks ()
      :post-load    (message "post-python")
      )
