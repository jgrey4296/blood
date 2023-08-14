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

(local! "+spec-defs" "+defs" "+vars") ;; Local package files
(defer-load! "bindings" :after re-doom-bindings-init) ;; deferred local loading till feature

(use! python-mode
      :recipe      'melpa
      :after       'blah
      :defer       t
      :debug       '(:pre "preloading python"
                     :post 'break
                     :complete "python loading completed"
                     )
      :autoloads   '(python-mode)
      :compile     'byte
      :constraints '(:version 1.5 :profile basic :emacs (>= 28.2) :commit "264FBA")
      :pre-load    '(
                     (message "Config Prior to requiring the package")

                     )
      :post-load   '(
                     (message "Config requiring the package")
                     )
      )

(provide 'module-example)
;;; module-example.el ends here


(re-doom--module-from-path "/Volumes/documents/github/lisp/modules/basic/example/blah/module-example.el")
