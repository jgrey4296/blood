;;; blood--package-el.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;; blood functions for bootstrapping and using package.el
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header
(loaded? blood-structs)
;; see emacs-lisp/package.el.gz
(llog! "Blood Backend: package.el")

(defconst blood--bootstrap-package-el-backend-default (blood-build-backend 'package-el
                                                                           :requires 'package
                                                                           :bootstrap nil
                                                                           :activator nil
                                                                           :sync nil
                                                                           :clean nil
                                                                           )
  )

(defun blood--backend-set-package-el ()
  (setq blood--backend-active blood--bootstrap-package-el-backend-default)
  )


(warn "TODO: blood--package-el")
(provide 'blood--backend-package-el)

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
;;; blood--package-el.el ends here
