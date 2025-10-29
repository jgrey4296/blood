;;; blood--package-el.el -*- lexical-binding: t; no-byte-compile: t; -*-
;; blood functions for bootstrapping and using package.el
;;

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
;;; blood--package-el.el ends here
