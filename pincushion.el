;;; pin-cushion.el -*- lexical-binding: t; -*-

;; Pincushion, a nice fileformat to set what version of a package to use
;; Format: BOL {package} [:version_num {} | :commit {} ] [:repo {}]? EOL
;; aligned on :version_num, :commit, and :repo


(defun generate-pincushion ()
  ;; For generating a listing of all installed packages, and their version / commits
  ;; which informs straight,
  ;; and can be diff'd with an updated pin-cushion
  (let ((pin-alist '()))
    ;; write to file
    )
  )

(defun apply-pincushion ( file )
  ;; apply a pincushion

  )

(defun pincushion-graph ( file )
;; also produce a dot graph of user-mentioned packages -> dependencies

  )

(setq jg-test (straight--lockfile-read-all))

(provide 'pincushion)
