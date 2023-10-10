;;; blood-dag.el -*- lexical-binding: t; no-byte-compile: t; -*-
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
(llog! "Dag")

(defconst blood-dag--root "__blood_root")
(defvar blood-dag--graph (let ((table (make-hash-table :test 'equal)))
                   (puthash blood-dag--root nil table)
                          table
                          )
  "A DAG of Packages. __blood_root is the root which everything gets connected to")

(defun blood-dag-h ()
  "Build a DAG of all installed package dependencies
For activation of packages in set order.

Order of activation only matters for module components,
so using straight / cask etc determine dependencies of each installed package,
insert into a graph,
topo sort the graph.

"
  (hlog! "Building Dag")
  (unless blood--straight-initialised (error "Can't build Packages DAG without straight"))
  (straight--load-build-cache)
  (let ((repos (mapcar #'(lambda (x) (symbol-name x))  straight-recipe-repositories)))
    (dolist (package (hash-table-keys straight--build-cache))
      (let ((deps (nth 1 (gethash package straight--build-cache))))
        (cond ((seq-contains-p repos package) nil)
              ((and (eq 1 (length deps)) (string-equal (car deps) "emacs"))
               (puthash package blood-dag--root blood-dag--graph)
               )
              (t
               (dolist (dep deps)
                 (unless (string-equal dep "emacs")
                   (push dep (gethash package blood-dag--graph))
                   )
                 )
               )
              )
        )
      )
    )
  (dolist (package (hash-table-keys blood-dag--graph))
    (ilog! "%s -> %s" package (gethash package blood-dag--graph))
    )
  ;; calculate the ideal load order
  ;; write the order
  )

(provide 'blood-dag)
;;; blood-dag.el ends here
