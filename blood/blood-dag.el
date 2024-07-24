;;; blood-dag.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- header
;;
;; Copyright (C) 2023 John Grey
;;
;; Author: John Grey <https://github.com/jgrey4296>
;; Maintainer: John Grey <johngrey@Johns-Mac-mini.local>
;; Created: September 08, 2023
;; Modified: September 08, 2023
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
(cl-assert (featurep 'blood-defs))
(cl-assert (featurep 'blood-log))
(cl-assert (featurep 'blood-utils))
(cl-assert (featurep 'blood-modules))
(llog! "Dag")

(defconst blood-dag--root '__blood_root)

(defvar blood-dag--graph (let ((table (make-hash-table :test 'equal)))
                           (puthash blood-dag--root nil table)
                           table
                           )
  "A DAG of Packages. __blood_root is the root which everything gets connected to")

(defvar blood-dag--order nil)

(blood-register-cache! :dag
                       #'(lambda (data) (string-join (mapcar #'symbol-name data) "\n"))
                       :read-lambda #'(lambda (text) (setq blood-dag--order (mapcar #'intern (split-string text "\n" t " +"))))
                       :header "The DFS'd, linear order of dependencies and :after annotations of module components, from [no-deps] to [most-deps]"
                       )

(defun blood-dag-h ()
  "Build a DAG of all installed package dependencies
For activation of packages in set order.

Order of activation only matters for module components,
so using straight / cask etc determine dependencies of each installed package,
insert into a graph,
topo sort the graph.

"
  (ghlog! "Building Dag")
  (unless (and (boundp'blood--straight-initialised)
               blood--straight-initialised)
    (error "Can't build Packages DAG without straight"))
  (straight--load-build-cache) ;; -- turn this into a hook?
  (blood-dag--build)
  (ghlog! "Package Dependencies Mapping: ")
  (dolist (package (sort (hash-table-keys blood-dag--graph) #'string-lessp))
    (ilog! "%-20s -> %s" package (gethash package blood-dag--graph))
    )
  (glogxs!)
  ;; calculate the ideal load order
  (blood-dag--dfs)
  (glog! "Package Order:")
  (ilog! "[START] %s [END]" (string-join (mapcar #'symbol-name blood-dag--order) " -> "))
  (glogxs!)
  ;; write the order
  (blood-cache! :dag (append blood-dag--order))
  (glogxs!)
  )

(defun blood-dag--build ()
  "build the dag by iterating over straight's metadata info"
  (let ((repos (append straight-recipe-repositories))
        )
    (dolist (package (hash-table-keys straight--build-cache)) ;; for each package in straight's cache
      (let* ((package-sym (intern package))
             (deps (mapcar #'intern (nth 1 (gethash package straight--build-cache))))

             (components (apply #'append (mapcar #'(lambda (y) (gethash y blood-modules--declared-components-ht))
                                                 (gethash package-sym blood-modules--package-component-map-ht))))
             (afters (ensure-list (apply #'append (mapcar #'(lambda (x) (blood--packages-s-after x)) components))))
             (full-deps (append deps afters))
            )
        (cond ((seq-contains-p repos package-sym) nil) ;; skip unknowns
              ((and (eq 1 (length full-deps)) (eq (car full-deps) 'emacs)) ;; root packages
               (puthash package-sym blood-dag--root blood-dag--graph)
               )
              (t
               (dolist (dep full-deps)
                 (unless (or (eq dep 'emacs) (seq-contains-p (gethash package-sym blood-dag--graph) dep))
                   (push dep (gethash package-sym blood-dag--graph))
                   )
                 )
               )
              )
        )
      )
    )
  )

(defun blood-dag--dfs ()
  " DFS over the dag to get a linear order of package loading
    returns a flag list
"
  (ilog! "DFSing the Dag into a linear order")
  (let ((remaining (append (hash-table-keys blood-modules--package-component-map-ht)))
        ordered
        )
    (while remaining
      (cond ((seq-contains-p ordered (car remaining))
             (pop remaining))
            ((null (gethash (car remaining) blood-dag--graph))
             (push (pop remaining) ordered))
            ((not (seq-contains-p ordered (car (gethash (car remaining) blood-dag--graph))))
             (push (pop (gethash (car remaining) blood-dag--graph)) ordered))
            ((gethash (car remaining) blood-dag--graph)
             (pop (gethash (car remaining) blood-dag--graph)))
            )
      )
    (setq blood-dag--order (reverse ordered))
    blood-dag--order
    )
  )

(provide 'blood-dag)
;;; blood-dag.el ends here
