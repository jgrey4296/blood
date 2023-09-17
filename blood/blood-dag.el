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

(defun blood-dag ()
  "Build a DAG of all installed package dependencies
For activation of packages in set order.

Order of activation only matters for module components,
so using straight / cask etc determine dependencies of each installed package,
insert into a graph,
topo sort the graph.

"

  )


(provide 'blood-dag)
;;; blood-dag.el ends here
