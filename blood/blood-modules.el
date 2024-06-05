;;; blood-modules.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- header
;;
;; Copyright (C) 2023 John Grey
;;
;; Author: John Grey <https://github.com/jgrey4296>
;; Maintainer: John Grey <johngrey@Johns-Mac-mini.local>
;; Created: September 09, 2023
;; Modified: September 09, 2023
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
(llog! "Modules lib")
(require 'blood-sync)
(require 'blood-trace)
(require 'blood-dag)
(require 'blood-structs)

(defconst BLOOD-MODULE-FILE-PATTERN--FD ".*(m-.+|module|module-.+).el$"  "blood will search and load all files named this in module directories, to get package specs")

(defvar blood-modules--declared-components-ht (make-hash-table) "Maps group:module:package to package spec. sym -> list[component]. `use!` adds to this.")
(defvar blood-modules--package-component-map-ht (make-hash-table) "Maps packages to all modules they are part of")

(defun blood-modules--sym-from-parts (group mod &optional package)
  "Build a symbol from a group, a module name, and maybe a package to uniquely
identify a module component"
  (intern (if package
              (format "%s:%s:%s" group mod package)
            (format "%s:%s" group mod))))

(defun blood-modules--init-packages-h ()
  "Start the activeprofile's modules"
  (blood--expand-loadpath)
  (hlog! "Initalizing Packages")
  (let* ((profile (blood-profile-current))
         (mod-files (blood-sync--module-specs-of-profile profile)) ;; get active modules
         (components  (blood-sync--collect-module-component-specs mod-files 'allow))
         )
    (ilog! "Found %s components to initialize" (length components))
    (dolist (package-spec components)
      (let ((package-sym (blood--identifier-s-package (blood--package-s-id package-spec))))
        (ghlog! "Initialising: %s" package-sym)
        (funcall (blood--package-s-pre-load package-spec))
        ;; TODO straight--load-package-autoloads
        ;; TODO setup advice
        ;; TODO setup hooks
        (glogxs!)
        )
      )
    )
  )

(defun blood-modules--config-packages-h ()
  "Config the active profile's modules"
  (hlog! "Configuring and Loading Components")
  (blood-read-cache! :dag)
  (let* ((components (apply #'append (mapcar #'(lambda (x) (gethash x blood-modules--package-component-map-ht)) blood-dag--order)))
         (specs      (apply #'append (mapcar #'(lambda (y) (gethash y blood-modules--declared-components-ht)) components)))
        )
    (ghlog! "Components to load: %s" (length specs))
    ;; Load the dag calculated order
    (dolist (comp-spec specs)
      (let* ((package-sym (blood--identifier-s-package (blood--package-s-id comp-spec))))
        (ghlog! "Loading: %s" package-sym)
        (blood-trace--memory-pre package-sym)
        (require package-sym)
        (funcall (blood--package-s-post-load comp-spec))
        (blood-trace--memory-post package-sym)
        (glogxs!)
        )
      )
    )
  (glogxs!)
  )

(provide 'blood-modules)
;;; blood-modules.el ends here
