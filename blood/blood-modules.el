;;; blood-modules.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- header
;;
;; Copyright (C) 2023 John Grey
;;
;; Author: John Grey <https://github.com/jgrey4296>
;; Maintainer: John Grey <johngrey@Johns-Mac-mini.local>
;; Created: September 09, 2023
;; Modified: September 09, 2023
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
(llog! "Modules lib")
(require 'blood-sync)
(require 'blood-trace)
(require 'blood-dag)

(defconst BLOOD-MODULE-FILE-PATTERN--FD ".*(m-.+|module|module-.+).el$"  "blood will search and load all files named this in module directories, to get package specs")

(defvar blood-modules--declared-components-ht (make-hash-table) "Maps modules to components. fullsym -> list[component]. `use!` adds to this.")
(defvar blood-modules--package-component-map-ht (make-hash-table) "Maps packages to all components they are part of")

(defmacro blood-modules--build-spec (name-spec pdata)
  "Make a module component specification. "
  (let ((compile-val (pcase (plist-get  pdata :compile)
               ('nil     nil)
                       (''byte   'byte)
                       (''native 'native)
                       (x        'bad-value)))
        (debug-val (plist-get pdata :debug))
        (constraints-val (plist-get pdata :constraints))
        (pre-load (blood--lambda! (plist-get pdata :pre-load)))
        (post-load (blood--lambda! (plist-get pdata :post-load)))

        )
    `(list
      :id           (quote ,name-spec)
      :disabled     ,(plist-get pdata :disabled)
      :recipe       (or     (mquote! ,(plist-get pdata :recipe)) 'default)
      :after        (mquote! ,(plist-get pdata :after)) ;; unnecessary when dependency tree activation is implemented
      :autoloads    (mquote! ,(plist-get pdata :autoloads))
      :compile      (mquote! ,compile-val)
      :constraints  (list :profile nil
                          :package-version nil
                          :emacs-version nil
                          :module-version nil
                          :commit nil
                          )
      :debug        ,(plist-get pdata :debug)
      :pre-load ,pre-load
      :post-load ,post-load
      :advice nil
      :hooks  nil
      )
    )
  )

(defun blood-modules--sym-from-parts (group mod &optional package)
  "Build a symbol from a group, a module name, and maybe a package to uniquely identify a module component"
  (intern (if package
              (format "%s:%s:%s" group mod package)
            (format "%s:%s" group mod))))

(defun blood-modules--id-from-path (source &optional package-name)
  "create a (:group %s :module %s :source %s :sym %s :fullsym) declaration from a path string
:sym is group:module
:fullsym is group:module:package
"
  (let* ((parts (split-string (expand-file-name source) "/" t))
         (relevant (last parts 4))
         )
    (if relevant
        `(:group ,(intern (cadr relevant))
          :module ,(intern (caddr relevant))
          :source ,source
          :package ,package-name
          :sym ,(blood-modules--sym-from-parts (cadr relevant) (caddr relevant))
          :fullsym ,(blood-modules--sym-from-parts (cadr relevant) (caddr relevant) package-name)
          )
      `(:bad-source ,source))
    )
  )

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
      (let ((package-sym (plist-get (plist-get package-spec :id) :package)))
        (ghlog! "Initialising: %s" package-sym)
        (funcall (plist-get package-spec :pre-load))
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
      (let* ((package-sym (plist-get (plist-get comp-spec :id) :package)))
        (ghlog! "Loading: %s" package-sym)
        (blood-trace--memory-pre package-sym)
        (require package-sym)
        (funcall (plist-get comp-spec :post-load))
        (blood-trace--memory-post package-sym)
        (glogxs!)
        )
      )
    )
  (glogxs!)
  )

(provide 'blood-modules)
;;; blood-modules.el ends here
