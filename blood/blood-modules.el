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
(ilog! "Loading Modules lib")
(require 'blood-sync)

(defconst BLOOD-MODULE-FILE-PATTERN ".*\(m-.+\|module\|module-.+\).el"  "blood will search and load all files named this in module directories, to get package specs")

(defvar blood-modules--declared-packages-ht (make-hash-table) "Registered module packages. modstr -> list")

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
              (format "%s-%s-%s" group mod package)
            (format "%s-%s" group mod))))

(defun blood-modules--id-from-path (source &optional package-name)
  "create a (:group %s :module %s :source %s :sym %s) declaration from a path string"
  (let* ((parts (split-string (expand-file-name source) "/" t))
         (relevant (last parts 4))
         )
    (if relevant
        `(:group ,(intern (cadr relevant))
          :module ,(intern (caddr relevant))
          :source ,source
          :package ,package-name
          :sym ,(blood-modules--sym-from-parts(cadr relevant) (caddr relevant))
          :fullsym ,(blood-modules--sym-from-parts (cadr relevant) (caddr relevant) package-name)
          )
      `(:bad-source ,source))
    )
  )

(defun blood-modules--init ()
  "Start the acvtive profile's modules"
  (glog! "TODO: init modules")
  (blood--update-loadpath)
  ;; TODO Ensure load path is setup
  (let* ((profile (blood-profile-current))
        (mod-files (blood-sync--module-specs profile)) ;; get active modules
        (packages  (blood-sync--collect-package-specs mod-files 'allow))
        )
    (dolist (package-spec packages)
      (ilog! "Initialising: %s" (plist-get (plist-get package-spec :id) :package))
      (funcall (plist-get package-spec :pre-load))
      ;; TODO straight--load-package-autoloads
      ;; TODO setup advice
      ;; TODO setup hooks
      )
    )
  (glogx!)
  )

(defun blood-modules--config ()
  "Config the active profile's modules"
  (glog! "TODO: config modules")
  (let ((packages  (apply #'append (hash-table-values blood-modules--declared-packages-ht)))
        )
    (dolist (package-spec packages)
      (ilog! "Loading: %s" (plist-get (plist-get package-spec :id) :package))
      ;; TODO wrap with eval-after-load if necessary
      (require (plist-get (plist-get package-spec :id) :package))
      (funcall (plist-get package-spec :post-load))
      )
    )
  (glogx!)
  )

(provide 'blood-modules)
;;; blood-modules.el ends here
