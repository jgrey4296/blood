;;; blood-core.el -*- lexical-binding: t; -*-
(loaded? blood-defs blood-log blood-utils blood-deferral blood-modules blood-structs)
(llog! "Core")

;;-- profile

(defmacro blood! (&rest args)
  " Specify a blood profile.

:default {bool}
:profile {sym}                              : names the profile
:package-installation {path}                : where packages are installed
:module-locations ({paths})                 : where modules can be found
[optionals]

:active-modules: [:{groupname} {modulename}] : modules to activate for this profile

optionals:
:on-system {type}                : constraint
:emacs-version {ver}             : constraint
:cache-to  {path}                : default to ~/.cache/emacs
:secrets {path}                  : for auth-sources
:block-compilation (list)        : packages to not byte/eln compile
:recipes {recipes}               : for adding to straight
:disabled t                      : for making a no-op during syncing
------

Has Three main modes of running:
- Batch       : install packages, byte/native compile them, setup autoloads
- Interactive : run modules that are active
- Report      : print a report on module / package state

"
  (declare (indent 2))

  (let* ((profile-name `(quote ,(plist-get args :profile)))
         (disabled     (plist-get args :disabled))
         (default      (plist-get args :default))
         (spec-builder `(blood-build-profile ,profile-name ,disabled ,default ',args))
         )

    (cond (disabled ;; don't do anything if the profile is disabled
           `(ilog! "- Profile: %s (disabled)" ,profile-name)
           )
          ((eq blood--cmd 'sync) ;; install packages
           `(let ((spec ,spec-builder))
              (blood-profile--register spec)
              ))
          ((eq blood--cmd 'clean)
           `(let ((spec ,spec-builder))
              (blood-profile--register spec)
              (push spec blood--clean-queue)
              ))
          ((eq blood--cmd 'report)
           `(let ((spec ,spec-builder))
              (blood-profile--register spec)
              ))
          ((not noninteractive)
           `(let ((spec ,spec-builder))
              (blood-profile--register spec)
              (push spec blood--bootstrap-queue)
              (if (and ,default (null blood-profile--default))
                  (setq blood-profile--default ,profile-name))

              (when (equal blood-profile--default ,profile-name)
                )
              )
           )
          (t nil)
          )
    )
  )

;;-- end profile

;;-- module-linked package

(defmacro use! (package-name &rest args)
  "Declarative activation of a single package.
  adds spec to blood--module-packages
"
  (let* ((debug-sym (gensym))
         (spec-sym (gensym))
         (source-sym (gensym))
         (uniq-sym (gensym))
         (module-sym (gensym))
         (source (file!))
         )

    `(let* ((,source-sym ,source)
               (,spec-sym (blood-build-package ,package-name ,source (quote ,args)))
               (,uniq-sym (blood-uniq-id ,spec-sym))
               (,module-sym (blood--id-sym ,spec-sym :profile nil :package nil :group t :module t))
               )
       (cond ((blood--package-s-disabled ,spec-sym)
              (ilog! "- Package %s (disabled) (%s)" (blood-uniq-id ,spec-sym) (blood--identifier-s-source (blood--package-s-id ,spec-sym))))
             ((gethash ,uniq-sym blood-modules--declared-components-ht)
              (ilog! "- Package %s (duplicated) (%s)" ,uniq-sym  (blood--identifier-s-source (blood--package-s-id ,spec-sym))))
             ((blood--package-s-p ,spec-sym)
              (ilog! "+ Package %s (%s)" ,uniq-sym (blood--identifier-s-source (blood--package-s-id ,spec-sym)))
              ;; Register the component spec
              (puthash ,uniq-sym ,spec-sym blood-modules--declared-components-ht)
              ;; Register the package under its group:module
              (push ,uniq-sym (gethash ,module-sym blood-modules--package-component-map-ht))
              ,spec-sym
              )
             (t
              (ilog! "- Package Spec Failed. (%s)" ,package-name)
              (push ,spec-sym blood--package-declaration-failures)
             )
       )
    )
    )
  )

;;-- end module-linked package

;;-- module-agnostic package

(defmacro install! (name)
  "specify a profile and module-agnostic package to install, usable by any profile"
    ;; TODO, using blood--package
  )

;;-- end module-agnostic package

(defalias 'skull! #'use!)

(provide 'blood-core)
