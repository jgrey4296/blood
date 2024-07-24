;;; blood-core.el -*- lexical-binding: t; -*-
(cl-assert (featurep 'blood-defs))
(cl-assert (featurep 'blood-log))
(cl-assert (featurep 'blood-utils))
(cl-assert (featurep 'blood-deferral))
(llog! "Core")
(require 'blood-bootstrap)
(require 'blood-cmds)

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

  (let* ((profile-name (plist-get args :profile))
         (disabled     (plist-get args :disabled))
         (default      (plist-get args :default))
         (is-interactive (not noninteractive))
         )

    (cond ((eq blood--cmd 'sync) ;; Option 0: non-interactive install packages
           `(let ((spec (blood-build-profile ,profile-name
                                             ,disabled
                                             ,default
                                             ,args)))
              (blood-profile--register spec)
              (blood--cmd-sync)
              ))
          ((eq blood--cmd 'clean)
           `(let ((spec (blood-build-profile ,profile-name ,default ,disabled ,args)))
              (blood-profile--register spec)
              (push spec blood--clean-queue)
              (blood--cmd-clean)
              ))
          ((eq blood--cmd 'report)
           `(let ((spec (blood-build-profile ,profile-name ,default ,disabled ,args)))
              (blood-profile--register spec)
              (blood--cmd-report)
              ))
          ((eq blood--cmd 'stub)
           (blood--cmd-stub))
          (is-interactive
           `(let ((spec (blood-build-profile ,profile-name ,default ,disabled ,args)))
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
         (source (file!))
         )

    `(let* ((,source-sym ,source)
            (,spec-sym (make-blood--package-s ,name ,args))
            )
       (cond ((eq (blood--package-s-compile ,spec-sym) 'bad-value)
              (warn "Bad Compile Value Provided in Package Spec" ,package-name ,source-sym ,spec-sym)
              (push ,spec-sym blood--package-declaration-failures))
             ((blood--identifier-s-p ,spec-sym))
              (warn "Bad Package Spec declared. Source: %s" ,(blood--identifier-s-source spec-sym))
              (push ,spec-sym blood--package-declaration-failures))
             ((eval (blood--package-s-disabled ,spec-sym))
              (ilog! "Package Disabled: %s (%s)" ',(blood-uniq-id spec-sym) ,(blood--identifier-s-source (blood--package-s-id spec-sym))
             (t
              ;; Register the component spec
              (puthash  (blood-uniq-id ,spec-sym) ,spec-sym blood-modules--declared-components-ht)
              ;; Register the package
              (push (blood-uniq-id ,spec-sym)
                    (gethash (blood--id-sym ,spec-sym :group t :module t) blood-modules--package-component-map-ht))
              ,spec-sym
              )
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
