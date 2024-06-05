;;; lib.el -*- lexical-binding: t; -*-
(llog! "Core")
(require 'blood-bootstrap)

(defgroup blood nil "Blood settings")

(defconst blood-version "0.1.0" "This Version of blood")

(defconst blood-available-cmds '(batch clean sync run report stub))

(defvar blood--original-load-path (copy-sequence load-path))

(defvar blood-module-locations (list (expand-file-name "modules" blood-config-dir)))

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

;; Commands

(defun blood--cmd-sync ()
  " "
  ;; queue profile for bootstrapping
  (require 'blood-sync)
  (require 'blood-dag)
  (require 'blood-modules)
  (add-hook 'after-init-hook #'blood-bootstrap-h (bloody-lazy! :bootstrap))
  (add-hook 'after-init-hook #'blood-sync-h               (bloody-lazy! :sync))
  (add-hook 'after-init-hook #'blood--setup-default-h     (bloody-lazy! :run))
  (add-hook 'after-init-hook #'blood-sync--complete-h     (bloody-lazy! :finalize))
  ;;(when ('build in cli-args) (add-hook 'after-init-hook #'blood--build-packages (plist-get blood-hook-priorities :build)))
  (add-hook 'after-init-hook #'(lambda () (ilog! "TODO: build version report")) (bloody-lazy! :build))
  (when ,is-interactive (add-hook 'after-init-hook #'(lambda () (require 'evil) (evil-mode)) (bloody-lazy! :finalize 1)))

  )

(defun blood--cmd-clean ()
  ""
  (require 'blood-clean)
  ;; load pincushion
  ;; Queue the packages for cleaning
  (add-hook 'after-init-hook #'blood--clean-h (bloody-lazy! :clean))
  )

(defun blood--cmd-report ()
  (require 'blood-report)
  (add-hook 'after-init-hook #'blood--report-h (bloody-lazy! :report))
  )

(defun blood--cmd-stub ()
  (require 'blood-stub)
  (add-hook 'after-init-hook #'blood-stub-h (bloody-lazy! :run))
  )

(defun blood--cmd-run ()
  ;; (ilog! "Setting up activation hooks: %s" ,profile-name)
  (require 'blood--native)
  (require 'blood-modules)
  (add-hook 'after-init-hook #'blood-bootstrap-h                     (bloody-lazy! :bootstrap))
  (add-hook 'after-init-hook #'blood--setup-default-h                (bloody-lazy! :run))
  (add-hook 'after-init-hook #'blood-profile-start-h                 (bloody-lazy! :profile-init))
  (add-hook 'after-init-hook #'blood-native--setup-h                 (bloody-lazy! :profile-init 5))
  (add-hook 'after-init-hook #'blood-modules--init-packages-h        (bloody-lazy! :module-init))
  (add-hook 'after-init-hook #'blood-modules--config-packages-h      (bloody-lazy! :module-config))
  (when blood--trace-memory
    (add-hook 'after-init-hook #'blood-trace--memory-report-h         (bloody-lazy! :module-config 2))
    )
  (add-hook 'after-init-hook #'blood-defer--start-h                  (bloody-lazy! :finalize))
  )

(defalias 'skull! #'use!)

(provide 'blood-core)
