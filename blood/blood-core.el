;;; lib.el -*- lexical-binding: t; -*-
(llog! "Core")
(require 'blood-bootstrap)

(defgroup blood nil "Blood settings")

(defconst blood-version              "0.1.0" "This Version of blood")

(defconst blood-available-cmds '(batch clean sync run report stub))


(defcustom blood-cache-dir (expand-file-name "~/.cache/blood/") "the directory to use as the emacs/straight cache head")

(defcustom blood-config-dir (expand-file-name "~/.config/blood/") "directory for config files")

(defvar blood--original-load-path (copy-sequence load-path))

;;-- profile

(defmacro blood! (&rest args)
  " like the doom! macro, specifies a profile of modules

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

  (let* ((profile-name (symbol-name (plist-get args :profile)))
         (disabled     (plist-get args :disabled))
         (default      (plist-get args :default))
         (is-interactive (not noninteractive))
         )

    (cond ((eq blood--cmd 'batch)
           '(
             ;; prep load-path for batch cmd's loads
             ))
          ((eq blood--cmd 'sync)
           ;; Option 1: non-interactive install packages
           `(let ((spec (blood-profile--build-spec ,profile-name ,default ,disabled ,args)))
              (blood-profile--register spec)
              ;; queue profile for bootstrapping
              (push spec blood--bootstrap-queue)
              (add-hook 'after-init-hook #'blood-bootstrap-h (bloody-lazy! :bootstrap))
              (require 'blood-sync)
              (require 'blood-modules)
              (require 'blood-dag)
              (add-hook 'after-init-hook #'blood-sync-h               (bloody-lazy! :sync))
              (add-hook 'after-init-hook #'blood--setup-default-h     (bloody-lazy! :run))
              (add-hook 'after-init-hook #'blood-sync--complete-h     (bloody-lazy! :finalize))
              (add-hook 'after-init-hook #'blood-dag-h                (bloody-lazy! :finalize -1))
              ;;(when ('build in cli-args) (add-hook 'after-init-hook #'blood--build-packages (plist-get blood-hook-priorities :build)))
              (add-hook 'after-init-hook #'(lambda () (ilog! "TODO: build version report")) (bloody-lazy! :build))
              ))
          ((eq blood--cmd 'clean)
           `(let ((spec (blood-profile--build-spec ,profile-name ,default ,disabled ,args)))
              (require 'blood-clean)
              (blood-profile--register spec)
              ;; load pincushion
              ;; Queue the packages for cleaning
              (push spec blood--clean-queue)
              (add-hook 'after-init-hook #'blood--clean-h (bloody-lazy! :clean))
             ))
          ((eq blood--cmd 'report)
           `(let ((spec (blood-profile--build-spec ,profile-name ,default ,disabled ,args)))
              (blood-profile--register spec)
              (require 'blood-report)
              ;; queue this profile to be reported
              (add-hook 'after-init-hook #'blood--report-h (bloody-lazy! :report))
              ))
          (is-interactive
           `(let ((spec (blood-profile--build-spec ,profile-name ,default ,disabled ,args))
                  )
              (blood-profile--register spec)
              (if (and ,default (null blood-profile--default))
                  (setq blood-profile--default ,profile-name))

              (when (equal blood-profile--default ,profile-name)
                (ilog! "Setting up activation hooks: %s" ,profile-name)
                (require 'blood--native)
                (require 'blood-modules)
                (require 'blood-dag)
                (add-hook 'after-init-hook #'blood-bootstrap-h                     (bloody-lazy! :bootstrap))
                (add-hook 'after-init-hook #'blood--setup-default-h                (bloody-lazy! :run))
                (add-hook 'after-init-hook #'blood-profile-start-h                 (bloody-lazy! :profile-init))
                (add-hook 'after-init-hook #'blood-native--setup-h                 (bloody-lazy! :profile-init 5))
                (add-hook 'after-init-hook #'blood-modules--init-packages-h        (bloody-lazy! :module-init))
                (add-hook 'after-init-hook #'blood-modules--config-packages-h      (bloody-lazy! :module-config))
                (add-hook 'after-init-hook #'blood-defer--start-h                  (bloody-lazy! :finalize))
                (add-hook 'after-init-hook #'blood-dag-h                           (bloody-lazy! :finalize))
                )
              )
           )
          (t nil)
          )
    )
  )
;;-- end profile

;;-- module

(defmacro use! (package-name &rest args)
  "Declarative activation of packages
  adds spec to blood--module-packages
"
  (let* ((debug-sym (gensym))
        (spec-sym (gensym))
        (source-sym (gensym))
        (source (file!))
        (id-spec (blood-modules--id-from-path source package-name))
        )

    `(let* ((,source-sym ,source)
            (,spec-sym (blood-modules--build-spec ,id-spec ,args))
            )
       (cond ((eq (plist-get ,spec-sym :compile) 'bad-value)
              (warn "Bad Compile Value Provided in Package Spec" ,package-name ,source-sym ,spec-sym)
              (push ,spec-sym blood--package-declaration-failures))
             ((plist-get (plist-get ,spec-sym :id) :bad-source)
              (warn "Bad Package Spec source, locate package specs in modules/{group}/{module}/config.el" ,package-name ,source-sym)
              (push ,spec-sym blood--package-declaration-failures))
             (t
              (push ,spec-sym
                    (gethash (plist-get (plist-get ,spec-sym :id) :sym)
                             blood-modules--declared-packages-ht)
                       )
              ,spec-sym
              )
             )
       )
    )
  )

;;-- end module

;;-- package

(defmacro install! (name)
  "specify a profile and module -agnostic package to install, usable by any profile"
  ;; TODO
  )

;;-- end package

;;-- misc-commands

(defun report! ()
  (interactive)
  ;; todo -- generate report on modules, packages,
  ;; load order, memory usage, load time, etc

  )

(defun stub-module! (group name &optional loc)
  (interactive)
  "create a stub module in a location
  defaults to wherever (dir init.el)/modules/group/name
  unless loc is provided, in which case loc/group/name is used"
  (warn "TODO")
  ;; mkdir
  ;; mk module file
  )

;;-- end misc-commands

(defalias 'skull! #'use!)

(provide 'blood-core)
