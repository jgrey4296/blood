;;; lib.el -*- lexical-binding: t; -*-
(message "----- Loading Core")
(require 'blood-bootstrap)

(defgroup blood nil "Blood settings")

(defconst blood-version              "0.1.0" "This Version of blood")

(defconst blood-available-cmds '(batch clean sync run report stub))

(defconst blood--hook-laziness '(;; Not Lazy
                                     :bootstrap    -99
                                     :clean        -90
                                     :sync         -85
                                     :build        -80
                                     :run          -70
                                     :profile-init -60
                                     :module-init  -50
                                     :module-config 25
                                     :user-min      50
                                     :user-max      99
                                     ) ;; Lazy
  )

(defconst BLOOD-PROFILE-FILE-PATTERN "profile\\(-.+\\)?.el" "blood will search and load all profiles in files with this name")

(defconst BLOOD-MODULE-FILE-PATTERN "module\\(-.+\\).el"  "blood will search and load all files named this in module directories, to get package specs")

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

    (cond ((and noninteractive (eq blood--cmd 'batch))
           '(
             ;; prep load-path for batch cmd's loads
             ))
          ((eq blood--cmd 'sync)
           ;; Option 1: non-interactive install packages
           `(let ((spec (blood--args-to-profile-spec ,profile-name ,default ,disabled ,args)))
              (blood-profile--register spec)
              ;; queue profile for bootstrapping
              (push spec blood--bootstrap-queue)
              (add-hook 'after-init-hook #'blood--bootstrap (plist-get blood--hook-laziness :bootstrap))
              (require 'blood-sync)
              (add-hook 'after-init-hook #'blood-sync (plist-get blood--hook-laziness :sync))
              ;;(when ('build in cli-args) (add-hook 'after-init-hook #'blood--build-packages (plist-get blood-hook-priorities :build)))
              ;; TODO after install, build profile pincushion
              ))
          ((and noninteractive (eq blood--cmd 'clean))
           `(let ((spec (blood--args-to-profile-spec ,profile-name ,default ,disabled ,args)))
              (require 'blood-clean)
              (blood-profile--register spec)
              ;; load pincushion
              ;; Queue the packages for cleaning
              (push spec blood--clean-queue)
              (add-hook 'after-init-hook #'blood--clean (plist-get blood--hook-laziness :clean))
             ))
          ((and noninteractive (eq blood--cmd 'report))
           `(let ((spec (blood--args-to-profile-spec ,profile-name ,default ,disabled ,args)))
              (blood-profile--register spec)
              (require 'blood-report)
              ;; queue this profile to be reported
              (add-hook 'after-init-hook #'blood--report (plist-get blood--hook-laziness :report))
              ))
          (is-interactive
           `(let ((spec (blood--args-to-profile-spec ,profile-name ,default ,disabled ,args))
                  )
              (blood-profile--register spec)
              (if (and ,default (null blood-profile--default))
                  (setq blood-profile--default ,profile-name))

              (when (equal blood-profile--default ,profile-name)
                (message "Setting up activation hooks: %s" ,profile-name)
                (require 'blood--native)
                (add-hook 'after-init-hook #'blood--bootstrap      (plist-get blood--hook-laziness :bootstrap))
                (add-hook 'after-init-hook #'blood--core-setup     (plist-get blood--hook-laziness :run))
                (add-hook 'after-init-hook #'blood-profile-start   (plist-get blood--hook-laziness :profile-init))
                (add-hook 'after-init-hook #'blood-native--setup   (+ 5 (plist-get blood--hook-laziness :profile-init)))
                (add-hook 'after-init-hook #'blood--init-modules   (plist-get blood--hook-laziness :module-init))
                (add-hook 'after-init-hook #'blood--config-modules (plist-get blood--hook-laziness :module-config))
                )
              )
           )
          (t (warn "Unrecognized blood initialisation possibility" 'interactive? noninteractive 'cmd blood--cmd 'profile blood-profile--default))
          )
    )
  )
;;-- end profile

;;-- module

(defmacro use! (package-name &rest args)
  "Declarative activation of packages
  adds spec to blood--module-packages
"
  (let ((compile-sym (gensym))
        (plist-sym (gensym))
        (debug-sym (gensym))
        (module-sym (gensym))
        (source-sym (gensym))
        (source (macroexp-file-name))
        )
    `(let* ((,plist-sym ',args)
            (,compile-sym (pcase (plist-get ',plist-sym :compile)
                            ('nil     nil)
                            (''byte   'byte)
                            (''native 'native)
                            (x        'bad-value)))
            (,source-sym ,source)
            (,module-sym (blood--module-from-path ,source))
            )
       (cond ((eq ,compile-sym 'bad-value)
              (warn "Bad Compile Value Provided in Package Spec" ,package-name ,source-sym ,compile-sym)
              (push (list ,package-name ,@plist-sym) blood--package-declaration-failures))
             ((eq (car ,module-sym) :bad-source)
              (warn "Bad Package Spec source, locate package specs in modules/{group}/{module}/config.el" ,package-name ,source-sym)
              (push (list ,package-name ,@plist-sym) blood--package-declaration-failures))
             (t
              ;; TODO change to puthash
              (push (list
                     :name         ,package-name
                     :source       ,source
                     :module       ,module-sym
                     :recipe       ,(or (plist-get ,plist-sym :recipe) 'default)
                     :after        (list ,@(plist-get ,plist-sym :after))
                     :autoloads    (list ,@(plist-get ,plist-sym :autoloads))
                     :compile      ,compile-sym
                     :constraints  '(:profile :package-version :emacs-version :module-version :commit)
                     :debug        '((:pre :require :post :complete) . (print break))
                     :pre-load     '(lambda () ,@(plist-get args :pre-load))
                     :post-load    '(lambda () ,@(plist-get args :post-load))
                     )
                    blood--module-packages
                    )
              )
             )
       )
    )
  )

;;-- end module

;;-- package

(defmacro install! (name)
  "specify a profile-agnostic package to install, usable by any profile"
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

  ;; mkdir
  ;; mk module file
  )

;;-- end misc-commands

(defalias skulls! #'use!)

(provide 'blood-core)

