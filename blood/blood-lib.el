;;; lib.el -*- lexical-binding: t; -*-
;;-- profile

(defmacro blood! (&rest args)
  " like the doom! macro, specifies a profile of modules

:default {bool}
:profile {sym}                              : names the profile
:package-installation {path}                : where packages are installed
:module-locations ({paths})                 : where modules can be found
[optionals]

:active-modules [:{groupname} {modulename}] : modules to activate for this profile

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
  (declare (indent 4))
  (let* ((profile-name (symbol-name (plist-get args :profile)))
         (disabled     (plist-get args :disabled))
         (default      (plist-get args :default))
         (is-interactive (not noninteractive))
         )
    ;; Handle non-interactive startup variations:
    (when (assq profile-name blood-profile-spec-list)
           (warn "Duplicated profile name, as the profile spec list is an alist, only the last profile of this name will be usable" 'profile profile-name))

    (cond ((and noninteractive (eq blood--cmd 'batch))
           '(
             ;; prep load-path for batch cmd's loads
             ))
          ((eq blood--cmd 'sync)
           ;; Option 1: non-interactive install packages
           `(let ((spec (blood--args-to-profile-spec ,profile-name ,default ,disabled ,args)))
              (princ "---------- Blood: Sync registration\n")
              ;; queue profile for bootstrapping
              (push spec blood--bootstrap-queue)
              (add-hook 'after-init-hook #'blood--bootstrap (plist-get blood--hook-laziness :bootstrap))
              ;; load profile pincushion
              ;; collect packages
              ;; queue packages for install
              (require 'blood-sync)
              (add-hook 'after-init-hook #'blood-sync (plist-get blood--hook-laziness :sync))
              ;;(when ('build in cli-args) (add-hook 'after-init-hook #'blood--build-packages (plist-get blood-hook-priorities :build)))
              ;; TODO after install, build profile pincushion
              )
           )
          ((and noninteractive (eq blood--cmd 'clean))
           `(progn
              ;; load pincushion
              ;; Queue the packages for cleaning
              (push 'blood--clean-queue '())
              (add-hook 'after-init-hook #'blood--clean-installation (plist-get blood--hook-laziness :clean))
             ))
          ((and noninteractive (eq blood--cmd 'report))
           `(progn
             ;; queue this profile to be reported
             (add-hook 'after-init-hook #'blood--report-profiles (plist-get blood--hook-laziness :report))
             )
           )
          (is-interactive
           `(let ((spec (blood--args-to-profile-spec ,profile-name ,default ,disabled ,args))
                  )
              (message "Registering Profile: %-10s : Active: %s" ,profile-name
                       ,(or (eq blood-profile profile-name) (and default (equal blood-profile "default"))))
              (push spec blood-profile-spec-list)
              ,(if (and default (equal blood-profile "default"))
                   `(setq blood-profile ,profile-name))
              ,@(when (or (eq blood-profile profile-name) (and default (equal blood-profile "default")))
                  '((add-hook 'after-init-hook #'blood--bootstrap      (plist-get blood--hook-laziness :bootstrap))
                    (add-hook 'after-init-hook #'blood--core-setup     (plist-get blood--hook-laziness :run))
                    (add-hook 'after-init-hook #'blood--start-profile  (plist-get blood--hook-laziness :profile-init))
                    (add-hook 'after-init-hook #'blood--init-modules   (plist-get blood--hook-laziness :module-init))
                    (add-hook 'after-init-hook #'blood--config-modules (plist-get blood--hook-laziness :module-config))
                    )
                  )
              )
           )
          (t (warn "Unrecognized doom-init possibility" 'interactive? noninteractive 'cmd blood--cmd 'profile blood-profile))
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
                     :defer        ,(or (plist-get ,plist-sym :defer) nil)
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
  ;; todo -- generate report on modules, packages,
  ;; load order, memory usage, load time, etc

  )

(defmacro log! (text &rest args)
  " A Simple, doom-less debug message when 'debug-on-error is true"
  `(when debug-on-error
     (let ((inhibit-message t))
       (funcall #'message ,text ,@args)
       )
     )
  )

(defun inhibit! (&rest inhibited)
  "WARNING: will break stuff. Add a feature to `features', so any `require's for it become no-ops. "
  (mapcar #'provide inhibited)
  )

(defun stub-module! (group name &optional loc)
  "create a stub module in a location
  defaults to wherever (dir init.el)/modules/group/name
  unless loc is provided, in which case loc/group/name is used"

  ;; mkdir
  ;; mk module file
  )

;;-- end misc-commands

;;-- utilities

(defun blood--generate-autoloads (&rest args)

  ;; todo - autoloads generator
  )

(defun doom-restore-menu-bar-in-gui-frames-h (&optional frame)
  "from doom for handling GUI's on mac with no menu bar"
  (when-let (frame (or frame (selected-frame)))
    (when (display-graphic-p frame)
      (set-frame-parameter frame 'menu-bar-lines 1)))
  )

(defun blood-module-from-path (str)
  "create a (:group %s :module %s) declaration from a path string"
  (let* ((parts (split-string (expand-file-name str) "/" t))
         (relevant (member "modules" parts))
         )
    (if (eq (length relevant) 4)
        `(:group ,(intern (cadr relevant))
          :module ,(intern (caddr relevant)))
      `(:bad-source ,str))
    )
  )

(defmacro blood--args-to-profile-spec (profile-name default disabled args)
  `(list
    :name                 ,profile-name
    :source               (file!)
    :default              ,default
    :disabled             ,disabled
    :bootstrap            ,(or (plist-get args :bootstrap) (list))
    :modules              ,(blood--args-to-module-decs (memq :active-modules args))
    :constraints          (list :system        ,(plist-get args :on-system)
                                :emacs-version ,(plist-get args :on-emacs)
                                )
    :paths                (list :install ,(plist-get args :install-to)
                                :build   ,(plist-get args :build-to)
                                :modules ,(cons 'list (plist-get args :modules-from))
                                )
    :recipes              ,(plist-get args :recipes)
    :block-compile-of     ,(plist-get args :block-compile-of)
    :post-activation      ,(when (plist-get args :on-activation)
                             `(lambda () ,@(plist-get args :on-activation)))
    )
  )

(defun blood--args-to-module-decs (lst)
  "Convert the remaining list into a list of (:group %s :module %s :allow () :disallow ())"
  (let ((source (cl-copy-list lst))
        curr res)
    (while source
      (pcase (pop source)
        (:active-modules nil)
        ((and kw (pred keywordp) (guard (memq kw '(:allow :disallow))))
         (setq curr (append curr (list kw (pop source))))
         )
        ((and kw (pred keywordp))
         (when curr (push curr res) (setq curr nil))
         (setq curr (append curr (list :group (substring (symbol-name kw) 1) :module (symbol-name (pop source)))))
         )
        (other
         (message "Unknown module dec: %s" other)
         )
        )
      )
    (push curr res)
    (list 'quote res)
    )
  )

;;-- end utilities

;;-- hooks
;;;; Installed on after-init-hook:

(defun blood--core-setup ()
  "set core settings"
  (set-language-environment "UTF-8")
  ;; General Startup Settings
  (setq default-input-method nil
        gc-cons-threshold                            (* 16 1024 1024)
        inhibit-startup-screen                       t
        inhibit-startup-echo-area-message            "Startup"
        package-enable-at-startup                    nil
        jka-compr-verbose                            init-file-debug
        auto-mode-case-fold                          nil
        bidi-display-reordering                      'left-to-right
        bidi-paragraph-direction                     'left-to-right
        cursor-in-non-selected-windows               nil
        highlight-nonselected-windows                nil
        fast-but-imprecise-scrolling                 t
        ffap-machine-p-known                         'reject
        idle-update-delay                            1.0
        inhibit-compacting-font-caches               t
        read-process-output-max                      (* 64 1024)  ; 64kb
        redisplay-skip-fontification-on-input        t
        gcmh-idle-delay                              'auto  ; default is 15s
        gcmh-auto-idle-delay-factor                  10
        gcmh-high-cons-threshold                     (* 16 1024 1024)  ; 16mb

        menu-bar-mode                  nil
        tool-bar-mode                  nil
        scroll-bar-mode                nil
        )
  (push '(menu-bar-lines . 0)   default-frame-alist)
  (push '(tool-bar-lines . 0)   default-frame-alist)
  (push '(vertical-scroll-bars) default-frame-alist)
  (when (eq system-type 'darwin)
    (add-hook 'window-setup-hook          #'doom-restore-menu-bar-in-gui-frames-h)
    (add-hook 'after-make-frame-functions #'doom-restore-menu-bar-in-gui-frames-h)
    )
    ;; todo - setup load timing
    ;; todo - user-emacs-dir -> cache
    ;; todo - modify load-path
    ;; todo - modify native-comp-eln-load-path
    ;; todo - limit native-comp-async-jobs-number
    ;; todo - set state dir
    ;; todo - set server-auth-dir
    ;; todo - set auth-sources , encrypted
    ;; todo - handle customized variables
    ;; todo - on mac regrab focus: (when (display-graphic-p (selected-frame)) (set-frame-parameter frame 'menu-bar-lines 1))
    )

(defun blood--start-profile ()
  "Start the cli specified / default profile"
  (blood-add-profile blood-profile)
  )

(defun blood-add-profile (spec-name)
  "interactively start an additional profile"
  (interactive)
  (message "TODO: activate spec: %s" spec-name)
)

(defun blood--init-modules ()
  "Start the acvtive profile's modules"
  (message "TODO: init modules")
  )

(defun blood--config-modules ()
  "Config the active profile's modules"
  (message "TODO: config modules")
  )

;;-- end hooks

(add-hook 'after-init-hook (lambda () (message "Starting After Init Hook"))
          (plist-get blood--hook-laziness :bootstrap))
(add-hook 'after-init-hook (lambda () (switch-to-buffer "*Messages*"))
          (plist-get blood--hook-laziness :user-max))

(provide 'blood-lib)
