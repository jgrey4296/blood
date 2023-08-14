;;; lib.el -*- lexical-binding: t; -*-
(require 're-doom-deferral)
(require 'cl-lib)
;;-- vars
(defgroup 're-doom)

(defconst re-doom-version "0.1.0" "This Version of re-doom")
(defconst re-doom-profile "default" "The current profile")

(defvar re-doom--active-profile-specs nil "The Specs of the active profiles, as a stack. the original profile is last.")
(defvar re-doom--active-module-specs  nil "The Modules to activate in after-init-hook")
(defvar re-doom-profile-spec-alist    nil "All declared profiles, which can be activated later")

(defvar re-doom--cmd nil)
(defvar re-doom-module-setup-filename "config.el" "re-doom will search and load all files named this in module directories, to get package specs")

(defconst re-doom--available-cmds '(batch clean sync run report stub))

(defvar re-doom--module-packages (make-hash-table) "Maps Module-name -> package-specs")
(defvar re-doom--package-declaration-failures nil)

;; hook priorities
(defconst re-doom--hook-laziness '(;; Not Lazy
                                     :bootstrap    -99
                                     :clean        -90
                                     :sync         -85
                                     :build        -80
                                     :run          -70
                                     :module-init  -50
                                     :module-config 25
                                     :user-min      50
                                     :user-max      99
                                     ) ;; Lazy
  )

;;-- end vars

;;-- structs
(defconst re-doom-profile-struct '(
                                   :name                 'unquoted-symbol
                                   :source               'string
                                   :default              'bool
                                   :disabled             'bool
                                   :bootstrapper-modules 'list
                                   :modules              'list
                                   :constraints          '(:system :emacs-version)
                                   :paths                '(:straight :build :modules)
                                   :recipes              'list
                                   :block-compile        'list
                                   :post-activation      'fn
                                   )
  )

(defconst re-doom-package-struct '(
                                   :name         'unquoted-symbol
                                   :recipe       'list-or-symbol
                                   :after        'list
                                   :defer        'bool
                                   :autoloads    'list
                                   :compile      '(byte native nil)
                                   :constraints  '(:profile :package-version :emacs-version :module-version :commit)
                                   :debug        '((:pre :require :post :complete) . (print break))
                                   :pre-load     'fn
                                   :post-load    'fn
                                   ))

;;-- end structs

(defmacro re-doom! (maybe-is-default &rest args)
  " like the doom! macro:

:default?
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
:more-straight-recipes {recipes} : for adding to straight
:disabled t : for making a no-op during syncing
------

Has Three main modes of running:
- Batch       : install packages, byte/native compile them, setup autoloads
- Interactive : run modules that are active
- Report      : print a report on module / package state

"
  (declare (indent 4))
  (let* ((arg-alist (if (eq is-default :default) args
                      (cons is-default args)))
         (profile-name (symbol-name (plist-get arg-alist :profile)))
         (disabled     (plit-get arg-alist :disabled))
         )
    ;; Handle non-interactive startup variations:
    (when (assq profile-name re-doom-profile-spec-alist)
           (warn
            "Duplicated profile name, as the profile spec list is an alist, only the last profile of this name will be usable"
            'profile profile-name))

    (cond ((and noninteractive (eq re-doom--cmd 'batch))
           '(
             ;; prep load-path for batch cmd's loads
             ))
          ((and noninteractive (eq re-doom--cmd 'sync))
           ;; Option 1: non-interactive install packages
           `(progn
             ;; queue profile for bootstrapping
             (push spec re-doom--bootstrap-queue)
             (add-hook 'after-init-hook #'re-doom--bootstrap (plist-get re-doom--hook-laziness :bootstrap))
             ;; load profile pincushion
             ;; collect packages
             ;; queue packages for install
             (add-hook 'after-init-hook #'re-doom--sync-straight (plist-get re-doom--hook-laziness :sync))
             ;;(when ('build in cli-args) (add-hook 'after-init-hook #'re-doom--build-packages (plist-get re-doom-hook-priorities :build)))
             ;; TODO after install, build profile pincushion
             ))
          ((and noninteractive (eq re-doom--cmd 'clean))
           `(progn
              ;; load pincushion
              ;; Queue the packages for cleaning
              (push 're-doom--clean-queue '())
              (add-hook 'after-init-hook #'re-doom--clean-installation (plist-get re-doom--hook-laziness :clean))
             ))
          ((and noninteractive (eq re-doom--cmd 'report))
           `(progn
             ;; queue this profile to be reported
             (add-hook 'after-init-hook #'re-doom--report-profiles (plist-get re-doom--hook-laziness :report))
             )
           )
          ((and (not noninteractive) (not (eq re-doom-profile profile-name)))
           ;; Option 2: interactive, wrong profile
           ;; build module spec so it can be activated manually
           `(progn
              ;; build specs
              (push specs re-doom-profile-spec-alist)
             ))
          ((and (not noninteractive) (eq re-doom-profile profile-name))
           ;; Option 3: interactive, right profile
           ;; if profile is not the set profile, no-op
           `(progn

             (add-hook 'after-init-hook #'re-doom--interactive-start-profile (plist-get re-doom--hook-laziness :run))
             ))
          (t (warn "Unrecognized doom-init possibility" 'interactive? noninteractive 'cmd re-doom--cmd 'profile re-doom-profile))
          )
    )
  )

(defmacro use! (package-name &rest args)
  ;; Declarative activation of packages
  ;; adds spec to re-doom--module-packages
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
            (,module-sym (re-doom--module-from-path ,source))
            )
       (cond ((eq ,compile-sym 'bad-value)
              (warn "Bad Compile Value Provided in Package Spec" ,package-name ,source-sym ,compile-sym)
              (push (list ,package-name ,@plist-sym) re-doom--package-declaration-failures))
             ((eq (car ,module-sym) :bad-source)
              (warn "Bad Package Spec source, locate package specs in modules/{group}/{module}/config.el" ,package-name ,source-sym)
              (push (list ,package-name ,@plist-sym) re-doom--package-declaration-failures))
             (t
              (push (list
                     :name         ,package-name
                     :source       ,source
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
                    re-doom--module-packages
                    )
              )
             )
       )
    )
  )

(defun report! ()
  ;; todo -- generate report on modules, packages,
  ;; load order, memory usage, load time, etc

  )

(defmacro dlog! (text &rest args)
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
  ;; create a stub module in a location
  ;; defaults to wherever (dir init.el)/modules/group/name
  ;; unless loc is provided, in which case loc/group/name is used

  )

(defun re-doom--generate-autoloads (&rest args)
  ;; todo - autoloads generator
  )

(defun doom-restore-menu-bar-in-gui-frames-h (&optional frame)
  "from doom for handling GUI's on mac with no menu bar"
  (when-let (frame (or frame (selected-frame)))
    (when (display-graphic-p frame)
      (set-frame-parameter frame 'menu-bar-lines 1)))
  )

(defun re-doom--module-from-path (str)
  (let* ((parts (split-string (expand-file-name str) "/" t))
         (relevant (member "modules" parts))
         )
    (if (eq (length relevant) 4)
        `(:group ,(intern (cadr relevant))
          :module ,(intern (caddr relevant)))
      `(:bad-source ,str))
    )
  )

;;;; Installed on after-init-hook:

(defun re-doom--interactive-start-profile ( specs )
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

(defun re-doom--init-modules ()

  )

(defun re-doom--sync-straight ()

  )

(defun re-doom--clean-installation ()

  )

(provide 're-doom-lib)
