;;; blood-structs.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;; Structs used in blood, in the form: blood--%s-s
;; Constructed using:                  blood-build-%s
;; See footer for licenses/metadata/notes as applicable
;;-- end Header
(loaded? cl-lib)
(llog! "Structs")

(cl-defstruct blood--profile-s
  "The structure of a blood profile"
  (id               nil :type blood--identifier-s    :doc "the name of the profile")
  (default          nil :type bool                   :doc "is this profile the default?")
  (disabled         nil :type bool                   :doc "is the profile disabled?")
  (backend          nil :type symbol                 :doc "The backend for package management to use")
  (bootstrap        nil :type list:fn                :doc "functions for the bootstrap stage")
  (modules          nil :type list:blood--module-dec :doc "the modules this profile activates")
  (constraints      nil :type list                   :doc "constraints the profile places on modules")
  (paths            nil :type blood--paths           :doc "the paths this profile uses")
  (recipes          nil :type list:blood--recipe     :doc "straight recipe declarations")
  (block-compile-of nil :type list                   :doc "packages and files that are not to be compiled")
  (init             nil :type lambda                 :doc "an init hook, when the profile activates, before packages are loaded")
  (config           nil :type lambda                 :doc "config hook, after packages been loaded and configured")

  )

(cl-defstruct blood--module-s
  "A Declaration of a module, used in profile specification"
  (id       nil :type blood--identifier-s)
  (allow    nil :type list)
  (disallow nil :type list)
  )

(cl-defstruct blood--package-s
  "A Specification of a package used in a module"
  (id nil          :type blood--identifier-s)
  (disabled nil    :type bool)
  (defer nil       :type bool)
  (debug nil       :type symbol :doc "trigger package specific debugging. nil|break|log")
  (recipe nil      :type symbol|blood--recipe)
  (autoloads nil   :type list:function :doc "autoload triggers")
  (constraints nil :type blood--constraint :doc "constraints for this package's use")
  (on-init nil     :type lambda)
  (on-load nil     :type lambda)
  (on-ready nil    :type lambda)
  (advice nil      :type list:blood--advice-s :doc "a list of triples describing advice to add")
  (hooks  nil      :type list:pair :doc "a list describing hooks to add. (*hookvars :> fns)")
  ;; TODO: possibly replace 'bind' with 'specs' for repeatability
  (bind   nil      :type list :doc "a list describing functions to call to make bindings")
  )

(cl-defstruct blood--constraint-s
  " A Constraint definition, limiting modules/packages/profiles to certain environments "
  (profile            nil          :type symbol      :doc "the constraint requires this profile")
  (package-version    nil          :type str         :doc "contextually, the package must be this version")
  (emacs-version      nil          :type str         :doc "emacs must have this version")
  (module-version     nil          :type str         :doc "the module must be this version")
  (os                 nil          :type str         :doc "the OS must be this")
  (system             nil          :type str         :doc "the system must have these cli programs")
  (commit             nil          :type str         :doc "the package must be this commit")
  (predicates         nil          :type list:lambda :doc "no-arg functions that must return non-nil to succeed")
  (conflicts          nil          :type list:symbol :doc "packages that disallow this package")
  (after              nil          :type list:symbol :doc "packages that must be loaded first")
  )

(cl-defstruct blood--paths-s
  "Describes the (relative) paths blood uses,
apart from the init entry, which is the expanded path to the relevant init.el "
  (cache   nil :type str)
  (install nil :type str)
  (build   nil :type str)
  (modules nil :type list)
  (secrets nil :type list)
  (init    nil :type str)
  )

(cl-defstruct blood--recipe-s
  "Recipe declarations for straight"
  (host        nil       :type str)
  (repo        nil       :type str)
  (files       nil       :type list)
  (local-repo  nil       :type list)
  (compilation nil       :type symbol)
  )

(cl-defstruct blood--identifier-s
  "A generalized identifier for profiles, modules, packages"
  (profile nil      :type symbol)
  (group   nil      :type symbol)
  (module  nil      :type symbol)
  (package nil      :type symbol)
  (version nil      :type str)
  (commit  nil      :type str)
  (source (file!)   :type str)
  )

(cl-defstruct blood--advice-s
  "declarative collection of advice"
  (targets nil :type "list[symbol]" :doc "functions to advise")
  (advisors nil :type "list[(where fn)]" :doc "list of advisors. eg: '((:around #'blah) (:override #'bloo))")
  )

;; Construction utils:
;; TODO move these into ctors ?

(defun blood-build-profile (name disabled default args) ;; ->  blood--profile-s
  "Macro-processor for blood! profile definition"
  (let ((id (make-blood--identifier-s :profile name))
        (paths (blood-build-paths (plist-get args :paths)))
        (backend (plist-get args :backend))
        (modules (blood-build-modules (plist-get args :active-modules:)))
        )
        (make-blood--profile-s :id id
                               :default default
                               :disabled disabled
                               :backend backend
                               :bootstrap (ensure-list (plist-get args :bootstrap))
                               :modules modules
                               :constraints (blood-build-constraints (plist-get args :constraints))
                               :paths paths
                               :recipes (mapcar #'blood-build-recipe (plist-get args :recipes))
                               :block-compile-of (ensure-list (plist-get args :no-compile))
                               :init   nil ;; TODO make the lambda
                               :config nil ;; TODO make the lambda
                               )
    )
  )

(defun blood-build-modules (modules) ;; -> list[blood--module-s]
  "Process profile syntax into module declarations"
  (let (results group module)
    (while modules
      (cond ((listp (car modules))
             (let ((data (pop modules)))
               (push (make-blood--module-s :id (make-blood--identifier-s :group (car data)
                                                                         :module (cadr data))
                                           :allow (plist-get data :allow)
                                           :disallow (plist-get data :disallow))
                     results)))
            ((keywordp (car modules))
             (setq group (pop modules)))
            ((symbolp (car modules))
             (setq module (pop modules)))
            (t (error "Unknown value used in modules" modules))
            )
      (when (and group module)
        (push (make-blood--module-s :id (make-blood--identifier-s :group group
                                                                  :module module))
              results)
        (setq group nil
              module nil))
      )
    results
    )
  )

(defun blood-build-package (name source args) ;; -> blood--package-s
  "use! uses this to build package definitions"
  (let* ((id (funcall blood-path-to-id-fn name source))
         )
    (make-blood--package-s
     :id id
     :disabled          (plist-get args :disabled)
     :recipe            (blood-build-recipe (plist-get args :recipe))
     :autoloads         (ensure-list (plist-get args :autoloads))
     :constraints       (blood-build-constraints (plist-get args :constraints))
     :on-init          `(lambda () ,@(plist-get args :on-init))
     :on-load          `(lambda () ,@(plist-get args :on-load))
     :on-ready         `(lambda () ,@(plist-get args :on-ready))
     :advice            (blood-build-advice (plist-get args :advice))
     :hooks             (ensure-list (plist-get args :hooks))
     :bind              (ensure-list (plist-get args :bind))
     )
    )
  )

(defun blood-build-recipe (args) ;; -> nil|symbol|blood--recipe-s
  ""
  (cond ((null args)
         nil)
        ((symbolp args)
         args)
        ((and (listp args) (eq (car args) 'quote))
         args
         )
        ((listp args)
         (apply #'make-blood--recipe-s args))
        (t args)
        )
  )

(defun blood-build-constraints (args) ;; -> blood--constraint-s
  (make-blood--constraint-s :emacs-version (plist-get args :emacs)
                            :os (plist-get args :os)
                            :system (ensure-list (plist-get args :system))
                            :predicates (ensure-list (plist-get args :predicates))
                            )
  )

(defun blood-build-advice (args) ;; -> nil|blood--advice-s
  "Build a description of advice to add, later"
  (let ((advisors (plist-get args :advisors)))
    (cl-loop for adv in advisors
             do
             (unless (eq 2 (length adv)) (error "Bad Advisor" adv))
             (unless (-contains? '(:around :before :after :override :after-until :after-while
                                   :before-until :before-while :filter-args :filter-return )
                                 (car adv))
               (error "Bad Advisor location" adv)
               )
             )
    (if advisors
        (make-blood--advice-s :targets   (ensure-list (plist-get args :targets))
                              :advisors  advisors)
      nil
      )
    )
  )

(defun blood-build-paths (args) ;; -> blood--paths-s
  "Build the blood path descriptors, using default values if necessary for everything except modules"
  (unless (plist-get args :modules) (error (format "No Modules location specified: %s" args) args))
  (let ((paths (make-blood--paths-s :cache   (or (plist-get args :cache) blood-cache-dir)
                                    :install (or (plist-get args :install) blood--install-loc-default)
                                    :build   (or (plist-get args :build) blood--build-loc-default)
                                    :modules (ensure-list (plist-get args :modules))
                                    :secrets (ensure-list (or (plist-get args :secrets) blood--secrets-loc-default))
                                    )))
    (log! :debug "Built Paths: %s" paths)
    paths
    )
  )

(defun blood-build-id-from-path (name path) ;; -> blood--identifier-s
  " build an identifier from a name and the path of its definition,
for pacakges
eg: (bbifp 'test' '/blah/bloo/blee/example.el') -> (iden :package test :group bloo :module blee)
"
  (let* ((parts (reverse (split-string path "/" t "\s+")))
         (file (pop parts))
         (module (pop parts))
         (group (pop parts))
         )
    (unless (and file module group)
      (error (format "To Build an Id from a path, it needs to have ../group/module/file.el: %s" path)))
    (make-blood--identifier-s :package (if (symbolp name) name (intern name))
                              :source  path
                              :group   (intern group)
                              :module  (intern module)
                              )
    )
  )

(cl-defun blood-uniq-id (obj) ;; -> symbol
  "Take an object and return a uniq id, as ...symbol?"
  (let ((result (cond ((blood--profile-s-p obj) (blood--identifier-s-profile (blood--profile-s-id obj)))
                      ((blood--module-s-p obj)  (blood--id-sym (blood--module-s-id obj) :group t :module t))
                      ((blood--package-s-p obj) (blood--id-sym (blood--package-s-id obj) :group t :module t :package t))
                      ((stringp obj) (intern obj))
                      (t (error "Unrecognized id source type" obj))
                      )
                )
        )
    (unless (symbolp result) (error "Building unique ID failed" obj))
    result
    )
  )

(cl-defun blood--id-sym (obj &key (profile nil) (group nil) (module nil) (package nil)) ;; -> symbol[*:*:...]
  "Returns a symbol of [group|module|package] interned together
 separated by colons, from the provided object.
   so (obj :module t) -> module
   (obj :group t :module t) -> group:module
   (obj :group t :package t) -> group:package
"

  (let ((id (cond ((blood--module-s-p obj) (blood--module-s-id obj))
                  ((blood--package-s-p obj) (blood--package-s-id obj))
                  ((blood--identifier-s-p obj) obj)
                  (t nil)))
        result
        )
    (unless id (error "Can't get a id component of a non-module or pacakge" obj))
    (when package (push (blood--identifier-s-package id) result))
    (when module  (push  (blood--identifier-s-module id) result))
    (when group   (push (blood--identifier-s-group id)   result))
    (when profile (push (blood--identifier-s-profile id) result))

    (unless result (error "Must get at least one part of the identifier"))
    (intern (string-join (mapcar #'symbol-name (seq-remove #'null result)) ":"))
    )
  )

;;-- utils

(defun blood--profile-s-name (profile)
  (blood--identifier-s-profile (blood--profile-s-id profile)))

(defun blood--package-s-id (package-spec)
  (blood--identifier-s-package (blood--package-s-id package-spec))
  )

;;-- end utils

(provide 'blood-structs)

;;-- Footer
;; Copyright (C) 2024 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    June 04, 2024
;; Modified:   June 04, 2024
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 29.1))
;;
;; This file is not part of GNU Emacs.
;;
;;-- end Footer
;;; blood-structs.el ends here
