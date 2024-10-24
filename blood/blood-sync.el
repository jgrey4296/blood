;;; blood-sync.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- header
;;
;; Copyright (C) 2023 John Grey
;;
;; Author: John Grey <https://github.com/jgrey4296>
;; Maintainer: John Grey <johngrey@Johns-Mac-mini.local>
;; Created: September 01, 2023
;; Modified: September 01, 2023
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
(cl-assert (featurep 'blood-defs))
(cl-assert (featurep 'blood-log))
(cl-assert (featurep 'blood-utils))
(llog! "Sync")
(require 'blood-dag)

(defvar blood--active-module-specs  nil "The Modules to activate in after-init-hook")

(defvar blood--module-packages (make-hash-table) "Maps Module-name -> package-specs")

(defvar blood--package-declaration-failures nil)

(defcustom blood-sync-module-find-fn #'blood-sync--find-with-fd "The function used for searching for `Blood-module-file-pattern` module files, takes two arguments: the search root, and the profile source location to `expand-file-name` it against''" :type 'symbol)

(blood-register-cache! :modules #'(lambda (data) (string-join data "\n")))
(blood-register-cache! :active-modules #'(lambda (data) (string-join data "\n")))

(defun blood-sync-h ()
  "Trigger the syncing of packages, "
  (hlog! "Syncing")
  (unless BLOOD-BOOTSTRAPPED
    (ilog! "Not Bootstrapped")
    (error "Tried to sync blood without it having bootstrapped"))
  (unless blood--backend-active
    (ilog! "No Backend")
    (error "Tried to sync blood without it having a backend"))

  (ilog! "Syncing Profiles: %s" (length (hash-table-values blood-profile--declared-ht)))
  (dolist (spec (hash-table-values blood-profile--declared-ht))
    (ghlog! "Syncing Profile: %s" (blood--profile-s-name spec))
    (blood-profile-start-h spec t t)
    (let* ((mod-files (blood-sync--module-specs-of-profile spec)) ;; get active modules
           (components (blood-sync--collect-module-component-specs mod-files))
           (install-fn (blood--backend-s-sync blood--backend-active))
           )
      (ghlog! "Installing Module Components: %s" (length components))
      (funcall install-fn components)
      (glogx!)
      (ghlog! "Byte Compiling")
      (blood-sync--byte-comp components)
      (glogx!)
      (ghlog! "Native Compiling")
      (blood-sync--native-comp components)
      (glogx!)
      (ghlog! "Generating Autoloads")
      (blood-sync--generate-autoloads components)
      (glogx!)
      (blood-dag-h)
      (ilog! "Profile Synced: %s" (blood--profile-s-name spec))
      )
    (glogx!)
    )
  )

(defun blood-sync--complete-h ()
  (hlog! "Sync Complete")
  )

(defun blood-sync--module-specs-of-profile (spec)
  "given a PROFILE spec, get all active modules -> list of paths to active module definitions"
  (glog! "Getting Active Module Specs")
  (let* ((module-locs (blood--paths-s-modules (blood--profile-s-paths spec))) ;; list of paths
         (modules (blood--profile-s-modules spec)) ;; declared active modules
         (module-symbols (mapcar #'(lambda (x) (blood-uniq-id x)) modules))
         (source (blood--identifier-s-source (blood--profile-s-id spec)))
         found-modules
         active-modules
         )
    ;; Find the module definitions
    (dolist (base module-locs)
      (let ((result (funcall blood-sync-module-find-fn base (file-name-directory source))))
        (if (zerop (string-to-number (car-safe result)))
            (setq found-modules (append (mapcar #'file-truename (split-string (cdr result) "\n" t " +")) found-modules))
          (warn "Searching in %s failed: %s" base result)
          )
        )
      )
    (ilog! "Search Found: %s modules" (length found-modules))
    (blood-cache! :modules found-modules)
    (setq active-modules (cl-remove nil (mapcar #'(lambda (x)
                                                    (let* ((dir-parts (reverse (split-string x "/" t " +")))
                                                           (sym (blood-modules--sym-from-parts (caddr dir-parts)
                                                                                               (cadr dir-parts)))
                                                           (in-active-modules (memq sym module-symbols))
                                                           )
                                                      (if in-active-modules x nil)))
                                                found-modules)))
    (ilog! "Active Modules In Profile: %s" (length active-modules))
    (blood-cache! :active-modules active-modules)
    (prog1 active-modules
      (glogx!)
      )
    )
  )

(defun blood-sync--collect-module-component-specs (files &optional allow-loads)
  "given a list of module component file paths, load them -> list of package spec structs (see blood-modules--build-spec)"
  (glog! "Collecting Module Component Specs from discovered modules: %s" files)
  (clrhash blood-modules--declared-components-ht)
  (let ((blood-defer--skip-loads (not allow-loads)))
    (dolist (file files)
      ;; Load module definition, suppressing any deferred loads,
      ;; but adding package declarations to `blood-modules--declared-components-ht`
      (llog! "%s (package collection)" file)
      (load file nil t)
      )
    (ilog! "Loaded Module Specs: %s" (mapcar #'(lambda (x) (if x (blood-uniq-id x)))
                                             (apply #'append (hash-table-values blood-modules--declared-components-ht))))
    (prog1 (apply #'append (hash-table-values blood-modules--declared-components-ht))
      (glogx!))
    )
  )

(defun blood-sync--native-comp (components)
  "Native compile packages marked as such"
  ;; native-compile-async
  (ghlog! "TODO Native Compiling: %s" components)
  (unless (featurep 'native-compile) (error "Can't Native Compile"))
  (prog1 nil
    (glogx!))
  )

(defun blood-sync--byte-comp (components)
  "Just byte compile a spec's packages"
  ;; byte-recompile-directory
  ;; async-byte-recompile-directory
  ;; byte-compile-dest-file
  (ghlog! "TODO Byte Compiling: %s" components)

  (prog1 nil
    (glogx!))
  )

(defun blood-sync--generate-autoloads (components)
  ;; todo - autoloads generator
  (ghlog! "TODO Generating Autoloads: %s" components)
  nil
  ;; loaddefs-generate
  ;; loaddefs-generate-batch
  ;; make-directory-autoloads
  (glogx!)
  )

(defun blood-sync--find-with-fd (base source)
  "TODO: use blood--find-with-fd instead"
  (ilog! "Searching: %s" (expand-file-name base source))
  (cond ((executable-find "fdfind")
         (blood--call "fdfind" "-i" BLOOD-MODULE-FILE-PATTERN--FD (expand-file-name base source))
         )
        ((executbale-find "fd")
         (blood--call "fd" "-i" BLOOD-MODULE-FILE-PATTERN--FD (expand-file-name base source))
         )
        (t (error "No usable fd found"))
        )
  )


(provide 'blood-sync)
;;; blood-sync.el ends here
