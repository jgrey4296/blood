;;; blood-sync.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- header
;;
;; Copyright (C) 2023 John Grey
;;
;; Author: John Grey <https://github.com/jgrey4296>
;; Maintainer: John Grey <johngrey@Johns-Mac-mini.local>
;; Created: September 01, 2023
;; Modified: September 01, 2023
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
(llog! "Sync")

(defvar blood--active-module-specs  nil "The Modules to activate in after-init-hook")

(defvar blood--module-packages (make-hash-table) "Maps Module-name -> package-specs")

(defvar blood--package-declaration-failures nil)

(defcustom blood-sync-module-find-fn #'blood-sync--find-with-fd "The function used for searching for `Blood-module-file-pattern` module files, takes two arguments: the search root, and the profile source location to `expand-file-name` it against''" :type 'symbol)

(defun blood-sync-h ()
  "Trigger the syncing of packages, "
  (hlog! "Syncing")
  (unless BLOOD-BOOTSTRAPPED
    (ilog! "Not Bootstrapped")
    (error "Tried to sync blood without it having bootstrapped"))

  (ilog! "Syncing Profiles: %s" (length (hash-table-values blood-profile--declared-ht)))
  (dolist (spec (hash-table-values blood-profile--declared-ht))
    (ghlog! "Syncing Profile: %s" (plist-get spec :name))
    (blood-profile-start-h spec t t)
    (blood-sync--profile spec)
    (glogx!)
    )
  )

(defun blood-sync--complete-h ()
  (hlog! "Sync Complete")
  )

(defun blood-sync--profile (spec)
  "For a profile, get its active modules, find the components of those modules, and sync those components
using the profile's backend"
  (let* ((mod-files (blood-sync--module-specs spec)) ;; get active modules
         (packages  (blood-sync--collect-package-specs mod-files))
         (sync-fn (plist-get (or (plist-get spec :backend) blood--backend-default) :sync))
         )
    ;; pass the specs to the backend to install
    (funcall sync-fn packages)
    (ilog! "TODO: cache files for interactive use")
    (ilog! "TODO: build if cli arg says so")
    (ilog! "TODO: build autoloads")
    (ilog! "Spec Synced: %s" (plist-get spec :name))
    )
  )

(defun blood-sync--module-specs (spec)
  "given a PROFILE spec, get all active modules -> list of paths to active module definitions"
  (glog! "Getting Active Module Specs")
  (let* ((module-locs (plist-get (plist-get spec :paths) :modules)) ;; list of paths
         (modules (plist-get spec :modules)) ;; declared active modules
         (module-symbols (mapcar #'(lambda (x) (blood-modules--sym-from-parts (plist-get x :group)
                                                                                     (plist-get x :module)))
                                        modules))
         (source (plist-get spec :source))
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
    (ilog! "Module Search Found: %s " found-modules)
    (setq active-modules (cl-remove nil (mapcar #'(lambda (x)
                                                    (let* ((dir-parts (reverse (split-string x "/" t " +")))
                                                           (sym (blood-modules--sym-from-parts (caddr dir-parts)
                                                                                               (cadr dir-parts)))
                                                           (in-active-modules (memq sym module-symbols))
                                                           )
                                                      (if in-active-modules x nil)))
                                                found-modules)))
    (ilog! "Found Modules which are active in profile: %s" active-modules)
    (prog1 active-modules
      (glogx!)
      )
    )
  )

(defun blood-sync--collect-package-specs (files &optional allow-loads)
  "given a list of module component file paths, load them -> list of package spec structs (see blood-modules--build-spec)"
  (glog! "Collecting Package Specs from discovered modules: %s" files)
  (clrhash blood-modules--declared-packages-ht)
  (let ((blood-defer--skip-loads (not allow-loads)))
    (dolist (file files)
      ;; Load module definition, suppressing any deferred loads,
      ;; but adding package declarations to `blood-modules--declared-packages-ht`
      (llog! "%s (package collection)" file)
      (load file nil t)
      )
    (prog1 (apply #'append (hash-table-values blood-modules--declared-packages-ht))
      (progn
        (ilog! "Loaded Module Specs: %s" (mapcar #'(lambda (x) (if x (plist-get (plist-get x :id) :fullsym)))
                                                 (apply #'append (hash-table-values blood-modules--declared-packages-ht))))
        (glogx!))
      )
    )
  )

(defun blood--sync-native-comp (spec)
  "TODO"
  (glog! "Triggering native compilation of spec: %s" spec)
  (prog1 nil (glogx!))
  )

(defun blood-sync--find-with-fd (base source)
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
