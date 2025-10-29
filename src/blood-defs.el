;; blood-defs.el -*- mode: elisp; lexical-binding: t; -*-
(llog! "Blood Defs")

(defgroup blood nil "Blood settings")

;;-- env vars

(defconst BLOOD-CONFIG-ENV-VAR "BLOOD_CONFIG")

(defconst BLOOD-CACHE-ENV-VAR  "BLOOD_CACHE")

(defconst BLOOD-PROFILE-ENV-VAR "BLOOD_PROFILE")

(defcustom blood-config-dir (expand-file-name (or (getenv BLOOD-CONFIG-ENV-VAR)
                                                  "~/.config/blood/"))
  "directory for config files")

(defcustom blood-cache-dir  (expand-file-name (or (getenv BLOOD-CACHE-ENV-VAR)
                                                  "~/.cache/blood"))
  "the directory to use as the emacs/straight cache head")

;;-- end env vars

;;-- consts

(defconst BLOOD-PROFILE-FILE-PATTERN "profile\\(-.+\\)?.el" "blood will search and load all profiles in files with this name")

(defconst BSD-TYPES '(darwin berkeley-unix gnu/kfreebsd))

(defconst MAC-TYPES '(darwin))

(defconst WIN-TYPES '(cygwin windows-ms ms-dos))

(defconst blood--eln-cache-name "eln-cache")

(defconst blood--noninteractive-cmds '(sync help report profiles))

(defconst blood--original-load-path (copy-sequence load-path))

(defconst blood-available-cmds '(batch clean sync run report stub))

(defconst blood-version "0.1.0" "This Version of blood")

(defconst blood--hook-laziness '(;; Not Lazy
                                 :cold-start   -99
                                 :bootstrap    -95
                                 :clean        -90 ;; TODO : move clean/sync/build/run to after user-max
                                 :sync         -85
                                 :build        -80
                                 :run          -70
                                 :profile-init -60
                                 :module-init  -50
                                 :module-config 25
                                 :user-min      50
                                 :user-max      90
                                 :finalize      99
                                 ) ;; Lazy
  "Standard laziness values for hooks.
if you're lazy, you run later than others.
use `bloody-lazy!' to convert the values
"
  )

(defconst blood--install-loc-default (expand-file-name "install" blood-cache-dir) "Default location for blood packages installation")

(defconst blood--build-loc-default   (expand-file-name "build" blood-cache-dir)   "Default build location of packages")

(defconst blood--secrets-loc-default "~/.config/secrets"       "Default location for secrets")
;;-- end consts

;;-- vars

(defvar BLOOD-BOOTSTRAPPED nil)

(defvar blood--caches (make-hash-table))

(defvar blood--cmd 'run "the startup command")

(defvar blood--trace-memory nil)

(defvar blood-module-locations (list (expand-file-name "modules" blood-config-dir)) "Where modules are searched for")

(defvar blood-path-to-id-fn #'blood-build-id-from-path "(lambda (package-name path)) -> identifier")

(defvar blood-profile--default (getenv BLOOD-PROFILE-ENV-VAR) "The current profile")
;;-- end vars

;;-- hooks

(defvar blood-bootstrap-hook nil "The hook to place functions for bootstrapping on")

(defvar blood-clean-hook nil "The hook for user to place functions for cleaning on")

;;-- end hooks

(provide 'blood-defs)
;;; blood-defs.el ends here
