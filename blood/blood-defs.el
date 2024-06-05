;; blood-defs.el -*- mode: elisp; lexical-binding: t; -*-

(defconst BLOOD-CACHE-ENV-VAR "BLOOD_CACHE")

(defconst BLOOD-PROFILE-ENV-VAR "BLOOD_PROFILE")

(defcustom blood-cache-dir (expand-file-name (or (getenv BLOOD-CACHE-ENV-VAR) "~/.cache/blood")) "the directory to use as the emacs/straight cache head")

(defvar blood--cmd 'run "the startup command")

(defvar blood-profile--default (getenv BLOOD-PROFILE-ENV-VAR) "The current profile")

(defvar blood--trace-memory nil)

(defconst blood--noninteractive-cmds '(sync help report profiles))

(defvar blood-path-to-id-fn #'blood-build-id-from-path
  "(lambda (package-name path)) -> identifier")

(provide 'blood-defs)
