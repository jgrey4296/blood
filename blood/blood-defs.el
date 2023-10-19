;; blood-defs.el -*- mode: elisp; lexical-binding: t; -*-

(defconst BLOOD-CACHE-ENV-VAR "BLOODCACHE")

(defconst BLOOD-PROFILE-ENV-VAR "BLOODPROFILE")

(defcustom blood-cache-dir (expand-file-name (or (getenv BLOOD-CACHE-ENV-VAR) "~/.cache/blood")) "the directory to use as the emacs/straight cache head")

(defvar blood--cmd 'run "the startup command")

(defvar blood-profile--default (getenv BLOOD-PROFILE-ENV-VAR) "The current profile")

(defvar blood--trace-memory nil)

(defconst blood--noninteractive-cmds '(sync help report profiles))

(provide 'blood-defs)
