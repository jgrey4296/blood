;; early-init.el -*- lexical-binding: t; no-byte-compile: t; -*-

;; finding BLOOD:
;; config files will be in BLOOD_CONFIG or ~/.config/blood
;; blood's source will be in BLOOD_SRC or BLOOD_CONFIG

;;-- load path setup
;; Add blood to load path
(set-default-toplevel-value 'load-path (append
                                        (list (file-name-concat (getenv "HOME") ".emacs.d/blood"))
                                        (default-toplevel-value 'load-path)))

;;-- end load path setup

(require 'blood-early-init)
