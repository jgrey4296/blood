;;; blood-early-init.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header

(require 'emacs "29.1")
(require 'cl-lib)
(require 'subr-x)
(require 'blood-log)

(hlog! "Early Init: %s" (getenv "TERM"))
(ilog! "CLI args: %s" command-line-args)

(require 'blood-defs)

;;-- arg processing
;; Doing this early, so a set profile or command is usable in the init file.
(let (processed-cli-args cmd profile top)
  (while command-line-args
    (setq top (pop command-line-args))
    (cond ((equal "--sync"     top) (setq blood--cmd 'sync))
          ((equal "--profile"  top) (setq blood-profile--default (pop command-line-args)))
          ((equal "--profiles" top) (setq blood--cmd 'profiles))
          ((equal "--stub"     top) (setq blood--cmd 'stub))
          ((equal "--report"   top) (setq blood--cmd 'report))
          ((equal "--batch"    top) (setq blood--cmd 'batch))
          ((equal "--clean"    top) (setq blood--cmd 'clean))
          ((equal "-h"         top) (setq blood--cmd 'help))
          (t (push             top processed-cli-args))
          )
    )
  (setq command-line-args (reverse processed-cli-args)
        ;; noninteractive (seq-contains-p blood--noninteractive-cmds blood--cmd)
        inhibit-message t
        )
  (hlog! "BLOOD")
  (ilog! "Profile: %s" blood-profile--default)
  (ilog! "Command: %s" blood--cmd)
  (ilog! "Remaining CLI Args:  %s"  command-line-args)
  (hlog! "-----")
  )

;;-- end arg processing

;;-- debug startup
;; Recognize and setup debugging:
(when (or (getenv "DEBUG") init-file-debug)
  (hlog! "Setting Debug")
  (setq init-file-debug t
        debug-on-error  t
        noninteractive nil
        blood--trace-memory t
        )
  ;; todo - load-file tracking
  )

;;-- end debug startup

(blood--force-terminal)
;;-- startup vars
;; pre-Startup Performance adjustments
(setq gc-cons-threshold            most-positive-fixnum ;; Don't run gc till after startup
      load-prefer-newer            noninteractive       ;; Don't check bytecode times
      frame-inhibit-implied-resize t                    ;; don't resize till after startup
                                                        ;; todo - delay tty-run-terminal-initialization
                                                        ;; todo - disable tool-bar-setup
                                                        ;; todo - disable any other modes?
      package-enable-at-startup                    nil  ;; don't use built in package.el
      inhibit-startup-screen                       t
      auto-mode-case-fold                          nil

      no-byte-compile                              t
      no-native-compile                            t
      comp-no-spawn                                t
      native-comp-jit-compilation                         nil
      native-comp-always-compile                          nil
      native-comp-enable-subr-trampolines                 nil
      package-native-compile                              nil
      comp-files-queue                                    nil
      async-bytecomp-allowed-packages                     nil
      native-comp-jit-compilation-deny-list      '(".")
      native-comp-bootstrap-deny-list            '(".")
      native-comp-async-jobs-number 1
      ;; native-comp-eln-load-path       nil ;;(list (file-name-as-directory (expand-file-name comp-native-version-dir invocation-directory)))
      auto-save-list-file-prefix (expand-file-name "autosave/.saves-" blood-cache-dir)

      ;; for any initial caches
      user-emacs-directory (expand-file-name "~/.cache/blood/profiles/startup")
      )

(startup-redirect-eln-cache (expand-file-name blood--eln-cache-name blood-cache-dir))

;; From Doom: Stricter security defaults
(setq gnutls-verify-error noninteractive
      gnutls-algorithm-priority (when (boundp 'libgnutls-version)
                                  (concat "SECURE128:+SECURE192:-VERS-ALL"
                                          (if (and (not (memq system-type WIN-TYPES)) (>= libgnutls-version 30605))
                                              ":+VERS-TLS1.3")
                                          ":+VERS-TLS1.2"))
      gnutls-min-prime-bits 3072 ;; `gnutls-min-prime-bits' is set based on recommendations from https://www.keylength.com/en/4/
      tls-checktrust gnutls-verify-error
      ;; Emacs is built with gnutls.el by default, so `tls-program' won't
      ;; typically be used, but in the odd case that it does, we ensure a more
      ;; secure default for it (falling back to `openssl' if absolutely
      ;; necessary). See https://redd.it/8sykl1 for details.
      tls-program '("openssl s_client -connect %h:%p -CAfile %t -nbio -no_ssl3 -no_tls1 -no_tls1_1 -ign_eof"
                    "gnutls-cli -p %p --dh-bits=3072 --ocsp --x509cafile=%t --strict-tofu --priority='SECURE192:+SECURE128:-VERS-ALL:+VERS-TLS1.2:+VERS-TLS1.3' %h"
                    "gnutls-cli -p %p %h" ;; compatibility fallbacks
                    )
      server-auth-dir (if (getenv "SECRETSDIR")
                          (expand-file-name "emacs" (getenv "SECRETSDIR"))
                        (expand-file-name "~/.config/secrets/emacs/"))
      )

;;-- end startup vars

(glog! "Early Init Values")
(ilog! "Load Path: %s\n\n" load-path)
(ilog! "ELN: %s\n" native-comp-eln-load-path)
(ilog! "User Emacs Dir: %s" user-emacs-directory)
(ilog! "User Init: %s" user-init-file)
(glogx!)

;;-- core package requires
(glog! "Loading Core Packages")
(require 'blood-deferral)
(require 'blood-structs)
(require 'blood-utils)
(require 'blood-backend)
(require 'blood-bootstrap)

(require 'blood-packages)
(require 'blood-modules)
(require 'blood-core)
(require 'blood-hooks)
(require 'blood-profile)
(require 'blood-cmds)
(glogx!)
;;-- end core package requires

(loaded? blood-defs blood-log blood-utils blood-deferral blood-core blood-hooks blood-profile blood-bootstrap blood-cmds)

(provide 'blood-early-init)

;;-- Footer
;; Copyright (C) 2024 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    January 04, 2024
;; Modified:   January 04, 2024
;; Keywords: :
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 29.1))
;;
;; This file is not part of GNU Emacs.
;;
;;-- end Footer
;;; blood-early-init.el ends here
