;; early-init.el -*- lexical-binding: t; no-byte-compile: t; -*-

;; BLOOD early-init.
;;-- load path setup
(defconst BLOOD-SRC-DIR-ENV-VAR "BLOODSRC")
(defconst BLOOD-USER-DIR-ENV-VAR "BLOODDIR")
(defcustom blood-config-dir (expand-file-name (or (getenv BLOOD-USER-DIR-ENV-VAR) "~/.config/blood/")) "directory for config files")
;; Add blood to load path
(set-default-toplevel-value 'load-path (append
                                        (list (or (getenv BLOOD-SRC-DIR-ENV-VAR) blood-config-dir))
                                        (list (file-name-concat (getenv "HOME") ".emacs.d/blood"))
                                        (default-toplevel-value 'load-path)))

;;-- end load path setup

(require 'cl-lib)
(require 'blood-utils)
(require 'blood-defs)

(log! :debug "Early Init: %s" (getenv "TERM"))
(log! :debug "CLI args: %s" command-line-args)

;;-- arg processing
;; Doing this early, so a set profile or command is usable in the init file.
(let (processed-cli-args cmd profile top)
  (while command-line-args
    (setq top (pop command-line-args))
    (cond ((equal "--sync" top) (setq blood--cmd 'sync))
          ((equal "--profile"top) (setq blood-profile--default (pop command-line-args)))
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
        noninteractive (seq-contains-p blood--noninteractive-cmds blood--cmd)
        inhibit-message (not noninteractive)
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
(when (or (getenv-internal "DEBUG") init-file-debug)
  (hlog! "Setting Debug")
  (setq init-file-debug t
        debug-on-error  t
        noninteractive nil
        blood--trace-memory t
        )
  ;; todo - load-file tracking
  )

;;-- end debug startup

(unless noninteractive (blood--force-terminal))

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
(require 'blood-core)
(require 'blood-hooks)
(require 'blood-deferral)
(require 'blood-profile)
(require 'blood-bootstrap)

;;-- end core package requires
