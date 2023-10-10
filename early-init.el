;; early-init.el -*- lexical-binding: t; no-byte-compile: t; -*-

;; BLOOD early-init.


;;-- load path setup
;; Add blood to load path
(defconst BLOOD-USER-DIR-ENV-VAR "BLOODDIR")
(set-default-toplevel-value 'load-path (append
                                        (if (getenv BLOOD-USER-DIR-ENV-VAR) (list (getenv BLOOD-USER-DIR-ENV-VAR)))
                                        (list (file-name-concat (getenv "HOME") ".emacs.d/blood"))
                                        (default-toplevel-value 'load-path)))

;;-- end load path setup
(require 'cl-lib)
(require 'blood-utils)

(hlog! "Early Init")
(log! "CLI args: %s" command-line-args)

;;-- arg processing
;; Doing this early, so a set profile or command is usable in the init file.
(let (processed-cli-args cmd profile)
  (while command-line-args
    (cond ((equal "--sync" (car command-line-args)) (setq cmd 'sync))
          ((equal "--profile"(car command-line-args)) (setq profile (pop command-line-args)))
          ((equal "--profiles" (car command-line-args)) (setq cmd 'profiles))
          ((equal "--report"   (car command-line-args)) (setq cmd 'report))
          ((equal "--batch"    (car command-line-args)) (setq cmd 'batch))
          ((equal "--clean"    (car command-line-args)) (setq cmd 'clean))
          ((equal "-h"         (car command-line-args)) (setq cmd 'help))
          (t (push             (car command-line-args) processed-cli-args))
          )
    (pop command-line-args)
    )
  (defvar blood--cmd (or cmd 'run) "the startup command")
  (defvar blood-profile--default profile "The current profile")
  (setq command-line-args (reverse processed-cli-args)
        noninteractive (not (eq blood--cmd 'run))
        initial-window-system nil
        inhibit-message (not noninteractive)
        )
  (hlog! "BLOOD: (profile %s) (command %s) (remaining %s)" blood-profile--default blood--cmd command-line-args)
  )

;;-- end arg processing

;;-- debug startup
;; Recognize and setup debugging:
(when (or (getenv-internal "DEBUG") init-file-debug)
  (hlog! "Setting Debug")
  (setq init-file-debug t
        debug-on-error  t
        noninteractive nil
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
      native-comp-jit-compilation-deny-list      '(".")
      native-comp-bootstrap-deny-list            '(".")
      native-comp-async-jobs-number 1
      comp-files-queue                nil
      async-bytecomp-allowed-packages nil
      native-comp-jit-compilation     nil
      ;; native-comp-eln-load-path       nil ;;(list (file-name-as-directory (expand-file-name comp-native-version-dir invocation-directory)))
      auto-save-list-file-prefix "~/.cache/blood/autosave/.saves-"
      )

(startup-redirect-eln-cache "~/.cache/blood/eln")

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
      server-auth-dir (expand-file-name "~/.secrets")
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
