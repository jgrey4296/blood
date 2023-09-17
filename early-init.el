;; early-init.el -*- lexical-binding: t; -*-

;; Important variables, functions, and locations:
;; startup--load-user-init-file
;; user-emacs-directory
;; user-init-file
;; command-line-functions
(message "-------------------- Blood: Early Init")
(message "args: %s" command-line-args)

;;-- arg processing
(let (processed-cli-args
      cmd
      profile)
  (while command-line-args
    (cond ((equal "--sync" (car command-line-args))
           (setq cmd 'sync
                 noninteractive nil)
           ;; make non-interactive
           )
          ((equal "--profile"(car command-line-args))
           (setq profile (pop command-line-args))
           )
          ((equal "--profiles" (car command-line-args))
           ;; TODO list the available profiles
           nil
           )
          ((equal "--report" (car command-line-args))
           (setq cmd 'report)
           )
          ((equal "--batch" (car command-line-args))
           (setq cmd 'batch)
           )
          ((equal "--clean" (car command-line-args))
           (setq cmd 'clean)
           )
          (t (push (car command-line-args) processed-cli-args))
          )
    (pop command-line-args)
    )
  (defvar blood--cmd (or cmd 'run) "the startup command")
  (defvar blood-profile--default profile "The current profile")
  (setq command-line-args (reverse processed-cli-args))
  (message "BLOOD: (profile %s) (command %s)" blood-profile--default blood--cmd)
  )

;;-- end arg processing

(defconst WIN-TYPES '(cygwin windows-ms ms-dos))

(defconst MAC-TYPES '(darwin))

(defconst BSD-TYPES '(darwin berkeley-unix gnu/kfreebsd))

(defconst BLOOD-USER-DIR-ENV-VAR "BLOODDIR")

;;-- debug startup
;; Recognize and setup debugging:
(when (or (getenv-internal "DEBUG") init-file-debug)
  (princ "Setting Debug")
  (setq init-file-debug t
        debug-on-error  t
        )
  ;; todo - load-file tracking
  )

;;-- end debug startup

;;-- startup vars
;; pre-Startup Performance adjustments
(setq gc-cons-threshold            most-positive-fixnum ;; Don't run gc till after startup
      load-prefer-newer            noninteractive       ;; Don't check bytecode times
      frame-inhibit-implied-resize t                    ;; don't resize till after startup
                                                        ;; todo - delay tty-run-terminal-initialization
                                                        ;; todo - disable tool-bar-setup
                                                        ;; todo - disable any other modes?
      package-enable-at-startup                    nil
      auto-mode-case-fold                          nil

      no-native-compile                            t
      native-comp-deferred-compilation             nil
      comp-enable-subr-trampolines                 nil
      package-native-compile nil
      native-comp-deferred-compilation-deny-list '(".")
      native-comp-bootstrap-deny-list            '(".")
      native-comp-async-jobs-number 1
      comp-files-queue nil
      native-comp-eln-load-path (list (file-name-as-directory (expand-file-name comp-native-version-dir invocation-directory)))
      )


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

;;-- load path setup
;; Add blood to load path
;; (unless (getenv BLOOD-USER-DIR-ENV-VAR)
;;   (error "No blood found"))

(set-default-toplevel-value 'load-path
                            (cons (or (getenv BLOOD-USER-DIR-ENV-VAR)
                                      (file-name-concat (getenv "HOME") ".emacs.d"))
                                   (cons (file-name-concat (getenv "HOME") ".emacs.d/blood")
                                         (default-toplevel-value 'load-path))))

;;-- end load path setup

(message "\n\nLoad Path: %s" load-path)
(message  "ELN: %s" native-comp-eln-load-path)
(message "ELN Queue: %s" comp-files-queue)
(message "\n\nUser Emacs Dir: %s" user-emacs-directory)
(message "User Init: %s" user-init-file)

;;-- core package requires
(require 'cl-lib)
(require 'blood-utils)
(require 'blood-core)
(require 'blood-hooks)
(require 'blood-deferral)
(require 'blood-profile)
(require 'blood-bootstrap)

;;-- end core package requires
