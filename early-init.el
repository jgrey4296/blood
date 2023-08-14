;;; early-init.el -*- lexical-binding: t; -*-

;; Important variables, functions, and locations:
;; startup--load-user-init-file
;; user-emacs-directory
;; user-init-file
;; command-line-functions

;; Recognize and setup debugging:
(when (or (getenv-internal "DEBUG") init-file-debug)
  (setq init-file-debug t
        debug-on-error  t
        )
  ;; todo - load-file tracking

  )

;; pre-Startup Performance adjustments
(setq gc-cons-threshold most-positive-fixnum ;; Don't run gc till after startup
      load-prefer-newer noninteractive       ;; Don't check bytecode times
      frame-inhibit-implied-resize t         ;; don't resize till after startup
      ;; todo - delay tty-run-terminal-initialization
      ;; todo - disable tool-bar-setup
      ;; todo - disable any other modes?
      )

;; From Doom: Stricter security defaults
(setq gnutls-verify-error noninteractive
      gnutls-algorithm-priority (when (boundp 'libgnutls-version)
                                  (concat "SECURE128:+SECURE192:-VERS-ALL"
                                          (if (and (not IS-WINDOWS)
                                                   (>= libgnutls-version 30605))
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
      )


;; Add re-doom to load path
(if (getenv "REDOOMDIR")
    (set-default-toplevel-value 'load-path
                                (cons (getenv "REDOOMDIR")
                                      (default-toplevel-value 'load-path)))
    )

(require 're-doom-bootstrap)
(require 're-doom-lib)