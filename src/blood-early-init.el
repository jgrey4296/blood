;;; blood-early-init.el -*- lexical-binding: t; no-byte-compile: t; -*-

(require 'emacs "29.1")
(require 'cl-lib)
(require 'subr-x)
(require 'blood-log)

(hlog! "Early Init: %s" (getenv "TERM"))
(ilog! "CLI args: %s" command-line-args)

(require 'blood-defs)

;;-- arg processing
;; Doing this early, so a set profile or command is usable in the init file.
;; TODO allow --cmd=%s
(let (processed-cli-args cmd profile top)
  (while command-line-args
    (setq top (pop command-line-args))
    (cond ((string-equal "--sync"     top) (setq blood--cmd 'sync))
          ((string-equal "--profile"  top) (setq blood-profile--default (pop command-line-args)))
          ((string-equal "--profiles" top) (setq blood--cmd 'profiles))
          ((string-equal "--stub"     top) (setq blood--cmd 'stub))
          ((string-equal "--report"   top) (setq blood--cmd 'report))
          ((string-equal "--batch"    top) (setq blood--cmd 'batch))
          ((string-equal "--clean"    top) (setq blood--cmd 'clean))
          ((string-equal "-h"         top) (setq blood--cmd 'help))
          (t (push             top processed-cli-args))
          )
    )
  (setq command-line-args (reverse processed-cli-args)
        ;; noninteractive (seq-contains-p blood--noninteractive-cmds blood--cmd)
        inhibit-message t
        )
  )

;;-- end arg processing

;;-- debug startup
;; Recognize and setup debugging:
(when (or (getenv "DEBUG") init-file-debug)
  (ilog! "!!! Setting Debug")
  (setq init-file-debug t
        debug-on-error  t
        noninteractive nil
        blood--trace-memory t
        )
  (blood-set-loglevel :debug)
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
      package-enable-at-startup                    nil  ;; don't use built in package.el
      inhibit-startup-screen                       t
      auto-mode-case-fold                          nil

      no-byte-compile                              t
      no-native-compile                            t
      comp-no-spawn                                t
      native-comp-jit-compilation                  nil
      native-comp-always-compile                   nil
      native-comp-enable-subr-trampolines          nil
      package-native-compile                       nil
      comp-files-queue                             nil
      async-bytecomp-allowed-packages              nil
      native-comp-jit-compilation-deny-list        (list ".")
      native-comp-bootstrap-deny-list              (list ".")
      auto-save-list-file-prefix                   (expand-file-name "auto-save/.saves-" blood-cache-dir)

      ;; for any initial caches
      user-emacs-directory (expand-file-name "profiles/startup" blood-cache-dir)
      )

(startup-redirect-eln-cache (expand-file-name blood--eln-cache-name blood-cache-dir))

;; From Doom: Stricter security defaults
(setq gnutls-verify-error noninteractive
      gnutls-algorithm-priority (when (boundp 'libgnutls-version)
                                  (concat "SECURE128:+SECURE192:-VERS-ALL"
                                          (if (and (not (memq system-type WIN-TYPES)) (>= libgnutls-version 30605))
                                              ":+VERS-TLS1.3")
                                          ":+VERS-TLS1.2"))
      gnutls-min-prime-bits     3072 ;; `gnutls-min-prime-bits' is set based on recommendations from https://www.keylength.com/en/4/
      tls-checktrust            gnutls-verify-error
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
(ilog! "Load Path: %s\n\n"  load-path)
(ilog! "ELN: %s\n"          native-comp-eln-load-path)
(ilog! "User Emacs Dir: %s" user-emacs-directory)
(ilog! "User Init: %s"      user-init-file)
(glogxs!)

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

;;-- queue cmd
(if-let  ((cmd (intern-soft (format "blood--cmd-%s" blood--cmd))))
    (progn (ilog! "Setting up Cmd: %s" blood--cmd)
           (funcall cmd)
           )
  (log! :warn (format "Command Not Defined: %s" blood--cmd)))
;;-- end queue cmd

(add-hook 'after-init-hook #'blood--clear-log-prefix    (bloody-lazy! :finalize))
(add-hook 'after-init-hook #'blood--report-after-init-h (bloody-lazy! :bootstrap))
(add-hook 'after-init-hook #'blood--final-report-h      (bloody-lazy! :finalize))
(add-hook 'after-init-hook #'blood--write-messages-to-log-file-h (bloody-lazy! :finalize 1))
(hlog! "Early Init Complete")
(blood--write-messages-to-log-file-h)

(provide 'blood-early-init)
;;; blood-early-init.el ends here
