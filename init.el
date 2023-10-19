;;; example-init.el -*- lexical-binding: t; -*-

(hlog! "init.el")

;; Profile declarations with module activations:
(blood! :profile basic :default t
        :modules-from ("./example")
        ;; :install-to  nil
        ;; :build-to nil
        ;; :disallow (mod-feature keybindings)
        :on-activation (
                        (message "blah")
                        )
        :active-modules:
        :blood example
  )

(blood! :profile quick :disabled nil
        :package-installation "quick"
        :active-modules:
        :group2 mod2
  )

(blood! :profile blah :disabled t
        )

(install! 'cl-lib) ;; profile-indpendent package requirement

(ilog! "")
(ghlog! "Current Variable Assignments")
(dolist (loc '(native-comp-eln-load-path data-directory doc-directory exec-directory
	       installation-directory invocation-directory invocation-name source-directory
               shared-game-score-directory noninteractive blood--cmd
               ))
  (ilog! "%-30s : %s" (symbol-name loc) (symbol-value loc))
  )
(glogx!)

(add-hook 'after-init-hook (lambda () (hlog! "Init Loaded, Processing..."))
          (plist-get blood--hook-laziness :bootstrap))
(add-hook 'after-init-hook (lambda () (switch-to-buffer "*Messages*"))
          (plist-get blood--hook-laziness :user-max))
