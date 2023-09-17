;;; example-init.el -*- lexical-binding: t; -*-

(ilog! "-------------------- Starting init")

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

(ilog! "-------------------- Finished init")
(dolist (loc '(native-comp-eln-load-path data-directory doc-directory exec-directory
	       installation-directory invocation-directory invocation-name source-directory shared-game-score-directory))
  (ilog! "%-30s : %s" (symbol-name loc) (symbol-value loc))
  )

(add-hook 'after-init-hook (lambda () (ilog! "Starting After Init Hook"))
          (plist-get blood--hook-laziness :bootstrap))
(add-hook 'after-init-hook (lambda () (switch-to-buffer "*Messages*"))
          (plist-get blood--hook-laziness :user-max))
