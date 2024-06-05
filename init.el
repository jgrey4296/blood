;;; example-init.el -*- lexical-binding: t; -*-

(hlog! "init.el")

;; Profile declarations with module activations:
(blood! :profile basic :default t
        :paths (:cache nil :install nil :build nil :modules ("./example") :secrets nil)
        :backend straight
        :bootstrap (#'fn1 #'fn2 #'fn3)
        :constraints (:emacs nil :os nil)
        :no-compile (package package2)
        :init (
               (message "Packages are about to be loaded")
               )
        :config (
                 (message "Packages have been loaded")
                 )
        :active-modules: (
                          (:blood example)
                          (:blood other :allow () :disallow ())
                          )
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
