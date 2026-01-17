;;; example-init.el -*- lexical-binding: t; -*-

(hlog! "init.el")

;; Profile declarations with module activations:
(blood! :profile basic :default t
        :paths (:cache nil :install nil :build nil :modules (".") :secrets nil)
        :backend straight
        :bootstrap (
                    (lambda (spec) (ilog! "Simple bootstrap fn"))
                    )
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

(blood! :profile quick :disabled t
        :package-installation "quick"
        :active-modules:
        :group2 mod2
  )

(blood! :profile blah :disabled t)

(install! 'cl-lib) ;; profile-indpendent package requirement

