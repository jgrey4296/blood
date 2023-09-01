;;; example-init.el -*- lexical-binding: t; -*-

;; Profile declarations with module activations:
(re-doom! :profile basic :default t
          :modules-installation "blah"
          :disallow '(mod-feature keybindings)

          :active-modules
          (:group mod :allow x :disallow y)
          :group2 mod2

          )

(re-doom! :profile quick
          :package-installation "quick"

          :active-modules

          :group2 mod2
          )

(re-doom! :profile blah :disabled t
          )

(install! 'cl-lib) ;; profile-indpendent package requirement
