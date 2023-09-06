;;; example-init.el -*- lexical-binding: t; -*-

;; Profile declarations with module activations:
(blood! :profile basic :default t
          :modules-installation "blah"
          :disallow '(mod-feature keybindings)

          :active-modules
          (:group mod :allow x :disallow y)
          :group2 mod2

          )

(blood! :profile quick
          :package-installation "quick"

          :active-modules

          :group2 mod2
          )

(blood! :profile blah :disabled t
          )

(install! 'cl-lib) ;; profile-indpendent package requirement
