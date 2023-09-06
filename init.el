;;; example-init.el -*- lexical-binding: t; -*-

(message "-------------------- Starting init")

;; Profile declarations with module activations:
(blood! :profile basic :default t
          :modules-from ("./blah")
          :install-to  nil
          :build-to nil
          :disallow (mod-feature keybindings)
          :on-activation (
                          (message "blah")
                          )
          :active-modules
          :group mod :allow x :disallow y
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

(message "-------------------- Finished init")
