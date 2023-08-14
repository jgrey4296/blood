;; general-test.el -*- lexical-binding: t; -*-

(defvar jg-test-map (make-sparse-keymap))

(general-define-key
 :keymaps 'jg-test-map
 "a" (cmd! (message "test"))

 :prefix "b"
 "a" nil
 "b" (cmd! (message "other"))
 )

(general-create-definer jg-test-definer
  :keymaps 'jg-test-map
  :prefix "C"
  )

(jg-test-definer
  "d" (cmd! (message "blah"))
  )

(general-create-definer jg-test-evil-definer
  :keymaps 'jg-test-map
  :states 'normal
  )

(jg-test-evil-definer
  "e" (cmd! (message "other"))

  )
