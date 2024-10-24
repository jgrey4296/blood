;; blood--test.el -*- mode: emacs-lisp; lexical-binding: t; -*-
;;
;; General ERT tests for blood
;;
(require 'ert)
(require 'f)

(defun blood--test-structs--setup ()
  (add-to-list 'load-path (f-parent default-directory))
  (load "blood-structs")
  )

(ert-deftest blood--test-sanity ()
  "Tests a basic identifier construction"
  :tags '(sanity struct)
  (blood--test-structs--setup)
  (let ((iden (make-blood--identifier-s :profile 'test)))
    (should iden)
    (should (blood--identifier-s-p iden))
    (should (eq (blood--identifier-s-profile iden) 'test))
    )
)

;; test identifiers
(ert-deftest blood--test-structs-id-test ()
  "Tests identifier construction"
  :tags '(struct)
  (blood--test-structs--setup)
  (let* ((iden (make-blood--identifier-s :profile 'test
                                         :group   'blah
                                         :module  'bloo
                                         :package 'basic
                                         ))
         )
    (should-error (blood--id-sym nil :profile t))
    (should-error (blood--id-sym iden))

    (should (eq (blood--id-sym iden :profile t) 'test))
    (should (eq (blood--id-sym iden :profile t :group t) 'test:blah))
    (should (eq (blood--id-sym iden :profile t :group t :module t) 'test:blah:bloo))
    (should (eq (blood--id-sym iden :profile t :group t :module t :package t) 'test:blah:bloo:basic))
    (should (eq (blood--id-sym iden :profile t :group nil :module t :package t) 'test:bloo:basic))
    (should (eq (blood--id-sym iden :profile nil :package nil :group t :module t) 'blah:bloo))

    )
)

(ert-deftest blood--test-structs-id-path-test ()
  "Tests identifier construction from a path"
  :tags '(struct)
  (blood--test-structs--setup)
  (let ((res (blood-build-id-from-path "simple" "/blah/bloo/blee/example.el")))
    (should res)
    (should (blood--identifier-s-p res))
    (should (blood--identifier-s-source res))
    (should (eq (blood--identifier-s-group res) 'bloo))
    (should (eq (blood--identifier-s-module res) 'blee))
    (should (string-equal (blood--identifier-s-source res) "/blah/bloo/blee/example.el"))
  )
)

;; test advice
(ert-deftest blood--test-structs-advice-empty-struct-test ()
    "Basic Test of empty advice data"
    :tags '(struct)
    (blood--test-structs--setup)
    (let ((res (blood-build-advice '(:targets #'message :advisors nil))))
      (should-not res)
    )
)

(ert-deftest blood--test-structs-advice-struct-test ()
    "Basic Test of advice"
    :tags '(struct)
    (blood--test-structs--setup)
    (let ((res (blood-build-advice '(:targets #'message :advisors ((:around #'blah)))) ))
      (should res)
      (should (blood--advice-s-p res))
    )
)

(ert-deftest blood--test-structs-advice-fail-struct-test ()
    "Ensure Advice errors on bad data"
    :tags '(struct)
    (blood--test-structs--setup)
    (should-error (blood-build-advice '(:targets #'message :advisors ((:around)))))
    (should-error (blood-build-advice '(:targets #'message :advisors ((:bad-target #'bloo)))))
)

;; test paths
(ert-deftest blood--test-structs-paths-test ()
  "Tests paths collection"
  :tags '(struct)
  (blood--test-structs--setup)
  ;; Error on no modules loc
  (should-error (blood-build-paths '()))
  ;; Normal use with defaults
  (let ((res (blood-build-paths '(:modules "."))))
    (should (blood--paths-s-p res))
    (should (string-equal (car (blood--paths-s-modules res)) "."))
    (should (string-equal (car (blood--paths-s-cache res)) blood--cache-loc-default))
    (should (string-equal (car (blood--paths-s-install res)) blood--install-loc-default))
    (should (string-equal (car (blood--paths-s-build res)) blood--build-loc-default))
    (should (string-equal (car (blood--paths-s-secrets res)) blood--secrets-loc-default))
    )
  ;; multi module locations
  (cl-loop for pair in (-zip-pair (blood--paths-s-modules (blood-build-paths '(:modules ("../first" "../second"))))
                             (list "../first" "../second"))
           do (should (string-equal (car pair) (cdr pair)))
           )
  ;; Defaults overridden
  (let ((res (blood-build-paths '(:modules "."))))
    (should (blood--paths-s-p res))
    )
)


;; test recipe
(ert-deftest blood--test-structs-recipe-test ()
  "Tests basic recipe construction"
  :tags '(struct)
  (blood--test-structs--setup)
  (let ((recipe (blood-build-recipe '()))
        )
    (should (not (blood-build-recipe nil)))
    (should (eq (blood-build-recipe 'melpa) 'melpa))
    (should recipe)
    (should (blood--recipe-s recipe))
    )
)

;; test constraint
(ert-deftest blood--test-structs-constraint-test ()
  "Tests basic activation constraint definition"
  :tags '(struct)
  (blood--test-structs--setup)
  (should (equal 0 1))
)

;; test package
(ert-deftest blood--test-structs-package-test ()
  "Tests package declaraction"
  :tags '(struct)
  (blood--test-structs--setup)
  (let ((package (blood-build-package 'evil "a/group/module/file.el"
                                      '(:on-init (
                                                  (message "Test")
                                                  )
                                        )
                                      )
                 )
        )
    (should package)
    (should (blood--package-s-p package))
    (should (equal (blood-uniq-id package) 'group:module:evil))
    (should (functionp (blood--package-s-on-init package)))

    )
  )

;; test module
(ert-deftest blood--test-structs-module-test ()
  "Tests module declaration"
  :tags '(struct)
  (blood--test-structs--setup)
  (should (equal 0 1))
)

;; test profile building
(ert-deftest blood--test-structs-profile-test ()
  "Tests profile declaration"
  :tags '(struct)
  (blood--test-structs--setup)
  (should (equal 0 1))
)

