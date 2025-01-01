;; blood--test.el -*- mode: emacs-lisp; lexical-binding: t; -*-
;;
;;
;;
(require 'buttercup)

(describe "identifiers"
  (it "is a sanity test" (expect t :to-be (not nil)))
  (it "should build"
    (expect (make-blood--identifier-s :profile 'test) :to-pass #'blood--identifier-s-p)
    )

  (it "should be able to construct symbol"
    (let* ((iden (make-blood--identifier-s :profile 'test :group   'blah :module  'bloo :package 'basic)))
      (expect (blood--id-sym nil :profile t) :to-raise 'error)
      (expect (blood--id-sym iden) :to-raise 'error)

      (expect (blood--id-sym iden :profile t) :to-be 'test)
      (expect (blood--id-sym iden :profile t :group t) :to-be 'test:blah)
      (expect (blood--id-sym iden :profile t :group t :module t) :to-be 'test:blah:bloo)
      (expect (blood--id-sym iden :profile t :group t :module t :package t) :to-be 'test:blah:bloo:basic)
      (expect (blood--id-sym iden :profile t :group nil :module t :package t) :to-be 'test:bloo:basic)
      (expect (blood--id-sym iden :profile nil :package nil :group t :module t) :to-be 'blah:bloo)
      )
    )
  (it "should be able to build from a path"
    (expect (blood-build-id-from-path "simple" "/blah/bloo/blee/example.el") :to-pass #'blood--identifier-s-p)
    )
  (it "should record the originating path"
    (expect (blood--identifier-s-source (blood-build-id-from-path "simple" "/blah/bloo/blee/example.el")) :to-equal "/blah/bloo/blee/example.el")
    )
  )

;; test advice
(describe "advice"
  ;; Vars:
  :var (a)
  ;; Setup
  (before-each nil)
  ;; Teardown
  (after-each nil)
  ;; Specs:
  (it "is a sanity test" (expect t :to-be (not nil)))
  (it "should build"
    (expect (blood-build-advice '(:targets #'message :advisors nil) :to-pass #'blood--advice-s-p)))
  (it "should fail on bad arguments"
    (expect (blood-build-advice '(:targets #'message :advisors ((:around)))) :to-raise 'error)
    (expect (blood-build-advice '(:targets #'message :advisors ((:bad-target #'bloo)))) :to-raise 'error)
    )
)

;; test paths
(describe "paths"
  ;; Vars:
  :var (a)
  ;; Setup
  (before-each nil)
  ;; Teardown
  (after-each nil)
  ;; Specs:
  (it "is a sanity test" (expect t :to-be (not nil)))
  (it "should error on bad args"
    (expect (blood-build-paths '()) :to-raise 'error)
    )
  (it "should build"
    (expect (blood-build-paths '(:modules ".")) :to-pass #'blood--paths-s-p)
    )
  (it "should have default locations"
    (let ((res (blood-build-paths '(:modules "."))))
      (expect (car (blood--paths-s-modules res)) :to-equal ".")
      (expect (car (blood--paths-s-cache res))   :to-equal blood--cache-loc-default)
      (expect (car (blood--paths-s-install res)) :to-equal blood--install-loc-default)
      (expect (car (blood--paths-s-build res))   :to-equal blood--build-loc-default)
      (expect (car (blood--paths-s-secrets res)) :to-equal blood--secrets-loc-default)
      )
    )

)
